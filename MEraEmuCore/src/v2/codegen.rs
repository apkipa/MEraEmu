use std::borrow::Cow;

use cstree::interning::InternKey;
use cstree::interning::Interner;
use cstree::interning::Resolver;
use cstree::interning::TokenKey;
use hashbrown::HashMap;
use indexmap::IndexMap;
use itertools::Itertools;
use rclite::Rc;
use rustc_hash::{FxBuildHasher, FxHasher};
use smallvec::smallvec;
use yoke::Yoke;

use crate::v2::parser::*;
use crate::v2::routines;
use crate::{
    types::*,
    util::{
        rcstr::{self, ArcStr},
        Ascii,
    },
};

use EraBytecodeKind as BcKind;
use EraTokenKind as Token;

use super::interpret::EraInterpretError;
use super::interpret::EraInterpreter;
use super::lexer::EraLexer;

type FxHashMap<K, V> = HashMap<K, V, FxBuildHasher>;
type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

type CompileResult<T> = Result<T, ()>;

/// Generates bytecode from the AST. Also adds variables into the global pool.
pub struct EraCodeGenerator<'ctx, 'i, Callback> {
    ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
    trim_nodes: bool,
    keep_src: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct EraBcChunkCheckpoint(usize);

impl EraBcChunkCheckpoint {
    fn pos(&self) -> usize {
        self.0
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct EraBcChunkJumpPoint(usize);

impl EraBcChunkJumpPoint {
    fn complete(self, builder: &mut EraBcChunkBuilder, dest: EraBcChunkCheckpoint) {
        builder.complete_jump(self, dest);
    }

    fn complete_here(self, builder: &mut EraBcChunkBuilder) {
        builder.complete_jump_here(self);
    }

    /// Discards the jump point without completing it. This makes the jump equivalent to
    /// a no-op.
    fn discard(self) {
        std::mem::forget(self);
    }
}

// // Makes `EraBcChunkJumpPoint` linear
// impl Drop for EraBcChunkJumpPoint {
//     fn drop(&mut self) {
//         if std::thread::panicking() {
//             return;
//         }

//         panic!("unresolved jump point");
//     }
// }

#[derive(Debug, Default)]
struct EraBcChunkBuilder {
    bc: Vec<u8>,
    src_spans: Vec<SrcSpan>,
}

impl EraBcChunkBuilder {
    fn new() -> Self {
        Self {
            bc: Vec::new(),
            src_spans: Vec::new(),
        }
    }

    fn checkpoint(&self) -> EraBcChunkCheckpoint {
        EraBcChunkCheckpoint(self.bc.len())
    }

    fn get_len(&self) -> usize {
        self.bc.len()
    }

    fn finish(self) -> EraBcChunk {
        EraBcChunk::new(Default::default(), self.bc, self.src_spans)
    }

    fn rollback_to(&mut self, checkpoint: EraBcChunkCheckpoint) {
        assert!(checkpoint.0 <= self.bc.len(), "invalid checkpoint");
        self.bc.truncate(checkpoint.0);
        self.src_spans.truncate(checkpoint.0);
    }

    fn push_bc(&mut self, bc: EraBytecodeKind, span: SrcSpan) {
        let bc = bc.to_bytes();
        self.bc.extend_from_slice(&bc);
        self.src_spans
            .extend(std::iter::repeat(span).take(bc.len()));
    }

    fn push_u8(&mut self, value: u8, span: SrcSpan) {
        self.bc.push(value);
        self.src_spans.push(span);
    }

    fn push_u16(&mut self, value: u16, span: SrcSpan) {
        self.push_u8((value & 0xff) as u8, span);
        self.push_u8((value >> 8) as u8, span);
    }

    fn push_u32(&mut self, value: u32, span: SrcSpan) {
        self.push_u16((value & 0xffff) as u16, span);
        self.push_u16((value >> 16) as u16, span);
    }

    fn push_u64(&mut self, value: u64, span: SrcSpan) {
        self.push_u32((value & 0xffffffff) as u32, span);
        self.push_u32((value >> 32) as u32, span);
    }

    #[must_use]
    fn push_jump(&mut self, span: SrcSpan) -> EraBcChunkJumpPoint {
        let pos = self.bc.len();
        let bc = BcKind::JumpWW { offset: 0 };
        self.push_bc(
            BcKind::JumpWW {
                offset: bc.bytes_len() as _,
            },
            span,
        );
        EraBcChunkJumpPoint(pos)
    }

    #[must_use]
    fn push_jump_if(&mut self, span: SrcSpan) -> EraBcChunkJumpPoint {
        let pos = self.bc.len();
        let bc = BcKind::JumpIfWW { offset: 0 };
        self.push_bc(
            BcKind::JumpIfWW {
                offset: bc.bytes_len() as _,
            },
            span,
        );
        EraBcChunkJumpPoint(pos)
    }

    #[must_use]
    fn push_jump_if_not(&mut self, span: SrcSpan) -> EraBcChunkJumpPoint {
        let pos = self.bc.len();
        let bc = BcKind::JumpIfNotWW { offset: 0 };
        self.push_bc(
            BcKind::JumpIfNotWW {
                offset: bc.bytes_len() as _,
            },
            span,
        );
        EraBcChunkJumpPoint(pos)
    }

    fn complete_jump(&mut self, jump_point: EraBcChunkJumpPoint, dest: EraBcChunkCheckpoint) {
        let mut delta: i32 = dest
            .0
            .abs_diff(jump_point.0)
            .try_into()
            .expect("jump offset overflow");
        if dest.0 < jump_point.0 {
            delta = -delta;
        }

        // Copy to bytecode
        // NOTE: We assume 4-byte offset size
        self.bc[jump_point.0 + 1..jump_point.0 + 5].copy_from_slice(&delta.to_ne_bytes());

        std::mem::forget(jump_point);
    }

    fn complete_jump_here(&mut self, jump_point: EraBcChunkJumpPoint) {
        let dest = self.checkpoint();
        self.complete_jump(jump_point, dest);
    }

    fn push_load_imm(&mut self, imm: i64, span: SrcSpan) {
        if let Ok(imm) = imm.try_into() {
            self.push_bc(BcKind::LoadImm8 { imm }, span);
        } else if let Ok(imm) = imm.try_into() {
            self.push_bc(BcKind::LoadImm16 { imm }, span);
        } else if let Ok(imm) = imm.try_into() {
            self.push_bc(BcKind::LoadImm32 { imm }, span);
        } else {
            self.push_bc(BcKind::LoadImm64 { imm }, span);
        }
    }

    fn push_pop_all(&mut self, count: u8, span: SrcSpan) {
        let bc = match count {
            0 => return,
            1 => BcKind::Pop,
            _ => BcKind::PopAllN { count },
        };
        self.push_bc(bc, span);
    }

    fn push_pop_one(&mut self, idx: u8, span: SrcSpan) {
        assert_ne!(idx, 0, "pop_one with idx 0");
        let bc = match idx {
            1 => BcKind::Pop,
            _ => BcKind::PopOneN { idx },
        };
        self.push_bc(bc, span);
    }

    fn push_duplicate_all(&mut self, count: u8, span: SrcSpan) {
        let bc = match count {
            0 => return,
            1 => BcKind::Duplicate,
            _ => BcKind::DuplicateAllN { count },
        };
        self.push_bc(bc, span);
    }

    fn push_duplicate_one(&mut self, idx: u8, span: SrcSpan) {
        assert_ne!(idx, 0, "duplicate_one with idx 0");
        let bc = match idx {
            1 => BcKind::Duplicate,
            _ => BcKind::DuplicateOneN { idx },
        };
        self.push_bc(bc, span);
    }

    fn push_build_string(&mut self, count: u8, span: SrcSpan) {
        let bc = match count {
            1 => return,
            _ => BcKind::BuildString { count },
        };
        self.push_bc(bc, span);
    }

    fn push_load_const_str(&mut self, token_key: TokenKey, span: SrcSpan) {
        self.push_bc(
            BcKind::LoadConstStr {
                idx: token_key.into_u32(),
            },
            span,
        );
    }
}

// pub struct EraCodeCompilation {
//     pub bc_chunks: Vec<EraBcChunk>,
//     pub func_entries: FxIndexMap<&'static Ascii<str>, Option<EraFuncInfo>>,
// }

/// Describes the destination of an parameter in a function prototype.
struct EraFuncArgsBinding {
    target: TokenKey,
    span: SrcSpan,
    dims: EraVarDims,
    // init: ScalarValue,
}

struct EraFuncPrebuildInfo {
    func_idx: u32,
    name: TokenKey,
    name_str: ArcStr,
    args: Vec<EraFuncArgsBinding>,
    event_kind: Option<EraEventFuncKind>,
    // (init_val_idx, var_span)
    dyn_vars: Vec<(u32, SrcSpan)>,
}

impl<'ctx, 'i, Callback: EraCompilerCallback> EraCodeGenerator<'ctx, 'i, Callback> {
    pub fn new(
        ctx: &'ctx mut EraCompilerCtx<'i, Callback>,
        trim_nodes: bool,
        keep_src: bool,
    ) -> Self {
        Self {
            ctx,
            trim_nodes,
            keep_src,
        }
    }

    /// Compiles given programs into bytecode chunks. Will also merge result into global context.
    /// Note that this function will return function entries that have relative chunk indices,
    /// and the caller should convert them as needed.
    pub fn compile_merge_many_programs(
        &mut self,
        filenames: &[ArcStr],
        mut progress_callback: impl FnMut(usize),
    ) {
        let mut prebuild_funcs: FxIndexMap<
            &'i Ascii<str>,
            (EraArcNodeRef, EraFuncPrebuildInfo, Option<EraFuncInfo<'i>>),
        > = Default::default();

        let bc_chunks = Rc::get_mut(&mut self.ctx.bc_chunks).unwrap();
        let sources = filenames
            .iter()
            .map(|filename| {
                let program = Rc::get_mut(&mut self.ctx.source_map)
                    .unwrap()
                    .get_mut(filename)
                    .expect("source not found");
                // TODO: Support keeping node_arena in src_map (by warpping it in Arc) (trim_nodes == false)
                let (program_ref, node_arena) = match program.ast_data.take() {
                    Some(x) => x,
                    None => {
                        // Rebuild AST
                        let is_header = program.is_header;
                        let src = program
                            .text
                            .as_deref()
                            .map(Cow::Borrowed)
                            .unwrap_or_else(|| {
                                let compressed = program
                                    .compressed_text
                                    .as_ref()
                                    .expect("compressed text not set");
                                let src = lz4_flex::decompress_size_prepended(compressed).unwrap();
                                Cow::Owned(String::from_utf8(src).unwrap())
                            });
                        let mut lexer = EraLexer::new(program.filename.clone(), &src, false);
                        let mut is_str_var_fn = |x: &str| {
                            self.ctx
                                .variables
                                .get_var(x)
                                .map_or(false, |x| x.kind().is_str())
                        };
                        let mut parser = EraParser::new(
                            &mut self.ctx.callback,
                            &mut lexer,
                            self.ctx.node_cache.interner(),
                            &self.ctx.global_replace,
                            &self.ctx.global_define,
                            &mut is_str_var_fn,
                            is_header,
                        );
                        let parsed_program = parser.parse_program();
                        (parsed_program.root_node, parsed_program.nodes)
                    }
                };
                // Reuse chunk or allocate new chunk
                let chunk_idx = bc_chunks
                    .iter()
                    .position(|x| x.name == *filename)
                    .unwrap_or_else(|| {
                        let chunk_idx = bc_chunks.len();
                        bc_chunks.push(EraBcChunk::new(filename.clone(), Vec::new(), Vec::new()));
                        chunk_idx
                    }) as u32;
                let program = EraArcNodeRef::new(program_ref, node_arena.into());
                (chunk_idx, filename, program)
            })
            .collect_vec();
        let sources_iter = sources
            .iter()
            .map(|(chunk_idx, filename, program)| (*chunk_idx, *filename, program));
        let _: u32 = bc_chunks.len().try_into().expect("too many programs");
        for (chunk_idx, _, _) in sources_iter.clone() {
            bc_chunks[chunk_idx as usize].clear();
        }

        // Prebuild functions and generate function table
        let mut site = EraCodeGenPrebuildSite::new(self);
        for (chunk_idx, filename, program) in sources_iter.clone() {
            site.o.ctx.active_source = filename.clone();

            let EraNode::Program(program_extra) = program.get() else {
                unreachable!();
            };
            let arena = program.arena();
            for item in program.arena().get_extra_data_view(program_extra) {
                let item_ref = EraNodeRef(*item);
                let Some(item) = EraNodeItemFunction::try_get_from(arena, item_ref) else {
                    continue;
                };
                let Ok((prebuild_info, mut func_info)) =
                    site.prebuild_function(arena, &item, |site, name| {
                        let func_info = prebuild_funcs
                            .get(Ascii::new_str(name))
                            .map(|x| x.2.as_ref().unwrap())
                            .or_else(|| {
                                site.o
                                    .ctx
                                    .func_entries
                                    .get(Ascii::new_str(name))
                                    .map(|x: &Option<EraFuncInfo<'_>>| x.as_ref().unwrap())
                            });
                        func_info.map(|x| {
                            let filename = site.o.ctx.bc_chunks[x.chunk_idx as usize].name.clone();
                            (filename, x.name_span)
                        })
                    })
                else {
                    continue;
                };
                let name = site.o.ctx.resolve_str(prebuild_info.name);
                func_info.chunk_idx = chunk_idx;
                // eprintln!("[Add func {}, span={:?}]", name, func_info.name_span);
                let item = EraArcNodeRef::new(item_ref, arena.clone());
                prebuild_funcs.insert(Ascii::new_str(name), (item, prebuild_info, Some(func_info)));
            }
        }
        drop(site);

        // Merge prebuild functions into global function table
        let func_entries = Rc::get_mut(&mut self.ctx.func_entries).unwrap();
        for (func_name, (_, prebuild_info, func_info)) in &mut prebuild_funcs {
            let func_info = func_info.take().unwrap();
            let (idx, _) = func_entries.insert_full(func_name, Some(func_info));
            prebuild_info.func_idx = idx.try_into().expect("too many functions");
        }
        _ = func_entries;

        let mut compile_units = FxIndexMap::<_, (EraBcChunkBuilder, Vec<_>)>::default();
        let funcs_cnt = prebuild_funcs.len();
        for (name, (node, prebuild_info, _)) in prebuild_funcs {
            let func_idx = prebuild_info.func_idx;
            let chunk_idx = self
                .ctx
                .func_entries
                .get_index(func_idx as _)
                .unwrap()
                .1
                .as_ref()
                .unwrap()
                .chunk_idx;
            let unit = compile_units.entry(chunk_idx).or_default();
            unit.1.push((name, node.to_owned(), prebuild_info));
        }

        drop(sources);

        // Now start compiling functions
        let mut current_progress = 0;
        let mut func_offsets = Vec::with_capacity(funcs_cnt);
        for (chunk, funcs) in compile_units.values_mut() {
            let funcs = std::mem::take(funcs);
            for (name, node, prebuild_info) in funcs {
                let func_idx = prebuild_info.func_idx;
                let func_info = self
                    .ctx
                    .func_entries
                    .get_index(func_idx as _)
                    .unwrap()
                    .1
                    .as_ref()
                    .unwrap();
                let chunk_idx = func_info.chunk_idx;
                let filename = self.ctx.bc_chunks[chunk_idx as usize].name.clone();
                self.ctx.active_source = filename.clone();
                let func_offset: u32 = chunk
                    .get_len()
                    .try_into()
                    .expect("function offset overflow");
                EraCodeGenSite::gen_function(self, filename, chunk, node, prebuild_info);
                let func_size = chunk.get_len() - func_offset as usize;
                func_offsets.push((func_idx, func_offset, func_size as _));
                chunk.push_bc(BcKind::DebugBreak, SrcSpan::default());
            }

            // Update progress
            current_progress += 1;
            progress_callback(current_progress);
        }

        self.ctx.active_source = ArcStr::default();

        // Finialize bytecode chunks
        let func_entries = Rc::get_mut(&mut self.ctx.func_entries).unwrap();
        for (func_idx, func_offset, func_size) in func_offsets {
            let func_info = func_entries
                .get_index_mut(func_idx as _)
                .unwrap()
                .1
                .as_mut()
                .unwrap();
            func_info.bc_offset = func_offset;
            func_info.bc_size = func_size;
        }
        let bc_chunks = Rc::get_mut(&mut self.ctx.bc_chunks).unwrap();
        for (chunk_idx, (chunk, _)) in compile_units {
            let mut chunk = chunk.finish();
            chunk.name = bc_chunks[chunk_idx as usize].name.clone();
            bc_chunks[chunk_idx as usize] = chunk;
        }
    }
}

struct EraCodeGenPrebuildSite<'o, 'ctx, 'i, Callback> {
    o: &'o mut EraCodeGenerator<'ctx, 'i, Callback>,
    var_local_size: u32,
    var_locals_size: u32,
    var_arg_size: u32,
    var_args_size: u32,
}

impl<'o, 'ctx, 'i, Callback: EraCompilerCallback> EraCodeGenPrebuildSite<'o, 'ctx, 'i, Callback> {
    fn new(o: &'o mut EraCodeGenerator<'ctx, 'i, Callback>) -> Self {
        let var_local_size = o.ctx.variables.get_var("LOCAL").unwrap().dims().unwrap()[0];
        let var_locals_size = o.ctx.variables.get_var("LOCALS").unwrap().dims().unwrap()[0];
        let var_arg_size = o.ctx.variables.get_var("ARG").unwrap().dims().unwrap()[0];
        let var_args_size = o.ctx.variables.get_var("ARGS").unwrap().dims().unwrap()[0];

        Self {
            o,
            var_local_size,
            var_locals_size,
            var_arg_size,
            var_args_size,
        }
    }

    /// Prebuilds a function, generating variables and some basic information.
    fn prebuild_function(
        &mut self,
        arena: &EraNodeArena,
        node: &EraNodeItemFunction,
        lookup_func_fn: impl Fn(&mut Self, &str) -> Option<(ArcStr, SrcSpan)>,
    ) -> CompileResult<(EraFuncPrebuildInfo, EraFuncInfo<'i>)> {
        let filename = self.o.ctx.active_source.clone();
        let make_diag_fn = || Diagnostic::with_file(filename.clone());

        let EraNode::Identifier(mut name) = arena.get_node(node.name) else {
            return Err(());
        };
        let name_span = arena.get_node_span(node.name);
        let name_str = self.o.ctx.node_cache.interner().resolve(name);
        let mut event_func_kind =
            routines::is_event_name(&name_str).then_some(EraEventFuncKind::Normal);
        let mut name_str = ArcStr::from(name_str);
        let EraNode::ListSharpDecl(decls) = arena.get_node(node.sharp_decls) else {
            unreachable!();
        };
        let decls = arena.get_extra_data_view(decls);
        if event_func_kind.is_some() {
            // Scan for event kind declarations first
            for decl in decls.iter().map(|x| EraNodeRef(*x)) {
                let decl_span = arena.get_node_span(decl);
                let EraNode::DeclEventKind(kind) = arena.get_node(decl) else {
                    continue;
                };
                let kind: EraEventFuncKind = kind.0;
                if event_func_kind != Some(EraEventFuncKind::Normal) {
                    let mut diag = make_diag_fn();
                    diag.span_err(
                        Default::default(),
                        decl_span,
                        "redundant event kind declaration",
                    );
                    diag.emit_to(self.o.ctx);
                    continue;
                }
                event_func_kind = Some(kind);
            }

            // HACK: Rename function based on event kind
            // TODO: Maybe support multiple declarations of event functions with same event kind?
            name_str = rcstr::format!("{}@{}", name_str, event_func_kind.unwrap());
            name = self.o.ctx.interner().get_or_intern(&name_str);
        }
        if let Some((prev_func_filename, prev_func_span)) = lookup_func_fn(self, &name_str) {
            let mut diag = make_diag_fn();
            diag.span_err(
                Default::default(),
                name_span,
                format!("redefinition of function `{}`", name_str),
            );
            diag.span_note(
                prev_func_filename,
                prev_func_span,
                "previous definition here",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }

        let mut var_local_size = self.var_local_size;
        let mut var_locals_size = self.var_locals_size;
        let mut var_arg_size = self.var_arg_size;
        let mut var_args_size = self.var_args_size;

        // Generate function info
        let mut func_info = EraFuncInfo {
            name,
            name_span,
            frame_info: Default::default(),
            chunk_idx: 0,
            bc_offset: 0,
            bc_size: 0,
            ret_kind: ScalarValueKind::Void,
            is_transient: false,
        };
        let interner = self.o.ctx.interner();
        let mut interp = EraInterpreter::new(self.o.ctx, arena, true);

        let mut add_frame_var_or_diag = |interp: &mut EraInterpreter<Callback>,
                                         name: TokenKey,
                                         var_info: EraFuncFrameVarInfo|
         -> CompileResult<()> {
            let name_span = var_info.span;
            let name_str = interner.resolve(name);
            if func_info
                .frame_info
                .vars
                .insert(Ascii::new_str(name_str), var_info)
                .is_some()
            {
                let mut diag = make_diag_fn();
                diag.span_err(
                    Default::default(),
                    name_span,
                    format!("redefinition of variable `{}`", name_str),
                );
                diag.emit_to(interp.get_ctx_mut());
                return Err(());
            }

            // Warn about variable shadowing
            if let Some(var) = interp.get_ctx().variables.get_var(name_str) {
                // let var_span = var.src_span();
                let mut diag = make_diag_fn();
                diag.span_warn(
                    Default::default(),
                    name_span,
                    "this declaration shadows a global variable with the same name",
                );
                // diag.span_note(
                //     Default::default(),
                //     var_span,
                //     "previous definition here",
                // );
                diag.emit_to(interp.get_ctx_mut());
            }
            if interp
                .get_ctx()
                .csv_indices
                .contains_key(Ascii::new_str(name_str))
            {
                let mut diag = make_diag_fn();
                diag.span_warn(
                    Default::default(),
                    name_span,
                    "this declaration might be shadowed by a CSV index name",
                );
                diag.emit_to(interp.get_ctx_mut());
            }

            Ok(())
        };

        // Stage 1 - Process (variable) declarations
        for decl in decls.iter().map(|x| EraNodeRef(*x)) {
            // TODO: Emuera doesn't support referencing local variables in declarations,
            //       even when they are declared CONST. Maybe we can support this?
            //       (evaluation with partial function context)
            let decl_span = arena.get_node_span(decl);
            match arena.get_node(decl) {
                EraNode::DeclFunction => {
                    func_info.ret_kind = ScalarValueKind::Int;
                }
                EraNode::DeclFunctionS => {
                    func_info.ret_kind = ScalarValueKind::Str;
                }
                EraNode::DeclEventKind(_) => {
                    // Ignore event kind as we have already processed it
                    if event_func_kind.is_none() {
                        let mut diag = make_diag_fn();
                        diag.span_err(
                            Default::default(),
                            decl_span,
                            "cannot use event kind declaration in non-event function",
                        );
                        diag.emit_to(interp.get_ctx_mut());
                    }
                }
                EraNode::DeclLocalSize(size) => {
                    let size_span = arena.get_node_span(size);
                    let Some(size) = interp.interpret_int_expr_ok(size) else {
                        continue;
                    };
                    let Ok(size) = size.try_into() else {
                        let mut diag = make_diag_fn();
                        diag.span_err(
                            Default::default(),
                            size_span,
                            "LOCAL size is either too large or too small",
                        );
                        diag.emit_to(interp.get_ctx_mut());
                        continue;
                    };
                    var_local_size = size;
                }
                EraNode::DeclLocalSSize(size) => {
                    let size_span = arena.get_node_span(size);
                    let Some(size) = interp.interpret_int_expr_ok(size) else {
                        continue;
                    };
                    let Ok(size) = size.try_into() else {
                        let mut diag = make_diag_fn();
                        diag.span_err(
                            Default::default(),
                            size_span,
                            "LOCALS size is either too large or too small",
                        );
                        diag.emit_to(interp.get_ctx_mut());
                        continue;
                    };
                    var_locals_size = size;
                }
                EraNode::DeclTransient => {
                    func_info.is_transient = true;
                }
                _ => {
                    // Assume variable definitions
                    let var_decl = match interp.interpret_var_decl(decl) {
                        Ok(x) => x,
                        Err(EraInterpretError::VarNotFound(var, var_span)) => {
                            let mut diag = make_diag_fn();
                            diag.span_err(
                                Default::default(),
                                var_span,
                                format!("undefined variable `{var}`"),
                            );
                            diag.emit_to(interp.get_ctx_mut());
                            continue;
                        }
                        _ => continue,
                    };

                    let dims_cnt = var_decl
                        .var_info
                        .val
                        .dims_cnt()
                        .unwrap()
                        .try_into()
                        .expect("too many dimensions");

                    // Materialize var_decl
                    if var_decl.is_dynamic {
                        // Dynamic variable; allocate on stack
                        use std::hash::Hasher;

                        // TODO: Maybe use a dedicated const var intern pool?
                        let mut var_info = var_decl.var_info;
                        let var_kind = var_info.val.kind();
                        let is_const = var_decl.is_const;
                        let is_charadata = var_info.is_charadata;
                        let mut hasher = FxHasher::default();
                        var_info.val.inner_hash(&mut hasher);
                        let const_name = rcstr::format!("$CONSTVAL@{:x}", hasher.finish());
                        var_info.name = Ascii::new(const_name);
                        let (var_idx, _) = interp.get_ctx_mut().variables.add_var_force(var_info);
                        let var_idx = var_idx.try_into().expect("too many variables");
                        if add_frame_var_or_diag(
                            &mut interp,
                            var_decl.name_key,
                            EraFuncFrameVarInfo {
                                name: var_decl.name_key,
                                span: var_decl.name_span,
                                is_ref: false,
                                is_const,
                                is_charadata,
                                in_local_frame: true,
                                var_idx, // Wait for later allocation
                                var_kind,
                                dims_cnt,
                            },
                        )
                        .is_err()
                        {
                            continue;
                        }
                    } else if var_decl.is_ref {
                        // Simply point variable to the argument slot
                        // NOTE: We will do this in a later stage
                        if add_frame_var_or_diag(
                            &mut interp,
                            var_decl.name_key,
                            EraFuncFrameVarInfo {
                                name: var_decl.name_key,
                                span: var_decl.name_span,
                                is_ref: true,
                                is_const: false,
                                is_charadata: false, // Pointless for REF variables
                                in_local_frame: true,
                                var_idx: u32::MAX, // Wait for later allocation
                                var_kind: var_decl.var_info.val.kind(),
                                dims_cnt,
                            },
                        )
                        .is_err()
                        {
                            continue;
                        }
                    } else {
                        // Normal variable; allocate in global pool
                        let mut var_info = var_decl.var_info;
                        // NOTE: Decorate variable name with function name
                        var_info.name =
                            Ascii::new(rcstr::format!("{}@{}", var_info.name, name_str));
                        let var_kind = var_info.val.kind();
                        let is_charadata = var_info.is_charadata;
                        let (var_idx, _) = interp.get_ctx_mut().variables.add_var_force(var_info);
                        if add_frame_var_or_diag(
                            &mut interp,
                            var_decl.name_key,
                            EraFuncFrameVarInfo {
                                name: var_decl.name_key,
                                span: var_decl.name_span,
                                is_ref: false,
                                is_const: var_decl.is_const,
                                is_charadata,
                                in_local_frame: false,
                                var_idx: var_idx.try_into().unwrap(),
                                var_kind,
                                dims_cnt,
                            },
                        )
                        .is_err()
                        {
                            continue;
                        }
                    }
                }
            }
        }

        if func_info.is_transient {
            // Transient function cannot have variables or arguments (especially DYNAMIC ones)
            if !func_info.frame_info.args.is_empty() || !func_info.frame_info.vars.is_empty() {
                let mut diag = make_diag_fn();
                diag.span_err(
                    Default::default(),
                    name_span,
                    "transient function cannot have variables or arguments",
                );
                diag.emit_to(interp.get_ctx_mut());
                return Err(());
            }
        }

        // Now materialize LOCAL(S), ARG(S) variables
        let mut add_builtin_var = |var_name, var_val: Value| -> CompileResult<()> {
            let var_kind = var_val.kind();
            let (var_idx, _) = interp.get_ctx_mut().variables.add_var_force(EraVarInfo {
                name: Ascii::new(rcstr::format!("{var_name}@{name_str}")),
                val: var_val,
                is_const: false,
                is_charadata: false,
                is_global: false,
                never_trap: true,
            });
            let var_name_key = interp.get_ctx_mut().interner().get_or_intern(var_name);
            // HACK: Use function name span for now
            let var_span = name_span;
            if func_info
                .frame_info
                .vars
                .insert(
                    Ascii::new_str(var_name),
                    EraFuncFrameVarInfo {
                        name: var_name_key,
                        span: var_span,
                        is_ref: false,
                        is_const: false,
                        is_charadata: false,
                        in_local_frame: false,
                        var_idx: var_idx.try_into().unwrap(),
                        var_kind,
                        dims_cnt: 1,
                    },
                )
                .is_some()
            {
                let mut diag = make_diag_fn();
                diag.span_err(
                    Default::default(),
                    name_span,
                    format!("redefinition of variable `{var_name}`"),
                );
                diag.emit_to(interp.get_ctx_mut());
                return Err(());
            }
            Ok(())
        };
        _ = add_builtin_var(
            "LOCAL",
            Value::new_int_arr(smallvec![var_local_size], Vec::new()),
        );
        _ = add_builtin_var(
            "LOCALS",
            Value::new_str_arr(smallvec![var_locals_size], Vec::new()),
        );
        _ = add_builtin_var(
            "ARG",
            Value::new_int_arr(smallvec![var_arg_size], Vec::new()),
        );
        _ = add_builtin_var(
            "ARGS",
            Value::new_str_arr(smallvec![var_args_size], Vec::new()),
        );

        // Stage 2 - Process argument bindings
        // NOTE: We treat failures of this stage as a fatal error, exiting early
        let mut arg_bindings = Vec::new();
        let EraNode::ListExpr(node_args) = arena.get_node(node.args) else {
            unreachable!();
        };
        let node_args = arena.get_extra_data_view(node_args);
        for (arg_idx, arg) in node_args.iter().map(|x| EraNodeRef(*x)).enumerate() {
            let arg_span = arena.get_node_span(arg);
            let arg_idx = arg_idx.try_into().map_err(|_| {
                let mut diag = make_diag_fn();
                diag.span_err(Default::default(), arg_span, "too many arguments");
                diag.emit_to(interp.get_ctx_mut());
                ()
            })?;

            let mut arg_dims = EraVarDims::default();
            let mut init_val = ScalarValue::Empty;

            // Handle init value
            let target = if let EraNode::ExprBinary(lhs, op, rhs) = arena.get_node(arg) {
                let lhs_span = arena.get_node_span(lhs);
                let rhs_span = arena.get_node_span(rhs);
                let op: Token = op.into();
                if op != Token::Assign {
                    let mut diag = make_diag_fn();
                    diag.span_err(
                        Default::default(),
                        arena.get_node_token_span(arg),
                        format!("unexpected operator `{op:?}`"),
                    );
                    diag.emit_to(interp.get_ctx_mut());
                    return Err(());
                }
                let Some(rhs) = interp.interpret_expr_ok(rhs) else {
                    return Err(());
                };
                init_val = rhs;
                lhs
            } else {
                arg
            };
            // Handle argument indices
            let target = if let Some(EraNodeExprVarIdx { var_indices }) =
                EraNodeExprVarIdx::try_get_from(arena, target)
            {
                let lhs = EraNodeRef(var_indices[0]);
                arg_dims = var_indices[1..]
                    .iter()
                    .map(|idx| {
                        let idx = EraNodeRef(*idx);
                        let idx_span = arena.get_node_span(idx);
                        // TODO: Support string indices?
                        let idx = interp.interpret_int_expr_ok(idx).ok_or(())?;
                        idx.try_into().map_err(|_| {
                            let mut diag = make_diag_fn();
                            diag.span_err(
                                Default::default(),
                                idx_span,
                                "argument index is either too large or too small",
                            );
                            diag.emit_to(interp.get_ctx_mut());
                            ()
                        })
                    })
                    .try_collect()?;
                lhs
            } else {
                target
            };
            // Handle argument target
            let target_span = arena.get_node_span(target);
            let EraNode::Identifier(target) = arena.get_node(target) else {
                let mut diag = make_diag_fn();
                diag.span_err(Default::default(), target_span, "expected identifier");
                diag.emit_to(interp.get_ctx_mut());
                return Err(());
            };
            let target_str = interp.get_ctx().interner().resolve(target);

            // Resolve REF-variable mappings
            let mut is_ref = false;
            let mut dims_cnt = 0;
            let var_kind;
            if let Some(local_var) = func_info
                .frame_info
                .vars
                .get_mut(Ascii::new_str(target_str))
            {
                if local_var.is_ref {
                    is_ref = true;
                    dims_cnt = local_var.dims_cnt;
                    if !arg_dims.is_empty() {
                        let mut diag = make_diag_fn();
                        diag.span_err(
                            Default::default(),
                            target_span,
                            "cannot specify dimensions for REF variable in argument list",
                        );
                        diag.emit_to(interp.get_ctx_mut());
                        return Err(());
                    }
                    if init_val != ScalarValue::Empty {
                        let mut diag = make_diag_fn();
                        diag.span_err(
                            Default::default(),
                            target_span,
                            "cannot assign to REF variable in argument list",
                        );
                        diag.emit_to(interp.get_ctx_mut());
                        return Err(());
                    }
                    // Update local variable info
                    if local_var.var_idx != u32::MAX {
                        let mut diag = make_diag_fn();
                        diag.span_err(
                            Default::default(),
                            target_span,
                            "REF variable is already bound to an argument",
                        );
                        let previous_bound_span =
                            arena.get_node_span(EraNodeRef(node_args[local_var.var_idx as usize]));
                        diag.span_note(
                            Default::default(),
                            previous_bound_span,
                            "previous binding here",
                        );
                        diag.emit_to(interp.get_ctx_mut());
                        return Err(());
                    }
                    local_var.var_idx = arg_idx;
                }
                if local_var.is_const {
                    let mut diag = make_diag_fn();
                    diag.span_err(
                        Default::default(),
                        target_span,
                        "cannot assign to CONST variable",
                    );
                    diag.emit_to(interp.get_ctx_mut());
                    return Err(());
                }
                var_kind = local_var.var_kind;
            } else if let Some(global_var) =
                interp.get_ctx().variables.get_var_info_by_name(target_str)
            {
                global_var.val.ensure_alloc();

                if global_var.is_const {
                    let mut diag = make_diag_fn();
                    diag.span_err(
                        Default::default(),
                        target_span,
                        "cannot assign to CONST variable",
                    );
                    diag.emit_to(interp.get_ctx_mut());
                    return Err(());
                }
                var_kind = global_var.val.kind();
            } else {
                let var_name = target_str;
                let mut diag = make_diag_fn();
                diag.span_err(
                    Default::default(),
                    target_span,
                    format!("undefined variable `{var_name}`"),
                );
                diag.emit_to(interp.get_ctx_mut());
                return Err(());
            }

            if init_val != ScalarValue::Empty {
                if var_kind.to_scalar() != init_val.kind() {
                    let mut diag = make_diag_fn();
                    diag.span_err(
                        Default::default(),
                        target_span,
                        "incompatible types in assignment",
                    );
                    diag.emit_to(interp.get_ctx_mut());
                    return Err(());
                }
            }

            // Add to args list
            func_info.frame_info.args.push(EraFuncFrameArgInfo {
                var_kind: if is_ref {
                    var_kind.with_arr()
                } else {
                    var_kind.without_arr()
                },
                dims_cnt: dims_cnt.try_into().expect("too many dimensions"),
                default_value: init_val,
            });
            if !is_ref {
                arg_bindings.push(EraFuncArgsBinding {
                    target,
                    span: target_span,
                    dims: arg_dims,
                });
            }
        }

        // Verify REF-variable mappings
        for var in func_info.frame_info.vars.values_mut() {
            if var.is_ref {
                if var.var_idx != u32::MAX {
                    continue;
                }

                // TODO: Attempt to recover from this error by discarding the variable
                let mut diag = make_diag_fn();
                diag.span_err(
                    Default::default(),
                    var.span,
                    "REF variable must be bound to an argument",
                );
                diag.emit_to(interp.get_ctx_mut());
                return Err(());
            }
        }

        // Allocate stack slots for local variables
        let mut dyn_vars = Vec::new();
        let mut dyn_vars_cnt = func_info
            .frame_info
            .args
            .len()
            .try_into()
            .expect("too many arguments");
        for var in func_info.frame_info.vars.values_mut() {
            assert!(var.var_idx != u32::MAX, "unbound variable");

            // Is the variable DYNAMIC?
            if !(var.in_local_frame && !var.is_ref) {
                continue;
            }

            let init_val_idx = var.var_idx;
            dyn_vars.push((init_val_idx, var.span));
            var.var_idx = dyn_vars_cnt;
            dyn_vars_cnt += 1;
        }

        Ok((
            EraFuncPrebuildInfo {
                func_idx: 0,
                name,
                name_str,
                args: arg_bindings,
                event_kind: event_func_kind,
                dyn_vars,
            },
            func_info,
        ))
    }
}

#[derive(Debug, Clone)]
struct EraGotoLabelInfo {
    /// The position of the label in the bytecode.
    pos: EraBcChunkCheckpoint,
    /// The scope generation ID of the label.
    scope_id: u32,
    span: SrcSpan,
}

#[derive(Debug)]
struct EraGotoJumpInfo {
    backtrack: EraBcChunkJumpPoint,
    /// The scope generation ID of the `GOTO`.
    scope_id: u32,
    target: TokenKey,
    span: SrcSpan,
}

#[derive(Debug, Default)]
struct EraLoopStructCodeQueue {
    continue_queue: Vec<EraBcChunkJumpPoint>,
    break_queue: Vec<EraBcChunkJumpPoint>,
}

#[derive(Debug)]
struct EraLoopStructCodeMetadata {
    continue_cp: EraBcChunkCheckpoint,
    break_cp: EraBcChunkCheckpoint,
}

struct EraCodeGenSite<'o, 'ctx, 'i, 'b, 'arena, Callback> {
    o: &'o mut EraCodeGenerator<'ctx, 'i, Callback>,
    filename: ArcStr,
    chunk: &'b mut EraBcChunkBuilder,
    arena: &'arena EraNodeArena,
    cur_func: Yoke<
        &'static EraFuncInfo<'static>,
        &'static FxIndexMap<&'static Ascii<str>, Option<EraFuncInfo<'static>>>,
    >,
    /// The position where the function body starts. Used by `RESTART`.
    body_start_pos: EraBcChunkCheckpoint,
    /// The scope generation ID allocation counter. Used to prevent invalid `GOTO`'s
    /// (i.e. jumping into a `FOR` loop from outside). This is incremented every time a new scope
    /// is created. Control flow can transfer into scopes with smaller IDs, but not larger IDs.
    scope_generation_id: u32,
    /// The scope stack balances. Used to store the balance of the scope stack at each scope.
    scope_stack_balances: Vec<u32>,
    /// GOTO labels. Used to store the position of labels for `GOTO` jumps.
    goto_labels: FxHashMap<&'i Ascii<str>, EraGotoLabelInfo>,
    /// GOTO jumps. Used to store unfinished `GOTO` jumps, which will be resolved when the function
    /// ends.
    pending_goto_jumps: Vec<EraGotoJumpInfo>,
    /// RETURN jumps. Used to store unfinished `RETURN` jumps, which will be resolved when the
    /// function ends.
    pending_return_jumps: Vec<EraBcChunkJumpPoint>,
    /// The current loop structure. Used to store the `CONTINUE` and `BREAK` jumps for loops.
    cur_loop_struct: Option<EraLoopStructCodeQueue>,
}

impl<'o, 'ctx, 'i, 'b, 'arena, Callback: EraCompilerCallback>
    EraCodeGenSite<'o, 'ctx, 'i, 'b, 'arena, Callback>
{
    fn allocate_scope_id(&mut self, parent_scope: u32, stack_size: u32) -> u32 {
        let id = self.scope_generation_id;
        self.scope_generation_id += 1;
        let new_size = self.scope_stack_balances[parent_scope as usize] + stack_size;
        self.scope_stack_balances.push(new_size);
        id
    }

    fn make_diag(&self) -> Diagnostic<'static> {
        Diagnostic::with_file(self.filename.clone())
    }

    /// Compiles a function into bytecode, based on prebuilt information. Note that this function
    /// never fails, since `prebuild_function` should have already checked for errors which would
    /// prevent the function from being compiled.
    fn gen_function(
        o: &'o mut EraCodeGenerator<'ctx, 'i, Callback>,
        filename: ArcStr,
        chunk: &'b mut EraBcChunkBuilder,
        node: EraArcNodeRef,
        prebuild_info: EraFuncPrebuildInfo,
    ) {
        let arena = node.arena();
        let func_span = node.get_span();
        let node = EraNodeItemFunction::try_get_from(arena, node.node_ref()).unwrap();

        // Stage 3 - Generate bytecode
        let cur_func_idx = prebuild_info.func_idx as _;
        let name = prebuild_info.name;
        // Lock down func_entries so that lifetime is decoupled from ctx, and content is freezed
        // (i.e. prevents modifications)
        let func_entries = o.ctx.func_entries.clone();
        // SAFETY: We pretend that func_entries is 'static in order to use it in Yoke
        let func_info_yoke: Yoke<&EraFuncInfo, &FxIndexMap<&Ascii<str>, Option<EraFuncInfo>>> = unsafe {
            Yoke::attach_to_cart(std::mem::transmute(&*func_entries), |x| {
                x.get_index(cur_func_idx).unwrap().1.as_ref().unwrap()
            })
        };
        let func_info = func_info_yoke.get();
        let name_span = func_info.name_span;

        // Generate function prologue (local frame)
        // Allocates stack slots for local variables
        let dyn_vars_cnt = prebuild_info.dyn_vars.len() as u32;
        for (init_val_idx, var_span) in prebuild_info.dyn_vars {
            chunk.push_bc(BcKind::LoadConstVarWW { idx: init_val_idx }, var_span);
        }

        // Assign arguments
        for (arg_idx, arg) in prebuild_info.args.iter().enumerate() {
            let arg_idx = arg_idx as _;

            // Load target
            let target_str = o.ctx.interner().resolve(arg.target);
            let (target_in_local_frame, target_idx, is_mdarray);
            if let Some(local_var) = func_info.frame_info.vars.get(Ascii::new_str(target_str)) {
                target_in_local_frame = local_var.in_local_frame;
                target_idx = local_var.var_idx;
                is_mdarray = local_var.dims_cnt > 1;
            } else if let Some(global_var_idx) = o
                .ctx
                .variables
                .get_var_idx(o.ctx.interner().resolve(arg.target))
            {
                target_in_local_frame = false;
                target_idx = global_var_idx.try_into().expect("too many variables");
                is_mdarray = o
                    .ctx
                    .variables
                    .get_var_by_idx(target_idx as _)
                    .unwrap()
                    .dims_cnt()
                    .unwrap()
                    > 1;
            } else {
                unreachable!("variable not found");
            }
            if target_in_local_frame {
                chunk.push_bc(
                    BcKind::LoadLocalVar {
                        idx: target_idx.try_into().expect("too many local variables"),
                    },
                    arg.span,
                );
            } else {
                chunk.push_bc(BcKind::LoadVarWW { idx: target_idx }, arg.span);
            }
            for dim in arg.dims.iter().copied() {
                chunk.push_load_imm(dim.into(), arg.span);
            }
            if arg.dims.is_empty() {
                chunk.push_load_imm(0, arg.span);
            }
            if is_mdarray {
                chunk.push_bc(
                    BcKind::BuildArrIdxFromMD {
                        count: arg.dims.len().try_into().expect("too many dimensions"),
                    },
                    arg.span,
                );
            }

            // Load argument
            chunk.push_bc(BcKind::LoadLocalVar { idx: arg_idx }, arg.span);

            // Assign to local variable
            chunk.push_bc(BcKind::SetArrValFlat, arg.span);
            chunk.push_pop_all(1, arg.span);
        }

        // Generate function body
        let body_start_pos = chunk.checkpoint();
        let mut site = EraCodeGenSite {
            o,
            filename,
            chunk,
            arena,
            cur_func: func_info_yoke,
            body_start_pos,
            scope_generation_id: 1,
            scope_stack_balances: vec![0],
            goto_labels: Default::default(),
            pending_goto_jumps: Default::default(),
            pending_return_jumps: Default::default(),
            cur_loop_struct: None,
        };

        {
            let EraNode::ListStmt(body) = arena.get_node(node.stmts) else {
                unreachable!();
            };
            let body = arena.get_extra_data_view(body);
            let root_scope = site.allocate_scope_id(0, dyn_vars_cnt);
            for stmt in body.iter().map(|x| EraNodeRef(*x)) {
                site.safe_statement(stmt, root_scope);
            }
        }

        // Generate function epilogue (return)
        let EraCodeGenSite {
            o,
            filename,
            chunk,
            arena: _,
            cur_func,
            scope_stack_balances,
            goto_labels,
            pending_goto_jumps,
            pending_return_jumps,
            ..
        } = std::convert::identity(site);
        for jump in pending_return_jumps {
            jump.complete_here(chunk);
        }
        // Fallback return
        match cur_func.get().ret_kind {
            ScalarValueKind::Int => {
                chunk.push_load_imm(0, func_span);
                chunk.push_bc(BcKind::ReturnInt, func_span);
            }
            ScalarValueKind::Str => {
                chunk.push_build_string(0, func_span);
                chunk.push_bc(BcKind::ReturnStr, func_span);
            }
            ScalarValueKind::Void => chunk.push_bc(BcKind::ReturnVoid, func_span),
            ScalarValueKind::Empty => unreachable!("empty return type"),
        }

        let make_diag_fn = || Diagnostic::with_file(filename.clone());

        // Complete `GOTO` jumps
        for jump in pending_goto_jumps {
            let jump_target = o.ctx.interner().resolve(jump.target);
            let Some(jump_target) = goto_labels.get(Ascii::new_str(jump_target)) else {
                let mut diag = make_diag_fn();
                diag.span_err(
                    Default::default(),
                    jump.span,
                    format!("undefined label `${}`", jump_target),
                );
                diag.span_note(
                    Default::default(),
                    name_span,
                    "function definition starts here",
                );
                diag.emit_to(o.ctx);
                // NOTE: We ignore the unresolved GOTO for now
                jump.backtrack.discard();
                continue;
            };
            // Check if the jump is valid
            if jump_target.scope_id > jump.scope_id {
                let mut diag = make_diag_fn();
                diag.span_err(
                    Default::default(),
                    jump.span,
                    "this jump will corrupt the integrity of control flow",
                );
                diag.span_note(Default::default(), jump_target.span, "label defined here");
                diag.emit_to(o.ctx);
                jump.backtrack.discard();
                continue;
            }
            // NOTE: If stack balance is different, we need to adjust the stack
            let src_balance = scope_stack_balances[jump.scope_id as usize];
            let dst_balance = scope_stack_balances[jump_target.scope_id as usize];
            if src_balance != dst_balance {
                // Jump to a dedicated subchunk to adjust stack balance
                assert!(src_balance > dst_balance, "invalid stack balance");
                let delta = (src_balance - dst_balance)
                    .try_into()
                    .expect("stack balance delta");
                jump.backtrack.complete_here(chunk);
                chunk.push_pop_all(delta, jump.span);
                chunk.push_jump(jump.span).complete(chunk, jump_target.pos);
            } else {
                jump.backtrack.complete(chunk, jump_target.pos);
            }
        }
    }

    fn safe_statement(&mut self, stmt: EraNodeRef, cur_scope: u32) {
        if self.statement(stmt, cur_scope).is_err() {
            let stmt_span = self.arena.get_node_span(stmt);
            let err_msg = self.o.ctx.interner().get_or_intern("Invalid code");
            self.chunk.push_bc(
                BcKind::LoadConstStr {
                    idx: err_msg.into_u32(),
                },
                stmt_span,
            );
            self.chunk.push_bc(BcKind::FailWithMsg, stmt_span);
        }
    }

    fn statement(&mut self, stmt: EraNodeRef, cur_scope: u32) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);

        match self.arena.get_node(stmt) {
            // ----- Basic statements -----
            EraNode::StmtLabel(label) => {
                let label_span = self.arena.get_node_span(label);
                let EraNode::Identifier(label) = self.arena.get_node(label) else {
                    let mut diag = self.make_diag();
                    diag.span_err(Default::default(), label_span, "expected identifier");
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                let label_str = self.o.ctx.resolve_str(label);
                if let Some(previous_label) = self.goto_labels.insert(
                    Ascii::new_str(label_str),
                    EraGotoLabelInfo {
                        pos: self.chunk.checkpoint(),
                        scope_id: cur_scope,
                        span: label_span,
                    },
                ) {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        label_span,
                        format!("redefinition of label `${}`", label_str),
                    );
                    diag.span_note(
                        Default::default(),
                        previous_label.span,
                        "previous definition here",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
            }
            EraNode::StmtNop => {
                // NOTE: For debugging purposes (performance & etc.)
                self.chunk.push_bc(BcKind::Nop, stmt_span);
            }
            EraNode::StmtExpr(expr) => {
                let expr_span = self.arena.get_node_span(expr);
                // NOTE: Emuera automatically fixes top-level `==` to `=`.
                match self.arena.get_node(expr) {
                    EraNode::ExprBinary(lhs, op, rhs) if Token::from(op) == Token::CmpEq => {
                        let mut diag = self.make_diag();
                        diag.span_warn(
                            Default::default(),
                            expr_span,
                            "`==` has no side effects, converting to assignment for compatibility with Emuera; did you misspell?",
                        );
                        diag.emit_to(self.o.ctx);

                        self.var_idx_set(lhs, |this| this.expression(rhs), false)?;
                    }
                    _ => {
                        let expr = self.expression(expr)?;
                        match expr {
                            ScalarValueKind::Int | ScalarValueKind::Str => {
                                self.chunk.push_pop_all(1, stmt_span);
                            }
                            ScalarValueKind::Void => (),
                            ScalarValueKind::Empty => unreachable!("empty expression"),
                        }
                    }
                }
            }
            EraNode::StmtRowAssign(..) => {
                self.stmt_row_assign(stmt)?;
            }
            EraNode::StmtResultCmdCall(cmd, args) => {
                let cmd_span = self.arena.get_node_span(cmd);
                let cmd = self.unwrap_identifier(cmd)?;
                let cmd = self.o.ctx.resolve_str(cmd);
                self.builtin_func_call(cmd, cmd_span, args, true)?;
            }
            EraNode::StmtDebugPrint(flags, args) => {
                // TODO: DebugPrintStmt
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    stmt_span,
                    "DEBUGPRINT is not yet supported and will be ignored for now",
                );
                diag.emit_to(self.o.ctx);
            }
            EraNode::StmtPrint(flags, args) => {
                let args = self.unwrap_list_expr(args)?;
                let mut parts_cnt = 0;
                for arg in args.iter().map(|x| EraNodeRef(*x)) {
                    let arg_span = self.arena.get_node_span(arg);
                    let arg = self.expression(arg)?;
                    match arg {
                        ScalarValueKind::Int => {
                            self.chunk.push_bc(BcKind::IntToStr, arg_span);
                        }
                        ScalarValueKind::Str => (),
                        ScalarValueKind::Empty => continue,
                        _ => unreachable!("invalid print argument"),
                    }
                    parts_cnt += 1;
                }
                self.chunk.push_build_string(parts_cnt, stmt_span);
                self.stmt_print_emit_bc(flags, stmt_span);
            }
            EraNode::StmtPrintData(flags, dest, data) => {
                let flags = flags.0;
                if !matches!(self.arena.get_node(dest), EraNode::Empty) {
                    // Assign random selection to dest
                    self.stmt_strdata_build(
                        |this, selections_count| {
                            this.var_idx_set(
                                dest,
                                |this| {
                                    // 2 (var + idx) + 1 (random selection)
                                    this.chunk.push_duplicate_one(2 + 1, stmt_span);
                                    Ok(ScalarValueKind::Int)
                                },
                                false,
                            )?;
                            Ok(())
                        },
                        data,
                        stmt_span,
                    )?;
                } else {
                    self.stmt_strdata_build(|_, _| Ok(()), data, stmt_span)?;
                }
                self.stmt_print_emit_bc(flags, stmt_span);
            }
            EraNode::StmtWait => {
                self.stmt_wait(false, false, stmt_span);
            }
            EraNode::StmtForceWait => {
                self.stmt_wait(false, true, stmt_span);
            }
            EraNode::StmtWaitAnyKey => {
                self.stmt_wait(true, false, stmt_span);
            }
            EraNode::StmtIf(..) => {
                self.stmt_if(cur_scope, stmt)?;
            }
            EraNode::StmtQuit => {
                self.chunk.push_bc(BcKind::Quit, stmt_span);
            }
            EraNode::StmtSelectCase(..) => {
                self.stmt_selectcase(cur_scope, stmt)?;
            }
            EraNode::StmtWhile(..) => {
                self.apply_loop_stmt(|this| this.stmt_while(cur_scope, stmt))?;
            }
            EraNode::StmtCall(..) => {
                self.stmt_call(stmt)?;
            }
            EraNode::StmtTryCall(..) => {
                self.stmt_trycall(stmt)?;
            }
            EraNode::StmtTryCCall(..) => {
                self.stmt_tryccall(cur_scope, stmt)?;
            }
            EraNode::StmtJump(..) => {
                self.stmt_call(stmt)?;
                self.pending_return_jumps
                    .push(self.chunk.push_jump(stmt_span));
            }
            EraNode::StmtTryJump(..) => {
                self.stmt_trycall(stmt)?;
                self.pending_return_jumps
                    .push(self.chunk.push_jump(stmt_span));
            }
            EraNode::StmtTryCJump(..) => {
                self.stmt_tryccall(cur_scope, stmt)?;
                self.pending_return_jumps
                    .push(self.chunk.push_jump(stmt_span));
            }
            EraNode::StmtReturn(values) => {
                let values = self.unwrap_list_expr(values)?;
                let ret_kind = self.cur_func.get().ret_kind;
                if ret_kind == ScalarValueKind::Void {
                    // Procedure, assign return values to RESULT(S)
                    // NOTE: Emuera only assigns values to RESULT:*, and we keep this behavior

                    // Load target
                    _ = self.var_static_idx("RESULT", stmt_span, 0)?;
                    // Load rhs
                    let mut rhs_count = 0;
                    for val in values.iter().map(|x| EraNodeRef(*x)) {
                        let val_span = self.arena.get_node_span(val);
                        let val = self.expression(val)?;
                        match val {
                            ScalarValueKind::Int => (),
                            ScalarValueKind::Str => {
                                // TODO: Maybe warn about this implicit conversion?
                                self.chunk.push_bc(BcKind::StrToInt, val_span);
                            }
                            ScalarValueKind::Empty => {
                                self.chunk.push_load_imm(0, val_span);
                            }
                            ScalarValueKind::Void => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    val_span,
                                    "cannot return void value",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        }

                        rhs_count += 1;
                    }
                    // Assign
                    self.chunk.push_bc(
                        BcKind::RowAssign {
                            vals_cnt: rhs_count,
                        },
                        stmt_span,
                    );
                    // Return to caller
                    self.chunk.push_bc(BcKind::ReturnVoid, stmt_span);
                } else {
                    // Function, return values normally
                    if values.len() > 1 {
                        let mut diag = self.make_diag();
                        diag.span_err(Default::default(), stmt_span, "too many return values");
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }

                    self.expr_or_default(
                        values.get(0).map(|x| EraNodeRef(*x)).or_span(stmt_span),
                        ret_kind,
                    )?;

                    // Return
                    let bc = match ret_kind {
                        ScalarValueKind::Int => BcKind::ReturnInt,
                        ScalarValueKind::Str => BcKind::ReturnStr,
                        _ => unreachable!("invalid return type"),
                    };
                    self.chunk.push_bc(bc, stmt_span);
                }
            }
            EraNode::StmtContinue => {
                let Some(cur_loop_struct) = &mut self.cur_loop_struct else {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        stmt_span,
                        "cannot CONTINUE outside loops",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                cur_loop_struct
                    .continue_queue
                    .push(self.chunk.push_jump(stmt_span));
            }
            EraNode::StmtBreak => {
                let Some(cur_loop_struct) = &mut self.cur_loop_struct else {
                    let mut diag = self.make_diag();
                    diag.span_err(Default::default(), stmt_span, "cannot BREAK outside loops");
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                cur_loop_struct
                    .break_queue
                    .push(self.chunk.push_jump(stmt_span));
            }
            EraNode::StmtRestart => {
                // NOTE: RESTART is unconditional GOTO start of function body
                self.chunk
                    .push_jump(stmt_span)
                    .complete(self.chunk, self.body_start_pos);
            }
            EraNode::StmtThrow(args) => {
                let args = self.unwrap_list_expr(args)?;
                let mut parts_cnt = 0;
                for arg in args.iter().map(|x| EraNodeRef(*x)) {
                    let arg_span = self.arena.get_node_span(arg);
                    let arg = self.expression(arg)?;
                    match arg {
                        ScalarValueKind::Int => {
                            self.chunk.push_bc(BcKind::IntToStr, arg_span);
                        }
                        ScalarValueKind::Str => (),
                        ScalarValueKind::Empty => continue,
                        _ => unreachable!("invalid print argument"),
                    }
                    parts_cnt += 1;
                }
                self.chunk.push_build_string(parts_cnt, stmt_span);
                self.chunk.push_bc(BcKind::Throw, stmt_span);
            }
            EraNode::StmtRepeat(..) => {
                self.apply_loop_stmt(|this| this.stmt_repeat_loop(cur_scope, stmt))?;
            }
            EraNode::StmtGoto(label) => {
                let label_span = self.arena.get_node_span(label);
                let label = self.unwrap_identifier(label)?;
                // Resolve GOTO's later
                self.pending_goto_jumps.push(EraGotoJumpInfo {
                    backtrack: self.chunk.push_jump(stmt_span),
                    scope_id: cur_scope,
                    target: label,
                    span: label_span,
                });
            }
            EraNode::StmtFor(..) => {
                self.apply_loop_stmt(|this| this.stmt_for_loop(cur_scope, stmt))?;
            }
            EraNode::StmtDoLoop(..) => {
                self.apply_loop_stmt(|this| this.stmt_do_loop(cur_scope, stmt))?;
            }
            // ----- Other commands -----
            EraNode::StmtSplit(args) => {
                let ([input, sep, dest], [count_dest]) = self.unpack_list_expr(args)?;
                self.str_expr(input)?;
                self.str_expr(sep)?;
                self.str_norm_var_idx(dest, true)?;
                if let EraExprOrSpan::Expr(count_dest) = count_dest {
                    self.int_norm_var_idx(count_dest, true)?;
                } else {
                    self.int_var_static_idx("RESULT", stmt_span, 0)?;
                }
                self.chunk.push_bc(BcKind::SplitString, stmt_span);
            }
            EraNode::StmtTimes(var, factor) => {
                let factor_span = self.arena.get_node_span(factor);
                let factor: f64 = {
                    let mut interp = EraInterpreter::new(self.o.ctx, self.arena, true);
                    let factor = interp.interpret_str_expr_ok(factor).ok_or(())?;
                    let Ok(factor) = factor.parse() else {
                        let mut diag = self.make_diag();
                        diag.span_err(Default::default(), factor_span, "invalid factor");
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    };
                    factor
                };
                self.int_norm_var_idx(var, true)?;
                self.chunk.push_load_imm(factor.to_bits() as _, factor_span);
                self.chunk.push_bc(BcKind::TimesFloat, stmt_span);
            }
            EraNode::StmtSetBit(args) => {
                let args_span = self.arena.get_node_span(args);
                let args = self.unwrap_list_expr(args)?;
                let args_count = args.len();
                if args_count < 2 {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        args_span,
                        format!("expected at least 2 arguments, found {args_count}"),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                let target = EraNodeRef(args[0]);
                let bits = &args[1..];
                self.int_norm_var_idx(target, true)?;
                self.chunk.push_duplicate_all(2, stmt_span);
                self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
                for bit in bits.iter().map(|x| EraNodeRef(*x)) {
                    let bit_span = self.arena.get_node_span(bit);
                    self.int_expr(bit)?;
                    self.chunk.push_bc(BcKind::SetBit, bit_span);
                }
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtClearBit(args) => {
                let args_span = self.arena.get_node_span(args);
                let args = self.unwrap_list_expr(args)?;
                let args_count = args.len();
                if args_count < 2 {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        args_span,
                        format!("expected at least 2 arguments, found {args_count}"),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                let target = EraNodeRef(args[0]);
                let bits = &args[1..];
                self.int_norm_var_idx(target, true)?;
                self.chunk.push_duplicate_all(2, stmt_span);
                self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
                for bit in bits.iter().map(|x| EraNodeRef(*x)) {
                    let bit_span = self.arena.get_node_span(bit);
                    self.int_expr(bit)?;
                    self.chunk.push_bc(BcKind::ClearBit, bit_span);
                }
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtInvertBit(args) => {
                let args_span = self.arena.get_node_span(args);
                let args = self.unwrap_list_expr(args)?;
                let args_count = args.len();
                if args_count < 2 {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        args_span,
                        format!("expected at least 2 arguments, found {args_count}"),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                let target = EraNodeRef(args[0]);
                let bits = &args[1..];
                self.int_norm_var_idx(target, true)?;
                self.chunk.push_duplicate_all(2, stmt_span);
                self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
                for bit in bits.iter().map(|x| EraNodeRef(*x)) {
                    let bit_span = self.arena.get_node_span(bit);
                    self.int_expr(bit)?;
                    self.chunk.push_bc(BcKind::InvertBit, bit_span);
                }
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtSetColor(..) => {
                self.stmt_setcolor(stmt, "@COLOR")?;
            }
            EraNode::StmtResetColor => {
                self.stmt_resetcolor(stmt, "@COLOR", "@DEFCOLOR")?;
            }
            EraNode::StmtSetBgColor(..) => {
                self.stmt_setcolor(stmt, "@BGCOLOR")?;
            }
            EraNode::StmtResetBgColor => {
                self.stmt_resetcolor(stmt, "@BGCOLOR", "@DEFBGCOLOR")?;
            }
            EraNode::StmtVarSet(args) => {
                let ([target], [value, start_index, end_index]) = self.unpack_list_expr(args)?;
                let target = self.norm_var_idx(target, true)?;
                self.expr_or_default(value, target)?;
                self.int_expr_or(start_index, 0)?;
                self.int_expr_or(end_index, -1)?;
                self.chunk.push_bc(BcKind::VarSet, stmt_span);
            }
            EraNode::StmtCVarSet(args) => {
                let ([target, index], [value, start_id, end_id]) = self.unpack_list_expr(args)?;
                let target = self.norm_var(target)?;
                self.int_expr(index)?;
                self.expr_or_default(value, target)?;
                self.int_expr_or(start_id, 0)?;
                self.int_expr_or(end_id, -1)?;
                self.chunk.push_bc(BcKind::CVarSet, stmt_span);
            }
            EraNode::StmtVarSize(args) => {
                let ([target], []) = self.unpack_list_expr(args)?;
                self.norm_var(target)?;
                self.chunk.push_bc(BcKind::GetVarAllSize, stmt_span);
            }
            EraNode::StmtSwap(args) => {
                let ([lhs, rhs], []) = self.unpack_list_expr(args)?;
                let lhs = self.norm_var_idx(lhs, true)?;
                let rhs = self.norm_var_idx(rhs, true)?;
                if lhs != rhs {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        stmt_span,
                        "cannot swap variables of different types",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                // TODO: Use dedicated bytecode SwapVar
                self.chunk.push_duplicate_one(4, stmt_span);
                self.chunk.push_duplicate_one(4, stmt_span);
                self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
                self.chunk.push_duplicate_all(5, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
                self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(3, stmt_span);
            }
            EraNode::StmtHtmlPrint(args) => {
                let ([text], []) = self.unpack_list_expr(args)?;
                self.str_expr(text)?;
                self.chunk.push_bc(BcKind::HtmlPrint, stmt_span);
            }
            EraNode::StmtPrintButton(..) => {
                let flags = EraPrintExtendedFlags::new();
                self.stmt_printbutton(stmt, flags)?;
            }
            EraNode::StmtPrintButtonC(..) => {
                let flags = EraPrintExtendedFlags::new().with_right_pad(true);
                self.stmt_printbutton(stmt, flags)?;
            }
            EraNode::StmtPrintButtonLC(..) => {
                let flags = EraPrintExtendedFlags::new().with_left_pad(true);
                self.stmt_printbutton(stmt, flags)?;
            }
            EraNode::StmtArrayRemove(args) => {
                let ([target, start_index, count], []) = self.unpack_list_expr(args)?;
                let target = self.norm_var_idx(target, true)?;
                self.int_expr(start_index)?;
                self.int_expr(count)?;
                self.chunk.push_bc(BcKind::ArrayRemove, stmt_span);
            }
            EraNode::StmtArraySort(args) => {
                let ([target], [ordering, start_index, count]) = self.unpack_list_expr(args)?;
                let mut is_ascending = true;
                if let EraExprOrSpan::Expr(ordering) = ordering {
                    let ordering_span = self.arena.get_node_span(ordering);
                    let ordering = self.unwrap_identifier(ordering)?;
                    let ordering = self.o.ctx.resolve_str(ordering);
                    if ordering.eq_ignore_ascii_case("FORWARD") {
                        is_ascending = true;
                    } else if ordering.eq_ignore_ascii_case("BACK") {
                        is_ascending = false;
                    } else {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            ordering_span,
                            "invalid ordering in ARRAYSORT",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                }
                let bc = if is_ascending {
                    BcKind::ArraySortAsc
                } else {
                    BcKind::ArraySortDesc
                };
                let target = self.norm_var_idx(target, true)?;
                self.int_expr_or(start_index, 0)?;
                self.int_expr_or(count, -1)?;
                self.chunk.push_bc(bc, stmt_span);
            }
            EraNode::StmtArrayMSort(args) => {
                let args = self.unwrap_list_expr(args)?;
                if args.len() < 1 {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        stmt_span,
                        "missing primary array in ARRAYMSORT",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                // TODO: Do deeper dimensions checking
                // NOTE: Emuera ignores indices on arrays for ARRAYMSORT
                let primary = EraNodeRef(args[0]);
                let primary = self.norm_var(primary)?;
                let mut subs_cnt = 0;
                for sub in args.iter().skip(1).map(|x| EraNodeRef(*x)) {
                    let sub_span = self.arena.get_node_span(sub);
                    let sub = self.norm_var(sub)?;
                    if primary != sub {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            sub_span,
                            "incompatible types in ARRAYMSORT",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                    subs_cnt += 1;
                }
                self.chunk
                    .push_bc(BcKind::ArrayMSort { subs_cnt }, stmt_span);
            }
            EraNode::StmtArrayCopy(args) => {
                // TODO: Support dynamic array names
                let ([source_name, dest_name], []) = self.unpack_list_expr(args)?;
                let source_span = self.arena.get_node_span(source_name);
                let dest_span = self.arena.get_node_span(dest_name);
                let source_name = self.const_eval_str(source_name)?;
                let dest_name = self.const_eval_str(dest_name)?;
                let source = self.var_static(&source_name, source_span)?;
                let dest = self.var_static(&dest_name, dest_span)?;
                if source != dest {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        stmt_span,
                        "incompatible types in ARRAYCOPY",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                self.chunk.push_bc(BcKind::ArrayCopy, stmt_span);
            }
            EraNode::StmtArrayShift(args) => {
                let ([target, shift_count, value], [start_index, target_count]) =
                    self.unpack_list_expr(args)?;
                let target = self.norm_var_idx(target, true)?;
                self.int_expr(shift_count)?;
                self.expr_or_default(value.into(), target)?;
                self.int_expr_or(start_index, 0)?;
                self.int_expr_or(target_count, -1)?;
                self.chunk.push_bc(BcKind::ArrayShift, stmt_span);
            }
            EraNode::StmtInput(..) => {
                self.stmt_input(stmt, false)?;
            }
            EraNode::StmtInputS(..) => {
                self.stmt_input(stmt, true)?;
            }
            EraNode::StmtTInput(..) => {
                self.stmt_tinput(stmt, false)?;
            }
            EraNode::StmtTInputS(..) => {
                self.stmt_tinput(stmt, true)?;
            }
            EraNode::StmtOneInput(..) => {
                self.stmt_oneinput(stmt, false)?;
            }
            EraNode::StmtOneInputS(..) => {
                self.stmt_oneinput(stmt, true)?;
            }
            EraNode::StmtTOneInput(..) => {
                self.stmt_toneinput(stmt, false)?;
            }
            EraNode::StmtTOneInputS(..) => {
                self.stmt_toneinput(stmt, true)?;
            }
            EraNode::StmtReuseLastLine(args) => {
                let ([content], []) = self.unpack_list_expr(args)?;
                self.str_expr(content)?;
                self.chunk.push_bc(BcKind::ReuseLastLine, stmt_span);
            }
            EraNode::StmtClearLine(args) => {
                let ([count], []) = self.unpack_list_expr(args)?;
                self.int_expr(count)?;
                self.chunk.push_bc(BcKind::ClearLine, stmt_span);
            }
            EraNode::StmtDrawLine => {
                self.str_var_static_idx("DRAWLINESTR", stmt_span, 0)?;
                self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
                self.chunk.push_bc(BcKind::PrintLine, stmt_span);
            }
            EraNode::StmtCustomDrawLine(args) => {
                let ([content], []) = self.unpack_list_expr(args)?;
                self.str_expr(content)?;
                self.int_var_static_idx("SCREENWIDTH", stmt_span, 0)?;
                self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
                self.chunk.push_bc(BcKind::ExtendStrToWidth, stmt_span);
                self.chunk.push_bc(BcKind::PrintLine, stmt_span);
            }
            EraNode::StmtTWait(args) => {
                let ([duration, force_wait], []) = self.unpack_list_expr(args)?;
                self.int_expr(duration)?;
                self.int_expr(force_wait)?;
                self.chunk.push_bc(BcKind::TWait, stmt_span);
            }
            EraNode::StmtFontStyle(args) => {
                let ([], [style]) = self.unpack_list_expr(args)?;
                self.int_var_static_idx("@STYLE", stmt_span, 0)?;
                self.int_expr_or(style, 0)?;
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtFontBold => {
                self.int_var_static_idx("@STYLE", stmt_span, 0)?;
                self.chunk.push_duplicate_all(2, stmt_span);
                self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
                self.chunk.push_load_imm(1, stmt_span);
                self.chunk.push_bc(BcKind::BitOrInt, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtFontItalic => {
                self.int_var_static_idx("@STYLE", stmt_span, 0)?;
                self.chunk.push_duplicate_all(2, stmt_span);
                self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
                self.chunk.push_load_imm(2, stmt_span);
                self.chunk.push_bc(BcKind::BitOrInt, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtFontRegular => {
                self.int_var_static_idx("@STYLE", stmt_span, 0)?;
                self.chunk.push_load_imm(0, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtSetFont(args) => {
                let ([], [font_name]) = self.unpack_list_expr(args)?;
                self.str_var_static_idx("@FONT", stmt_span, 0)?;
                self.str_expr_or(font_name, "")?;
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtStrData(dest, data) => {
                if let EraNode::Empty = self.arena.get_node(dest) {
                    self.str_var_static_idx("RESULTS", stmt_span, 0)?;
                } else {
                    self.str_norm_var_idx(dest, true)?;
                }
                self.stmt_strdata_build(|_, _| Ok(()), data, stmt_span)?;
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtPutForm(args) => {
                let ([content], []) = self.unpack_list_expr(args)?;
                self.str_var_static_idx("SAVEDATA_TEXT", stmt_span, 0)?;
                self.chunk.push_duplicate_all(2, stmt_span);
                self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
                self.str_expr(content)?;
                self.chunk.push_build_string(2, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtSkipDisp(args) => {
                let ([is_skip], []) = self.unpack_list_expr(args)?;
                self.int_var_static_idx("@SKIPDISP", stmt_span, 0)?;
                self.int_expr(is_skip)?;
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtBegin(args) => {
                let ([procedure], []) = self.unpack_list_expr(args)?;
                let proc_span = self.arena.get_node_span(procedure);
                let procedure = self.unwrap_identifier(procedure)?;
                let procedure = self.o.ctx.resolve_str(procedure).to_ascii_uppercase();
                let (func_name, reset_exec) = match procedure.as_bytes() {
                    b"FIRST" => ("SYSPROC_BEGIN_FIRST", false),
                    b"TITLE" => ("SYSPROC_BEGIN_TITLE", false),
                    b"TRAIN" => ("SYSPROC_BEGIN_TRAIN", true),
                    b"AFTERTRAIN" => ("SYSPROC_BEGIN_AFTERTRAIN", false),
                    b"ABLUP" => ("SYSPROC_BEGIN_ABLUP", false),
                    b"TURNEND" => ("SYSPROC_BEGIN_TURNEND", false),
                    b"SHOP" => ("SYSPROC_BEGIN_SHOP", false),
                    _ => {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            proc_span,
                            "invalid procedure name in BEGIN",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                };
                // Find internal function
                let Some((func_idx, _, Some(func_info))) =
                    self.o.ctx.func_entries.get_full(Ascii::new_str(func_name))
                else {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        proc_span,
                        format!("internal function `{func_name}` not found"),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                if !func_info.frame_info.args.is_empty() {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        proc_span,
                        format!("internal function `{func_name}` must not have arguments"),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                // Execute function
                self.chunk.push_load_imm(func_idx as _, stmt_span);
                let bc = if reset_exec {
                    BcKind::RestartExecAtFun
                } else {
                    BcKind::CallFun { args_cnt: 0 }
                };
                self.chunk.push_bc(bc, stmt_span);
            }
            EraNode::StmtDoTrain(args) => {
                let ty = self.static_func_call("SYSPROC_DOTRAIN", stmt_span, args, false)?;
                if ty.is_value() {
                    self.chunk.push_pop_all(1, stmt_span);
                }
            }
            EraNode::StmtRedraw(args) => {
                let ([arg], []) = self.unpack_list_expr(args)?;
                self.int_var_static_idx("@REDRAW", stmt_span, 0)?;
                self.int_expr(arg)?;
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtStrLen(args) => {
                let ([content], []) = self.unpack_list_expr(args)?;
                self.int_var_static_idx("RESULT", stmt_span, 0)?;
                self.str_expr(content)?;
                self.chunk.push_bc(BcKind::StrLen, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtStrLenU(args) => {
                let ([content], []) = self.unpack_list_expr(args)?;
                self.int_var_static_idx("RESULT", stmt_span, 0)?;
                self.str_expr(content)?;
                self.chunk.push_bc(BcKind::StrLenU, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtAlignment(args) => {
                let ([alignment], []) = self.unpack_list_expr(args)?;
                let align_span = self.arena.get_node_span(alignment);
                let alignment = self.unwrap_identifier(alignment)?;
                let alignment = self.o.ctx.resolve_str(alignment).to_ascii_uppercase();
                let alignment = match alignment.as_bytes() {
                    b"LEFT" => EraAlignmentKind::Left,
                    b"CENTER" => EraAlignmentKind::Center,
                    b"RIGHT" => EraAlignmentKind::Right,
                    _ => {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            align_span,
                            "invalid alignment in ALIGNMENT",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                };
                self.int_var_static_idx("@ALIGN", stmt_span, 0)?;
                self.chunk.push_load_imm(alignment as _, align_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtToolTipSetDelay(args) => {
                let ([delay], []) = self.unpack_list_expr(args)?;
                self.int_var_static_idx("@TOOLTIP_DELAY", stmt_span, 0)?;
                self.int_expr(delay)?;
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtToolTipSetDuration(args) => {
                let ([duration], []) = self.unpack_list_expr(args)?;
                self.int_var_static_idx("@TOOLTIP_DURATION", stmt_span, 0)?;
                self.int_expr(duration)?;
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtRandomize(..) => {
                // TODO: RandomizeStmt
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    stmt_span,
                    "RANDOMIZE is not yet supported; ignoring",
                );
                diag.emit_to(self.o.ctx);
            }
            EraNode::StmtDumpRand => {
                // TODO: DumpRandStmt
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    stmt_span,
                    "DUMPRAND is not yet supported; ignoring",
                );
                diag.emit_to(self.o.ctx);
            }
            EraNode::StmtInitRand => {
                // TODO: InitRandStmt
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    stmt_span,
                    "INITRAND is not yet supported; ignoring",
                );
                diag.emit_to(self.o.ctx);
            }
            EraNode::StmtBar(args) => {
                let ([value, max_value, length], []) = self.unpack_list_expr(args)?;
                self.int_expr(value)?;
                self.int_expr(max_value)?;
                self.int_expr(length)?;
                self.chunk.push_bc(BcKind::BuildBarStr, stmt_span);
                self.chunk.push_bc(BcKind::Print, stmt_span);
            }
            EraNode::StmtBarL(args) => {
                let ([value, max_value, length], []) = self.unpack_list_expr(args)?;
                self.int_expr(value)?;
                self.int_expr(max_value)?;
                self.int_expr(length)?;
                self.chunk.push_bc(BcKind::BuildBarStr, stmt_span);
                self.chunk.push_bc(BcKind::PrintLine, stmt_span);
            }
            EraNode::StmtAddChara(args) => {
                let args = self.unwrap_list_expr(args)?;
                for chara in args.iter().map(|x| EraNodeRef(*x)) {
                    self.int_expr(chara)?;
                    self.chunk.push_bc(BcKind::AddChara, stmt_span);
                }
            }
            EraNode::StmtPickUpChara(args) => {
                let args = self.unwrap_list_expr(args)?;
                let mut charas_cnt = 0;
                for chara in args.iter().map(|x| EraNodeRef(*x)) {
                    self.int_expr(chara)?;
                    charas_cnt += 1;
                }
                self.chunk
                    .push_bc(BcKind::PickUpChara { charas_cnt }, stmt_span);
            }
            EraNode::StmtDelChara(args) => {
                let args = self.unwrap_list_expr(args)?;
                let mut charas_cnt = 0;
                for chara in args.iter().map(|x| EraNodeRef(*x)) {
                    self.int_expr(chara)?;
                    charas_cnt += 1;
                }
                self.chunk
                    .push_bc(BcKind::DeleteChara { charas_cnt }, stmt_span);
            }
            EraNode::StmtSwapChara(args) => {
                let ([chara1, chara2], []) = self.unpack_list_expr(args)?;
                self.int_expr(chara1)?;
                self.int_expr(chara2)?;
                self.chunk.push_bc(BcKind::SwapChara, stmt_span);
            }
            EraNode::StmtAddCopyChara(args) => {
                let ([chara], []) = self.unpack_list_expr(args)?;
                self.int_expr(chara)?;
                self.chunk.push_bc(BcKind::AddCopyChara, stmt_span);
            }
            EraNode::StmtResetStain(args) => {
                let ([chara], []) = self.unpack_list_expr(args)?;
                self.int_expr(chara)?;
                self.chunk.push_bc(BcKind::ResetCharaStain, stmt_span);
            }
            EraNode::StmtSaveChara(args) => {
                let args = self.unwrap_list_expr(args)?;
                if args.len() < 2 {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        stmt_span,
                        "expected at least 2 arguments in SAVECHARA",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                let filename = EraNodeRef(args[0]);
                let memo = EraNodeRef(args[1]);
                self.str_expr(filename)?;
                self.str_expr(memo)?;
                let mut charas_cnt = 0;
                for chara in args.iter().skip(2).map(|x| EraNodeRef(*x)) {
                    self.int_expr(chara)?;
                    charas_cnt += 1;
                }
                self.chunk
                    .push_bc(BcKind::SaveChara { charas_cnt }, stmt_span);
            }
            EraNode::StmtLoadChara(args) => {
                let ([filename], []) = self.unpack_list_expr(args)?;
                self.int_var_static_idx("RESULT", stmt_span, 0)?;
                self.str_expr(filename)?;
                self.chunk.push_bc(BcKind::LoadChara, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtSetAnimeTimer(args) => {
                let ([duration], []) = self.unpack_list_expr(args)?;
                self.int_var_static_idx("@ANIMETIMER", stmt_span, 0)?;
                self.int_expr(duration)?;
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtHtmlTagSplit(args) => {
                let ([html], [var_tags, var_count]) = self.unpack_list_expr(args)?;
                self.str_expr(html)?;
                if let EraExprOrSpan::Expr(var_tags) = var_tags {
                    self.str_norm_var_idx(var_tags, true)?;
                } else {
                    self.str_var_static_idx("RESULTS", stmt_span, 0)?;
                }
                if let EraExprOrSpan::Expr(var_count) = var_count {
                    self.int_norm_var_idx(var_count, true)?;
                } else {
                    self.int_var_static_idx("RESULT", stmt_span, 0)?;
                }
                self.chunk.push_bc(BcKind::HtmlTagSplit, stmt_span);
            }
            EraNode::StmtPower(args) => {
                let ([target, base, exponent], []) = self.unpack_list_expr(args)?;
                self.int_norm_var_idx(target, true)?;
                self.int_expr(base)?;
                self.int_expr(exponent)?;
                self.chunk.push_bc(BcKind::PowerInt, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtLoadData(args) => {
                // Compared to function form, statement form will also call SYSPROC_LOADDATAEND
                // let ([save_id], []) = self.unpack_list_expr(args)?;
                let ty = self.builtin_func_call("LOADDATA", stmt_span, args, false)?;
                assert_eq!(ty, ScalarValueKind::Int);
                let args = &[ValueKind::Int];
                let func_idx = self.match_user_func_sig(
                    "SYSPROC_LOADDATAEND",
                    stmt_span,
                    args,
                    ScalarValueKind::Int,
                )?;
                self.chunk.push_load_imm(func_idx as _, stmt_span);
                self.chunk.push_bc(
                    BcKind::CallFun {
                        args_cnt: args.len() as _,
                    },
                    stmt_span,
                );
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtSaveData(args) => {
                let ([save_id, save_info], []) = self.unpack_list_expr(args)?;
                self.int_expr(save_id)?;
                self.str_expr(save_info)?;
                self.chunk.push_bc(BcKind::SaveData, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            // TODO: CheckDataStmt
            // Cmd::CheckData(x) => {
            //     self.expr_int(x.save_id)?;
            //     let bc_start_pos = self.chunk.cur_bytes_cnt();
            //     self.arr_set("RESULT", vec![], x.src_info, |this| {
            //         this.chunk.emit_bytecode(CheckData, x.src_info);
            //         Some(TInteger)
            //     })?;
            //     self.peephole_optimization_pop(bc_start_pos, x.src_info);
            // }
            EraNode::StmtGetTime => {
                self.int_var_static_idx("RESULT", stmt_span, 0)?;
                self.chunk.push_bc(BcKind::GetHostTime, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
                self.str_var_static_idx("RESULTS", stmt_span, 0)?;
                self.chunk.push_bc(BcKind::GetHostTimeS, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtLoadGlobal => {
                self.int_var_static_idx("RESULT", stmt_span, 0)?;
                self.chunk.push_bc(BcKind::LoadGlobal, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtSaveGlobal => {
                self.int_var_static_idx("RESULT", stmt_span, 0)?;
                self.chunk.push_bc(BcKind::SaveGlobal, stmt_span);
                self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
                self.chunk.push_pop_all(1, stmt_span);
            }
            EraNode::StmtLoadGame => {
                let func_idx = self.match_user_func_sig(
                    "SYSPROC_LOADGAME",
                    stmt_span,
                    &[],
                    ScalarValueKind::Void,
                )?;
                self.chunk.push_load_imm(func_idx as _, stmt_span);
                self.chunk
                    .push_bc(BcKind::CallFun { args_cnt: 0 }, stmt_span);
            }
            EraNode::StmtSaveGame => {
                let func_idx = self.match_user_func_sig(
                    "SYSPROC_SAVEGAME",
                    stmt_span,
                    &[],
                    ScalarValueKind::Void,
                )?;
                self.chunk.push_load_imm(func_idx as _, stmt_span);
                self.chunk
                    .push_bc(BcKind::CallFun { args_cnt: 0 }, stmt_span);
            }
            EraNode::StmtDebugClear => {
                // TODO: DebugClearStmt
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    stmt_span,
                    "DEBUGCLEAR is not yet supported; ignoring",
                );
                diag.emit_to(self.o.ctx);
            }
            EraNode::StmtResetData => {
                self.chunk.push_bc(BcKind::ResetData, stmt_span);
            }
            _ => {
                let mut diag = self.make_diag();
                diag.span_err(Default::default(), stmt_span, "invalid statement");
                diag.emit_to(self.o.ctx);
                return Err(());
            }
        }

        Ok(())
    }

    fn stmt_row_assign(&mut self, stmt: EraNodeRef) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let EraNode::StmtRowAssign(base_assign, extra_values) = self.arena.get_node(stmt) else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                stmt_span,
                "invalid row assignment statement",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let EraNode::ExprBinary(target, _, first_value) = self.arena.get_node(base_assign) else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(base_assign),
                "invalid left-hand side in assignment",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let EraNode::ListExpr(extra_values) = self.arena.get_node(extra_values) else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(extra_values),
                "invalid right-hand side in assignment",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let extra_values = self.arena.get_extra_data_view(extra_values);

        // Load target
        let target_span = self.arena.get_node_span(target);
        let target_kind = self.norm_var_idx(target, true)?;

        // Load rhs
        let mut rhs_count = 0;
        for rhs in Some(first_value)
            .into_iter()
            .chain(extra_values.iter().map(|x| EraNodeRef(*x)))
        {
            let rhs_span = self.arena.get_node_span(rhs);
            let rhs = match self.expression(rhs)? {
                ScalarValueKind::Empty => {
                    match target_kind {
                        ScalarValueKind::Int => {
                            self.chunk.push_load_imm(0, rhs_span);
                        }
                        ScalarValueKind::Str => {
                            self.chunk.push_build_string(0, rhs_span);
                        }
                        _ => unreachable!("invalid target kind"),
                    }
                    target_kind
                }
                other => other,
            };
            if target_kind != rhs {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    rhs_span,
                    "incompatible types in assignment",
                );
                diag.emit_to(self.o.ctx);
                return Err(());
            }
            rhs_count += 1;
        }

        // Assign
        self.chunk.push_bc(
            BcKind::RowAssign {
                vals_cnt: rhs_count,
            },
            stmt_span,
        );

        Ok(())
    }

    fn stmt_print_emit_bc(&mut self, flags: EraPrintExtendedFlags, span: SrcSpan) {
        use EraPrintExtendedFlags as Flags;
        match u8::from(flags) {
            _ if flags == Flags::new() => {
                self.chunk.push_bc(BcKind::Print, span);
            }
            _ if flags == Flags::new().with_is_line(true) => {
                self.chunk.push_bc(BcKind::PrintLine, span);
            }
            _ => {
                self.chunk.push_bc(BcKind::PrintExtended { flags }, span);
            }
        }
    }

    fn stmt_wait(&mut self, any_key: bool, is_force: bool, span: SrcSpan) {
        let flags = EraWaitFlags::new()
            .with_any_key(any_key)
            .with_is_force(is_force);
        self.chunk.push_bc(BcKind::Wait { flags }, span);
    }

    /// Stack input: `<none>`
    ///
    /// Stack output: composed string
    fn stmt_strdata_build<'a>(
        &mut self,
        after_selection_fn: impl FnOnce(&mut Self, u32) -> CompileResult<()>,
        data: EraExtraDataRef,
        span: SrcSpan,
    ) -> CompileResult<()> {
        // let EraNode::ListExpr(data) = self.arena.get_node(data) else {
        //     let mut diag = self.make_diag();
        //     diag.span_err(Default::default(), span, "invalid STRDATA statement");
        //     diag.emit_to(self.o.ctx);
        //     return Err(());
        // };
        // let data = self.arena.get_extra_data_view(data);
        let data = self.arena.get_list_extra_data(data);

        let jp_cond = self.chunk.push_jump(span);

        // Generate string parts
        let mut parts_cnt = 0;
        let mut cp_starts = Vec::new();
        let mut jp_ends = Vec::new();
        for part in data.iter().map(|x| EraNodeRef(*x)) {
            cp_starts.push(self.chunk.checkpoint());
            let part_span = self.arena.get_node_span(part);
            match self.arena.get_node(part) {
                EraNode::ListExpr(part) => {
                    let part = self.arena.get_extra_data_view(part);
                    let mut parts_cnt = 0;
                    let newline_key = self.o.ctx.interner().get_or_intern("\n");
                    for part in part.iter().map(|x| EraNodeRef(*x)) {
                        if parts_cnt != 0 {
                            self.chunk.push_load_const_str(newline_key, part_span);
                            parts_cnt += 1;
                        }
                        // let part_span = self.arena.get_node_span(part);
                        self.str_expr(part)?;
                        parts_cnt += 1;
                    }
                    self.chunk.push_build_string(parts_cnt, part_span);
                }
                _ => {
                    self.str_expr(part)?;
                }
            }

            parts_cnt += 1;
            jp_ends.push(self.chunk.push_jump(part_span));
        }

        // Generate random selections & check conditions
        jp_cond.complete_here(self.chunk);
        self.chunk.push_load_imm(parts_cnt as _, span);
        self.chunk.push_bc(BcKind::GetRandomMax, span);
        after_selection_fn(self, parts_cnt)?;
        for (i, cp_start) in cp_starts.into_iter().enumerate() {
            self.chunk.push_duplicate_all(1, span);
            self.chunk.push_load_imm(i as _, span);
            self.chunk.push_bc(BcKind::CmpIntEq, span);
            self.chunk.push_jump_if(span).complete(self.chunk, cp_start);
        }

        // Fallback (should be unreachable)
        self.chunk.push_build_string(0, span);

        // Finialize
        for jp_end in jp_ends {
            jp_end.complete_here(self.chunk);
        }
        // NOTE: We can safely pop later since no `GOTO` can happen here
        self.chunk.push_pop_one(2, span);

        Ok(())
    }

    fn stmt_if(&mut self, cur_scope: u32, stmt: EraNodeRef) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let EraNode::StmtIf(stmt) = self.arena.get_node(stmt) else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid IF statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let stmt = self.arena.get_extra_data_view(stmt);

        let mut jp_else: Option<EraBcChunkJumpPoint> = None;
        let mut jp_ends = Vec::new();
        let mut iter = stmt.chunks_exact(2);
        while let Some(chunk) = iter.next() {
            if let Some(jp_else) = jp_else.take() {
                jp_else.complete_here(self.chunk);
            }

            let cond = EraNodeRef(chunk[0]);
            let body = EraNodeRef(chunk[1]);
            let cond_span = self.arena.get_node_span(cond);
            self.int_expr(cond)?;
            jp_else = Some(self.chunk.push_jump_if_not(cond_span));
            self.statements_list(body, cur_scope)?;
            jp_ends.push(self.chunk.push_jump(stmt_span));
        }
        // ELSE part
        if let Some(jp_else) = jp_else {
            jp_else.complete_here(self.chunk);
        }
        if let Some(body) = iter.remainder().get(0).map(|x| EraNodeRef(*x)) {
            self.statements_list(body, cur_scope)?;
        }

        // Complete jumps
        for jp_end in jp_ends {
            jp_end.complete_here(self.chunk);
        }

        Ok(())
    }

    fn stmt_selectcase(&mut self, cur_scope: u32, stmt: EraNodeRef) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let EraNode::StmtSelectCase(value, body) = self.arena.get_node(stmt) else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid IF statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let body = self.arena.get_list_extra_data(body);

        let value_span = self.arena.get_node_span(value);
        let value = self.expression(value)?;
        if !value.is_value() {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                value_span,
                "SELECTCASE value cannot be void or empty",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }

        // SELECTCASE body
        let mut jp_else: Option<EraBcChunkJumpPoint> = None;
        let mut jp_ends = Vec::new();
        let mut iter = body.chunks_exact(2);
        while let Some(chunk) = iter.next() {
            if let Some(jp_else) = jp_else.take() {
                jp_else.complete_here(self.chunk);
            }

            let preds = EraNodeRef(chunk[0]);
            let body = EraNodeRef(chunk[1]);
            let preds_span = self.arena.get_node_span(preds);
            let EraNode::ListSelectCasePred(preds) = self.arena.get_node(preds) else {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    preds_span,
                    "invalid SELECTCASE predicates",
                );
                diag.emit_to(self.o.ctx);
                return Err(());
            };
            let preds = self.arena.get_extra_data_view(preds);
            // Generate CASE conditions
            let mut jp_bodies = Vec::new();
            for pred in preds.iter().map(|x| EraNodeRef(*x)) {
                let pred_span = self.arena.get_node_span(pred);
                self.chunk.push_duplicate_all(1, pred_span);
                let cond_ty = match self.arena.get_node(pred) {
                    EraNode::SelectCaseCondSingle(value) => {
                        let value_span = self.arena.get_node_span(value);
                        let value = self.expression(value)?;
                        let bc = match value {
                            ScalarValueKind::Int => BcKind::CmpIntEq,
                            ScalarValueKind::Str => BcKind::CmpStrEq,
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    value_span,
                                    "invalid SELECTCASE predicate",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        };
                        self.chunk.push_bc(bc, value_span);
                        value
                    }
                    EraNode::SelectCaseCondRange(start, end) => {
                        let start_span = self.arena.get_node_span(start);
                        let end_span = self.arena.get_node_span(end);
                        let start = self.expression(start)?;
                        let end = self.expression(end)?;
                        if start != end {
                            let mut diag = self.make_diag();
                            diag.span_err(
                                Default::default(),
                                start_span,
                                "incompatible types in SELECTCASE range condition",
                            );
                            diag.emit_to(self.o.ctx);
                            return Err(());
                        }
                        let bc = match start {
                            ScalarValueKind::Int => BcKind::InRangeInt,
                            ScalarValueKind::Str => BcKind::InRangeStr,
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    start_span,
                                    "invalid SELECTCASE range condition",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        };
                        self.chunk.push_bc(bc, start_span);
                        start
                    }
                    EraNode::SelectCaseCondOperator(op, rhs) => {
                        let op_span = self.arena.get_node_token_span(pred);
                        let rhs_span = self.arena.get_node_span(rhs);
                        let rhs = self.expression(rhs)?;
                        let bc = match rhs {
                            ScalarValueKind::Int => match op {
                                Token::CmpEq => BcKind::CmpIntEq,
                                Token::CmpNEq => BcKind::CmpIntNEq,
                                Token::CmpLT => BcKind::CmpIntLT,
                                Token::CmpLEq => BcKind::CmpIntLEq,
                                Token::CmpGT => BcKind::CmpIntGT,
                                Token::CmpGEq => BcKind::CmpIntGEq,
                                Token::BitAnd => BcKind::BitAndInt,
                                _ => {
                                    let mut diag = self.make_diag();
                                    diag.span_err(
                                        Default::default(),
                                        op_span,
                                        "invalid operator in SELECTCASE condition",
                                    );
                                    diag.emit_to(self.o.ctx);
                                    return Err(());
                                }
                            },
                            ScalarValueKind::Str => match op {
                                Token::CmpEq => BcKind::CmpStrEq,
                                Token::CmpNEq => BcKind::CmpStrNEq,
                                Token::CmpLT => BcKind::CmpStrLT,
                                Token::CmpLEq => BcKind::CmpStrLEq,
                                Token::CmpGT => BcKind::CmpStrGT,
                                Token::CmpGEq => BcKind::CmpStrGEq,
                                _ => {
                                    let mut diag = self.make_diag();
                                    diag.span_err(
                                        Default::default(),
                                        op_span,
                                        "invalid operator in SELECTCASE condition",
                                    );
                                    diag.emit_to(self.o.ctx);
                                    return Err(());
                                }
                            },
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    rhs_span,
                                    "invalid SELECTCASE condition",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        };
                        self.chunk.push_bc(bc, op_span);
                        rhs
                    }
                    _ => {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            pred_span,
                            "invalid SELECTCASE predicate",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                };
                if cond_ty != value {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        pred_span,
                        "type of SELECTCASE condition is incompatible with value",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                jp_bodies.push(self.chunk.push_jump_if(pred_span));
            }

            // Finialize CASE jumps
            jp_else = Some(self.chunk.push_jump(preds_span));
            for jp_body in jp_bodies {
                jp_body.complete_here(self.chunk);
            }

            // Pop stack early to prevent unbalanced stack
            self.chunk.push_pop_all(1, preds_span);

            self.statements_list(body, cur_scope)?;
            jp_ends.push(self.chunk.push_jump(stmt_span));
        }
        // CASEELSE part
        if let Some(jp_else) = jp_else {
            jp_else.complete_here(self.chunk);
        }
        // Pop stack early to prevent unbalanced stack
        self.chunk.push_pop_all(1, stmt_span);
        if let Some(body) = iter.remainder().get(0).map(|x| EraNodeRef(*x)) {
            self.statements_list(body, cur_scope)?;
        }

        // Complete jumps
        for jp_end in jp_ends {
            jp_end.complete_here(self.chunk);
        }

        Ok(())
    }

    fn stmt_while(
        &mut self,
        cur_scope: u32,
        stmt: EraNodeRef, // StmtWhile
    ) -> CompileResult<EraLoopStructCodeMetadata> {
        let stmt_span = self.arena.get_node_span(stmt);
        let EraNode::StmtWhile(cond, body) = self.arena.get_node(stmt) else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid WHILE statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let cond_span = self.arena.get_node_span(cond);

        let cp_continue = self.chunk.checkpoint();
        self.int_expr(cond)?;
        let jp_end = self.chunk.push_jump_if_not(cond_span);
        self.statements_list(body, cur_scope)?;
        self.chunk
            .push_jump(cond_span)
            .complete(self.chunk, cp_continue);
        let cp_break = self.chunk.checkpoint();
        jp_end.complete_here(self.chunk);

        Ok(EraLoopStructCodeMetadata {
            continue_cp: cp_continue,
            break_cp: cp_break,
        })
    }

    fn stmt_call(&mut self, stmt: EraNodeRef) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let (EraNode::StmtCall(name, args) | EraNode::StmtJump(name, args)) =
            self.arena.get_node(stmt)
        else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid CALL statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };

        if let EraNode::Identifier(name_key) = self.arena.get_node(name) {
            // Prefer static function call
            let name_span = self.arena.get_node_span(name);
            let name_str = self.o.ctx.resolve_str(name_key);
            let result = self.static_func_call(name_str, name_span, args, false)?;
            if result.is_value() {
                self.chunk.push_pop_all(1, stmt_span);
            }
        } else {
            // Dynamic function call
            if self.dynamic_func_call(name, args, false)?.is_value() {
                self.chunk.push_pop_all(1, stmt_span);
            }
        }

        Ok(())
    }

    fn stmt_trycall(&mut self, stmt: EraNodeRef) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let (EraNode::StmtTryCall(name, args) | EraNode::StmtTryJump(name, args)) =
            self.arena.get_node(stmt)
        else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid TRYCALL statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };

        if let EraNode::Identifier(name_key) = self.arena.get_node(name) {
            // Prefer static function call
            let name_span = self.arena.get_node_span(name);
            let name_str = self.o.ctx.resolve_str(name_key);
            let result = self.static_func_call(name_str, name_span, args, true)?;
            if result.is_value() {
                self.chunk.push_pop_all(1, stmt_span);
            }
        } else {
            // Dynamic function call
            if self.dynamic_func_call(name, args, true)?.is_value() {
                self.chunk.push_pop_all(1, stmt_span);
            }
        }

        Ok(())
    }

    fn stmt_tryccall(&mut self, cur_scope: u32, stmt: EraNodeRef) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let Some(EraNodeStmtTryCCallHomo {
            name,
            args,
            then_stmts,
            catch_stmts,
        }) = EraNodeStmtTryCCallHomo::try_get_from(self.arena, stmt)
        else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid TRYCCALL statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };

        if let EraNode::Identifier(name_key) = self.arena.get_node(name) {
            // Prefer static function call
            let name_span = self.arena.get_node_span(name);
            let name_str = self.o.ctx.resolve_str(name_key);
            let result = self.static_func_call(name_str, name_span, args, true)?;
            if result.is_value() {
                self.chunk.push_pop_all(1, stmt_span);
            }
            // NOTE: We will not compile the unreachable code, but will still perform code checking.
            let is_success = !result.is_empty();
            // Success body
            {
                let cp = self.chunk.checkpoint();
                self.statements_list(then_stmts, cur_scope)?;
                if !is_success {
                    self.chunk.rollback_to(cp);
                }
            }
            // Catch body
            {
                let cp = self.chunk.checkpoint();
                self.statements_list(catch_stmts, cur_scope)?;
                if is_success {
                    self.chunk.rollback_to(cp);
                }
            }
        } else {
            // Dynamic function call
            assert!(self.dynamic_func_call(name, args, true)?.is_value());
            let jp_catch = self.chunk.push_jump_if_not(stmt_span);
            self.statements_list(then_stmts, cur_scope)?;
            let jp_end = self.chunk.push_jump(stmt_span);
            jp_catch.complete_here(self.chunk);
            self.statements_list(catch_stmts, cur_scope)?;
            jp_end.complete_here(self.chunk);
        }

        Ok(())
    }

    fn stmt_repeat_loop(
        &mut self,
        cur_scope: u32,
        stmt: EraNodeRef, // StmtRepeat
    ) -> CompileResult<EraLoopStructCodeMetadata> {
        // NOTE: We simplify the process by constructing a FOR loop on stack.
        let stmt_span = self.arena.get_node_span(stmt);
        let EraNode::StmtRepeat(count, stmts) = self.arena.get_node(stmt) else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid REPEAT statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let count_span = self.arena.get_node_span(count);

        // Build loop structure
        let var = self.var_static_idx("COUNT", stmt_span, 0)?;
        if var != ScalarValueKind::Int {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                stmt_span,
                "COUNT variable must be integer",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        self.chunk.push_duplicate_all(2, stmt_span);
        self.chunk.push_load_imm(0, stmt_span);
        self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
        self.chunk.push_pop_all(1, stmt_span);
        let count = self.expression(count)?;
        if count != ScalarValueKind::Int {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                count_span,
                "REPEAT loop count must be an integer",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        self.chunk.push_load_imm(1, stmt_span);
        let cur_scope = self.allocate_scope_id(cur_scope, 4);

        // REPEAT loop prologue
        let jp_body = self.chunk.push_jump(stmt_span);
        // Continue part
        let cp_continue = self.chunk.checkpoint();
        self.chunk.push_bc(BcKind::ForLoopStep, stmt_span);
        let jp_end = self.chunk.push_jump_if_not(stmt_span);

        // REPEAT loop body
        jp_body.complete_here(self.chunk);
        self.statements_list(stmts, cur_scope)?;
        self.chunk
            .push_jump(stmt_span)
            .complete(self.chunk, cp_continue);

        // REPEAT loop epilogue
        // NOTE: Emulates Eramaker behavior (inc COUNT even when break'ing)
        let cp_break = self.chunk.checkpoint();
        self.chunk.push_duplicate_one(4, stmt_span);
        self.chunk.push_duplicate_one(4, stmt_span);
        self.chunk.push_duplicate_all(2, stmt_span);
        self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
        self.chunk.push_duplicate_one(4, stmt_span);
        self.chunk.push_bc(BcKind::AddInt, stmt_span);
        self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
        self.chunk.push_pop_all(1, stmt_span);
        jp_end.complete_here(self.chunk);
        self.chunk.push_pop_all(4, stmt_span);

        Ok(EraLoopStructCodeMetadata {
            continue_cp: cp_continue,
            break_cp: cp_break,
        })
    }

    fn stmt_for_loop(
        &mut self,
        cur_scope: u32,
        stmt: EraNodeRef, // StmtFor
    ) -> CompileResult<EraLoopStructCodeMetadata> {
        let stmt_span = self.arena.get_node_span(stmt);
        // Prepare var+idx, start_val, end_val, step_val
        let Some(EraNodeStmtFor {
            var,
            start,
            end,
            step,
            stmts,
        }) = EraNodeStmtFor::try_get_from(self.arena, stmt)
        else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid FOR statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let var_span = self.arena.get_node_span(var);
        let start_span = self.arena.get_node_span(start);
        let end_span = self.arena.get_node_span(end);
        let step_span = self.arena.get_node_span(step);

        // Build FOR loop structure
        self.int_norm_var_idx(var, true)?;
        // NOTE: Duplicate to assign `start` to `var`
        // ----- Start assign of initial -----
        self.chunk.push_duplicate_all(2, var_span);
        self.int_expr(start)?;
        self.chunk.push_bc(BcKind::SetArrValFlat, start_span);
        self.chunk.push_pop_all(1, stmt_span);
        // ----- End assign of initial -----
        self.int_expr(end)?;
        self.int_expr_or(step.into(), 1)?;
        let cur_scope = self.allocate_scope_id(cur_scope, 4);

        // FOR loop prologue
        let jp_body = self.chunk.push_jump(stmt_span);
        // Continue part
        let cp_continue = self.chunk.checkpoint();
        self.chunk.push_bc(BcKind::ForLoopStep, stmt_span);
        let jp_end = self.chunk.push_jump_if_not(stmt_span);

        // FOR loop body
        jp_body.complete_here(self.chunk);
        self.statements_list(stmts, cur_scope)?;
        self.chunk
            .push_jump(stmt_span)
            .complete(self.chunk, cp_continue);

        // FOR loop epilogue
        // NOTE: Emulates Eramaker behavior (inc COUNT even when break'ing)
        let cp_break = self.chunk.checkpoint();
        self.chunk.push_duplicate_one(4, stmt_span);
        self.chunk.push_duplicate_one(4, stmt_span);
        self.chunk.push_duplicate_all(2, stmt_span);
        self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
        self.chunk.push_duplicate_one(4, stmt_span);
        self.chunk.push_bc(BcKind::AddInt, stmt_span);
        self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
        self.chunk.push_pop_all(1, stmt_span);
        jp_end.complete_here(self.chunk);
        self.chunk.push_pop_all(4, stmt_span);

        Ok(EraLoopStructCodeMetadata {
            continue_cp: cp_continue,
            break_cp: cp_break,
        })
    }

    fn stmt_do_loop(
        &mut self,
        cur_scope: u32,
        stmt: EraNodeRef, // StmtDoLoop
    ) -> CompileResult<EraLoopStructCodeMetadata> {
        let stmt_span = self.arena.get_node_span(stmt);
        let EraNode::StmtDoLoop(stmts, cond) = self.arena.get_node(stmt) else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid DO statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let cond_span = self.arena.get_node_span(cond);

        let cp_body = self.chunk.checkpoint();
        self.statements_list(stmts, cur_scope)?;
        let cp_continue = self.chunk.checkpoint();
        self.int_expr(cond)?;
        self.chunk
            .push_jump_if(cond_span)
            .complete(self.chunk, cp_body);
        let cp_break = self.chunk.checkpoint();

        Ok(EraLoopStructCodeMetadata {
            continue_cp: cp_continue,
            break_cp: cp_break,
        })
    }

    fn stmt_setcolor(&mut self, stmt: EraNodeRef, dest_var: &str) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let (EraNode::StmtSetColor(args) | EraNode::StmtSetBgColor(args)) =
            self.arena.get_node(stmt)
        else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid SETCOLOR statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let args = self.unwrap_list_expr(args)?;
        let args_count = args.len();
        self.int_var_static_idx(dest_var, stmt_span, 0)?;
        // 0x00RRGGBB
        match args_count {
            1 => {
                let a0 = EraNodeRef(args[0]);
                self.int_expr(a0)?;
            }
            3 => {
                // TODO: Check argument range [0, 255]
                let a0 = EraNodeRef(args[0]);
                let a1 = EraNodeRef(args[1]);
                let a2 = EraNodeRef(args[2]);
                self.int_expr(a0)?;
                self.chunk.push_load_imm(16, stmt_span);
                self.chunk.push_bc(BcKind::ShlInt, stmt_span);
                self.int_expr(a1)?;
                self.chunk.push_load_imm(8, stmt_span);
                self.chunk.push_bc(BcKind::ShlInt, stmt_span);
                self.int_expr(a2)?;
                self.chunk.push_bc(BcKind::BitOrInt, stmt_span);
                self.chunk.push_bc(BcKind::BitOrInt, stmt_span);
            }
            _ => {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    stmt_span,
                    format!("SETCOLOR does not take {args_count} arguments"),
                );
                diag.emit_to(self.o.ctx);
                return Err(());
            }
        }
        self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
        self.chunk.push_pop_all(1, stmt_span);
        Ok(())
    }

    fn stmt_resetcolor(
        &mut self,
        stmt: EraNodeRef,
        dest_var: &str,
        orig_var: &str,
    ) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let (EraNode::StmtResetColor | EraNode::StmtResetBgColor) = self.arena.get_node(stmt)
        else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                stmt_span,
                "invalid RESETCOLOR statement",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        self.int_var_static_idx(dest_var, stmt_span, 0)?;
        self.int_var_static_idx(orig_var, stmt_span, 0)?;
        self.chunk.push_bc(BcKind::GetArrValFlat, stmt_span);
        self.chunk.push_bc(BcKind::SetArrValFlat, stmt_span);
        self.chunk.push_pop_all(1, stmt_span);
        Ok(())
    }

    fn stmt_printbutton(
        &mut self,
        stmt: EraNodeRef,
        flags: EraPrintExtendedFlags,
    ) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let (EraNode::StmtPrintButton(args)
        | EraNode::StmtPrintButtonC(args)
        | EraNode::StmtPrintButtonLC(args)) = self.arena.get_node(stmt)
        else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                stmt_span,
                "invalid PRINTBUTTON statement",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let ([content, value], []) = self.unpack_list_expr(args)?;
        self.str_expr(content)?;
        let value = self.expression(value)?;
        match value {
            ScalarValueKind::Int => {
                self.chunk.push_bc(BcKind::IntToStr, stmt_span);
            }
            ScalarValueKind::Str => (),
            ScalarValueKind::Empty => {
                self.chunk.push_build_string(0, stmt_span);
            }
            ScalarValueKind::Void => {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    stmt_span,
                    "void cannot be used as a value here",
                );
                diag.emit_to(self.o.ctx);
                return Err(());
            }
        }
        self.chunk.push_bc(BcKind::PrintButton { flags }, stmt_span);
        Ok(())
    }

    fn stmt_input(&mut self, stmt: EraNodeRef, is_string: bool) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let (EraNode::StmtInput(args) | EraNode::StmtInputS(args)) = self.arena.get_node(stmt)
        else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid INPUT statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let ([], [default_value, can_click, allow_skip]) = self.unpack_list_expr(args)?;
        let flags = EraInputExtendedFlags::new()
            .with_is_string(is_string)
            .with_is_one(false)
            .with_is_timed(false)
            .with_has_default_value(default_value.is_expr());
        {
            let ty = if is_string {
                ScalarValueKind::Str
            } else {
                ScalarValueKind::Int
            };
            self.expr_or_default(default_value, ty)?;
        }
        self.int_expr_or(can_click, 0)?;
        self.int_expr_or(allow_skip, 0)?;
        self.chunk.push_bc(BcKind::Input { flags }, stmt_span);
        Ok(())
    }

    fn stmt_tinput(&mut self, stmt: EraNodeRef, is_string: bool) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let (EraNode::StmtTInput(args) | EraNode::StmtTInputS(args)) = self.arena.get_node(stmt)
        else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid TINPUT statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let ([time_limit, default_value], [show_prompt, expiry_msg, can_click]) =
            self.unpack_list_expr(args)?;
        let flags = EraInputExtendedFlags::new()
            .with_is_string(is_string)
            .with_is_one(false)
            .with_is_timed(true)
            .with_has_default_value(true);
        self.int_expr(time_limit)?;
        if is_string {
            self.str_expr(default_value)?;
        } else {
            self.int_expr(default_value)?;
        }
        self.int_expr_or(show_prompt, 1)?;
        // TODO: Load default expiry message from config or CSV
        self.str_expr_or(expiry_msg, "時間切れ")?;
        self.int_expr_or(can_click, 0)?;
        self.chunk.push_bc(BcKind::Input { flags }, stmt_span);
        Ok(())
    }

    fn stmt_oneinput(&mut self, stmt: EraNodeRef, is_string: bool) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let (EraNode::StmtOneInput(args) | EraNode::StmtOneInputS(args)) =
            self.arena.get_node(stmt)
        else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid ONEINPUT statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let ([], [default_value]) = self.unpack_list_expr(args)?;
        let flags = EraInputExtendedFlags::new()
            .with_is_string(is_string)
            .with_is_one(true)
            .with_is_timed(false)
            .with_has_default_value(default_value.is_expr());
        {
            let ty = if is_string {
                ScalarValueKind::Str
            } else {
                ScalarValueKind::Int
            };
            self.expr_or_default(default_value, ty)?;
        }
        self.chunk.push_bc(BcKind::Input { flags }, stmt_span);
        Ok(())
    }

    fn stmt_toneinput(&mut self, stmt: EraNodeRef, is_string: bool) -> CompileResult<()> {
        let stmt_span = self.arena.get_node_span(stmt);
        let (EraNode::StmtTOneInput(args) | EraNode::StmtTOneInputS(args)) =
            self.arena.get_node(stmt)
        else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), stmt_span, "invalid TONEINPUT statement");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let ([time_limit, default_value], [show_prompt, expiry_msg, can_click]) =
            self.unpack_list_expr(args)?;
        let flags = EraInputExtendedFlags::new()
            .with_is_string(is_string)
            .with_is_one(true)
            .with_is_timed(true)
            .with_has_default_value(true);
        self.int_expr(time_limit)?;
        if is_string {
            self.str_expr(default_value)?;
        } else {
            self.int_expr(default_value)?;
        }
        self.int_expr_or(show_prompt, 1)?;
        // TODO: Load default expiry message from config or CSV
        self.str_expr_or(expiry_msg, "時間切れ")?;
        self.int_expr_or(can_click, 0)?;
        self.chunk.push_bc(BcKind::Input { flags }, stmt_span);
        Ok(())
    }

    fn apply_loop_stmt(
        &mut self,
        func: impl FnOnce(&mut Self) -> CompileResult<EraLoopStructCodeMetadata>,
    ) -> CompileResult<()> {
        let previous_loop = self.cur_loop_struct.replace(Default::default());
        let mut this = scopeguard::guard(self, |this| {
            this.cur_loop_struct = previous_loop;
        });
        let loop_data = func(*this)?;
        let loop_struct = this.cur_loop_struct.take().unwrap();
        for jp in loop_struct.continue_queue {
            jp.complete(this.chunk, loop_data.continue_cp);
        }
        for jp in loop_struct.break_queue {
            jp.complete(this.chunk, loop_data.break_cp);
        }
        Ok(())
    }

    // TODO: Add an parameter `need_retval` to optimize out the return value if not needed
    fn expression(&mut self, expr: EraNodeRef) -> CompileResult<ScalarValueKind> {
        let expr_span = self.arena.get_node_span(expr);
        match self.arena.get_node(expr) {
            EraNode::Invalid => {
                // Silently ignore invalid expressions from the parser
                Err(())
            }
            EraNode::LiteralInt(x) => {
                self.chunk.push_load_imm(x.into(), expr_span);
                Ok(ScalarValueKind::Int)
            }
            EraNode::LiteralStr(x) => {
                self.chunk.push_load_const_str(x, expr_span);
                Ok(ScalarValueKind::Str)
            }
            EraNode::Identifier(_) | EraNode::ExprVarNamespace(..) | EraNode::ExprVarIdx(_) => {
                self.var_idx_get(expr)
            }
            EraNode::Empty => Ok(ScalarValueKind::Empty),
            EraNode::ExprBinary(lhs, op, rhs) => {
                let op: Token = op.into();
                let lhs_span = self.arena.get_node_span(lhs);
                let rhs_span = self.arena.get_node_span(rhs);
                let op_span = self.arena.get_node_token_span(expr);

                if matches!(op, Token::Assign | Token::ExprAssign) {
                    self.var_idx_set(lhs, |this| this.expression(rhs), true)
                } else if matches!(op, Token::LogicalAnd | Token::LogicalOr) {
                    let lhs = self.expression(lhs)?;
                    if lhs != ScalarValueKind::Int {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            lhs_span,
                            "left-hand side of logical operator must be an integer",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                    self.chunk.push_duplicate_all(1, op_span);
                    let jp_end = if op == Token::LogicalAnd {
                        self.chunk.push_jump_if_not(op_span)
                    } else {
                        self.chunk.push_jump_if(op_span)
                    };
                    self.chunk.push_pop_all(1, op_span);
                    let rhs = self.expression(rhs)?;
                    if rhs != ScalarValueKind::Int {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            rhs_span,
                            "right-hand side of logical operator must be an integer",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                    jp_end.complete_here(self.chunk);
                    Ok(ScalarValueKind::Int)
                } else if matches!(
                    op,
                    Token::PlusAssign
                        | Token::MinusAssign
                        | Token::MultiplyAssign
                        | Token::DivideAssign
                        | Token::ModuloAssign
                        | Token::BitAndAssign
                        | Token::BitOrAssign
                        | Token::BitXorAssign
                ) {
                    let id_info = self.var_idx(lhs, true)?;
                    let EraIdVariableKind::Normal(lhs) = id_info.kind else {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            lhs_span,
                            "cannot assign to pseudo-variable",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    };
                    let lhs = lhs.to_scalar();
                    self.chunk.push_duplicate_all(2, op_span);
                    self.chunk.push_bc(BcKind::GetArrValFlat, op_span);

                    let rhs = self.expression(rhs)?;
                    let result = match op {
                        Token::PlusAssign => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::AddInt, op_span);
                                ScalarValueKind::Int
                            }
                            (ScalarValueKind::Str, ScalarValueKind::Str) => {
                                self.chunk.push_build_string(2, op_span);
                                ScalarValueKind::Str
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `+` have incompatible types",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::MinusAssign => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::SubInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `-` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::MultiplyAssign => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::MulInt, op_span);
                                ScalarValueKind::Int
                            }
                            (ScalarValueKind::Str, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::RepeatStr, op_span);
                                ScalarValueKind::Str
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "cannot multiply by string",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::DivideAssign => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::DivInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `/` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::ModuloAssign => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::ModInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `%` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::BitAndAssign => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::BitAndInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `&` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::BitOrAssign => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::BitOrInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `|` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::BitXorAssign => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::BitXorInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `^` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        _ => unreachable!(),
                    };
                    self.chunk.push_bc(BcKind::SetArrValFlat, op_span);
                    Ok(result)
                } else {
                    let lhs = self.expression(lhs)?;
                    let rhs = self.expression(rhs)?;
                    let result = match op {
                        Token::Plus => match (lhs, rhs) {
                            (ScalarValueKind::Str, ScalarValueKind::Str) => {
                                self.chunk.push_build_string(2, op_span);
                                ScalarValueKind::Str
                            }
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::AddInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `+` have incompatible types",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::Minus => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::SubInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `-` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::Multiply => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::MulInt, op_span);
                                ScalarValueKind::Int
                            }
                            (ScalarValueKind::Str, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::RepeatStr, op_span);
                                ScalarValueKind::Str
                            }
                            (ScalarValueKind::Int, ScalarValueKind::Str) => {
                                self.chunk.push_bc(BcKind::Swap2, op_span);
                                self.chunk.push_bc(BcKind::RepeatStr, op_span);
                                ScalarValueKind::Str
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "cannot multiply string by string",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::Divide => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::DivInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `/` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::Percentage => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::ModInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `%` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::CmpEq => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::CmpIntEq, op_span);
                                ScalarValueKind::Int
                            }
                            (ScalarValueKind::Str, ScalarValueKind::Str) => {
                                self.chunk.push_bc(BcKind::CmpStrEq, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `==` must be of the same type",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::CmpNEq => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::CmpIntNEq, op_span);
                                ScalarValueKind::Int
                            }
                            (ScalarValueKind::Str, ScalarValueKind::Str) => {
                                self.chunk.push_bc(BcKind::CmpStrNEq, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `!=` must be of the same type",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::CmpLT => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::CmpIntLT, op_span);
                                ScalarValueKind::Int
                            }
                            (ScalarValueKind::Str, ScalarValueKind::Str) => {
                                self.chunk.push_bc(BcKind::CmpStrLT, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `<` must be of the same type",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::CmpLEq => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::CmpIntLEq, op_span);
                                ScalarValueKind::Int
                            }
                            (ScalarValueKind::Str, ScalarValueKind::Str) => {
                                self.chunk.push_bc(BcKind::CmpStrLEq, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `<=` must be of the same type",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::CmpGT => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::CmpIntGT, op_span);
                                ScalarValueKind::Int
                            }
                            (ScalarValueKind::Str, ScalarValueKind::Str) => {
                                self.chunk.push_bc(BcKind::CmpStrGT, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `>` must be of the same type",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::CmpGEq => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::CmpIntGEq, op_span);
                                ScalarValueKind::Int
                            }
                            (ScalarValueKind::Str, ScalarValueKind::Str) => {
                                self.chunk.push_bc(BcKind::CmpStrGEq, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `>=` must be of the same type",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::BitAnd => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::BitAndInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `&` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::BitOr => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::BitOrInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `|` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::BitXor => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::BitXorInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `^` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::BitShiftL => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::ShlInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `<<` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::BitShiftR => match (lhs, rhs) {
                            (ScalarValueKind::Int, ScalarValueKind::Int) => {
                                self.chunk.push_bc(BcKind::ShrInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operands to binary `>>` must be integers",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        _ => unimplemented!("unsupported operator `{:?}`", op),
                    };
                    Ok(result)
                }
            }
            EraNode::ExprParen(expr) => self.expression(expr),
            EraNode::ExprTernary(..) => {
                let EraNodeExprTernary {
                    cond,
                    then_expr,
                    else_expr,
                } = EraNodeExprTernary::get_from(self.arena, expr);
                let cond_span = self.arena.get_node_span(cond);
                let then_span = self.arena.get_node_span(then_expr);
                let else_span = self.arena.get_node_span(else_expr);

                let cond = self.expression(cond)?;
                if cond != ScalarValueKind::Int {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        cond_span,
                        "condition in ternary operator must be an integer",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                let jp_else = self.chunk.push_jump_if_not(cond_span);
                let then_t = self.expression(then_expr)?;
                let jp_end = self.chunk.push_jump(cond_span);
                self.chunk.complete_jump(jp_else, self.chunk.checkpoint());
                let else_t = self.expression(else_expr)?;
                self.chunk.complete_jump(jp_end, self.chunk.checkpoint());
                if then_t != else_t {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        else_span,
                        "incompatible types in ternary expression",
                    );
                    diag.span_note(
                        Default::default(),
                        then_span,
                        format!("true branch is of type `{then_t}`"),
                    );
                    diag.span_note(
                        Default::default(),
                        else_span,
                        format!("false branch is of type `{else_t}`"),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                Ok(then_t)
            }
            EraNode::StringForm(extra_data) => {
                let parts = self.arena.get_small_extra_data_view(extra_data);
                let parts_cnt = parts.len().try_into().expect("too many string form parts");
                for part in parts.iter().map(|x| EraNodeRef(*x)) {
                    let part_span = self.arena.get_node_span(part);
                    match self.arena.get_node(part) {
                        EraNode::StringFormInterpPart(_) => {
                            let EraNodeStringFormInterpPart {
                                expr,
                                width,
                                alignment,
                            } = EraNodeStringFormInterpPart::get_from(self.arena, part);
                            let expr_span = self.arena.get_node_span(expr);
                            let expr = self.expression(expr)?;
                            match expr {
                                ScalarValueKind::Int => {
                                    self.chunk.push_bc(BcKind::IntToStr, expr_span);
                                }
                                ScalarValueKind::Str => (),
                                _ => {
                                    let mut diag = self.make_diag();
                                    diag.span_err(
                                        Default::default(),
                                        expr_span,
                                        "invalid expression in string interpolation",
                                    );
                                    diag.emit_to(self.o.ctx);
                                    return Err(());
                                }
                            }
                            if let Some(width) = width {
                                self.int_expr(width)?;
                                let flags = if let Some(align) = alignment {
                                    let mut flags = EraPadStringFlags::new();
                                    let align_span = self.arena.get_node_span(align);
                                    let align = self.unwrap_identifier(align)?;
                                    let align = self.o.ctx.interner().resolve(align);
                                    if align.eq_ignore_ascii_case("LEFT") {
                                        flags.set_left_pad(true);
                                    } else if align.eq_ignore_ascii_case("RIGHT") {
                                        flags.set_right_pad(true);
                                    } else {
                                        let mut diag = self.make_diag();
                                        diag.span_err(
                                            Default::default(),
                                            align_span,
                                            "invalid alignment in string interpolation",
                                        );
                                        diag.emit_to(self.o.ctx);
                                        return Err(());
                                    }
                                    flags
                                } else {
                                    EraPadStringFlags::new()
                                };
                                self.chunk.push_bc(BcKind::PadString { flags }, expr_span);
                            }
                        }
                        _ => {
                            // String literals
                            self.str_expr(part)?;
                        }
                    }
                }
                if parts_cnt != 1 {
                    self.chunk.push_build_string(parts_cnt, expr_span);
                }
                Ok(ScalarValueKind::Str)
            }
            EraNode::ExprPreUnary(op, rhs) => {
                let op_span = self.arena.get_node_token_span(expr);
                let rhs_span = self.arena.get_node_span(rhs);

                if matches!(op, Token::Increment | Token::Decrement) {
                    let id_info = self.var_idx(rhs, true)?;
                    let EraIdVariableKind::Normal(rhs) = id_info.kind else {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            rhs_span,
                            "cannot assign to pseudo-variable",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    };
                    let rhs = rhs.to_scalar();
                    self.chunk
                        .push_bc(BcKind::DuplicateAllN { count: 2 }, op_span);
                    self.chunk.push_bc(BcKind::GetArrValFlat, op_span);
                    self.chunk.push_load_imm(1, op_span);
                    let result = match op {
                        Token::Increment => match rhs {
                            ScalarValueKind::Int => {
                                self.chunk.push_bc(BcKind::AddInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "cannot increment string",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::Decrement => match rhs {
                            ScalarValueKind::Int => {
                                self.chunk.push_bc(BcKind::SubInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "cannot decrement string",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        _ => unreachable!(),
                    };
                    self.chunk.push_bc(BcKind::SetArrValFlat, op_span);
                    Ok(result)
                } else {
                    let rhs = self.expression(rhs)?;
                    let result = match op {
                        Token::Plus => match rhs {
                            ScalarValueKind::Int => ScalarValueKind::Int,
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operand to unary `+` must be an integer",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::Minus => match rhs {
                            ScalarValueKind::Int => {
                                self.chunk.push_bc(BcKind::NegInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operand to unary `-` must be an integer",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::BitNot => match rhs {
                            ScalarValueKind::Int => {
                                self.chunk.push_bc(BcKind::BitNotInt, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operand to unary `~` must be an integer",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        Token::LogicalNot => match rhs {
                            ScalarValueKind::Int => {
                                self.chunk.push_bc(BcKind::LogicalNot, op_span);
                                ScalarValueKind::Int
                            }
                            _ => {
                                let mut diag = self.make_diag();
                                diag.span_err(
                                    Default::default(),
                                    op_span,
                                    "operand to unary `!` must be an integer",
                                );
                                diag.emit_to(self.o.ctx);
                                return Err(());
                            }
                        },
                        _ => unimplemented!("unsupported operator `{:?}`", op),
                    };
                    Ok(result)
                }
            }
            EraNode::ExprPostUnary(lhs, op) => {
                let op_span = self.arena.get_node_token_span(expr);
                let lhs_span = self.arena.get_node_span(lhs);

                let id_info = self.var_idx(lhs, true)?;
                let EraIdVariableKind::Normal(lhs) = id_info.kind else {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        lhs_span,
                        "cannot assign to pseudo-variable",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                let lhs = lhs.to_scalar();
                self.chunk
                    .push_bc(BcKind::DuplicateAllN { count: 2 }, op_span);
                self.chunk.push_bc(BcKind::GetArrValFlat, op_span);
                self.chunk.push_load_imm(1, op_span);
                let post_bc;
                let result = match op {
                    Token::Increment => match lhs {
                        ScalarValueKind::Int => {
                            self.chunk.push_bc(BcKind::AddInt, op_span);
                            post_bc = BcKind::SubInt;
                            ScalarValueKind::Int
                        }
                        _ => {
                            let mut diag = self.make_diag();
                            diag.span_err(Default::default(), op_span, "cannot increment string");
                            diag.emit_to(self.o.ctx);
                            return Err(());
                        }
                    },
                    Token::Decrement => match lhs {
                        ScalarValueKind::Int => {
                            self.chunk.push_bc(BcKind::SubInt, op_span);
                            post_bc = BcKind::AddInt;
                            ScalarValueKind::Int
                        }
                        _ => {
                            let mut diag = self.make_diag();
                            diag.span_err(Default::default(), op_span, "cannot decrement string");
                            diag.emit_to(self.o.ctx);
                            return Err(());
                        }
                    },
                    _ => unreachable!(),
                };
                self.chunk.push_bc(BcKind::SetArrValFlat, op_span);
                self.chunk.push_load_imm(1, op_span);
                self.chunk.push_bc(post_bc, op_span);
                Ok(result)
            }
            EraNode::ExprFunCall(name, args) => {
                let name_span = self.arena.get_node_span(name);
                let name = self.unwrap_identifier(name)?;
                let name = self.o.ctx.resolve_str(name);
                self.static_func_call(name, name_span, args, false)
            }
            _ => {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    expr_span,
                    format!("invalid expression {:?}", expr),
                );
                diag.emit_to(self.o.ctx);
                Err(())
            }
        }
    }

    fn id_variable(&mut self, var: EraNodeRef) -> CompileResult<EraIdVariableInfo> {
        let var_span = self.arena.get_node_span(var);
        let name;
        let name_str = match self.arena.get_node(var) {
            EraNode::Identifier(name_key) => {
                name = Some(name_key);
                self.o.ctx.interner().resolve(name_key)
            }
            EraNode::ExprVarNamespace(var_name, namespace) => {
                name = None;
                let var_name = self.unwrap_identifier(var_name)?;
                let namespace = self.unwrap_identifier(namespace)?;
                let var_name = self.o.ctx.interner().resolve(var_name);
                let namespace = self.o.ctx.interner().resolve(namespace);
                &format!("{var_name}@{namespace}")
            }
            _ => {
                let mut diag = self.make_diag();
                diag.span_err(Default::default(), var_span, "invalid variable expression");
                diag.emit_to(self.o.ctx);
                return Err(());
            }
        };

        let func_info = self.cur_func.get();

        if let Some(name) = name {
            // Try lookup in local scope
            if let Some(var) = func_info.frame_info.vars.get(Ascii::new_str(name_str)) {
                let var_idx = var.var_idx;
                if var.in_local_frame {
                    self.chunk
                        .push_bc(BcKind::LoadLocalVar { idx: var_idx as _ }, var_span);
                } else {
                    self.chunk
                        .push_bc(BcKind::LoadVarWW { idx: var_idx }, var_span);
                }
                return Ok(EraIdVariableInfo {
                    kind: EraIdVariableKind::Normal(var.var_kind),
                    dims_cnt: var.dims_cnt,
                    name_token: None, // Ignored because not interesting to the caller
                    is_charadata: var.is_charadata,
                });
            }
        }

        if let Some(var_idx) = self.o.ctx.variables.get_var_idx(name_str) {
            let var_info = self.o.ctx.variables.get_var_info(var_idx).unwrap();
            var_info.val.ensure_alloc();
            self.chunk
                .push_bc(BcKind::LoadVarWW { idx: var_idx as _ }, var_span);
            Ok(EraIdVariableInfo {
                kind: EraIdVariableKind::Normal(var_info.val.kind()),
                dims_cnt: var_info.val.dims_cnt().unwrap().try_into().unwrap(),
                name_token: name,
                is_charadata: var_info.is_charadata,
            })
        } else if name_str.eq_ignore_ascii_case("RAND") {
            Ok(EraIdVariableInfo {
                kind: EraIdVariableKind::Pseudo(EraPseudoVariableKind::Rand),
                dims_cnt: 1,
                name_token: None,
                is_charadata: false,
            })
        } else if name_str.eq_ignore_ascii_case("CHARANUM") {
            Ok(EraIdVariableInfo {
                kind: EraIdVariableKind::Pseudo(EraPseudoVariableKind::CharaNum),
                dims_cnt: 0,
                name_token: None,
                is_charadata: false,
            })
        } else if name_str.eq_ignore_ascii_case("CALLERFUNCNAME") {
            Ok(EraIdVariableInfo {
                kind: EraIdVariableKind::Pseudo(EraPseudoVariableKind::CallerFuncName),
                dims_cnt: 0,
                name_token: None,
                is_charadata: false,
            })
        } else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                var_span,
                format!("undefined variable `{}`", name_str),
            );
            diag.emit_to(self.o.ctx);
            Err(())
        }
    }

    fn var_idx(&mut self, node: EraNodeRef, fix_chara: bool) -> CompileResult<EraIdVariableInfo> {
        self.var_idx_inner(node, fix_chara, true)
    }

    fn var_mdidx(&mut self, node: EraNodeRef, fix_chara: bool) -> CompileResult<EraIdVariableInfo> {
        self.var_idx_inner(node, fix_chara, false)
    }

    fn var_idx_inner(
        &mut self,
        node: EraNodeRef,
        fix_chara: bool,
        flatten_indices: bool,
    ) -> CompileResult<EraIdVariableInfo> {
        let node_span = self.arena.get_node_span(node);

        let _var_indices;
        let (var, indices);
        indices = if let Some(EraNodeExprVarIdx { var_indices }) =
            EraNodeExprVarIdx::try_get_from(self.arena, node)
        {
            var = EraNodeRef(var_indices[0]);
            _var_indices = Some(var_indices);
            &_var_indices.as_ref().unwrap()[1..]
        } else {
            var = node;
            _var_indices = None;
            &[]
        };

        let id_info = self.id_variable(var)?;
        match id_info.kind {
            EraIdVariableKind::Normal(kind) => {
                let var_kind = kind.with_arr();
                let dims_cnt = id_info.dims_cnt;
                let mut idxs_cnt = indices.len() as u8;
                let is_charadata = id_info.is_charadata;
                let (is_chara_nodim, csv_var_kind);
                if let Some(name_token) = id_info.name_token {
                    let name = self.o.ctx.interner().resolve(name_token);
                    is_chara_nodim = routines::is_chara_nodim(name);
                    csv_var_kind = EraCsvVarKind::try_from_var(name);
                } else {
                    is_chara_nodim = false;
                    csv_var_kind = None;
                }

                // For CHARA variables without dimensions, we need to fix the index by appending 0
                let mut prepend_target: bool = false;
                let mut prepend_zero = false;
                let mut append_zeros_cnt = 0;
                if is_chara_nodim {
                    idxs_cnt += 1;
                    append_zeros_cnt += 1;
                }

                if idxs_cnt > dims_cnt {
                    let mut diag = self.make_diag();
                    diag.span_err(Default::default(), node_span, "too many indices into array");
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                if dims_cnt > 1 && idxs_cnt < dims_cnt {
                    if is_charadata {
                        if fix_chara {
                            // Push implicit TARGET as the first index
                            prepend_target = true;
                        } else {
                            prepend_zero = true;
                        }
                        idxs_cnt += 1;
                        if idxs_cnt < dims_cnt {
                            let mut diag = self.make_diag();
                            diag.span_warn(
                                Default::default(),
                                node_span,
                                "too few indices into CHARADATA variable",
                            );
                            diag.emit_to(self.o.ctx);
                            // Push implicit 0 as last index to workaround bugs in some game code
                            append_zeros_cnt += dims_cnt - idxs_cnt;
                        }
                    } else {
                        let mut diag = self.make_diag();
                        diag.span_warn(
                            Default::default(),
                            node_span,
                            format!("non-compliant use of {}-D array variable", dims_cnt),
                        );
                        diag.emit_to(self.o.ctx);
                    }
                }
                if dims_cnt == 1 && idxs_cnt < dims_cnt {
                    // Push 0 implicitly
                    append_zeros_cnt += 1;
                }

                // Compile indices
                if prepend_target {
                    let target = self
                        .o
                        .ctx
                        .variables
                        .get_var_idx("TARGET")
                        .expect("TARGET not found");
                    self.chunk
                        .push_bc(BcKind::LoadVarWW { idx: target as _ }, node_span);
                    self.chunk.push_load_imm(0, node_span);
                    self.chunk.push_bc(BcKind::GetArrValFlat, node_span);
                }
                if prepend_zero {
                    self.chunk.push_load_imm(0, node_span);
                }
                for idx in indices.iter().map(|x| EraNodeRef(*x)) {
                    let idx_span = self.arena.get_node_span(idx);

                    // Try handle CSV contextual indices
                    if csv_var_kind.is_some()
                        && matches!(self.arena.get_node(idx), EraNode::Identifier(_))
                    {
                        let csv_var_kind = csv_var_kind.unwrap();
                        let name = self.unwrap_identifier(idx).unwrap();
                        let name = self.o.ctx.interner().resolve(name);
                        if let Some(idx) = self
                            .o
                            .ctx
                            .csv_indices
                            .get(Ascii::new_str(name))
                            .and_then(|x| x.iter().find(|x| x.0 == csv_var_kind))
                            .map(|x| x.1)
                        {
                            self.chunk.push_load_imm(idx.into(), idx_span);
                            continue;
                        }
                    }

                    // Proceed with normal index handling
                    let idx_kind = self.expression(idx)?;
                    match idx_kind {
                        ScalarValueKind::Int => (),
                        ScalarValueKind::Str if csv_var_kind.is_some() => {
                            // Dynamic CSV string to index translation
                            let csv_var_kind = csv_var_kind.unwrap();
                            {
                                let mut diag = self.make_diag();
                                diag.span_warn(
                                    Default::default(),
                                    idx_span,
                                    "using strings as array indices is discouraged; consider wrapping in `GETNUM`",
                                );
                                diag.emit_to(self.o.ctx);
                            }
                            self.chunk
                                .push_bc(BcKind::CsvGetNum { kind: csv_var_kind }, idx_span);
                        }
                        _ => {
                            let mut diag = self.make_diag();
                            diag.span_err(
                                Default::default(),
                                idx_span,
                                "array indices must be integers",
                            );
                            diag.emit_to(self.o.ctx);
                            return Err(());
                        }
                    }
                }
                for _ in 0..append_zeros_cnt {
                    self.chunk.push_load_imm(0, node_span);
                }

                // Flatten indices
                if flatten_indices && dims_cnt > 1 {
                    self.chunk.push_bc(
                        BcKind::BuildArrIdxFromMD {
                            count: dims_cnt.try_into().unwrap(),
                        },
                        node_span,
                    );
                }
            }
            EraIdVariableKind::Pseudo(kind) => match kind {
                EraPseudoVariableKind::Rand => {
                    if indices.len() != 1 {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            node_span,
                            "`RAND` requires exactly one parameter",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }

                    let idx = EraNodeRef(indices[0]);
                    let idx_span = self.arena.get_node_span(idx);
                    let idx_kind = self.expression(idx)?;
                    if idx_kind != ScalarValueKind::Int {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            idx_span,
                            "array indices must be integers",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                }
                EraPseudoVariableKind::CharaNum => {
                    if indices.len() != 0 {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            node_span,
                            "`CHARANUM` cannot be indexed",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                }
                EraPseudoVariableKind::CallerFuncName => {
                    if indices.len() != 0 {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            node_span,
                            "`CALLERFUNCNAME` cannot be indexed",
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                }
            },
        }

        Ok(id_info)
    }

    fn var_idx_get(&mut self, node: EraNodeRef) -> CompileResult<ScalarValueKind> {
        let node_span = self.arena.get_node_span(node);
        let id_info = self.var_idx(node, true)?;
        match id_info.kind {
            EraIdVariableKind::Normal(kind) => {
                self.chunk.push_bc(BcKind::GetArrValFlat, node_span);
                Ok(kind.to_scalar())
            }
            EraIdVariableKind::Pseudo(kind) => match kind {
                EraPseudoVariableKind::Rand => {
                    self.chunk.push_bc(BcKind::GetRandomMax, node_span);
                    Ok(ScalarValueKind::Int)
                }
                EraPseudoVariableKind::CharaNum => {
                    self.chunk.push_bc(BcKind::GetCharaNum, node_span);
                    Ok(ScalarValueKind::Int)
                }
                EraPseudoVariableKind::CallerFuncName => {
                    self.chunk.push_bc(BcKind::GetCallerFuncName, node_span);
                    Ok(ScalarValueKind::Str)
                }
            },
        }
    }

    fn var_idx_set(
        &mut self,
        node: EraNodeRef,
        rhs: impl FnOnce(&mut Self) -> CompileResult<ScalarValueKind>,
        keep_retval: bool,
    ) -> CompileResult<ScalarValueKind> {
        let node_span = self.arena.get_node_span(node);
        let id_info = self.var_idx(node, true)?;
        match id_info.kind {
            EraIdVariableKind::Normal(kind) => {
                let kind = kind.to_scalar();
                let value_kind = rhs(self)?;
                if kind != value_kind {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        node_span,
                        format!("incompatible types in assignment (expected {kind}, found {value_kind})"),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                self.chunk.push_bc(BcKind::SetArrValFlat, node_span);
                // TODO: Optimize based on `keep_retval`
                if !keep_retval {
                    self.chunk.push_pop_all(1, node_span);
                }
                Ok(kind)
            }
            EraIdVariableKind::Pseudo(_) => {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    node_span,
                    "cannot assign to pseudo-variable",
                );
                diag.emit_to(self.o.ctx);
                return Err(());
            }
        }
    }

    /// Usually used programmatically (i.e. not from the user script). For example, you can use this
    /// to specify `RESULT:1` without creating nodes. Note that pseudo-variables are not supported here.
    fn var_static_idx(
        &mut self,
        name: &str,
        name_span: SrcSpan,
        idx: u32,
    ) -> CompileResult<ScalarValueKind> {
        // TODO: Maybe also support searching in function scope?

        let Some(var_idx) = self.o.ctx.variables.get_var_idx(name) else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                name_span,
                format!("undefined variable `{}`", name),
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let var_info = self.o.ctx.variables.get_var_info(var_idx).unwrap();
        let var_kind = var_info.val.kind().to_scalar();

        // Load variable
        var_info.val.ensure_alloc();
        self.chunk.push_bc(
            BcKind::LoadVarWW {
                idx: var_idx.try_into().unwrap(),
            },
            name_span,
        );
        self.chunk.push_load_imm(idx.into(), name_span);

        Ok(var_kind)
    }

    fn var_static(&mut self, name: &str, name_span: SrcSpan) -> CompileResult<ScalarValueKind> {
        let Some(var_idx) = self.o.ctx.variables.get_var_idx(name) else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                name_span,
                format!("undefined variable `{}`", name),
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let var_info = self.o.ctx.variables.get_var_info(var_idx).unwrap();
        let var_kind = var_info.val.kind().to_scalar();

        // Load variable
        var_info.val.ensure_alloc();
        self.chunk.push_bc(
            BcKind::LoadVarWW {
                idx: var_idx.try_into().unwrap(),
            },
            name_span,
        );

        Ok(var_kind)
    }

    fn int_var_static(&mut self, name: &str, name_span: SrcSpan) -> CompileResult<()> {
        let ty = self.var_static(name, name_span)?;
        if ty != ScalarValueKind::Int {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), name_span, "expected integer variable");
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        Ok(())
    }

    fn str_var_static(&mut self, name: &str, name_span: SrcSpan) -> CompileResult<()> {
        let ty = self.var_static(name, name_span)?;
        if ty != ScalarValueKind::Str {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), name_span, "expected string variable");
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        Ok(())
    }

    fn norm_var(&mut self, node: EraNodeRef) -> CompileResult<ScalarValueKind> {
        let info = self.id_variable(node)?;
        if let EraIdVariableKind::Normal(kind) = info.kind {
            Ok(kind.to_scalar())
        } else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(node),
                "expected non-pseudo variable",
            );
            diag.emit_to(self.o.ctx);
            Err(())
        }
    }

    fn int_norm_var(&mut self, node: EraNodeRef) -> CompileResult<()> {
        let info = self.id_variable(node)?;
        let EraIdVariableKind::Normal(ty) = info.kind else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(node),
                "expected non-pseudo variable",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let ty = ty.to_scalar();
        if ty != ScalarValueKind::Int {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(node),
                "expected integer variable",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        Ok(())
    }

    fn str_norm_var(&mut self, node: EraNodeRef) -> CompileResult<()> {
        let info = self.id_variable(node)?;
        let EraIdVariableKind::Normal(ty) = info.kind else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(node),
                "expected non-pseudo variable",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let ty = ty.to_scalar();
        if ty != ScalarValueKind::Str {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(node),
                "expected string variable",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        Ok(())
    }

    fn norm_var_idx(
        &mut self,
        node: EraNodeRef,
        fix_chara: bool,
    ) -> CompileResult<ScalarValueKind> {
        let info = self.var_idx(node, fix_chara)?;
        if let EraIdVariableKind::Normal(kind) = info.kind {
            Ok(kind.to_scalar())
        } else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(node),
                "expected non-pseudo variable",
            );
            diag.emit_to(self.o.ctx);
            Err(())
        }
    }

    fn int_norm_var_idx(&mut self, node: EraNodeRef, fix_chara: bool) -> CompileResult<()> {
        let info = self.var_idx(node, fix_chara)?;
        let EraIdVariableKind::Normal(ty) = info.kind else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(node),
                "expected non-pseudo variable",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let ty = ty.to_scalar();
        if ty != ScalarValueKind::Int {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(node),
                "expected integer variable",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        Ok(())
    }

    fn str_norm_var_idx(&mut self, node: EraNodeRef, fix_chara: bool) -> CompileResult<()> {
        let info = self.var_idx(node, fix_chara)?;
        let EraIdVariableKind::Normal(ty) = info.kind else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(node),
                "expected non-pseudo variable",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let ty = ty.to_scalar();
        if ty != ScalarValueKind::Str {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(node),
                "expected string variable",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        Ok(())
    }

    fn int_var_static_idx(
        &mut self,
        name: &str,
        name_span: SrcSpan,
        idx: u32,
    ) -> CompileResult<()> {
        let ty = self.var_static_idx(name, name_span, idx)?;
        if ty != ScalarValueKind::Int {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), name_span, "expected integer variable");
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        Ok(())
    }

    fn str_var_static_idx(
        &mut self,
        name: &str,
        name_span: SrcSpan,
        idx: u32,
    ) -> CompileResult<()> {
        let ty = self.var_static_idx(name, name_span, idx)?;
        if ty != ScalarValueKind::Str {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), name_span, "expected string variable");
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        Ok(())
    }

    fn int_expr(&mut self, expr: EraNodeRef) -> CompileResult<()> {
        let ty = self.expression(expr)?;
        if ty != ScalarValueKind::Int {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(expr),
                "expected integer expression",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        Ok(())
    }

    fn str_expr(&mut self, expr: EraNodeRef) -> CompileResult<()> {
        let ty = self.expression(expr)?;
        if ty != ScalarValueKind::Str {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(expr),
                "expected string expression",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        Ok(())
    }

    fn int_expr_or(&mut self, expr: EraExprOrSpan, fallback: i64) -> CompileResult<()> {
        match expr {
            EraExprOrSpan::Expr(expr) => {
                let expr_span = self.arena.get_node_span(expr);
                let expr_ty = self.expression(expr)?;
                match expr_ty {
                    ScalarValueKind::Int => (),
                    ScalarValueKind::Empty => {
                        self.chunk.push_load_imm(fallback, expr_span);
                    }
                    _ => {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            expr_span,
                            format!("expected integer expression, found {}", expr_ty),
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                }
            }
            EraExprOrSpan::Span(span) => {
                self.chunk.push_load_imm(fallback, span);
            }
        }
        Ok(())
    }

    fn str_expr_or(&mut self, expr: EraExprOrSpan, fallback: &str) -> CompileResult<()> {
        match expr {
            EraExprOrSpan::Expr(expr) => {
                let expr_span = self.arena.get_node_span(expr);
                let expr_ty = self.expression(expr)?;
                match expr_ty {
                    ScalarValueKind::Str => (),
                    ScalarValueKind::Empty => {
                        let value = self.o.ctx.interner().get_or_intern(fallback);
                        self.chunk.push_bc(
                            BcKind::LoadConstStr {
                                idx: value.into_u32(),
                            },
                            expr_span,
                        );
                    }
                    _ => {
                        let mut diag = self.make_diag();
                        diag.span_err(
                            Default::default(),
                            expr_span,
                            format!("expected string expression, found {}", expr_ty),
                        );
                        diag.emit_to(self.o.ctx);
                        return Err(());
                    }
                }
            }
            EraExprOrSpan::Span(span) => {
                let value = self.o.ctx.interner().get_or_intern(fallback);
                self.chunk.push_bc(
                    BcKind::LoadConstStr {
                        idx: value.into_u32(),
                    },
                    span,
                );
            }
        }
        Ok(())
    }

    fn expr_or_default(&mut self, expr: EraExprOrSpan, ty: ScalarValueKind) -> CompileResult<()> {
        assert!(
            ty == ScalarValueKind::Int || ty == ScalarValueKind::Str,
            "invalid expression type {:?}",
            ty
        );

        match expr {
            EraExprOrSpan::Expr(expr) => {
                let expr_span = self.arena.get_node_span(expr);
                let expr_ty = self.expression(expr)?;
                if expr_ty == ScalarValueKind::Empty {
                    match ty {
                        ScalarValueKind::Int => {
                            self.chunk.push_load_imm(0, expr_span);
                        }
                        ScalarValueKind::Str => {
                            self.chunk
                                .push_bc(BcKind::LoadConstStr { idx: 0 }, expr_span);
                        }
                        _ => unreachable!(),
                    }
                } else if expr_ty != ty {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        expr_span,
                        format!("expected {} expression, found {}", ty, expr_ty),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
            }
            EraExprOrSpan::Span(span) => match ty {
                ScalarValueKind::Int => {
                    self.chunk.push_load_imm(0, span);
                }
                ScalarValueKind::Str => {
                    self.chunk.push_bc(BcKind::LoadConstStr { idx: 0 }, span);
                }
                _ => unreachable!(),
            },
        }

        Ok(())
    }

    // fn expr_to_var_opt_idx<'a>(
    //     &mut self,
    //     expr: EraExprNodeOrLeaf<'a>,
    // ) -> CompileResult<EraVarOptIdxExprConstruct<'a>> {
    //     let Some(expr) = EraVarOptIdxExprConstruct::cast(expr.inner()) else {
    //         let mut diag = self.make_diag();
    //         diag.span_err(
    //             Default::default(),
    //             expr.src_span(),
    //             "expected variable expression",
    //         );
    //         diag.emit_to(self.o.ctx);
    //         return Err(());
    //     };
    //     Ok(expr)
    // }

    // fn expr_to_var<'a>(
    //     &mut self,
    //     expr: EraExprNodeOrLeaf<'a>,
    // ) -> CompileResult<EraVarExprNodeOrLeaf<'a>> {
    //     let Some(expr) = EraVarExprNodeOrLeaf::cast(expr.inner()) else {
    //         let mut diag = self.make_diag();
    //         diag.span_err(
    //             Default::default(),
    //             expr.src_span(),
    //             "expected variable expression",
    //         );
    //         diag.emit_to(self.o.ctx);
    //         return Err(());
    //     };
    //     Ok(expr)
    // }

    fn const_eval_int(&mut self, expr: EraNodeRef) -> CompileResult<i64> {
        let mut interp = EraInterpreter::new(self.o.ctx, self.arena, true);
        interp.interpret_int_expr_ok(expr).ok_or(())
    }

    fn const_eval_str(&mut self, expr: EraNodeRef) -> CompileResult<String> {
        let mut interp = EraInterpreter::new(self.o.ctx, self.arena, true);
        interp.interpret_str_expr_ok(expr).ok_or(())
    }

    fn static_func_call(
        &mut self,
        name: &str,
        name_span: SrcSpan,
        args: EraNodeRef, // ListExpr
        is_fallible: bool,
    ) -> CompileResult<ScalarValueKind> {
        // Find in user-defined functions first
        let Some((target_idx, target_func)) = self
            .cur_func
            .backing_cart()
            .get_full(Ascii::new_str(name))
            .and_then(|(idx, _, v)| v.as_ref().map(|x| (idx, x)))
        else {
            if is_fallible {
                return Ok(ScalarValueKind::Empty);
            }
            // Look in builtin functions instead
            return self.builtin_func_call(name, name_span, args, false);
        };

        let target_chunk_idx = target_func.chunk_idx as usize;

        let args_span = self.arena.get_node_span(args);
        let EraNode::ListExpr(args) = self.arena.get_node(args) else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), args_span, "expected argument list");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let args = self.arena.get_extra_data_view(args);
        let mut args_it = args.iter().map(|x| EraNodeRef(*x)).fuse();

        // Fill parameters
        for target_param in &target_func.frame_info.args {
            // NOTE: If argument does not exist, treat as Empty
            if target_param.var_kind.is_arr() {
                // REF parameter; resolve to array itself
                let Some(arg) = args_it.next() else {
                    let arg_span = SrcSpan::new(args_span.end(), 0);
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        arg_span,
                        "reference parameter cannot be omitted",
                    );
                    diag.span_note(
                        self.o.ctx.bc_chunks[target_chunk_idx].name.clone(),
                        target_func.name_span,
                        "see signature of callee",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                if let EraNode::Empty = self.arena.get_node(arg) {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        self.arena.get_node_span(arg),
                        "reference parameter cannot be omitted",
                    );
                    diag.span_note(
                        self.o.ctx.bc_chunks[target_chunk_idx].name.clone(),
                        target_func.name_span,
                        "see signature of callee",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                let arg = if let Some(EraNodeExprVarIdx { var_indices }) =
                    EraNodeExprVarIdx::try_get_from(self.arena, arg)
                {
                    let mut diag = self.make_diag();
                    diag.span_warn(
                        Default::default(),
                        self.arena.get_node_span(arg),
                        "for REF parameters, specifying indices is discouraged, and will not be evaluated",
                    );
                    diag.emit_to(self.o.ctx);
                    let arg = EraNodeRef(var_indices[0]);
                    arg
                } else {
                    arg
                };

                // Load variable
                let arg_span = self.arena.get_node_span(arg);
                let arg = self.id_variable(arg)?;
                let EraIdVariableKind::Normal(arg_kind) = arg.kind else {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        arg_span,
                        "cannot pass pseudo-variable as reference",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                if arg_kind.to_scalar() != target_param.var_kind.to_scalar() {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        arg_span,
                        "incompatible types in reference argument",
                    );
                    diag.span_note(
                        self.o.ctx.bc_chunks[target_chunk_idx].name.clone(),
                        target_func.name_span,
                        "see signature of callee",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                if arg.dims_cnt != target_param.dims_cnt {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        arg_span,
                        "incompatible dimensions in reference argument",
                    );
                    diag.span_note(
                        self.o.ctx.bc_chunks[target_chunk_idx].name.clone(),
                        target_func.name_span,
                        "see signature of callee",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
            } else {
                // Normal parameter
                let arg_span;
                let arg = if let Some(arg) = args_it.next() {
                    arg_span = self.arena.get_node_span(arg);
                    self.expression(arg)?
                } else {
                    arg_span = SrcSpan::new(args_span.end(), 0);
                    ScalarValueKind::Empty
                };
                // Fill default value if argument is omitted
                let arg = if arg.is_empty() {
                    match target_param.var_kind {
                        ValueKind::Int => self.chunk.push_load_imm(0, arg_span),
                        ValueKind::Str => self.chunk.push_build_string(0, arg_span),
                        _ => unreachable!(),
                    }
                    target_param.var_kind.to_scalar()
                } else {
                    arg
                };

                if arg != target_param.var_kind.to_scalar() {
                    match (arg, target_param.var_kind.to_scalar()) {
                        (ScalarValueKind::Int, ScalarValueKind::Str) => {
                            // Implicitly cast to string
                            self.chunk.push_bc(BcKind::IntToStr, arg_span);
                        }
                        _ => {
                            let mut diag = self.make_diag();
                            diag.span_err(
                                Default::default(),
                                arg_span,
                                "incompatible types in argument",
                            );
                            diag.span_note(
                                self.o.ctx.bc_chunks[target_chunk_idx].name.clone(),
                                target_func.name_span,
                                "see signature of callee",
                            );
                            diag.emit_to(self.o.ctx);
                            return Err(());
                        }
                    }
                }
            }
        }

        if let Some(arg) = args_it.next() {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                self.arena.get_node_span(arg),
                "too many arguments",
            );
            diag.span_note(
                self.o.ctx.bc_chunks[target_chunk_idx].name.clone(),
                target_func.name_span,
                "see signature of callee",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }

        // Push function number
        self.chunk
            .push_load_imm(target_idx as _, target_func.name_span);
        // Call function
        self.chunk.push_bc(
            BcKind::CallFun {
                args_cnt: target_func.frame_info.args.len() as _,
            },
            args_span,
        );

        Ok(target_func.ret_kind)
    }

    fn dynamic_func_call(
        &mut self,
        name: EraNodeRef,
        args: EraNodeRef,
        is_fallible: bool,
    ) -> CompileResult<ScalarValueKind> {
        let name_span = self.arena.get_node_span(name);
        let args_span = self.arena.get_node_span(args);
        let name = self.expression(name)?;
        if name != ScalarValueKind::Str {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                name_span,
                "function name must be a string",
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }

        /* Design notes:
         *
         * For dynamic function calls, we need to check the function signature at runtime,
         * and coerce arguments as needed. To do this, we introduce `Argument Pack`, which
         * is a pair of `Value`'s. In order to support REF parameters, for expressions that
         * are REF candidates (i.e. variable name plus optional indices), we push values as
         * (Array, FlatIndex) pair. FlatIndex is needed for VM to resolve the pack to scalar
         * values, in case the actual parameter is not REF. If expressions only evaluate to
         * scalar values, we push the pair (Scalar, 0). In case of empty argument, we push a
         * pair (0, 1). (Note the second `1`, which indicates that the value is omitted.)
         */
        // TODO: Maybe we can convert dynamic function call into `eval` call, which is more
        //       flexible and can support built-in functions as well.

        let EraNode::ListExpr(args) = self.arena.get_node(args) else {
            let mut diag = self.make_diag();
            diag.span_err(Default::default(), args_span, "expected argument list");
            diag.emit_to(self.o.ctx);
            return Err(());
        };
        let args = self.arena.get_extra_data_view(args);
        let args_cnt = args.len().try_into().expect("too many arguments");
        for arg in args.iter().map(|x| EraNodeRef(*x)) {
            let arg_span = self.arena.get_node_span(arg);
            match self.arena.get_node(arg) {
                EraNode::ExprVarIdx(_) | EraNode::ExprVarNamespace(..) | EraNode::Identifier(_) => {
                    // REF candidate
                    self.var_idx(arg, false)?;
                }
                EraNode::Empty => {
                    // Empty argument
                    self.chunk.push_load_imm(0, arg_span);
                    self.chunk.push_load_imm(1, arg_span);
                }
                _ => {
                    // Pure value
                    self.expression(arg)?;
                    self.chunk.push_load_imm(0, arg_span);
                }
            }
        }

        // Call function
        let ty = if is_fallible {
            // Try call
            self.chunk
                .push_bc(BcKind::TryCallFun { args_cnt }, args_span);
            ScalarValueKind::Int
        } else {
            // Force call
            self.chunk
                .push_bc(BcKind::TryCallFunForce { args_cnt }, args_span);
            ScalarValueKind::Void
        };

        Ok(ty)
    }

    /// # Arguments
    ///
    /// * `args` - If `true`, the function is called in a command context. Return values are
    /// stored in `RESULT` or `RESULTS`.
    fn builtin_func_call(
        &mut self,
        name: &str,
        name_span: SrcSpan,
        args: EraNodeRef,
        is_cmd: bool,
    ) -> CompileResult<ScalarValueKind> {
        use std::ops::{Deref, DerefMut};

        struct Site<'this, 'o, 'ctx, 'i, 'b, 'arena, 'name, Callback> {
            this: &'this mut EraCodeGenSite<'o, 'ctx, 'i, 'b, 'arena, Callback>,
            ret_kind: ScalarValueKind,
            name: &'name str,
            name_span: SrcSpan,
            assign_to_result: bool,
        }

        impl<'this, 'o, 'ctx, 'i, 'b, 'arena, Callback> Deref
            for Site<'this, 'o, 'ctx, 'i, 'b, 'arena, '_, Callback>
        {
            type Target = EraCodeGenSite<'o, 'ctx, 'i, 'b, 'arena, Callback>;

            fn deref(&self) -> &Self::Target {
                self.this
            }
        }

        impl<'this, 'o, 'ctx, 'i, 'b, 'arena, Callback> DerefMut
            for Site<'this, 'o, 'ctx, 'i, 'b, 'arena, '_, Callback>
        {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.this
            }
        }

        impl<'arena, Callback: EraCompilerCallback> Site<'_, '_, '_, '_, '_, 'arena, '_, Callback> {
            fn result(&mut self) -> CompileResult<()> {
                self.ret_kind = ScalarValueKind::Int;
                if !self.assign_to_result {
                    return Ok(());
                }
                self.this.int_var_static_idx("RESULT", self.name_span, 0)
            }

            fn results(&mut self) -> CompileResult<()> {
                self.ret_kind = ScalarValueKind::Str;
                if !self.assign_to_result {
                    return Ok(());
                }
                self.this.str_var_static_idx("RESULTS", self.name_span, 0)
            }

            fn load_args(&mut self, args: EraNodeRef) -> CompileResult<&'arena [u32]> {
                let EraNode::ListExpr(args) = self.arena.get_node(args) else {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        self.arena.get_node_span(args),
                        "expected argument list",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                Ok(self.arena.get_extra_data_view(args))
            }

            fn unpack_args<'a, const N: usize>(
                &mut self,
                args: EraNodeRef,
            ) -> CompileResult<[EraNodeRef; N]> {
                let args = self.load_args(args)?;
                let args_cnt = args.len();
                if args_cnt != N {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        self.name_span,
                        format!(
                            "function `{}` expects {} arguments, but {} were given",
                            self.name, N, args_cnt
                        ),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                Ok(std::array::from_fn(|i| EraNodeRef(args[i])))
            }

            fn unpack_opt_args<'a, const N: usize>(
                &mut self,
                args: EraNodeRef,
                min_cnt: usize,
            ) -> CompileResult<[Option<EraNodeRef>; N]> {
                let args = self.load_args(args)?;
                let args_cnt = args.len();
                if args_cnt < min_cnt {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        self.name_span,
                        format!(
                            "function `{}` expects at least {} arguments, but {} were given",
                            self.name, min_cnt, args_cnt
                        ),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                if args_cnt > N {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        self.name_span,
                        format!(
                            "function `{}` expects at most {} arguments, but {} were given",
                            self.name, N, args_cnt
                        ),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                Ok(std::array::from_fn(|i| args.get(i).map(|x| EraNodeRef(*x))))
            }

            fn finish(mut self) -> ScalarValueKind {
                let name_span = self.name_span;
                if !self.assign_to_result {
                    return self.ret_kind;
                }
                match self.ret_kind {
                    ScalarValueKind::Int | ScalarValueKind::Str => {
                        self.chunk.push_bc(BcKind::SetArrValFlat, name_span);
                        self.chunk.push_pop_all(1, name_span);
                    }
                    ScalarValueKind::Void => (),
                    _ => unreachable!(),
                }
                self.ret_kind
            }
        }

        let mut site = Site {
            this: self,
            ret_kind: ScalarValueKind::Void,
            name,
            name_span,
            assign_to_result: is_cmd,
        };

        macro_rules! apply_one_arg {
            ($span:expr, $var_name:ident:i) => {
                site.int_expr($var_name)?;
            };
            ($span:expr, $var_name:ident:s) => {
                site.str_expr($var_name)?;
            };
            ($span:expr, $var_name:ident:v) => {{
                // let $var_name = site.expr_to_var($var_name)?;
                site.norm_var($var_name)?;
            }};
            ($span:expr, $var_name:ident:vi) => {{
                // let $var_name = site.expr_to_var($var_name)?;
                site.int_norm_var($var_name)?;
            }};
            ($span:expr, $var_name:ident:vs) => {{
                // let $var_name = site.expr_to_var($var_name)?;
                site.str_norm_var($var_name)?;
            }};
            ($span:expr, $var_name:ident:vii) => {{
                // let $var_name = site.expr_to_var_opt_idx($var_name)?;
                site.int_norm_var_idx($var_name, true)?;
            }};
            ($span:expr, $var_name:ident:vis) => {{
                // let $var_name = site.expr_to_var_opt_idx($var_name)?;
                site.str_norm_var_idx($var_name, true)?;
            }};
            ($span:expr, $var_name:ident:vi || $fallback:expr) => {{
                if let Some($var_name) = $var_name {
                    // let $var_name = site.expr_to_var($var_name)?;
                    site.int_norm_var($var_name)?;
                } else {
                    site.int_var_static($fallback, $span)?;
                }
            }};
            ($span:expr, $var_name:ident:vs || $fallback:expr) => {{
                if let Some($var_name) = $var_name {
                    // let $var_name = site.expr_to_var($var_name)?;
                    site.str_norm_var($var_name)?;
                } else {
                    site.str_var_static($fallback, $span)?;
                }
            }};
            ($span:expr, $var_name:ident:vii || $fallback:expr) => {{
                if let Some($var_name) = $var_name {
                    // let $var_name = site.expr_to_var_opt_idx($var_name)?;
                    site.int_norm_var_idx($var_name, true)?;
                } else {
                    site.int_var_static_idx($fallback, $span, 0)?;
                }
            }};
            ($span:expr, $var_name:ident:vis || $fallback:expr) => {{
                if let Some($var_name) = $var_name {
                    // let $var_name = site.expr_to_var_opt_idx($var_name)?;
                    site.str_norm_var_idx($var_name, true)?;
                } else {
                    site.str_var_static_idx($fallback, $span, 0)?;
                }
            }};
            ($span:expr, $var_name:ident:i?) => {
                site.expr_or_default($var_name.or_span($span), ScalarValueKind::Int)?;
            };
            ($span:expr, $var_name:ident:s?) => {
                site.expr_or_default($var_name.or_span($span), ScalarValueKind::Str)?;
            };
            ($span:expr, $var_name:ident:i || $fallback:expr) => {
                site.int_expr_or($var_name.or_span($span), $fallback)?;
            };
            ($span:expr, $var_name:ident:s || $fallback:expr) => {
                site.str_expr_or($var_name.or_span($span), $fallback)?;
            };
        }
        macro_rules! apply_args {
            ($span:expr,) => {};
            ($span:expr, $arg:ident:$ty:ident, $($tokens:tt)*) => {
                apply_one_arg!($span, $arg:$ty);
                apply_args!($span, $($tokens)*);
            };
            ($span:expr, $arg:ident:$ty:ident?, $($tokens:tt)*) => {
                apply_one_arg!($span, $arg:$ty?);
                apply_args!($span, $($tokens)*);
            };
            ($span:expr, $arg:ident:$ty:ident || $fallback:expr, $($tokens:tt)*) => {
                apply_one_arg!($span, $arg:$ty || $fallback);
                apply_args!($span, $($tokens)*);
            };
            ($span:expr, $arg:ident:$ty:ident) => {
                apply_one_arg!($span, $arg:$ty);
            };
            ($span:expr, $arg:ident:$ty:ident?) => {
                apply_one_arg!($span, $arg:$ty?);
            };
            ($span:expr, $arg:ident:$ty:ident || $fallback:expr) => {
                apply_one_arg!($span, $arg:$ty || $fallback);
            };
        }
        macro_rules! unwrap {
            ($($var:ident),*) => {
                $(let $var = $var.ok_or(())?;)*
            };
        }

        let upper_name = name.to_ascii_uppercase();
        match upper_name.as_bytes() {
            b"GCREATE" => {
                site.result()?;
                let [gid, width, height] = site.unpack_args(args)?;
                apply_args!(name_span, gid:i, width:i, height:i);
                site.chunk.push_bc(BcKind::GCreate, name_span);
            }
            b"GCREATEFROMFILE" => {
                site.result()?;
                let [gid, file_path] = site.unpack_args(args)?;
                apply_args!(name_span, gid:i, file_path:s);
                site.chunk.push_bc(BcKind::GCreateFromFile, name_span);
            }
            b"GDISPOSE" => {
                site.result()?;
                let [gid] = site.unpack_args(args)?;
                apply_args!(name_span, gid:i);
                site.chunk.push_bc(BcKind::GDispose, name_span);
            }
            b"GCREATED" => {
                site.result()?;
                let [gid] = site.unpack_args(args)?;
                apply_args!(name_span, gid:i);
                site.chunk.push_bc(BcKind::GCreated, name_span);
            }
            b"GDRAWSPRITE" => {
                site.result()?;
                let [gid, sprite_name, dest_x, dest_y, dest_width, dest_height, a_cm] =
                    site.unpack_opt_args(args, 2)?;
                unwrap!(gid, sprite_name);
                apply_args!(
                    name_span,
                    gid:i,
                    sprite_name:s,
                    dest_x:i || 0,
                    dest_y:i || 0,
                    dest_width:i || -1,
                    dest_height:i || -1,
                );
                if let Some(color_matrix) = a_cm {
                    apply_one_arg!(name_span, color_matrix:vi);
                    site.chunk
                        .push_bc(BcKind::GDrawSpriteWithColorMatrix, name_span);
                } else {
                    site.chunk.push_bc(BcKind::GDrawSprite, name_span);
                }
            }
            b"GCLEAR" => {
                site.result()?;
                let [gid, color] = site.unpack_args(args)?;
                apply_args!(name_span, gid:i, color:i);
                site.chunk.push_bc(BcKind::GClear, name_span);
            }
            b"SPRITECREATE" => {
                site.result()?;
                let [sprite_name, gid, x, y, width, height] = site.unpack_opt_args(args, 2)?;
                unwrap!(sprite_name, gid);
                apply_args!(
                    name_span,
                    sprite_name:s,
                    gid:i,
                    x:i || 0,
                    y:i || 0,
                    width:i || -1,
                    height:i || -1,
                );
                site.chunk.push_bc(BcKind::SpriteCreate, name_span);
            }
            b"SPRITEDISPOSE" => {
                site.result()?;
                let [name] = site.unpack_args(args)?;
                apply_args!(name_span, name:s);
                site.chunk.push_bc(BcKind::SpriteDispose, name_span);
            }
            b"SPRITECREATED" => {
                site.result()?;
                let [name] = site.unpack_args(args)?;
                apply_args!(name_span, name:s);
                site.chunk.push_bc(BcKind::SpriteCreated, name_span);
            }
            b"SPRITEANIMECREATE" => {
                site.result()?;
                let [name, width, height] = site.unpack_args(args)?;
                apply_args!(name_span, name:s, width:i, height:i);
                site.chunk.push_bc(BcKind::SpriteAnimeCreate, name_span);
            }
            b"SPRITEANIMEADDFRAME" => {
                site.result()?;
                let [name, gid, x, y, width, height, offset_x, offset_y, delay] =
                    site.unpack_args(args)?;
                apply_args!(
                    name_span,
                    name:s,
                    gid:i,
                    x:i,
                    y:i,
                    width:i,
                    height:i,
                    offset_x:i,
                    offset_y:i,
                    delay:i,
                );
                site.chunk.push_bc(BcKind::SpriteAnimeAddFrame, name_span);
            }
            b"SPRITEWIDTH" => {
                site.result()?;
                let [name] = site.unpack_args(args)?;
                apply_args!(name_span, name:s);
                site.chunk.push_bc(BcKind::SpriteWidth, name_span);
            }
            b"SPRITEHEIGHT" => {
                site.result()?;
                let [name] = site.unpack_args(args)?;
                apply_args!(name_span, name:s);
                site.chunk.push_bc(BcKind::SpriteHeight, name_span);
            }
            b"GETBIT" => {
                site.result()?;
                let [val, bit] = site.unpack_args(args)?;
                apply_args!(name_span, val:i, bit:i);
                site.chunk.push_bc(BcKind::GetBit, name_span);
            }
            b"GETSTYLE" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@STYLE", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"CHKFONT" => {
                site.result()?;
                let [font_name] = site.unpack_args(args)?;
                apply_args!(name_span, font_name:s);
                site.chunk.push_bc(BcKind::CheckFont, name_span);
            }
            b"GETFONT" => {
                site.results()?;
                let [] = site.unpack_args(args)?;
                site.str_var_static_idx("@FONT", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"REPLACE" => {
                site.results()?;
                let [haystack, needle, replace_with] = site.unpack_args(args)?;
                apply_args!(name_span, haystack:s, needle:s, replace_with:s);
                site.chunk.push_bc(BcKind::ReplaceStr, name_span);
            }
            b"SUBSTRING" | b"SUBSTRINGU" => {
                site.results()?;
                let [haystack, start_pos, length] = site.unpack_opt_args(args, 2)?;
                unwrap!(haystack, start_pos);
                apply_args!(name_span, haystack:s, start_pos:i, length:i || -1);
                site.chunk.push_bc(
                    match upper_name.as_bytes() {
                        b"SUBSTRING" => BcKind::SubStr,
                        b"SUBSTRINGU" => BcKind::SubStrU,
                        _ => unreachable!(),
                    },
                    name_span,
                );
            }
            b"STRFIND" | b"STRFINDU" => {
                site.result()?;
                let [haystack, needle, start_pos] = site.unpack_opt_args(args, 2)?;
                unwrap!(haystack, needle);
                apply_args!(
                    name_span,
                    haystack:s,
                    needle:s,
                    start_pos:i || 0,
                );
                site.chunk.push_bc(
                    match upper_name.as_bytes() {
                        b"STRFIND" => BcKind::StrFind,
                        b"STRFINDU" => BcKind::StrFindU,
                        _ => unreachable!(),
                    },
                    name_span,
                );
            }
            b"STRLENS" | b"STRLENSU" => {
                site.result()?;
                let [haystack] = site.unpack_args(args)?;
                apply_args!(name_span, haystack:s);
                site.chunk.push_bc(
                    match upper_name.as_bytes() {
                        b"STRLENS" => BcKind::StrLen,
                        b"STRLENSU" => BcKind::StrLenU,
                        _ => unreachable!(),
                    },
                    name_span,
                );
            }
            b"STRCOUNT" => {
                site.result()?;
                let [haystack, needle] = site.unpack_args(args)?;
                apply_args!(name_span, haystack:s, needle:s);
                site.chunk.push_bc(BcKind::CountSubStr, name_span);
            }
            b"CHARATU" => {
                site.results()?;
                let [haystack, pos] = site.unpack_args(args)?;
                apply_args!(name_span, haystack:s, pos:i);
                site.chunk.push_bc(BcKind::StrCharAtU, name_span);
            }
            b"CURRENTREDRAW" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@REDRAW", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"CURRENTALIGN" => {
                site.results()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@ALIGN", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
                let func_idx = site.match_user_func_sig(
                    "SYSFUNC_ALIGN_INT_TO_STR",
                    name_span,
                    &[ValueKind::Int],
                    ScalarValueKind::Str,
                )?;
                site.chunk.push_load_imm(func_idx as _, name_span);
                site.chunk
                    .push_bc(BcKind::CallFun { args_cnt: 1 }, name_span);
            }
            b"MAX" => {
                site.result()?;
                let mut iter = site.load_args(args)?.iter().map(|&x| EraNodeRef(x));
                let Some(a0) = iter.next() else {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        name_span,
                        "function `MAX` expects at least one argument",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                site.int_expr(a0)?;
                for arg in iter {
                    site.int_expr(arg)?;
                    site.chunk.push_bc(BcKind::MaxInt, name_span);
                }
            }
            b"MIN" => {
                site.result()?;
                let mut iter = site.load_args(args)?.iter().map(|&x| EraNodeRef(x));
                let Some(a0) = iter.next() else {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        name_span,
                        "function `MIN` expects at least one argument",
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                site.int_expr(a0)?;
                for arg in iter {
                    site.int_expr(arg)?;
                    site.chunk.push_bc(BcKind::MinInt, name_span);
                }
            }
            b"LIMIT" => {
                site.result()?;
                let [a, amin, amax] = site.unpack_args(args)?;
                apply_args!(name_span, a:i, amin:i, amax:i);
                site.chunk.push_bc(BcKind::ClampInt, name_span);
            }
            b"INRANGE" => {
                site.result()?;
                let [a, amin, amax] = site.unpack_args(args)?;
                apply_args!(name_span, a:i, amin:i, amax:i);
                site.chunk.push_bc(BcKind::InRangeInt, name_span);
            }
            b"CHKCHARADATA" => {
                site.result()?;
                // TODO: CHKCHARADATA
                site.chunk.push_load_imm(0, name_span);
            }
            b"SAVETEXT" => {
                site.result()?;
                let [text, file_no, force_save_dir, force_utf8] = site.unpack_opt_args(args, 2)?;
                unwrap!(text, file_no);
                apply_args!(
                    name_span,
                    text:s,
                    file_no:i,
                    force_save_dir:i || 0,
                    force_utf8:i || 0,
                );
                site.chunk.push_bc(BcKind::SaveText, name_span);
            }
            b"LOADTEXT" => {
                site.results()?;
                let [file_no, force_save_dir, force_utf8] = site.unpack_opt_args(args, 1)?;
                unwrap!(file_no);
                apply_args!(
                    name_span,
                    file_no:i,
                    force_save_dir:i || 0,
                    force_utf8:i || 0,
                );
                site.chunk.push_bc(BcKind::LoadText, name_span);
            }
            b"FINDELEMENT" | b"FINDLASTELEMENT" => {
                site.result()?;
                let [arr, value, start_idx, end_idx, complete_match] =
                    site.unpack_opt_args(args, 2)?;
                unwrap!(arr, value);
                let arr = site.norm_var_idx(arr, true)?;
                site.expr_or_default(value.into(), arr)?;
                apply_args!(
                    name_span,
                    start_idx:i || 0,
                    end_idx:i || -1,
                    complete_match:i || 0,
                );
                site.chunk.push_bc(
                    match upper_name.as_bytes() {
                        b"FINDELEMENT" => BcKind::FindElement,
                        b"FINDLASTELEMENT" => BcKind::FindLastElement,
                        _ => unreachable!(),
                    },
                    name_span,
                );
            }
            b"FINDCHARA" | b"FINDLASTCHARA" => {
                site.result()?;
                let [chara_var, value, start_id, end_id] = site.unpack_opt_args(args, 2)?;
                unwrap!(chara_var, value);
                let chara_var = site.norm_var_idx(chara_var, false)?;
                site.expr_or_default(value.into(), chara_var)?;
                apply_args!(
                    name_span,
                    start_id:i || 0,
                    end_id:i || -1,
                );
                site.chunk.push_bc(
                    match upper_name.as_bytes() {
                        b"FINDCHARA" => BcKind::FindChara,
                        b"FINDLASTCHARA" => BcKind::FindLastChara,
                        _ => unreachable!(),
                    },
                    name_span,
                );
            }
            b"GETCOLOR" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@COLOR", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"GETBGCOLOR" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@BGCOLOR", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"GETDEFCOLOR" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@DEFCOLOR", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"GETDEFBGCOLOR" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@DEFBGCOLOR", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"GETFOCUSCOLOR" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@FOCUSCOLOR", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"MATCH" | b"CMATCH" => {
                site.result()?;
                let [array, value, start_idx, end_idx] = site.unpack_opt_args(args, 2)?;
                unwrap!(array, value);
                // let array = site.expr_to_var_opt_idx(array)?;
                let array = site.norm_var_idx(array, false)?;
                site.expr_or_default(value.into(), array)?;
                apply_args!(
                    name_span,
                    start_idx:i || 0,
                    end_idx:i || -1,
                );
                site.chunk.push_bc(
                    match upper_name.as_bytes() {
                        b"MATCH" => BcKind::ArrayCountMatches,
                        b"CMATCH" => BcKind::CArrayCountMatches,
                        _ => unreachable!(),
                    },
                    name_span,
                );
            }
            b"SUMARRAY" | b"SUMCARRAY" | b"MAXARRAY" | b"MAXCARRAY" | b"MINARRAY"
            | b"MINCARRAY" => {
                site.result()?;
                let [array, start_idx, end_idx] = site.unpack_opt_args(args, 1)?;
                unwrap!(array);
                // let array = site.expr_to_var_opt_idx(array)?;
                let array = site.norm_var_idx(array, false)?;
                apply_args!(
                    name_span,
                    start_idx:i || 0,
                    end_idx:i || -1,
                );
                site.chunk.push_bc(
                    match upper_name.as_bytes() {
                        b"SUMARRAY" => BcKind::SumArray,
                        b"SUMCARRAY" => BcKind::SumCArray,
                        b"MAXARRAY" => BcKind::MaxArray,
                        b"MAXCARRAY" => BcKind::MaxCArray,
                        b"MINARRAY" => BcKind::MinArray,
                        b"MINCARRAY" => BcKind::MinCArray,
                        _ => unreachable!(),
                    },
                    name_span,
                );
            }
            b"INRANGEARRAY" | b"INRANGECARRAY" => {
                site.result()?;
                let [array, lower, upper, start_idx, end_idx] = site.unpack_opt_args(args, 3)?;
                unwrap!(array, lower, upper);
                // let array = site.expr_to_var_opt_idx(array)?;
                let array = site.norm_var_idx(array, false)?;
                site.int_expr(lower)?;
                site.int_expr(upper)?;
                apply_args!(
                    name_span,
                    start_idx:i || 0,
                    end_idx:i || -1,
                );
                site.chunk.push_bc(
                    match upper_name.as_bytes() {
                        b"INRANGEARRAY" => BcKind::InRangeArray,
                        b"INRANGECARRAY" => BcKind::InRangeCArray,
                        _ => unreachable!(),
                    },
                    name_span,
                );
            }
            b"GETNUM" => {
                site.result()?;
                let [target, index] = site.unpack_args(args)?;
                // let target = site.expr_to_var(target)?;
                let EraNode::Identifier(target) = site.arena.get_node(target) else {
                    let mut diag = self.make_diag();
                    diag.span_err(Default::default(), name_span, "expected identifier");
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                let Some(kind) = EraCsvVarKind::try_from_var(site.o.ctx.interner().resolve(target))
                else {
                    let mut diag = self.make_diag();
                    diag.span_err(Default::default(), name_span, "unknown CSV var type");
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                site.str_expr(index)?;
                site.chunk.push_bc(BcKind::CsvGetNum { kind }, name_span);
            }
            b"GROUPMATCH" | b"NOSAMES" | b"ALLSAMES" => {
                site.result()?;
                let mut iter = site.load_args(args)?.iter().map(|&x| EraNodeRef(x));
                let Some(a0) = iter.next() else {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        name_span,
                        format!("`{name}` requires at least 1 argument", name = name),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                };
                let a0 = site.expression(a0)?;
                if !a0.is_value() {
                    let mut diag = self.make_diag();
                    diag.span_err(
                        Default::default(),
                        name_span,
                        format!("first argument of `{name}` cannot be void", name = name),
                    );
                    diag.emit_to(self.o.ctx);
                    return Err(());
                }
                let mut count = 0;
                for arg in iter {
                    site.expr_or_default(arg.into(), a0)?;
                    count += 1;
                }
                site.chunk.push_bc(BcKind::GroupMatch { count }, name_span);
                match upper_name.as_bytes() {
                    b"NOSAMES" => {
                        site.chunk.push_load_imm(0, name_span);
                        site.chunk.push_bc(BcKind::CmpIntEq, name_span);
                    }
                    b"ALLSAMES" => {
                        site.chunk.push_load_imm(count as _, name_span);
                        site.chunk.push_bc(BcKind::CmpIntEq, name_span);
                    }
                    _ => (),
                }
            }
            b"GETTIME" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.chunk.push_bc(BcKind::GetHostTime, name_span);
            }
            b"GETTIMES" => {
                site.results()?;
                let [] = site.unpack_args(args)?;
                site.chunk.push_bc(BcKind::GetHostTimeS, name_span);
            }
            b"UNICODE" => {
                // TODO: Optimize for constexpr
                site.results()?;
                let [a0] = site.unpack_args(args)?;
                apply_args!(name_span, a0:i);
                site.chunk.push_bc(BcKind::UnicodeToStr, name_span);
            }
            b"GETMILLISECOND" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.chunk.push_bc(BcKind::GetHostTimeRaw, name_span);
            }
            b"GETSECOND" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.chunk.push_bc(BcKind::GetHostTimeRaw, name_span);
                site.chunk.push_load_imm(1000, name_span);
                site.chunk.push_bc(BcKind::DivInt, name_span);
            }
            b"CSVNAME" | b"CSVCALLNAME" | b"CSVNICKNAME" | b"CSVMASTERNAME" | b"CSVBASE"
            | b"CSVCSTR" | b"CSVABL" | b"CSVTALENT" | b"CSVMARK" | b"CSVEXP" | b"CSVRELATION"
            | b"CSVJUEL" | b"CSVEQUIP" | b"CSVCFLAG" => {
                use crate::types::EraCharaCsvPropType::*;
                let (is_string, csv_kind) = match upper_name.as_bytes() {
                    b"CSVNAME" => (true, CsvName),
                    b"CSVCALLNAME" => (true, CsvCallName),
                    b"CSVNICKNAME" => (true, CsvNickName),
                    b"CSVMASTERNAME" => (true, CsvMasterName),
                    b"CSVBASE" => (false, CsvBase),
                    b"CSVCSTR" => (true, CsvCStr),
                    b"CSVABL" => (false, CsvAbl),
                    b"CSVTALENT" => (false, CsvTalent),
                    b"CSVMARK" => (false, CsvMark),
                    b"CSVEXP" => (false, CsvExp),
                    b"CSVRELATION" => (false, CsvRelation),
                    b"CSVJUEL" => (false, CsvJuel),
                    b"CSVEQUIP" => (false, CsvEquip),
                    b"CSVCFLAG" => (false, CsvCFlag),
                    _ => unreachable!(),
                };
                if is_string {
                    site.results()?;
                } else {
                    site.result()?;
                }
                match upper_name.as_bytes() {
                    b"CSVNAME" | b"CSVCALLNAME" | b"CSVNICKNAME" | b"CSVMASTERNAME" => {
                        let [chara_no] = site.unpack_args(args)?;
                        apply_args!(name_span, chara_no:i);
                        site.chunk.push_load_imm(0, name_span);
                        site.chunk
                            .push_bc(BcKind::CsvGetProp2 { csv_kind }, name_span);
                    }
                    _ => {
                        let [chara_no, index] = site.unpack_args(args)?;
                        apply_args!(name_span, chara_no:i, index:i);
                        site.chunk
                            .push_bc(BcKind::CsvGetProp2 { csv_kind }, name_span);
                    }
                }
            }
            b"POWER" => {
                site.result()?;
                let [base, expo] = site.unpack_args(args)?;
                apply_args!(name_span, base:i, expo:i);
                site.chunk.push_bc(BcKind::PowerInt, name_span);
            }
            b"RAND" => {
                site.result()?;
                let [a0, a1] = site.unpack_opt_args(args, 1)?;
                unwrap!(a0);
                if let Some(a1) = a1 {
                    // lower, upper
                    apply_args!(name_span, a0:i, a1:i);
                    site.chunk.push_bc(BcKind::GetRandomRange, name_span);
                } else {
                    // upper
                    apply_args!(name_span, a0:i);
                    site.chunk.push_bc(BcKind::GetRandomMax, name_span);
                }
            }
            b"SQRT" | b"CBRT" | b"LOG" | b"LOG10" | b"EXPONENT" => {
                site.result()?;
                let bc = match upper_name.as_bytes() {
                    b"SQRT" => BcKind::SqrtInt,
                    b"CBRT" => BcKind::CbrtInt,
                    b"LOG" => BcKind::LogInt,
                    b"LOG10" => BcKind::Log10Int,
                    b"EXPONENT" => BcKind::ExponentInt,
                    _ => unreachable!(),
                };
                let [value] = site.unpack_args(args)?;
                apply_args!(name_span, value:i);
                site.chunk.push_bc(bc, name_span);
            }
            b"ABS" => {
                site.result()?;
                let [value] = site.unpack_args(args)?;
                apply_args!(name_span, value:i);
                site.chunk.push_bc(BcKind::AbsInt, name_span);
            }
            b"SIGN" => {
                site.result()?;
                let [value] = site.unpack_args(args)?;
                apply_args!(name_span, value:i);
                site.chunk.push_bc(BcKind::SignInt, name_span);
            }
            b"TOSTR" | b"MONEYSTR" => {
                let is_money = upper_name == "MONEYSTR";
                // TODO: Support https://learn.microsoft.com/ja-jp/dotnet/api/system.int64.tostring
                site.results()?;
                if is_money {
                    let key = site.o.ctx.interner().get_or_intern("$");
                    site.chunk.push_bc(
                        BcKind::LoadConstStr {
                            idx: key.into_u32(),
                        },
                        name_span,
                    );
                }
                let [value, format] = site.unpack_opt_args(args, 1)?;
                unwrap!(value);
                if let Some(format) = format {
                    apply_args!(name_span, value:i, format:s);
                    site.chunk.push_bc(BcKind::FormatIntToStr, name_span);
                } else {
                    apply_args!(name_span, value:i);
                    site.chunk.push_bc(BcKind::IntToStr, name_span);
                }
                if is_money {
                    site.chunk.push_build_string(2, name_span);
                }
            }
            b"TOINT" => {
                site.result()?;
                let [value] = site.unpack_args(args)?;
                apply_args!(name_span, value:s);
                site.chunk.push_bc(BcKind::StrToInt, name_span);
            }
            b"ISNUMERIC" => {
                site.result()?;
                let [value] = site.unpack_args(args)?;
                apply_args!(name_span, value:s);
                site.chunk.push_bc(BcKind::StrIsValidInt, name_span);
            }
            b"VARSIZE" => {
                // TODO: Optimize for constexpr
                site.result()?;
                let [var_name, dimension] = site.unpack_opt_args(args, 1)?;
                unwrap!(var_name);
                apply_args!(name_span, var_name:s, dimension:i || -1);
                site.chunk.push_bc(BcKind::GetVarSizeByName, name_span);
            }
            b"TOUPPER" | b"TOLOWER" | b"TOHALF" | b"TOFULL" => {
                site.results()?;
                let bc = match upper_name.as_bytes() {
                    b"TOUPPER" => BcKind::StrToUpper,
                    b"TOLOWER" => BcKind::StrToLower,
                    b"TOHALF" => BcKind::StrToHalf,
                    b"TOFULL" => BcKind::StrToFull,
                    _ => unreachable!(),
                };
                let [value] = site.unpack_args(args)?;
                apply_args!(name_span, value:s);
                site.chunk.push_bc(bc, name_span);
            }
            b"EXISTCSV" => {
                site.result()?;
                let [chara_no] = site.unpack_args(args)?;
                apply_args!(name_span, chara_no:i);
                site.chunk.push_bc(BcKind::CharaCsvExists, name_span);
            }
            b"GETPALAMLV" | b"GETEXPLV" => {
                site.result()?;
                let bc = match upper_name.as_bytes() {
                    b"GETPALAMLV" => BcKind::GetPalamLv,
                    b"GETEXPLV" => BcKind::GetExpLv,
                    _ => unreachable!(),
                };
                let [value, max_lv] = site.unpack_args(args)?;
                apply_args!(name_span, value:i, max_lv:i);
                site.chunk.push_bc(bc, name_span);
            }
            b"GETCHARA" => {
                site.result()?;
                let [chara_no] = site.unpack_args(args)?;
                apply_args!(name_span, chara_no:i);
                site.chunk.push_bc(BcKind::GetCharaRegNum, name_span);
            }
            b"ESCAPE" => {
                site.results()?;
                let [haystack] = site.unpack_args(args)?;
                apply_args!(name_span, haystack:s);
                site.chunk.push_bc(BcKind::EscapeRegexStr, name_span);
            }
            b"LINEISEMPTY" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@LINEISEMPTY", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"ENCODETOUNI" => {
                site.result()?;
                let [haystack, pos] = site.unpack_opt_args(args, 1)?;
                unwrap!(haystack);
                apply_args!(name_span, haystack:s, pos:i || 0);
                site.chunk.push_bc(BcKind::EncodeToUnicode, name_span);
            }
            b"GETCONFIG" => {
                site.result()?;
                let [name] = site.unpack_args(args)?;
                apply_args!(name_span, name:s);
                site.chunk.push_bc(BcKind::GetConfig, name_span);
            }
            b"GETCONFIGS" => {
                site.results()?;
                let [name] = site.unpack_args(args)?;
                apply_args!(name_span, name:s);
                site.chunk.push_bc(BcKind::GetConfigS, name_span);
            }
            b"ISSKIP" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@SKIPDISP", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"MOUSESKIP" | b"MESSKIP" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@MESSKIP", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"CONVERT" => {
                site.results()?;
                let [value, base] = site.unpack_args(args)?;
                apply_args!(name_span, value:i, base:i);
                site.chunk.push_bc(BcKind::IntToStrWithBase, name_span);
            }
            b"PRINTCPERLINE" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@PRINTCPERLINE", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            b"PRINTCLENGTH" => {
                site.result()?;
                let [] = site.unpack_args(args)?;
                site.int_var_static_idx("@PRINTCLENGTH", name_span, 0)?;
                site.chunk.push_bc(BcKind::GetArrValFlat, name_span);
            }
            // b"COLOR_FROMNAME" => {
            //     site.result()?;
            //     // TODO...
            // }
            b"COLOR_FROMRGB" => {
                site.result()?;
                let [r, g, b] = site.unpack_args(args)?;
                // TODO: Check argument range [0, 255]
                site.int_expr(r)?;
                site.chunk.push_load_imm(16, name_span);
                site.chunk.push_bc(BcKind::ShlInt, name_span);
                site.int_expr(g)?;
                site.chunk.push_load_imm(8, name_span);
                site.chunk.push_bc(BcKind::ShlInt, name_span);
                site.int_expr(b)?;
                site.chunk.push_bc(BcKind::BitOrInt, name_span);
                site.chunk.push_bc(BcKind::BitOrInt, name_span);
            }
            b"HTML_TOPLAINTEXT" => {
                site.results()?;
                let [html] = site.unpack_args(args)?;
                apply_args!(name_span, html:s);
                site.chunk.push_bc(BcKind::HtmlToPlainText, name_span);
            }
            b"HTML_ESCAPE" => {
                site.results()?;
                let [html] = site.unpack_args(args)?;
                apply_args!(name_span, html:s);
                site.chunk.push_bc(BcKind::HtmlEscape, name_span);
            }
            b"GETKEY" => {
                site.result()?;
                let [keycode] = site.unpack_args(args)?;
                apply_args!(name_span, keycode:i);
                site.chunk.push_bc(BcKind::KbGetKeyState, name_span);
                site.chunk.push_load_imm(15, name_span);
                site.chunk.push_bc(BcKind::GetBit, name_span);
            }
            b"GETKEYTRIGGERED" => {
                site.result()?;
                let [keycode] = site.unpack_args(args)?;
                apply_args!(name_span, keycode:i);
                site.chunk.push_bc(BcKind::KbGetKeyState, name_span);
                site.chunk.push_load_imm(1, name_span);
                site.chunk.push_bc(BcKind::BitAndInt, name_span);
            }
            b"FIND_CHARADATA" => {
                site.result()?;
                let [filename] = site.unpack_args(args)?;
                apply_args!(name_span, filename:s);
                site.chunk.push_bc(BcKind::FindCharaDataFile, name_span);
            }
            b"LOADDATA" => {
                site.result()?;
                let [save_id] = site.unpack_args(args)?;
                apply_args!(name_span, save_id:i);
                site.chunk.push_bc(BcKind::LoadData, name_span);
            }
            b"CHKDATA" => {
                site.result()?;
                let [save_id] = site.unpack_args(args)?;
                apply_args!(name_span, save_id:i);
                site.chunk.push_bc(BcKind::CheckData, name_span);
            }
            _ if name.eq_ignore_ascii_case("SYSINTRINSIC_LoadGameInit") => {
                // Do nothing
            }
            _ if name.eq_ignore_ascii_case("SYSINTRINSIC_LoadGameUninit") => {
                // Do nothing
            }
            // _ if name.eq_ignore_ascii_case("SYSINTRINSIC_LoadGamePrintText") => {
            //     // Do nothing
            // }
            _ => {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    name_span,
                    format!("function `{name}` is undefined or has no matching overloads"),
                );
                diag.emit_to(self.o.ctx);
                return Err(());
            }
        }

        Ok(site.finish())
    }

    fn statements_list(&mut self, stmts: EraNodeRef, cur_scope: u32) -> CompileResult<()> {
        let stmts = self.unwrap_list_stmt(stmts)?;
        for stmt in stmts.iter().map(|x| EraNodeRef(*x)) {
            self.safe_statement(stmt, cur_scope);
        }
        Ok(())
    }

    fn match_user_func_sig(
        &mut self,
        func_name: &str,
        span: SrcSpan,
        args: &[ValueKind],
        ret: ScalarValueKind,
    ) -> CompileResult<usize> {
        let Some((func_idx, _, Some(func_info))) =
            self.o.ctx.func_entries.get_full(Ascii::new_str(func_name))
        else {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                span,
                format!("internal function `{func_name}` not found"),
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        };

        // Check parameters
        if args.len() != func_info.frame_info.args.len() {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                span,
                format!(
                    "internal function `{func_name}` must take exactly {} arguments",
                    func_info.frame_info.args.len()
                ),
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }
        for i in 0..args.len() {
            let arg = args[i];
            let param = func_info.frame_info.args[i].var_kind;
            if arg != param {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    span,
                    format!(
                        "internal function `{func_name}` argument {} must be of type {}",
                        i + 1,
                        param
                    ),
                );
                diag.emit_to(self.o.ctx);
                return Err(());
            }
        }

        // Check return
        if ret != func_info.ret_kind {
            let mut diag = self.make_diag();
            diag.span_err(
                Default::default(),
                span,
                format!(
                    "internal function `{func_name}` must return a value of type {}",
                    func_info.ret_kind
                ),
            );
            diag.emit_to(self.o.ctx);
            return Err(());
        }

        Ok(func_idx)
    }

    fn unwrap_identifier(&mut self, node: EraNodeRef) -> CompileResult<TokenKey> {
        match self.arena.get_node(node) {
            EraNode::Identifier(token) => Ok(token),
            _ => {
                let node_span = self.arena.get_node_span(node);
                let mut diag = self.make_diag();
                diag.span_err(Default::default(), node_span, "expected identifier");
                diag.emit_to(self.o.ctx);
                Err(())
            }
        }
    }

    fn unwrap_list_expr(&mut self, node: EraNodeRef) -> CompileResult<&'arena [u32]> {
        let data = self.arena.get_node(node);
        match data {
            EraNode::ListExpr(data_ref) => Ok(self.arena.get_extra_data_view(data_ref)),
            _ => {
                let node_span = self.arena.get_node_span(node);
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    node_span,
                    format!("expected list of expressions, found {data:?}"),
                );
                diag.emit_to(self.o.ctx);
                Err(())
            }
        }
    }

    fn unwrap_list_stmt(&mut self, node: EraNodeRef) -> CompileResult<&'arena [u32]> {
        let data = self.arena.get_node(node);
        match data {
            EraNode::ListStmt(data_ref) => Ok(self.arena.get_extra_data_view(data_ref)),
            _ => {
                let node_span = self.arena.get_node_span(node);
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    node_span,
                    format!("expected list of statements, found {data:?}"),
                );
                diag.emit_to(self.o.ctx);
                Err(())
            }
        }
    }

    fn unpack_list_expr<const N: usize, const OPT_N: usize>(
        &mut self,
        node: EraNodeRef,
    ) -> CompileResult<([EraNodeRef; N], [EraExprOrSpan; OPT_N])> {
        let node_span = self.arena.get_node_span(node);
        let exprs = self.unwrap_list_expr(node)?;
        let exprs_count = exprs.len();
        if OPT_N == 0 {
            if exprs_count != N {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    node_span,
                    format!("expected {N} arguments, but {exprs_count} were given"),
                );
                diag.emit_to(self.o.ctx);
                return Err(());
            }
        } else {
            if exprs_count < N {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    node_span,
                    format!("expected at least {N} arguments, but {exprs_count} were given"),
                );
                diag.emit_to(self.o.ctx);
                return Err(());
            }
            if exprs_count > N + OPT_N {
                let mut diag = self.make_diag();
                diag.span_err(
                    Default::default(),
                    node_span,
                    format!(
                        "expected at most {} arguments, but {} were given",
                        N + OPT_N,
                        exprs_count
                    ),
                );
                diag.emit_to(self.o.ctx);
                return Err(());
            }
        }
        let args = std::array::from_fn(|i| EraNodeRef(exprs[i]));
        let args_opt =
            std::array::from_fn(|i| exprs.get(i + N).map(|&x| EraNodeRef(x)).or_span(node_span));
        Ok((args, args_opt))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EraPseudoVariableKind {
    Rand,
    CharaNum,
    CallerFuncName,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EraIdVariableKind {
    Normal(ValueKind),
    Pseudo(EraPseudoVariableKind),
}

impl EraIdVariableKind {
    fn is_normal(&self) -> bool {
        matches!(self, Self::Normal(_))
    }

    fn is_pseudo(&self) -> bool {
        matches!(self, Self::Pseudo(_))
    }

    fn as_normal(&self) -> Option<ValueKind> {
        match self {
            Self::Normal(kind) => Some(*kind),
            _ => None,
        }
    }

    fn as_pseudo(&self) -> Option<EraPseudoVariableKind> {
        match self {
            Self::Pseudo(kind) => Some(*kind),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct EraIdVariableInfo {
    kind: EraIdVariableKind,
    dims_cnt: u8,
    name_token: Option<TokenKey>,
    is_charadata: bool,
}

#[derive(Debug)]
enum EraExprOrSpan {
    Expr(EraNodeRef),
    Span(SrcSpan),
}

impl EraExprOrSpan {
    pub fn is_expr(&self) -> bool {
        matches!(self, Self::Expr(_))
    }

    pub fn is_span(&self) -> bool {
        matches!(self, Self::Span(_))
    }
}

impl From<EraNodeRef> for EraExprOrSpan {
    fn from(value: EraNodeRef) -> Self {
        Self::Expr(value)
    }
}

impl From<SrcSpan> for EraExprOrSpan {
    fn from(value: SrcSpan) -> Self {
        Self::Span(value)
    }
}

trait ExprIntoEraExprOrSpan {
    fn or_span(self, span: SrcSpan) -> EraExprOrSpan;
}

impl ExprIntoEraExprOrSpan for Option<EraNodeRef> {
    fn or_span(self, span: SrcSpan) -> EraExprOrSpan {
        match self {
            Some(expr) => EraExprOrSpan::Expr(expr),
            None => EraExprOrSpan::Span(span),
        }
    }
}

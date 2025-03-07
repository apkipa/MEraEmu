// Contains necessary macros & functions to generate aarch64 assembly code & unwind information.

use dynasm::dynasm;
use dynasmrt::{aarch64::Assembler, DynasmApi, DynasmLabelApi};
use dynasmrt::{AssemblyOffset, DynamicLabel};
use paste::paste;

use super::EraVmExecSite;
use crate::types::*;

// TODO: Maybe introduce a function literal table at rXX to reduce code size?

/* Design notes:
 * - 16 bytes of stack space is for red zone (on Windows). The rest is used by the JIT code.
 * - Stack must be aligned to 16 bytes.
 */

pub(super) fn new_assembler() -> Assembler {
    let mut asm = Assembler::new().unwrap();
    dynasm!(asm
        ; .arch aarch64
        ; ->main_block_entry:
    );
    asm
}

#[cfg(target_os = "windows")]
mod unwind {
    use super::*;
    use std::num::NonZeroUsize;

    // TODO: aarch64-windows unwind
}

// TODO: Verify whether android's unwind code is correct
#[cfg(target_os = "android")]
mod unwind {
    use super::*;
    use gimli::{
        write::{
            Address, CallFrameInstruction, CommonInformationEntry, DwarfUnit, EndianVec,
            FrameDescriptionEntry, FrameTable, Range, Writer,
        },
        Encoding, LittleEndian, Register,
    };
    use std::ffi::c_void;
    use std::num::NonZeroUsize;
    use std::ops::Deref;

    unsafe fn do_register_frame(eh_frame_addr: *const c_void) {
        // Assuming using libgcc's __register_frame
        extern "C" {
            fn __register_frame(eh_frame_addr: *const c_void);
        }

        unsafe {
            __register_frame(eh_frame_addr);
        }
    }

    unsafe fn do_unregister_frame(eh_frame_addr: *const c_void) {
        // Assuming using libgcc's __deregister_frame
        extern "C" {
            fn __deregister_frame(eh_frame_addr: *const c_void);
        }

        unsafe {
            __deregister_frame(eh_frame_addr);
        }
    }

    pub fn emit_unwind_data(_ops: &mut Assembler) {
        // Postpone the generation of unwind data
    }

    fn write_unwind_data(buf: &mut Vec<u8>, base_addr: usize) {
        let func_size = buf.len() as u32;

        // Create DWARF encoding
        let encoding = Encoding {
            address_size: 8,
            format: gimli::Format::Dwarf32,
            version: 4,
        };

        // Create a new frame table
        let mut table = FrameTable::default();

        // Create CIE (Common Information Entry)
        let mut cie = CommonInformationEntry::new(
            encoding,
            4,            // Code Alignment Factor
            -8,           // Data Alignment Factor
            Register(30), // Return Address Register (x30/LR in AArch64)
        );

        // Set up the CIE instructions
        cie.add_instruction(CallFrameInstruction::Cfa(
            Register(31), // SP
            0,            // Initial CFA = SP
        ));
        cie.add_instruction(CallFrameInstruction::Offset(
            Register(30), // x30 (LR)
            1,            // x30 saved at [CFA-8]
        ));

        let cie_id = table.add_cie(cie);

        // Create FDE (Frame Description Entry)
        let mut fde = FrameDescriptionEntry::new(Address::Constant(base_addr as _), func_size);

        // Add our unwind instructions to the FDE
        // Corresponds to the prologue `stp x29, x30, [sp, #-0x30]!`
        // This adjusts SP by 0x30 bytes and stores FP and LR
        fde.add_instruction(0, CallFrameInstruction::CfaOffset(6));
        fde.add_instruction(0, CallFrameInstruction::Offset(Register(29), 2)); // x29 saved at [CFA-16]
        fde.add_instruction(0, CallFrameInstruction::Offset(Register(30), 1)); // x30 saved at [CFA-8]

        // Add FDE to the table
        table.add_fde(cie_id, fde);

        // Write the table to the .eh_frame section
        let mut eh_frame = gimli::write::EhFrame::from(EndianVec::new(LittleEndian));
        table
            .write_eh_frame(&mut eh_frame)
            .expect("Failed to write eh_frame");

        // Write the .eh_frame section to the buffer
        buf.extend(eh_frame.slice());
    }

    pub struct JitUnwindRegistryGuard {
        eh_frame: Vec<u8>,
    }

    impl JitUnwindRegistryGuard {
        pub fn add_function_table(
            _eh_frame: *const u8,
            base_addr: usize,
        ) -> Option<JitUnwindRegistryGuard> {
            let mut buf = Vec::new();
            let pos = buf.len();
            write_unwind_data(&mut buf, base_addr);
            let eh_frame = buf.as_ptr();

            unsafe {
                do_register_frame(eh_frame as *const c_void);

                Some(JitUnwindRegistryGuard { eh_frame: buf })
            }
        }
    }

    impl Drop for JitUnwindRegistryGuard {
        fn drop(&mut self) {
            unsafe {
                do_unregister_frame(self.eh_frame.as_ptr() as *const c_void);
            }
        }
    }
}

pub(super) use unwind::*;

fn emit_mov_i64(ops: &mut Assembler, dst: u32, imm: i64) {
    // Special case for 0
    if imm == 0 {
        dynasm!(ops
            ; .arch aarch64
            ; movz X(dst), #0
        );
        return;
    }
    if imm == -1 {
        dynasm!(ops
            ; .arch aarch64
            ; movn X(dst), #0
        );
        return;
    }

    let mut remaining = imm;
    let mut shift = 0;
    let mut is_negative = false;

    if imm < 0 {
        remaining = !remaining;
        is_negative = true;
    }

    while remaining != 0 {
        let part = (remaining & 0xFFFF) as u32;
        if shift == 0 {
            if is_negative {
                dynasm!(ops
                    ; .arch aarch64
                    ; movn X(dst), part
                );
            } else {
                dynasm!(ops
                    ; .arch aarch64
                    ; movz X(dst), part
                );
            }
        } else {
            if is_negative {
                dynasm!(ops
                    ; .arch aarch64
                    ; movk X(dst), part, LSL (shift * 16)
                );
            } else {
                dynasm!(ops
                    ; .arch aarch64
                    ; movk X(dst), part, LSL (shift * 16)
                );
            }
        }
        remaining >>= 16;
        shift += 1;
    }
}

pub(super) fn emit_prologue(ops: &mut Assembler) {
    dynasm!(ops
        ; .arch aarch64
        ; ->jit_entry:
        // Prepares the stack, then jumps to the JITed code
        // Local variables:
        // [sp+0x20~0x30]: red zone (?)
        // [sp+0x10]: self
        // [sp+0x8]: return address
        // [sp+0x0]: frame pointer
        ; stp x29, x30, [sp, #-0x30]!
        ; str x0, [sp, #0x10]
        ; mov x29, sp
        ; br x1
    );
}

pub(super) fn emit_epilogue(ops: &mut Assembler) {
    dynasm!(ops
        ; .arch aarch64
        // Restores the stack and returns
        ; ldp x29, x30, [sp], #0x30
        ; ret
    );
}

macro_rules! define_emit_call_subroutine {
    ($name:ident $(,$arg_no:literal)*) => {
        paste! {
            pub(super) fn $name<Callback: EraCompilerCallback, R: AssemblerRoutineReturnValue $(,[<A $arg_no>]: AssemblerRoutineArgument)*>(
                ops: &mut Assembler,
                f: extern "C-unwind" fn(&mut EraVmExecSite<Callback> $(,[<A $arg_no>]::Recv)*) -> R,
                $([<a $arg_no>]: [<A $arg_no>]),*
            ) {
                dynasm!(ops
                    ; .arch aarch64
                    ;; emit_mov_i64(ops, 8, f as _)
                    ; ldr x0, [sp, #0x10] // self
                    $(;; [<a $arg_no>].[<emit_mov_to_arg_ $arg_no>](ops))*
                    ; blr x8
                );
            }
        }
    };
}

// NOTE: 1 is reserved for `site`
define_emit_call_subroutine!(emit_call_subroutine_0);
define_emit_call_subroutine!(emit_call_subroutine_1, 2);
define_emit_call_subroutine!(emit_call_subroutine_2, 2, 3);
define_emit_call_subroutine!(emit_call_subroutine_3, 2, 3, 4);

pub(super) fn emit_test_jump_to_return_ip_or_break(ops: &mut Assembler) {
    dynasm!(ops
        ; .arch aarch64
        ; cmp x0, #0
        ; b.eq >skipcall
        ; br x0
        ; skipcall:
    );
    emit_epilogue(ops);
}

pub(super) fn emit_jump(ops: &mut Assembler, asm_ip: AssemblyOffset) {
    dynasm!(ops
        ; .arch aarch64
        ; b ->main_block_entry + asm_ip.0 as _
    );
}

pub(super) fn emit_jump_dynamic(ops: &mut Assembler, asm_ip: DynamicLabel) {
    dynasm!(ops
        ; .arch aarch64
        ; b =>asm_ip
    );
}

pub(super) trait AssemblerRoutineArgument: 'static {
    type Recv: 'static;

    fn emit_mov_to_arg_1(self, ops: &mut Assembler);
    fn emit_mov_to_arg_2(self, ops: &mut Assembler);
    fn emit_mov_to_arg_3(self, ops: &mut Assembler);
    fn emit_mov_to_arg_4(self, ops: &mut Assembler);
}

pub(super) trait AssemblerRoutineReturnValue: 'static {
    fn has_value() -> bool;
    fn emit_cmp_0(ops: &mut Assembler);
    fn emit_cmp_0_and_jump_if(ops: &mut Assembler, asm_ip: AssemblyOffset);
    fn emit_cmp_0_and_jump_if_not(ops: &mut Assembler, asm_ip: AssemblyOffset);
    fn emit_cmp_0_and_jump_dynamic_if(ops: &mut Assembler, asm_ip: DynamicLabel);
    fn emit_cmp_0_and_jump_dynamic_if_not(ops: &mut Assembler, asm_ip: DynamicLabel);
}

impl AssemblerRoutineReturnValue for () {
    fn has_value() -> bool {
        false
    }
    fn emit_cmp_0(_ops: &mut Assembler) {
        panic!("No return value to compare")
    }
    fn emit_cmp_0_and_jump_if(_ops: &mut Assembler, _asm_ip: AssemblyOffset) {
        panic!("No return value to compare")
    }
    fn emit_cmp_0_and_jump_if_not(_ops: &mut Assembler, _asm_ip: AssemblyOffset) {
        panic!("No return value to compare")
    }
    fn emit_cmp_0_and_jump_dynamic_if(_ops: &mut Assembler, _asm_ip: DynamicLabel) {
        panic!("No return value to compare")
    }
    fn emit_cmp_0_and_jump_dynamic_if_not(_ops: &mut Assembler, _asm_ip: DynamicLabel) {
        panic!("No return value to compare")
    }
}

impl AssemblerRoutineReturnValue for bool {
    fn has_value() -> bool {
        true
    }
    fn emit_cmp_0(ops: &mut Assembler) {
        dynasm!(ops
            ; .arch aarch64
            ; tst w0, w0
        );
    }
    fn emit_cmp_0_and_jump_if(ops: &mut Assembler, asm_ip: AssemblyOffset) {
        dynasm!(ops
            ; .arch aarch64
            ; tst w0, w0
            ; b.ne ->main_block_entry + asm_ip.0 as _
        );
    }
    fn emit_cmp_0_and_jump_if_not(ops: &mut Assembler, asm_ip: AssemblyOffset) {
        dynasm!(ops
            ; .arch aarch64
            ; tst w0, w0
            ; b.eq ->main_block_entry + asm_ip.0 as _
        );
    }
    fn emit_cmp_0_and_jump_dynamic_if(ops: &mut Assembler, asm_ip: DynamicLabel) {
        dynasm!(ops
            ; .arch aarch64
            ; tst w0, w0
            ; b.ne =>asm_ip
        );
    }
    fn emit_cmp_0_and_jump_dynamic_if_not(ops: &mut Assembler, asm_ip: DynamicLabel) {
        dynasm!(ops
            ; .arch aarch64
            ; tst w0, w0
            ; b.eq =>asm_ip
        );
    }
}

impl AssemblerRoutineReturnValue for usize {
    fn has_value() -> bool {
        true
    }
    fn emit_cmp_0(ops: &mut Assembler) {
        dynasm!(ops
            ; .arch aarch64
            ; cmp x0, #0
        );
    }
    fn emit_cmp_0_and_jump_if(ops: &mut Assembler, asm_ip: AssemblyOffset) {
        dynasm!(ops
            ; .arch aarch64
            ; cmp x0, #0
            ; b.ne ->main_block_entry + asm_ip.0 as _
        );
    }
    fn emit_cmp_0_and_jump_if_not(ops: &mut Assembler, asm_ip: AssemblyOffset) {
        dynasm!(ops
            ; .arch aarch64
            ; cmp x0, #0
            ; b.eq ->main_block_entry + asm_ip.0 as _
        );
    }
    fn emit_cmp_0_and_jump_dynamic_if(ops: &mut Assembler, asm_ip: DynamicLabel) {
        dynasm!(ops
            ; .arch aarch64
            ; cmp x0, #0
            ; b.ne =>asm_ip
        );
    }
    fn emit_cmp_0_and_jump_dynamic_if_not(ops: &mut Assembler, asm_ip: DynamicLabel) {
        dynasm!(ops
            ; .arch aarch64
            ; cmp x0, #0
            ; b.eq =>asm_ip
        );
    }
}

// TODO: Maybe we don't need $reg_prefix here?
macro_rules! impl_assembler_argument_for {
    ($type:ty, $reg_prefix:ident) => {
        impl AssemblerRoutineArgument for $type {
            type Recv = $type;

            fn emit_mov_to_arg_1(self, ops: &mut Assembler) {
                emit_mov_i64(ops, 0, self as _);
            }
            fn emit_mov_to_arg_2(self, ops: &mut Assembler) {
                emit_mov_i64(ops, 1, self as _);
            }
            fn emit_mov_to_arg_3(self, ops: &mut Assembler) {
                emit_mov_i64(ops, 2, self as _);
            }
            fn emit_mov_to_arg_4(self, ops: &mut Assembler) {
                emit_mov_i64(ops, 3, self as _);
            }
        }
    };
}

#[cfg(target_pointer_width = "64")]
impl_assembler_argument_for!(usize, x);
#[cfg(target_pointer_width = "32")]
impl_assembler_argument_for!(usize, w);

impl_assembler_argument_for!(u64, x);
impl_assembler_argument_for!(u32, w);
impl_assembler_argument_for!(u16, w);
impl_assembler_argument_for!(u8, w);
impl_assembler_argument_for!(i64, x);
impl_assembler_argument_for!(i32, w);
impl_assembler_argument_for!(i16, w);
impl_assembler_argument_for!(i8, w);

macro_rules! impl_assembler_argument_for_flags {
    ($type:ty, $underlying:ty) => {
        impl AssemblerRoutineArgument for $type
        where
            $underlying: From<$type>,
            // $type: From<$underlying> + Copy,
            $type: Copy,
        {
            type Recv = $type;

            fn emit_mov_to_arg_1(self, ops: &mut Assembler) {
                let value = unsafe { std::mem::transmute::<$type, $underlying>(self) };
                emit_mov_i64(ops, 0, value as _);
            }
            fn emit_mov_to_arg_2(self, ops: &mut Assembler) {
                let value = unsafe { std::mem::transmute::<$type, $underlying>(self) };
                emit_mov_i64(ops, 1, value as _);
            }
            fn emit_mov_to_arg_3(self, ops: &mut Assembler) {
                let value = unsafe { std::mem::transmute::<$type, $underlying>(self) };
                emit_mov_i64(ops, 2, value as _);
            }
            fn emit_mov_to_arg_4(self, ops: &mut Assembler) {
                let value = unsafe { std::mem::transmute::<$type, $underlying>(self) };
                emit_mov_i64(ops, 3, value as _);
            }
        }
    };
}

impl_assembler_argument_for_flags!(EraPadStringFlags, u8);
impl_assembler_argument_for_flags!(EraPrintExtendedFlags, u8);
impl_assembler_argument_for_flags!(EraWaitFlags, u8);
impl_assembler_argument_for_flags!(EraInputExtendedFlags, u8);
impl_assembler_argument_for_flags!(EraCsvVarKind, u8);
impl_assembler_argument_for_flags!(EraCharaCsvPropType, u8);

pub(super) struct NextLocalLabel;
impl NextLocalLabel {
    pub(super) fn add_here(ops: &mut Assembler) {
        dynasm!(ops
            ; .arch aarch64
            ; internal__next_local_label:
        );
    }
}
impl AssemblerRoutineArgument for NextLocalLabel {
    type Recv = usize;

    fn emit_mov_to_arg_1(self, ops: &mut Assembler) {
        dynasm!(ops
            ; .arch aarch64
            ; adr x0, >internal__next_local_label
        );
    }
    fn emit_mov_to_arg_2(self, ops: &mut Assembler) {
        dynasm!(ops
            ; .arch aarch64
            ; adr x1, >internal__next_local_label
        );
    }
    fn emit_mov_to_arg_3(self, ops: &mut Assembler) {
        dynasm!(ops
            ; .arch aarch64
            ; adr x2, >internal__next_local_label
        );
    }
    fn emit_mov_to_arg_4(self, ops: &mut Assembler) {
        dynasm!(ops
            ; .arch aarch64
            ; adr x3, >internal__next_local_label
        );
    }
}

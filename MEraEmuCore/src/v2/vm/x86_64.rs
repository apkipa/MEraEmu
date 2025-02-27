// Contains necessary macros & functions to generate x86-64 assembly code & unwind information.

use dynasm::dynasm;
use dynasmrt::{x64::Assembler, DynasmApi, DynasmLabelApi};
use dynasmrt::{AssemblyOffset, DynamicLabel};
use paste::paste;

use super::EraVmExecSite;
use crate::types::*;

/* Design notes:
 * - Windows x86-64 calling convention requires the stack to be aligned to 16 bytes on function entry.
 * Therefore a common prologue is to `sub rsp, 0x28` to align the stack (0x8 unused space for
 * 16-byte alignment because of `CALL` instruction pushing the return address, 0x20 shadow
 * space for 4 arguments). Integer arguments are passed in registers rcx, rdx, r8, r9.
 * - System V x86-64 calling convention requires the stack to be aligned to 16 bytes on function entry.
 * Therefore a common prologue is just like Windows x86-64, `sub rsp, 0x28` to align the stack.
 * Integer arguments are passed in registers rdi, rsi, rdx, rcx, r8, r9.
 */

// #[cfg(target_os = "windows")]
// macro_rules! asm_arg_reg {
//     (1) => {
//         rcx
//     };
//     (2) => {
//         rdx
//     };
//     (3) => {
//         r8
//     };
//     (4) => {
//         r9
//     };
// }
// #[cfg(target_os = "linux")]
// macro_rules! asm_arg_reg {
//     (1) => {
//         rdi
//     };
//     (2) => {
//         rsi
//     };
//     (3) => {
//         rdx
//     };
//     (4) => {
//         rcx
//     };
//     (5) => {
//         r8
//     };
//     (6) => {
//         r9
//     };
// }

#[cfg(target_os = "windows")]
macro_rules! dynasm_arg_reg {
    ($ops:expr, {$($before:tt)*} QWORD 1 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* rcx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} DWORD 1 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* ecx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} WORD 1 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* cx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} BYTE 1 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* cl $($after)*)
    };
    ($ops:expr, {$($before:tt)*} QWORD 2 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* rdx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} DWORD 2 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* edx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} WORD 2 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* dx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} BYTE 2 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* dl $($after)*)
    };
    ($ops:expr, {$($before:tt)*} QWORD 3 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r8 $($after)*)
    };
    ($ops:expr, {$($before:tt)*} DWORD 3 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r8d $($after)*)
    };
    ($ops:expr, {$($before:tt)*} WORD 3 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r8w $($after)*)
    };
    ($ops:expr, {$($before:tt)*} BYTE 3 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r8b $($after)*)
    };
    ($ops:expr, {$($before:tt)*} QWORD 4 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r9 $($after)*)
    };
    ($ops:expr, {$($before:tt)*} DWORD 4 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r9d $($after)*)
    };
    ($ops:expr, {$($before:tt)*} WORD 4 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r9w $($after)*)
    };
    ($ops:expr, {$($before:tt)*} BYTE 4 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r9b $($after)*)
    };
}

#[cfg(target_os = "linux")]
macro_rules! dynasm_arg_reg {
    ($ops:expr, {$($before:tt)*} QWORD 1 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* rdi $($after)*)
    };
    ($ops:expr, {$($before:tt)*} DWORD 1 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* edi $($after)*)
    };
    ($ops:expr, {$($before:tt)*} WORD 1 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* di $($after)*)
    };
    ($ops:expr, {$($before:tt)*} BYTE 1 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* dil $($after)*)
    };
    ($ops:expr, {$($before:tt)*} QWORD 2 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* rsi $($after)*)
    };
    ($ops:expr, {$($before:tt)*} DWORD 2 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* esi $($after)*)
    };
    ($ops:expr, {$($before:tt)*} WORD 2 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* si $($after)*)
    };
    ($ops:expr, {$($before:tt)*} BYTE 2 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* sil $($after)*)
    };
    ($ops:expr, {$($before:tt)*} QWORD 3 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* rdx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} DWORD 3 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* edx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} WORD 3 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* dx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} BYTE 3 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* dl $($after)*)
    };
    ($ops:expr, {$($before:tt)*} QWORD 4 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* rcx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} DWORD 4 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* ecx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} WORD 4 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* cx $($after)*)
    };
    ($ops:expr, {$($before:tt)*} BYTE 4 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* cl $($after)*)
    };
    ($ops:expr, {$($before:tt)*} QWORD 5 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r8 $($after)*)
    };
    ($ops:expr, {$($before:tt)*} DWORD 5 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r8d $($after)*)
    };
    ($ops:expr, {$($before:tt)*} WORD 5 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r8w $($after)*)
    };
    ($ops:expr, {$($before:tt)*} BYTE 5 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r8b $($after)*)
    };
    ($ops:expr, {$($before:tt)*} QWORD 6 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r9 $($after)*)
    };
    ($ops:expr, {$($before:tt)*} DWORD 6 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r9d $($after)*)
    };
    ($ops:expr, {$($before:tt)*} WORD 6 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r9w $($after)*)
    };
    ($ops:expr, {$($before:tt)*} BYTE 6 $($after:tt)*) => {
        dynasm!($ops; .arch x64; $($before)* r9b $($after)*)
    };
}

pub(super) fn new_assembler() -> Assembler {
    let mut asm = Assembler::new().unwrap();
    dynasm!(asm
        ; .arch x64
        ; ->main_block_entry:
    );
    asm
}

#[cfg(target_os = "windows")]
mod unwind {
    use super::*;
    use std::num::NonZeroUsize;

    #[allow(non_snake_case)]
    #[allow(non_camel_case_types)]
    // #[cfg_attr(rustfmt, rustfmt::skip)]
    pub fn emit_unwind_data(ops: &mut Assembler) {
        assert_eq!(ops.offset().0 % 4, 0, "Unwind data must be 4-byte aligned");

        use crate::util::transmute_to_bytes;
        use windows_sys::Win32::System::Diagnostics::Debug::*;

        let func_size = ops.offset().0 as u32;

        /*
           UBYTE: 3	Version
           UBYTE: 5	Flags
           UBYTE	Size of prolog
           UBYTE	Count of unwind codes
           UBYTE: 4	Frame Register
           UBYTE: 4	Frame Register offset (scaled)
           USHORT * n	Unwind codes array
           variable	Can either be of form (1) or (2) below
        */
        #[derive(Clone, Copy, Debug)]
        #[repr(C)]
        pub struct UNWIND_INFO_N<const N: usize> {
            pub VersionFlags: u8,
            pub SizeOfProlog: u8,
            pub CountOfCodes: u8,
            pub FrameRegisterOffset: u8,
            pub UnwindCode: [UNWIND_CODE; N],
        }
        #[derive(Clone, Copy, Debug)]
        #[repr(C)]
        pub struct UNWIND_CODE {
            pub CodeOffset: u8,
            pub UnwindOpInfo: u8,
        }
        pub const UWOP_PUSH_NONVOL: u8 = 0; // info == register number
        pub const UWOP_ALLOC_LARGE: u8 = 1; // no info, alloc size in next 2 slots
        pub const UWOP_ALLOC_SMALL: u8 = 2; // info == size of allocation / 8 - 1
        pub const UWOP_SET_FPREG: u8 = 3; // no info, FP = RSP + UNWIND_INFO.FPRegOffset*16
        pub const UWOP_SAVE_NONVOL: u8 = 4; // info == register number, offset in next slot
        pub const UWOP_SAVE_NONVOL_FAR: u8 = 5; // info == register number, offset in next 2 slots
        pub const UWOP_SAVE_XMM128: u8 = 8; // info == XMM reg number, offset in next slot
        pub const UWOP_SAVE_XMM128_FAR: u8 = 9; // info == XMM reg number, offset in next 2 slots
        pub const UWOP_PUSH_MACHFRAME: u8 = 10; // info == 0: no error-code, 1: error-code

        let entry = IMAGE_RUNTIME_FUNCTION_ENTRY {
            BeginAddress: 0,
            EndAddress: func_size,
            Anonymous: IMAGE_RUNTIME_FUNCTION_ENTRY_0 {
                UnwindInfoAddress: func_size
                    + std::mem::size_of::<IMAGE_RUNTIME_FUNCTION_ENTRY>() as u32,
            },
        };
        let unwind_info = UNWIND_INFO_N::<1> {
            VersionFlags: 1 | (0 << 3),
            SizeOfProlog: 0,
            CountOfCodes: 1,
            FrameRegisterOffset: 0 | (0 << 4),
            UnwindCode: [UNWIND_CODE {
                CodeOffset: 0,
                UnwindOpInfo: UWOP_ALLOC_SMALL | (4 << 4),
            }],
        };
        unsafe {
            ops.extend(transmute_to_bytes(&entry));
            ops.extend(transmute_to_bytes(&unwind_info));
        }
    }

    pub struct JitUnwindRegistryGuard {
        identifier: NonZeroUsize,
    }
    impl JitUnwindRegistryGuard {
        pub fn add_function_table(
            func_table: *const windows_sys::Win32::System::Diagnostics::Debug::IMAGE_RUNTIME_FUNCTION_ENTRY,
            base_addr: usize,
        ) -> Option<JitUnwindRegistryGuard> {
            use windows_sys::Win32::System::Diagnostics::Debug::*;

            unsafe {
                let r = RtlAddFunctionTable(func_table, 1, base_addr as _);
                if r != 0 {
                    Some(JitUnwindRegistryGuard {
                        identifier: NonZeroUsize::new(func_table as _).unwrap(),
                    })
                } else {
                    None
                }
            }
        }
    }
    impl Drop for JitUnwindRegistryGuard {
        fn drop(&mut self) {
            use windows_sys::Win32::System::Diagnostics::Debug::*;

            unsafe {
                RtlDeleteFunctionTable(self.identifier.get() as _);
            }
        }
    }
}

// TODO: Verify whether linux's unwind code is correct
#[cfg(target_os = "linux")]
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
            Register(16), // Return Address Register (RIP)
        );

        // Set up the CIE instructions
        cie.add_instruction(CallFrameInstruction::Cfa(
            Register(7), // RSP
            8,           // Initial CFA = RSP+8 (because of CALL)
        ));
        cie.add_instruction(CallFrameInstruction::Offset(
            Register(16), // RIP
            1,            // RIP = CFA-8 (因为数据对齐因子是-8，所以这里用1)
        ));

        let cie_id = table.add_cie(cie);

        // Create FDE (Frame Description Entry)
        let mut fde = FrameDescriptionEntry::new(Address::Constant(base_addr as _), func_size);

        // Add our unwind instructions to the FDE
        // Corresponds to the prologue `sub rsp, 0x28`
        // 由于数据对齐因子是-8，0x28要除以8得到5（向上取整）
        fde.add_instruction(0, CallFrameInstruction::CfaOffset(5 + 1)); // +1 for the initial 8 bytes

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

pub(super) fn emit_prologue(ops: &mut Assembler) {
    dynasm!(ops
        ; .arch x64
        ; ->jit_entry:
        // Prepares the stack, then jumps to the JITed code
        // Local variables:
        // [rsp+0x28]: return address
        // [rsp+0x20]: self
        ; sub rsp, 0x28 // Align stack to 16 bytes
        ;; dynasm_arg_reg!(ops, {mov [rsp+0x20],} QWORD 1)
        ; jmp rdx
    );
}

pub(super) fn emit_epilogue(ops: &mut Assembler) {
    dynasm!(ops
        ; .arch x64
        // Restores the stack and returns
        ; add rsp, 0x28 // Restore stack
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
                    ; .arch x64
                    ; mov rax, QWORD f as _
                    ;; dynasm_arg_reg!(ops, {mov} QWORD 1, [rsp+0x20])
                    $(;; [<a $arg_no>].[<emit_mov_to_arg_ $arg_no>](ops))*
                    ; call rax
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
        ; .arch x64
        ; test rax, rax
        ; jz >skipcall
        ; jmp rax
        ; skipcall:
    );
    emit_epilogue(ops);
}

pub(super) fn emit_jump(ops: &mut Assembler, asm_ip: AssemblyOffset) {
    dynasm!(ops
        ; .arch x64
        ; jmp ->main_block_entry + asm_ip.0 as _
    );
}

pub(super) fn emit_jump_dynamic(ops: &mut Assembler, asm_ip: DynamicLabel) {
    dynasm!(ops
        ; .arch x64
        ; jmp =>asm_ip
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
    // Compares the return value with 0 and jumps if the value is non-zero.
    fn emit_cmp_0_and_jump_if(ops: &mut Assembler, asm_ip: AssemblyOffset);
    // Compares the return value with 0 and jumps if the value is zero.
    fn emit_cmp_0_and_jump_if_not(ops: &mut Assembler, asm_ip: AssemblyOffset);
    fn emit_cmp_0_and_jump_dynamic_if(ops: &mut Assembler, asm_ip: DynamicLabel);
    fn emit_cmp_0_and_jump_dynamic_if_not(ops: &mut Assembler, asm_ip: DynamicLabel);
}

macro_rules! impl_assembler_argument_for {
    ($type:ty, $size_kw:ident) => {
        impl AssemblerRoutineArgument for $type {
            type Recv = $type;

            fn emit_mov_to_arg_1(self, ops: &mut Assembler) {
                dynasm_arg_reg!(ops, {mov} $size_kw 1, $size_kw self as _)
            }
            fn emit_mov_to_arg_2(self, ops: &mut Assembler) {
                dynasm_arg_reg!(ops, {mov} $size_kw 2, $size_kw self as _)
            }
            fn emit_mov_to_arg_3(self, ops: &mut Assembler) {
                dynasm_arg_reg!(ops, {mov} $size_kw 3, $size_kw self as _)
            }
            fn emit_mov_to_arg_4(self, ops: &mut Assembler) {
                dynasm_arg_reg!(ops, {mov} $size_kw 4, $size_kw self as _)
            }
        }
    };
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
            ; .arch x64
            ; test al, al
        );
    }
    fn emit_cmp_0_and_jump_if(ops: &mut Assembler, asm_ip: AssemblyOffset) {
        dynasm!(ops
            ; .arch x64
            ; test al, al
            ; jnz ->main_block_entry + asm_ip.0 as _
        );
    }
    fn emit_cmp_0_and_jump_if_not(ops: &mut Assembler, asm_ip: AssemblyOffset) {
        dynasm!(ops
            ; .arch x64
            ; test al, al
            ; jz ->main_block_entry + asm_ip.0 as _
        );
    }
    fn emit_cmp_0_and_jump_dynamic_if(ops: &mut Assembler, asm_ip: DynamicLabel) {
        dynasm!(ops
            ; .arch x64
            ; test al, al
            ; jnz =>asm_ip
        );
    }
    fn emit_cmp_0_and_jump_dynamic_if_not(ops: &mut Assembler, asm_ip: DynamicLabel) {
        dynasm!(ops
            ; .arch x64
            ; test al, al
            ; jz =>asm_ip
        );
    }
}
impl AssemblerRoutineReturnValue for usize {
    fn has_value() -> bool {
        true
    }
    fn emit_cmp_0(ops: &mut Assembler) {
        dynasm!(ops
            ; .arch x64
            ; test rax, rax
        );
    }
    fn emit_cmp_0_and_jump_if(ops: &mut Assembler, asm_ip: AssemblyOffset) {
        dynasm!(ops
            ; .arch x64
            ; test rax, rax
            ; jnz ->main_block_entry + asm_ip.0 as _
        );
    }
    fn emit_cmp_0_and_jump_if_not(ops: &mut Assembler, asm_ip: AssemblyOffset) {
        dynasm!(ops
            ; .arch x64
            ; test rax, rax
            ; jz ->main_block_entry + asm_ip.0 as _
        );
    }
    fn emit_cmp_0_and_jump_dynamic_if(ops: &mut Assembler, asm_ip: DynamicLabel) {
        dynasm!(ops
            ; .arch x64
            ; test rax, rax
            ; jnz =>asm_ip
        );
    }
    fn emit_cmp_0_and_jump_dynamic_if_not(ops: &mut Assembler, asm_ip: DynamicLabel) {
        dynasm!(ops
            ; .arch x64
            ; test rax, rax
            ; jz =>asm_ip
        );
    }
}

#[cfg(target_pointer_width = "64")]
impl_assembler_argument_for!(usize, QWORD);
#[cfg(target_pointer_width = "32")]
impl_assembler_argument_for!(usize, DWORD);

impl_assembler_argument_for!(u64, QWORD);
impl_assembler_argument_for!(u32, DWORD);
impl_assembler_argument_for!(u16, WORD);
impl_assembler_argument_for!(u8, BYTE);
impl_assembler_argument_for!(i64, QWORD);
impl_assembler_argument_for!(i32, DWORD);
impl_assembler_argument_for!(i16, WORD);
impl_assembler_argument_for!(i8, BYTE);

macro_rules! impl_assembler_argument_for_flags {
    ($type:ty, $underlying:ty, $size_kw:ident) => {
        impl AssemblerRoutineArgument for $type
        where
            $underlying: From<$type>,
            // $type: From<$underlying> + Copy,
            $type: Copy,
        {
            type Recv = $type;

            fn emit_mov_to_arg_1(self, ops: &mut Assembler) {
                dynasm_arg_reg!(ops, {mov} $size_kw 1, $size_kw unsafe { std::mem::transmute(self) })
            }
            fn emit_mov_to_arg_2(self, ops: &mut Assembler) {
                dynasm_arg_reg!(ops, {mov} $size_kw 2, $size_kw unsafe { std::mem::transmute(self) })
            }
            fn emit_mov_to_arg_3(self, ops: &mut Assembler) {
                dynasm_arg_reg!(ops, {mov} $size_kw 3, $size_kw unsafe { std::mem::transmute(self) })
            }
            fn emit_mov_to_arg_4(self, ops: &mut Assembler) {
                dynasm_arg_reg!(ops, {mov} $size_kw 4, $size_kw unsafe { std::mem::transmute(self) })
            }
        }
    };
}

impl_assembler_argument_for_flags!(EraPadStringFlags, u8, BYTE);
impl_assembler_argument_for_flags!(EraPrintExtendedFlags, u8, BYTE);
impl_assembler_argument_for_flags!(EraWaitFlags, u8, BYTE);
impl_assembler_argument_for_flags!(EraInputExtendedFlags, u8, BYTE);
impl_assembler_argument_for_flags!(EraCsvVarKind, u8, BYTE);
impl_assembler_argument_for_flags!(EraCharaCsvPropType, u8, BYTE);

// impl<T> AssemblerRoutineArgument for T
// where
//     T: From<u8> + Copy,
//     u8: From<T>,
// {
//     type Recv = T;

//     fn emit_mov_to_arg_1(self, ops: &mut Assembler) {
//         dynasm_arg_reg!(ops, {mov} BYTE 1, BYTE u8::from(self) as _)
//     }
//     fn emit_mov_to_arg_2(self, ops: &mut Assembler) {
//         dynasm_arg_reg!(ops, {mov} BYTE 2, BYTE u8::from(self) as _)
//     }
//     fn emit_mov_to_arg_3(self, ops: &mut Assembler) {
//         dynasm_arg_reg!(ops, {mov} BYTE 3, BYTE u8::from(self) as _)
//     }
//     fn emit_mov_to_arg_4(self, ops: &mut Assembler) {
//         dynasm_arg_reg!(ops, {mov} BYTE 4, BYTE u8::from(self) as _)
//     }
// }

pub(super) struct NextLocalLabel;
impl NextLocalLabel {
    pub(super) fn add_here(ops: &mut Assembler) {
        dynasm!(ops
            ; .arch x64
            ; internal__next_local_label:
        );
    }
}
impl AssemblerRoutineArgument for NextLocalLabel {
    type Recv = usize;

    fn emit_mov_to_arg_1(self, ops: &mut Assembler) {
        dynasm_arg_reg!(ops, {lea} QWORD 1, [>internal__next_local_label])
    }
    fn emit_mov_to_arg_2(self, ops: &mut Assembler) {
        dynasm_arg_reg!(ops, {lea} QWORD 2, [>internal__next_local_label])
    }
    fn emit_mov_to_arg_3(self, ops: &mut Assembler) {
        dynasm_arg_reg!(ops, {lea} QWORD 3, [>internal__next_local_label])
    }
    fn emit_mov_to_arg_4(self, ops: &mut Assembler) {
        dynasm_arg_reg!(ops, {lea} QWORD 4, [>internal__next_local_label])
    }
}

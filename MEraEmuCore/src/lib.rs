mod bytecode;
mod compiler;
mod csv;
mod engine;
mod lexer;
mod parser;
mod util;
mod vm;

#[cxx::bridge]
mod ffi {
    // TODO: ffi
}

pub use engine::{
    EraScriptErrorInfo, MEraEngine, MEraEngineConfig, MEraEngineError, MEraEngineSysCallback,
};

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, sync::atomic::AtomicBool};

    use indoc::indoc;

    use super::*;

    struct MockEngineCallback<'a> {
        errors: &'a RefCell<String>,
        output: String,
    }
    impl<'a> MockEngineCallback<'a> {
        fn new(errors: &'a RefCell<String>) -> Self {
            Self {
                errors,
                output: String::new(),
            }
        }
    }
    impl MEraEngineSysCallback for &mut MockEngineCallback<'_> {
        fn on_compile_error(&mut self, info: &EraScriptErrorInfo) {
            *self.errors.borrow_mut() += &format!(
                "{}({},{}): compile {}: {}\n",
                info.filename,
                info.src_info.line,
                info.src_info.column,
                if info.is_error { "error" } else { "warning" },
                info.msg
            );
        }
        fn on_execute_error(&mut self, info: &EraScriptErrorInfo) {
            panic!(
                "{}({},{}): execute {}: {}\nlast exec output: {}",
                info.filename,
                info.src_info.line,
                info.src_info.column,
                if info.is_error { "error" } else { "warning" },
                info.msg,
                self.output
            );
        }
        fn on_get_rand(&mut self) -> u64 {
            42
        }
        fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags) {
            self.output += content;
        }
        fn on_html_print(&mut self, content: &str) {
            self.output += content;
        }
        fn on_wait(&mut self, is_force: bool) {}
        fn on_input_int(
            &mut self,
            default_value: i64,
            can_click: bool,
            allow_skip: bool,
        ) -> Option<i64> {
            Some(0)
        }
        fn on_input_str(
            &mut self,
            default_value: &str,
            can_click: bool,
            allow_skip: bool,
        ) -> Option<String> {
            Some(String::new())
        }
        fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error> {
            Ok(0)
        }
        fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error> {
            Ok(name.to_string())
        }
        fn on_var_set_int(
            &mut self,
            name: &str,
            idx: usize,
            val: i64,
        ) -> Result<(), anyhow::Error> {
            Ok(())
        }
        fn on_var_set_str(
            &mut self,
            name: &str,
            idx: usize,
            val: &str,
        ) -> Result<(), anyhow::Error> {
            println!("Setting variable `{name}` to `{val}`...");
            Ok(())
        }
        // Graphics
        fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64 {
            0
        }
        fn on_gdispose(&mut self, gid: i64) -> i64 {
            0
        }
        fn on_gcreated(&mut self, gid: i64) -> i64 {
            0
        }
    }

    #[test]
    fn basic_engine() -> anyhow::Result<()> {
        let main_erb = indoc! {r#"
            ; Simple counter pure function
            @DO_COUNT(upper = 100)
                #FUNCTION
                #DIM upper
                #DIM result, 1, 1 = 0
                ;[SKIPSTART]
                ;#DIM cnt = 100
                ;wdwd
                ;[SKIPEND]
                #DIM cnt = 100
                cnt '= upper
                ;results = oook
                ;locals@DO_COUNT = oook
                ;PRINTFORM [Start with cnt={cnt},result={result}]
                WHILE cnt > 0
                    ;PRINTFORM []
                    ;result = result + cnt:0--
                    result += cnt:0--
                    ;SIF cnt < 98
                    ;    BREAK
                    ;cnt = cnt - 1
                    ;cnt = --cnt
                    ;--cnt
                WEND
                ;PRINTFORM [Returning {result}]
                PRINTFORM [Ret]
                RETURNF result
            @BAD_GOTO
                ;$LABEL1
                REPEAT 1
                    ;GOTO LABEL1
                REND
            @SYSTEM_TITLE()
                ;#DIM REF xre
                #DIM val = 1 + 1 ;*0
                #DIMS world_str, -2 + 3 = ""

                ;#DIM cnt = 10000000
                #DIM cnt = 100
                WHILE cnt > 0
                    cnt = cnt - 1
                WEND
                cnt = 100

                ;world_str = \@ 0 ? aaa # bb{4+1}b \@
                ;world_str '= @"~\@ 0 ? aaa # bb{4+1}b \@~"
                ;PRINTV world_str
                world_str '= @"worl{"d"}"

                val:0 = val + 1
                ; Print hello world
                Printform Hello, {val + 1,2,LEFT}the %world_str%!
                IF 1 + 2 - 3;
                    PRINTFORM true
                ELSE
                    PRINTFORM false
                ENDIF
                PRINTFORM Done
                CALL DO_COUNT
                PRINTV DO_COUNT(0 ? 0 # 10), @"~{-DO_COUNT()}~"
                PRINTV WINDOW_TITLE += "!"

                SELECTCASE 42
                    CASE 1, is >= 42
                        PRINTFORM [1]
                        ;THROW "??? {4+1}"
                    CASE 43 to 41, 42
                        PRINTFORM [2]
                    CASEELSE
                        PRINTFORM [else]
                ENDSELECT

                REPEAT 5
                    SIF COUNT == 2
                        CONTINUE
                    SIF COUNT == 4
                        BREAK
                    PRINTFORM {COUNT}
                REND
                PRINTFORM {COUNT}

                QUIT
                PRINTFORM not printed
        "#};
        let errors = RefCell::new(String::new());
        let mut callback = MockEngineCallback::new(&errors);
        let mut engine = MEraEngine::new();
        engine.install_sys_callback(Box::new(&mut callback));
        engine.register_global_var("COUNT", false, 1, false)?;
        engine.register_global_var("WINDOW_TITLE", true, 1, true)?;
        _ = engine.load_erb("main.erb", main_erb.as_bytes());
        _ = engine.finialize_load_srcs();
        {
            let errors = errors.borrow();
            if errors.contains("error:") {
                drop(engine);
                panic!(
                    "compile output:\n{errors}\nexec output:\n{}",
                    callback.output
                );
            }
            if !errors.is_empty() {
                println!("compile output:\n{errors}");
            }
        }
        let stop_flag = AtomicBool::new(false);
        engine.do_execution(&stop_flag, u64::MAX)?;
        assert!(engine.get_is_halted());
        drop(engine);
        assert_eq!(
            &callback.output,
            "Hello, 4 the world!falseDone[Ret][Ret][Ret]55~-5050~"
        );

        Ok(())
    }
    #[test]
    fn assembled_game() -> anyhow::Result<()> {
        // TODO: Redact this
        let game_base_dir = r#"D:\MyData\Games\Others\1\eraTW\TW4.881画蛇添足版（04.07更新）\"#;

        let errors = RefCell::new(String::new());
        let mut callback = MockEngineCallback::new(&errors);
        let mut engine = MEraEngine::new();
        engine.install_sys_callback(Box::new(&mut callback));
        engine.register_global_var("COUNT", false, 1, false)?;
        engine.register_global_var("WINDOW_TITLE", true, 1, true)?;
        let mut total_cnt = 0usize;
        let mut pass_cnt = 0usize;
        for i in walkdir::WalkDir::new(format!("{game_base_dir}CSV")) {
            use crate::engine::EraCsvLoadKind;
            let i = i?;
            if !i.file_type().is_file() {
                continue;
            }
            let file_name = i.file_name().to_ascii_lowercase();
            let file_name = file_name.as_encoded_bytes();
            if file_name.eq_ignore_ascii_case(b"_Rename.csv") {
                let csv = std::fs::read(i.path())?;
                let csv = csv.strip_prefix("\u{feff}".as_bytes()).unwrap_or(&csv);
                engine.load_csv(&i.path().to_string_lossy(), csv, EraCsvLoadKind::_Rename)?;
            }
        }
        for i in walkdir::WalkDir::new(format!("{game_base_dir}ERB")) {
            let i = i?;
            if i.file_type().is_file()
                && i.file_name()
                    .to_ascii_lowercase()
                    .as_encoded_bytes()
                    .ends_with(b".erb")
            {
                let mut erb = std::fs::read(i.path())?;
                // HACK: Fix missing newlines
                if !erb.ends_with(b"\n") {
                    erb.push(b'\n');
                }
                let erb = erb.strip_prefix("\u{feff}".as_bytes()).unwrap_or(&erb);
                if engine.load_erb(&i.path().to_string_lossy(), erb).is_ok() {
                    pass_cnt += 1;
                }
                total_cnt += 1;
            }
        }
        _ = engine.finialize_load_srcs();
        {
            let errors = errors.borrow();
            if errors.contains("error:") {
                drop(engine);
                panic!(
                    "compile output:\n{errors}\nexec output:\n{}\nCompiled {}/{} files",
                    callback.output, pass_cnt, total_cnt
                );
            }
            if !errors.is_empty() {
                println!("compile output:\n{errors}");
            }
        }
        let stop_flag = AtomicBool::new(false);
        engine.do_execution(&stop_flag, u64::MAX)?;
        assert!(engine.get_is_halted());
        drop(engine);

        Ok(())
    }
}

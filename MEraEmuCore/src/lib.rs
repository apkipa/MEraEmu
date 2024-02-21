mod bytecode;
mod compiler;
mod csv;
mod engine;
mod lexer;
mod parser;
mod routine;
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

    use colored::{Color, Colorize};
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
            let (noun, color) = if info.is_error {
                ("error", Color::Red)
            } else {
                ("warning", Color::BrightYellow)
            };
            *self.errors.borrow_mut() += &format!(
                "{}({},{}): compile {}: {}\n",
                info.filename, info.src_info.line, info.src_info.column, noun, info.msg
            )
            .color(color)
            .to_string();
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
                ;upper:d():1
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
                ;CALL DO_COUNT
                TRYCCALLFORM DO_%"COUN"+"T"%(50)
                    PRINTFORM [OK]
                CATCH
                    PRINTFORM [FAIL]
                ENDCATCH
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
        // engine.register_global_var("COUNT", false, 1, false)?;
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
        engine.register_global_var("WINDOW_TITLE", true, 1, true)?;
        let mut total_cnt = 0usize;
        let mut pass_cnt = 0usize;
        // Load CSV files
        let mut load_csv = |file_path: &std::path::Path, kind| -> anyhow::Result<()> {
            // let Ok(content) = std::fs::read(file_path) else {
            //     // Proceed if we cannot read the file
            //     return Ok(());
            // };
            let content = std::fs::read(file_path)?;
            let content = content
                .strip_prefix("\u{feff}".as_bytes())
                .unwrap_or(&content);
            engine.load_csv(&file_path.to_string_lossy(), content, kind)?;
            Ok(())
        };
        let mut misc_csvs = Vec::new();
        let mut chara_csvs = Vec::new();
        for i in walkdir::WalkDir::new(format!("{game_base_dir}CSV")) {
            use crate::engine::EraCsvLoadKind::*;
            let i = i?;
            if !i.file_type().is_file() {
                continue;
            }
            let file_name = i.file_name().to_ascii_lowercase();
            let file_name = file_name.as_encoded_bytes();
            let path = i.path();
            if file_name.eq_ignore_ascii_case(b"_Rename.csv") {
                load_csv(path, _Rename)?;
            } else if file_name.eq_ignore_ascii_case(b"VariableSize.csv") {
                load_csv(path, VariableSize)?;
            } else {
                let file_name = file_name.to_ascii_uppercase();
                if !file_name.ends_with(b".CSV") {
                    continue;
                }
                if file_name.starts_with(b"CHARA") {
                    chara_csvs.push(path.to_owned());
                } else {
                    misc_csvs.push(path.to_owned());
                }
            }
        }
        for csv in misc_csvs {
            use engine::EraCsvLoadKind::*;
            let csv_name = csv
                .file_name()
                .unwrap()
                .as_encoded_bytes()
                .to_ascii_uppercase();
            let kind = match &csv_name[..] {
                b"ABL.CSV" => Abl,
                b"EXP.CSV" => Exp,
                b"TALENT.CSV" => Talent,
                b"PALAM.CSV" => Palam,
                b"TRAIN.CSV" => Train,
                b"MARK.CSV" => Mark,
                b"ITEM.CSV" => Item,
                b"BASE.CSV" => Base,
                b"SOURCE.CSV" => Source,
                b"EX.CSV" => Ex,
                b"STR.CSV" => Str,
                b"EQUIP.CSV" => Equip,
                b"TEQUIP.CSV" => TEquip,
                b"FLAG.CSV" => Flag,
                b"TFLAG.CSV" => TFlag,
                b"CFLAG.CSV" => CFlag,
                b"TCVAR.CSV" => TCVar,
                b"CSTR.CSV" => CStr,
                b"STAIN.CSV" => Stain,
                b"CDFLAG1.CSV" => CDFlag1,
                b"CDFLAG2.CSV" => CDFlag2,
                b"STRNAME.CSV" => StrName,
                b"TSTR.CSV" => TStr,
                b"SAVESTR.CSV" => SaveStr,
                b"GLOBAL.CSV" => Global,
                b"GLOBALS.CSV" => Globals,
                b"GAMEBASE.CSV" => GameBase,
                _ => continue,
            };
            //load_csv(&csv, kind)?;
            _ = load_csv(&csv, kind);
        }
        for csv in chara_csvs {
            load_csv(&csv, engine::EraCsvLoadKind::Chara_)?;
        }
        let mut erhs = Vec::new();
        let mut erbs = Vec::new();
        for i in walkdir::WalkDir::new(format!("{game_base_dir}ERB")) {
            let i = i?;
            if !i.file_type().is_file() {
                continue;
            }
            let file_name = i.file_name().to_ascii_lowercase();
            let file_name = file_name.as_encoded_bytes();
            if file_name.ends_with(b".erb") {
                erbs.push(i.path().to_owned());
            } else if file_name.ends_with(b".erh") {
                erhs.push(i.path().to_owned());
            } else {
                // println!("warn: skipping file `{}`", i.path().display());
            }
        }
        for erb_path in erhs.into_iter().chain(erbs.into_iter()) {
            let mut erb = std::fs::read(&erb_path)?;
            // HACK: Fix missing newlines
            if !erb.ends_with(b"\n") {
                erb.push(b'\n');
            }
            let erb = erb.strip_prefix("\u{feff}".as_bytes()).unwrap_or(&erb);
            if engine.load_erb(&erb_path.to_string_lossy(), erb).is_ok() {
                pass_cnt += 1;
            } else {
                // *errors.borrow_mut() +=
                //     &format!("[File `{}` failed to compile]\n", erb_path.display());
                panic!(
                    "{}\n[File `{}` failed to compile]\n",
                    errors.borrow(),
                    erb_path.display()
                );
            }
            total_cnt += 1;
        }
        *errors.borrow_mut() += "[FINIALIZE]\n";
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

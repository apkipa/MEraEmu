mod bytecode;
mod compiler;
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
    use std::sync::atomic::AtomicBool;

    use indoc::indoc;

    use super::*;

    struct MockEngineCallback {
        output: String,
    }
    impl MockEngineCallback {
        fn new() -> Self {
            Self {
                output: String::new(),
            }
        }
    }
    impl MEraEngineSysCallback for &mut MockEngineCallback {
        fn on_compile_error(&mut self, info: &EraScriptErrorInfo) {
            panic!(
                "compile error: {}({},{}): {}",
                info.filename, info.src_info.line, info.src_info.column, info.msg
            );
        }
        fn on_execute_error(&mut self, info: &EraScriptErrorInfo) {
            panic!(
                "execute error: {}({},{}): {}",
                info.filename, info.src_info.line, info.src_info.column, info.msg
            );
        }
        fn on_get_rand(&mut self) -> u64 {
            0
        }
        fn on_html_print(&mut self, content: &str) {}
        fn on_input(&mut self, XXXXXXXXXXXX: u32) {}
        fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags) {
            self.output += content;
        }
        fn on_var_get(&mut self, name: &str, idx: u32) -> Result<crate::bytecode::Value, String> {
            Err("empty".to_owned())
        }
        fn on_var_set(
            &mut self,
            name: &str,
            idx: u32,
            val: &crate::bytecode::Value,
        ) -> Result<(), String> {
            Err("empty".to_owned())
        }
        fn on_wait(&mut self, XXXXXXXXXXXX: u32) {}
    }

    #[test]
    fn basic_engine() -> anyhow::Result<()> {
        let main_erb = indoc! {r#"
            @DO_COUNT
                #FUNCTION
                #DIM result = 0
                #DIM cnt = 100
                WHILE cnt > 0
                    result = result + cnt
                    cnt = cnt - 1
                WEND
                ;RETURNF result
            @SYSTEM_TITLE
                ;#DIM REF xre
                #DIM val = 1 + 1 ;*0
                #DIMS world_str, -2 + 3 = "world"

                ;#DIM cnt = 10000000
                #DIM cnt = 100
                WHILE cnt > 0
                    cnt = cnt - 1
                WEND

                val:0 = val + 1
                ; Print hello world
                Printform Hello, {val + 1} the {world_str}!
                IF 1 + 2 - 3;
                    PRINTFORM true
                ELSE
                    PRINTFORM false
                ENDIF
                PRINTFORM Done
                QUIT
                PRINTFORM not printed
        "#};
        let mut callback = MockEngineCallback::new();
        let mut engine = MEraEngine::new();
        engine.install_sys_callback(Box::new(&mut callback));
        engine.load_erb("main.erb", main_erb.as_bytes())?;
        engine.finialize_load_srcs()?;
        let stop_flag = AtomicBool::new(false);
        engine.do_execution(&stop_flag, u64::MAX)?;
        assert!(engine.get_is_halted());
        drop(engine);
        assert_eq!(&callback.output, "Hello, 4 the world!falseDone");

        Ok(())
    }
}

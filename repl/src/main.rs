#![allow(unused_imports)]

use core::cell::RefCell;
use std::io::{
    stdin,
    BufRead,
    BufReader,
    Write,
};
use std::rc::Rc;

use termcolor::{
    Color,
    ColorChoice,
    ColorSpec,
    StandardStream,
    WriteColor
};

use monkey_core::ast::Value;
use monkey_core::environment::Environment;
use monkey_core::environment::MutableEnvironment;
use monkey_core::evaluator::Evaluate;
use monkey_core::lexer::Lexer;
use monkey_core::parser::Parser;

const PROMPT: &str=">>>";

const HELP: &str=r#".help    Prints this message to the screen
.exit    Exit the interpreter"#;

fn prompt(mut output: &mut StandardStream) -> Result<(), std::io::Error> {
    output.set_color(ColorSpec::new().set_fg(Some(Color::Rgb(255, 165, 0))))?;
    write!(&mut output, "{} ", PROMPT)?;
    output.set_color(ColorSpec::new().set_fg(Some(Color::Rgb(255, 255, 255))))?;
    output.flush()?;
    Ok(())
}

fn execute_command(mut output: &mut StandardStream, line: &str) -> Result<(), std::io::Error> {
    match line {
        ".help" => writeln!(&mut output, "{}", HELP)?,
        ".mode" => unimplemented!(),
        _ => return Err(std::io::ErrorKind::Unsupported.into()),
    };
    Ok(())
}

fn execute_line(mut output: &mut StandardStream, line: &str, env: MutableEnvironment) -> Result<(), std::io::Error> {
    let lexer = Lexer::new(line);
    let mut parser = Parser::new(lexer);

    let program = parser.parse();
    if let Some(err) = parser.errors.first() {
        output.set_color(ColorSpec::new().set_fg(Some(Color::Rgb(255, 0, 0))))?;
        writeln!(&mut output, "{:?}", err)?;
    };
    match program.eval(env) {
        Ok(result) => {
            match result {
                Value::Null => {},
                _ => writeln!(&mut output, "{}", result)?,
            }
        },
        Err(err) => {
            output.set_color(ColorSpec::new().set_fg(Some(Color::Rgb(255, 0, 0))))?;
            writeln!(&mut output, "{:?}", err)?;
        },
    };

    Ok(())
}

fn start() -> Result<(), std::io::Error> {
    let buffer = BufReader::new(stdin());
    let mut output = StandardStream::stdout(ColorChoice::Always);

    prompt(&mut output)?;
    let env = Rc::new(RefCell::new(Environment::new(None)));

    for result in buffer.lines() {

        let line = result?;
        if line == ".exit" {
            break
        } else if line.starts_with(".") {
            execute_command(&mut output, &line)?;
        } else {
            execute_line(&mut output, &line, env.clone())?;
        };

        prompt(&mut output)?;
    }
    output.reset()?;
    Ok(())
}

fn main() {
    if let Err(err) = start() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

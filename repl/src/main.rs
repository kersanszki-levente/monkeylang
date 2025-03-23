#![allow(unused_imports)]

use std::io::{
    stdin, BufRead, BufReader, Read, Write
};
use std::fmt::Debug;
use std::fmt::Display;
use std::fs::File;
use std::path::Path;

use clap::Parser as ClapParser;
use termcolor::{
    Color,
    ColorChoice,
    ColorSpec,
    StandardStream,
    WriteColor
};

use monkey_core::ast::Value;
use monkey_core::environment::Environment;
use monkey_core::environment::SharedEnvironment;
use monkey_core::evaluator::Evaluate;
use monkey_core::lexer::Lexer;
use monkey_core::parser::Parser;

const PROMPT: &str=">>>";

const HELP: &str=r#".help    Prints this message to the screen
.exit    Exit the interpreter"#;

struct RuntimeError(String);

impl Debug for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

type RuntimeResult<T>=Result<T, RuntimeError>;

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

fn execute_line(mut output: &mut StandardStream, line: &str, env: &SharedEnvironment) -> Result<(), std::io::Error> {
    let lexer = Lexer::new(line);
    let mut parser = Parser::new(lexer);

    let program = parser.parse();
    if let Some(err) = parser.errors.first() {
        output.set_color(ColorSpec::new().set_fg(Some(Color::Rgb(255, 0, 0))))?;
        writeln!(&mut output, "{}", err)?;
        return Ok(())
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
    let env = Environment::new_shared(None);

    for result in buffer.lines() {

        let line = result?;
        if line == ".exit" {
            break
        } else if line.starts_with(".") {
            execute_command(&mut output, &line)?;
        } else {
            execute_line(&mut output, &line, &env)?;
        };

        prompt(&mut output)?;
    }
    output.reset()?;
    Ok(())
}

fn read_source_file(path: String) -> RuntimeResult<String> {
    if !Path::new(&path).exists() {
        return Err(RuntimeError(format!("{path} file does not exist")))
    }
    let mut buffer = String::new();
    BufReader::new(File::open(&path).map_err(|_| RuntimeError(format!("Failed to read file {}", &path)))?)
        .read_to_string(&mut buffer)
        .map_err(|_| RuntimeError(format!("Failed to read file {}", &path)))?;
    Ok(buffer)
}

fn evaluate_source_file(source_file_path: String) -> RuntimeResult<()> {
    let buffer = read_source_file(source_file_path)?;
    let lexer = Lexer::new(&buffer);
    let mut parser = Parser::new(lexer);

    let program = parser.parse();
    if let Some(err) = parser.errors.first() {
        return Err(RuntimeError(format!("Parsing error: {}", err)));
    };

    let env = Environment::new_shared(None);
    match program.eval(&env) {
        Ok(result) => {
            match result {
                Value::Null => {},
                _ => return Err(RuntimeError(format!("{}", result))),
            }
        },
        Err(err) => return Err(RuntimeError(format!("{:?}", err))),
    };
    Ok(())
}

#[derive(Debug, ClapParser)]
struct Arguments {
    file: Option<String>,
}

fn main() {
    let args = Arguments::parse();
    if let Some(source_file) = args.file {
        if let Err(err) = evaluate_source_file(source_file) {
            eprintln!("{err}");
        };
        std::process::exit(0);
    }
    if let Err(err) = start() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

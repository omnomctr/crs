mod parser;

pub use parser::Lexer;
use std::env::args;
use std::io::Read;
use std::process::exit;
use std::fs::File;

#[derive(Debug, Eq, PartialEq)]
enum CompilationStage {
    Lex,
    Parse,
    Tacky,
    Unspecified,
}

fn parse_outfile(s: String) -> Option<String> {
    if !s.ends_with(".c") || s.len() <= 2 {
        None
    } else {
        Some(s[0..s.len() - 2].into())
    }
}

fn main() {

    let mut compilation_stage: CompilationStage = CompilationStage::Unspecified;
    let mut infile = None;
    let mut outfile = None;

    if args().len() == 1 {
        println!("Usage: crs [--stage] <infile>");
        exit(-1);
    }

    for arg in args().skip(1) {
        use CompilationStage as s;
        match arg.as_str() {
            "--lex" => compilation_stage = s::Lex,
            "--parse" => compilation_stage = s::Parse,
            "--tacky" => compilation_stage = s::Tacky,
            _ => {
                infile = Some(arg.clone());
                outfile = Some(parse_outfile(arg).expect("<infile> must be a c file"));
            },
        }
    }

    let infile = infile.unwrap();
    let outfile = outfile.unwrap();

    let mut infile_str = String::new();
    File::open(infile.as_str()).unwrap().read_to_string(&mut infile_str).unwrap();

    let lex = Lexer::new(infile_str.as_str(), infile.as_str()).unwrap();

    for token in lex {
        println!("{:?}", token.unwrap());
    }

    println!("compilation stage: {:?}, infile: {}, outfile: {}", compilation_stage, infile, outfile);
}

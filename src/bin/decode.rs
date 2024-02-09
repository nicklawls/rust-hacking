use rustbook::decoder;
use std::{env, fs, io};

fn main() -> io::Result<()> {
    let filename = env::args().nth(1).expect("file not found");
    let fl = fs::File::open(filename.to_string())?;
    let file_len = fl.metadata()?.len();
    eprintln!("{}, {:#?} B", filename, file_len);
    let instruction_stream = io::Read::bytes(io::BufReader::new(fl))
        // have to map away results. A bit hacky, but avoids collecting
        // too eagerly.
        .map(|x| x.expect("oops"));
    println!("bits 16");
    println!("");

    let instructions = match decoder::decode_instruction_stream(instruction_stream) {
        Ok(instructions) => instructions,
        Err((instructions, error)) => {
            eprintln!("{error:#?}");
            instructions
        }
    };

    for instruction in instructions
        .iter()
        .map(decoder::pp_asm)
        .collect::<Vec<String>>()
    {
        println!("{instruction}");
    }
    Ok(())
}

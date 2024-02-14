use rustbook::decoder;
use std::{env, fs, io};

fn main() -> io::Result<()> {
    let filename = env::args()
        .nth(1)
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "no second arg"))?;
    let fl = fs::File::open(filename.to_string())?;
    let file_len = fl.metadata()?.len();
    eprintln!("{}, {:#?} B", filename, file_len);
    let instruction_stream = io::Read::bytes(io::BufReader::new(fl))
        // have to map away results. A bit hacky, but avoids collecting
        // too eagerly.
        .map(|x| x.expect("oops"));

    let instructions = decoder::decode_instruction_stream(instruction_stream);

    println!("bits 16");
    println!("");
    for instruction in instructions {
        match instruction {
            Ok(instruction) => println!("{}", decoder::pp_asm(&instruction)),
            Err(error) => {
                eprintln!("Stopped on error: {error:#?}");
            }
        }
    }
    Ok(())
}

use rustbook::decode_instruction_stream;
use std::fs;
use std::io;

fn main() -> io::Result<()> {
    let file = fs::File::open("listing_0037_single_register_mov")?;
    let instruction_stream = io::Read::bytes(io::BufReader::new(file));

    let y = instruction_stream.map(|x| x.expect("oops"));

    // read file lines, get bytes from str, then group into u16s
    let decoded_instruction_stream =
        decode_instruction_stream(y).map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    println!("{decoded_instruction_stream:#?}");
    Ok(())
}

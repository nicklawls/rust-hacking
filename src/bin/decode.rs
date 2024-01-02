use rustbook::decode_instruction_stream;
use std::fs;
use std::io;

const FILES: [&str; 2] = [
    "listing_0037_single_register_mov",
    "listing_0038_many_register_mov",
];

fn main() -> io::Result<()> {
    for filename in FILES {
        let fl = fs::File::open(filename)?;
        let file_len = fl.metadata()?.len();
        println!("{}, {:#?} B", filename, file_len);
        let instruction_stream = io::Read::bytes(io::BufReader::new(fl));
        

        let y = instruction_stream.map(|x| x.expect("oops"));

        // read file lines, get bytes from str, then group into u16s
        let decoded_instruction_stream =
            decode_instruction_stream(y).map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

        println!("{decoded_instruction_stream:#?}");
    }
    Ok(())
}

use libnanoc::parse;
use memmap::MmapOptions;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Config {
    #[structopt(short, long)]
    input_file: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let config = Config::from_args();
    let file = std::fs::File::open(&config.input_file)?;
    let mmap = unsafe { MmapOptions::new().map(&file)? };

    let input = std::str::from_utf8(&mmap)?;
    println!("Output: {:#?}", parse(input));
    Ok(())
}

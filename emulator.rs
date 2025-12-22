use std::env;
use std::fs;
use std::io::Error;

fn slice(value : u16, low : u8, bits : u8) -> u16 {
    (value >> low) & ((1 << bits) - 1)
}

// TODO replace with image preprocessing?
fn load(mem : &Vec<u8>, addr : u32) -> u32 {
    mem[(4 * addr) as usize] as u32 + 
    ((mem[(4 * addr + 1) as usize] as u32) << 8) + 
    ((mem[(4 * addr + 2) as usize] as u32) << 16) + 
    ((mem[(4 * addr + 3) as usize] as u32) << 24)
}

fn store(mem : &mut Vec<u8>, addr : u32, data : u32) {
    mem[(4 * addr) as usize] = data as u8;
    mem[(4 * addr + 1) as usize] = (data >> 8) as u8;
    mem[(4 * addr + 2) as usize] = (data >> 16) as u8;
    mem[(4 * addr + 3) as usize] = (data >> 24) as u8;
}

// TODO replace with declarative rules-based system?
fn main() -> Result<(), Error> {

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        assert!(false); // TODO better error handling
    }

    // initialize processor state
    let mut pc : u32 = 0;
    let mut alignment : bool = false;
    let mut reg : Vec<u32> = vec![0; 8];

    // initialize memory from image file
    let image : Vec<u8> = fs::read(&args[1])?;
    let mut mem : Vec<u8> = vec![0; 1 << 34];
    let mut k : usize = 0;
    while k < image.len() {
        mem[k] = image[k];
        k += 1;
    }
    println!("Beginning emulation.");

    // fetch-decode-execute loop
    loop {
        let instruction : u16 = (load(&mem, pc) >> (if alignment {16} else {0})) as u16;
        if slice(instruction, 6, 10) == 0b0000000000 { // halt
            println!("Emulation complete, printing results:");
            let mut j : u32 = 0;
            let p : usize = slice(instruction, 3, 3) as usize;
            let l : usize = slice(instruction, 0, 3) as usize;
            while j < reg[l] {
                println!("{}", load(&mem, reg[p] + j));
                j += 1;
            }
            break;
        } else if slice(instruction, 7, 9) == 0b001100000 { // jmp
            let c : usize = slice(instruction, 3, 3) as usize;
            if reg[c] != 0 {
                let t : usize = slice(instruction, 0, 3) as usize;
                let h : u16 = slice(instruction, 6, 1);
                pc = reg[t];
                alignment = if h == 1 {true} else {false};
                continue;
            }
        } else if slice(instruction, 6, 10) == 0b0100000000 { // ld
            let p : usize = slice(instruction, 3, 3) as usize;
            let t : usize = slice(instruction, 0, 3) as usize;
            reg[t] = load(&mem, reg[p]);
        } else if slice(instruction, 6, 10) == 0b0101000000 { // st
            let p : usize = slice(instruction, 3, 3) as usize;
            let t : usize = slice(instruction, 0, 3) as usize;
            store(&mut mem, reg[p], reg[t]);
        } else if slice(instruction, 13, 3) == 0b011 { // mov
            let w : u8 = slice(instruction, 11, 2) as u8;
            let i : u32 = slice(instruction, 3, 8) as u32;
            let t : usize = slice(instruction, 0, 3) as usize;
            let mask : u32 = !((1 << (8 * (w + 1))) - (1 << (8 * w)));
            reg[t] = (reg[t] & mask) | (i << (8 * w));
        } else if slice(instruction, 9, 7) == 0b1000000 { // sub
            let a : usize = slice(instruction, 6, 3) as usize;
            let b : usize = slice(instruction, 3, 3) as usize;
            let t : usize = slice(instruction, 0, 3) as usize;
            reg[t] = reg[a].wrapping_sub(reg[b]);
        } else {
            println!("illegal instruction"); // TODO more debug information
            break;
            // TODO return error
        }
        if alignment {
            pc += 1;
        } 
        alignment = !alignment;
    }
    Ok(())
}

use std::env;
use std::fs;
use std::io::Error;

fn slice(value : u32, low : u8, bits : u8) -> u32 {
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
    let mut reg : Vec<u32> = vec![0; 32];

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
        let instruction : u32 = load(&mem, pc);
        if slice(instruction, 10, 22) == 0b0000000000000000000000 { // halt
            let mut j : u32 = 0;
            let p : usize = slice(instruction, 5, 5) as usize;
            let l : usize = slice(instruction, 0, 5) as usize;
            println!("{}", if reg[l] == 0 {"Emulation complete, printing results:"} else {"No results to print."});
            while j < reg[l] {
                println!("{}", load(&mem, reg[p] + j));
                j += 1;
            }
            break;
        } else if slice(instruction, 10, 22) == 0b0011000000000000000000 { // jmp
            let c : usize = slice(instruction, 5, 5) as usize;
            if reg[c] != 0 {
                let t : usize = slice(instruction, 0, 5) as usize;
                pc = reg[t];
                continue; // avoid incrementing pc as normal
            }
        } else if slice(instruction, 10, 22) == 0b0100000000000000000000 { // ld
            let p : usize = slice(instruction, 5, 5) as usize;
            let t : usize = slice(instruction, 0, 5) as usize;
            reg[t] = load(&mem, reg[p]);
        } else if slice(instruction, 10, 22) == 0b0101000000000000000000 { // st
            let p : usize = slice(instruction, 5, 5) as usize;
            let t : usize = slice(instruction, 0, 5) as usize;
            store(&mut mem, reg[p], reg[t]);
        } else if slice(instruction, 22, 10) == 0b0110000000 { // mov
            let w : u8 = slice(instruction, 21, 1) as u8;
            let i : u32 = slice(instruction, 0, 16) as u32;
            let t : usize = slice(instruction, 16, 5) as usize;
            // TODO refactor below as simple ternary
            let mask : u32 = !((1 << (8 * (w + 1))) - (1 << (8 * w)));
            reg[t] = (reg[t] & mask) | (i << (8 * w));
        } else if slice(instruction, 21, 11) == 0b10000000000 { // sub
            let a : usize = slice(instruction, 5, 5) as usize;
            let b : usize = slice(instruction, 0, 5) as usize;
            let t : usize = slice(instruction, 16, 5) as usize;
            reg[t] = reg[a].wrapping_sub(reg[b]);
        } else {
            println!("illegal instruction {:032b}", instruction); // TODO more debug information
            break;
            // TODO return error
        }
        pc += 1;
    }
    Ok(())
}

# The Nano-Arch Instruction Set

## Opcode Table

| name | opcode | assembly | description |
| :---: | :-----: | :--------: | :--------: |
| *halt* | `0000 0000 00pp plll` | `halt rp rl` | halts , pointing to `mem[reg[p]:reg[p]+reg[l]]`
| *jmp* | `0011 arrr nhcc cttt` | `jmp rc rt +h ` | if `reg[c] != 0`, jump to `mem[reg[t]] + h/2`
| *ld* | `0100 mmma aapp pttt` | `ld rp rt` | `reg[t] = mem[reg[p]]`
| *st* | `0101 mmma aapp pttt` | `st rp rt` | `mem[reg[p]] = reg[t]`
| *mov* | `011w wiii iiii ittt` | `mov @w $i rt` | `reg[t][8*w:8*(w+1)] = i`
| *sub* | `1000 000a aabb bttt` | `sub ra rb rt` | `reg[t] = reg[a] - reg[b]`
# The Nano-Arch Instruction Set

## Opcode Table

| name | opcode | assembly | description |
| :---: | :-----: | :--------: | :--------: |
| *halt* | `0000 0000 0000 0000 0000 00pp pppl llll` | `halt rp rl` | halts, pointing to `mem[reg[p]:reg[p]+reg[l]]`
| *jmp* | `0011 iiii iiii iiii iaaa aacc ccct tttt` | `jmp rc rt` | if `reg[c] != 0`, jump to `mem[reg[t]]`
| *ld* | `0100 mmmi iiii iiii iaaa aapp pppt tttt` | `ld rp rt` | `reg[t] = mem[reg[p]]`
| *st* | `0101 mmmi iiii iiii iaaa aapp pppt tttt` | `st rp rt` | `mem[reg[p]] = reg[t]`
| *mov* | `0110 0000 0owt tttt iiii iiii iiii iiii` | `mov @w $i rt` | `reg[t][16*w:16*(w+1)] = i`
| *sub* | `1000 0000 000t tttt iiii iiaa aaab bbbb` | `sub ra rb rt` | `reg[t] = reg[a] - reg[b]`
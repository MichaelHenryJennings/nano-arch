# The Nano-Arch Instruction Set

## Opcode Table

| name | opcode | assembly | description |
| :---: | :-----: | :--------: | :--------: |
| *halt* | `0000 0000 0000 0000 0000 00pp pppl llll` | `halt @rp #rl` | halts, pointing to `mem[reg[p]:reg[p]+reg[l]]`
| *jump* | `0011 iiii iiii iiii iiii iicc ccct tttt` | `jump ?rc -> rt` | if `reg[c] != 0`, jump to `mem[reg[t]]`
| *load* | `0100 iiii iiii iiii iiii iipp pppm mmmm` | `rm <= @rp` | `reg[m] = mem[reg[p]]`
| *store* | `0101 iiii iiii iiii iiii iipp pppv vvvv` | `@rp <= rv` | `mem[reg[p]] = reg[v]`
| *move* | `0110 0000 0owm mmmm iiii iiii iiii iiii` | `(lower/upper) rm <- $i` | `reg[t][16*w:16*(w+1)] = i`
| *subtract* | `1000 0000 000m mmmm iiii iiaa aaab bbbb` | `rm <= ra - rb` | `reg[t] = reg[a] - reg[b]`
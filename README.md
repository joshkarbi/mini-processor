# mini-processor
A VHDL implementation of a small processor, loaded with a program to generate prime numbers.

##### ISA:
Instruction format (each field is 3 bits long): `OPCODE` `DST` `SRC`

| Assembly Mnemonic | Opcode | Description |
| ------------- | ------------- | ---------|
| `MULT`| `000` | Compute `DST` * `SRC` and store in `HL` |
| `MOV` | `001` | Copy `SRC` to `DST` | 
| `BRA` | `011` | Branch to absolute location |
| `DIV` | `100` | Compute `HL` / `SRC`, store in `DST`, and set `Z` flag based on result |
| `MOD` | `101` | Compute `HL` % `SRC`, store in `DST`, and set `Z` flag based on result |
| `BEZ` | `110` | Branch to absolute location if `Z` flag is 1 |
| `SUB` | `111` | Compute `DST` - `SRC` and set `Z` based on result. |

##### Registers:
The processor has 8-bit registers, similar to the Intel 8080, (indexed from 000 to 111):
`B`, `C`, `D`, `E`, `H`, `L`, `mem`, and `A`

##### Memory:
The processor has a block of memory, `MEM`, where programs can be loaded. 

The processor always fetches the first instruction at startup. 

##### Testing:
The processor is currently loaded with a program to generate the sequence of prime numbers in register A.

Simulation results are in `sim/`.
# Tiger compiler
Implementation of [Appel's tiger language](https://www.cs.princeton.edu/~appel/modern/ml/) in Standard ML. Made as final project for Compilers subject in Computer Science at [National University of Rosario (UNR)](https://web.fceia.unr.edu.ar/es). Also contributed to this work Natalia Eyherabide ([@enati](https://github.com/enati)) and Alejo Rios ([@alejolcc](https://github.com/alejolcc)).

Build for x86 architecture. It uses GCC compiler as linker.

## Dependencies

- MosML >= 2.10.1
- g++-multilib >= 4:8.3.0-1
- gcc >= 8.3.0

## Build

To build the compiler, simply run:

```
make
```

Built executable (tigerc) along with runtime object (runtime.o) will be located in `bin/`.

## Usage

```
./tigerc [-arbol] [-asm] FILE.tig
```

Where:
 - **-arbol**: Prints AST.
 - **-asm**: Outputs assembler source code only.
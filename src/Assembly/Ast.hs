module Assembly.Ast where

type Opcode =  [Bool]
type Reg =     [Bool]
type Shamt =   [Bool]
type Funct =   [Bool]
type Imm =     [Bool]
type Address = [Bool]

type Rexpr = (Opcode, Reg, Reg, Reg, Shamt, Funct)
type Iexpr = (Opcode, Reg, Reg, Imm)
type Jexpr = (Opcode, Address)



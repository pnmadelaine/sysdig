module Assembly.Ast where

type Opcode =  [Bool]
type Reg =     [Bool]
type Shamt =   [Bool]
type Funct =   [Bool]
type Imm =     [Bool]
type Address = [Bool]
type Label = String

-- Opcode : little endian
-- Reg : little endian

data Instr = Rexpr Opcode Reg Reg Reg Shamt Funct
           | Iexpr Opcode Reg Reg Imm
           | Jexpr Opcode Label
           | Bexpr Opcode Reg Reg Label
           | Lexpr Label
           | Jump Opcode Address


type Prog = [Instr]

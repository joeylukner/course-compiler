(** This file contains type declarations and functions related to the compiler's
    representation of x86-64 assembly language. *)

open Batteries;;

(** Describes the registers of x86-64 that our code will use. *)
type register =
  | RAX
  | RSP
  | R10
  | R11
  | RDI
  | RSI
  | RDX
  | RCX
  | R8
  | R9
  | RBP
  (*additional callee saved registers*)
  | RBX
  | R12
  | R13
  | R14
  | R15
;;

(** Describes a memory address expression in x86-64 assembly. *)
type address =
  | AddrByRegister of register
  | AddrByRegisterOffset of register * int
  | AddrByLabel of string
  | AddrbyRegisterProductOffset of register * register * int
;;

(** Describes the type of arguments in our x86-64 assembly representation.  We
    use this type somewhat loosely: not every argument is valid everywhere an
    argument type is written below, but capturing the precise syntax limitations
    of x86 would make our assembly language types a lot more complicated.

    Note that the string argument of ArgConstant is the textual representation
    of the constant to be emitted to the assembly file, such as "5" or
    "0xFFFFFFFFFFFFFFFE".
*)
type argument =
  | ArgConstant of string
  | ArgRegister of register
  | ArgMemory of address
  | ArgLabelOffset of string * string
;;

(** The type that represents single x86 instructions. *)
type instruction =
  | AsmAdd of argument * argument
  | AsmIMul of argument * argument
  | AsmMov of argument * argument
  | AsmSub of argument * argument
  | AsmRet
  | AsmShl of argument * argument
  | AsmShr of argument * argument
  | AsmSal of argument * argument
  | AsmSar of argument * argument
  | AsmAnd of argument * argument
  | AsmOr  of argument * argument
  | AsmXor of argument * argument
  | AsmLabel of string
  | AsmCmp of argument * argument
  | AsmJmp of string
  | AsmJe of string
  | AsmJne of string
  | AsmJl of string
  | AsmJle of string
  | AsmJg of string
  | AsmPush of argument 
  | AsmPop of argument
  | AsmCall of string
  | AsmSection of string
  | AsmAlign of argument
  | AsmDq of string list
  | AsmRep
  | AsmSto


;;

(** A function which transforms an x86 register into a string suitable for
    writing into an assembly language file. *)
let code_of_register (register : register) : string =
  match register with
  (*simple <3*)
  |RAX -> "RAX"
  |RSP -> "RSP"
  |R10 -> "R10"
  |R11 -> "R11"
  |RDI -> "RDI"
  |RSI -> "RSI"
  |RDX -> "RDX"
  |RCX -> "RCX"
  |R8 -> "R8"
  |R9 -> "R9"
  |RBP -> "RBP"
  | RBX -> "RBX"
  | R12 -> "R12"
  | R13 -> "R13"
  | R14 -> "R14"
  | R15 -> "R15"
;;

(** A function which transforms an x86 address expression into a string suitable
    for writing into an assembly language file. *)
let code_of_address (address : address) : string =
  (*slightly less simple <3*)
  match address with 
  | AddrByRegister (reg) -> "["^code_of_register(reg)^"]"
  | AddrByRegisterOffset (reg, integer)-> 
    (*[Reg - integer]*)
    "[" ^ code_of_register(reg)^ " + "^ string_of_int(integer) ^ "]"
  | AddrByLabel (name)-> 
    "[" ^ name ^ "]"
  | AddrbyRegisterProductOffset (rax, reg, integer) ->
    "[" ^ code_of_register(rax) ^ " + " ^ code_of_register(reg) ^ " * " ^ string_of_int(integer) ^ "]"
;;

(** A function which transforms an x86 argument into a string suitable for
    writing into an assembly language file. *)
let code_of_argument (argument : argument) : string =
  match argument with
  (*simple <3*)
  | ArgConstant (str) -> str 
  | ArgRegister (reg) -> code_of_register(reg)
  | ArgMemory (add) -> code_of_address(add)
  | ArgLabelOffset (closure, int) -> closure ^ " + " ^ int ^ "\n"
;;

(** A function which transforms an x86 instruction into a string suitable for
    writing into an assembly language file.  For example, given the input
    AsmRet, an appropriate return value might be "  ret\n".
*)
let code_of_instruction (instruction : instruction) : string =
  match instruction with 
  | AsmAdd (add1, add2) ->
    "  add "^code_of_argument(add1)^", "^ code_of_argument(add2)^ "\n"
  | AsmIMul (mul1, mul2) ->
    "  imul "^code_of_argument(mul1)^", "^ code_of_argument(mul2)^  "\n"
  | AsmMov (dest, src) -> 
    "  mov "^code_of_argument(dest)^", "^ code_of_argument(src)^  "\n"
  | AsmSub (sub1, sub2) ->
    "  sub "^code_of_argument(sub1)^", "^ code_of_argument(sub2) ^"\n"
  | AsmRet -> 
    "  ret\n"
  | AsmShl (x, y) -> 
    "  shl "^code_of_argument(x)^", "^code_of_argument(y)^"\n"
  | AsmShr (x, y) ->
    "  shr "^code_of_argument(x)^", "^code_of_argument(y)^"\n"
  | AsmSal  (x, y) -> 
    "  sal "^code_of_argument(x)^", "^code_of_argument(y)^"\n"
  | AsmSar (x, y) -> 
    "  sar "^code_of_argument(x)^", "^code_of_argument(y)^"\n"
  | AsmAnd  (x, y) ->
    "  and "^code_of_argument(x)^", "^code_of_argument(y)^"\n"
  | AsmOr   (x, y) ->
    "  or "^code_of_argument(x)^", "^code_of_argument(y)^"\n"
  | AsmXor  (x, y) ->
    "  xor "^code_of_argument(x)^", "^code_of_argument(y)^"\n"
  | AsmLabel  (str) ->
    str ^ ": "
  | AsmCmp  (x, y) ->
    "  cmp "^code_of_argument(x)^", "^code_of_argument(y)^"\n"
  | AsmJmp  (str) ->
    "  jmp "^str^"\n"
  | AsmJe  (str) ->
    "  je "^str^"\n"
  | AsmJne  (str) ->
    "  jne "^str^"\n"
  | AsmJl (str)->
    "  jl "^str^"\n"
  | AsmJle (str)->
    "  jle "^str^"\n"
  | AsmJg (str) ->
    "  jg "^str^"\n"
  | AsmPush (arg) -> 
    "  push "^code_of_argument(arg)^"\n"
  | AsmPop (arg) ->
    "  pop "^code_of_argument(arg)^"\n"
  | AsmCall (str) ->
    "  call "^str^"\n"
  | AsmSection (str)-> 
    "section ." ^ str ^ "\n"
  | AsmAlign (int)->
    "align "^ code_of_argument int ^"\n"
  | AsmDq (_) -> "  dq "
  | AsmRep -> "  rep movsq\n"
  | AsmSto -> "  rep stosq\n"
;;

(** A function which transforms a list of x86 instructions into a string
    suitable for writing into an assembly language file. *)
let rec code_of_instruction_list (instruction_list : instruction list) : string =
  match instruction_list with
  |[] -> ""
  |x::y -> 
    code_of_instruction(x) ^code_of_instruction_list(y)
;;

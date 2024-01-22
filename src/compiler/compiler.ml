(** This file contains the definition of the compiler: the tool that translates
    an AST in our language into assembly language. *)

open Batteries;;
open HatchLanguage;;

open Assembly;;
open Asts;;
open Environment;;
open Printf;;
open Freshening;;

let string_of_twice_int (n : int) : string =
  Int64.to_string (Int64.mul (Int64.of_int n) (Int64.of_int 2))

let stack_memory_of_memory_address (addr : address) : int = 
  match addr with
  | AddrByRegister (_)-> 0
  | AddrByRegisterOffset (_, integer) -> -1 * integer
  | AddrByLabel (_) -> 0
  | AddrbyRegisterProductOffset(_, _, _) ->  0
;;
let stack_memory_of_argument (argu : argument ) : int = 
  match argu with
  | ArgConstant (_)->  0
  | ArgRegister (_) ->  0
  | ArgMemory (addr) -> stack_memory_of_memory_address(addr)
  | ArgLabelOffset (_,_) -> 0
;;

let compare_stack_mems (arg1: argument) (arg2: argument) : int =
  if(stack_memory_of_argument(arg1) > stack_memory_of_argument(arg2)) then
    stack_memory_of_argument(arg1)
  else
    stack_memory_of_argument(arg2)
;;
let stack_memory_of_instruction (inst : instruction) : int =
  match inst with 
  | AsmAdd (arg1, arg2)->
    compare_stack_mems arg1 arg2
  | AsmIMul (arg1, arg2)->
    compare_stack_mems arg1 arg2
  | AsmMov (arg1, arg2)->
    compare_stack_mems arg1 arg2
  | AsmSub (arg1, arg2) ->
    compare_stack_mems arg1 arg2
  | AsmRet ->0
  | AsmShl  (arg1, arg2) ->
    compare_stack_mems arg1 arg2
  | AsmShr (arg1, arg2) ->
    compare_stack_mems arg1 arg2
  | AsmSal  (arg1, arg2) ->
    compare_stack_mems arg1 arg2
  | AsmSar (arg1, arg2) ->
    compare_stack_mems arg1 arg2
  | AsmAnd (arg1, arg2) ->
    compare_stack_mems arg1 arg2
  | AsmOr (arg1, arg2) ->
    compare_stack_mems arg1 arg2
  | AsmXor (arg1, arg2) ->
    compare_stack_mems arg1 arg2
  | AsmLabel (str) ->ignore str;0
  | AsmCmp (arg1, arg2) ->
    compare_stack_mems arg1 arg2
  | AsmJmp (_) ->0
  | AsmJe (_)  ->0
  | AsmJne (_)  ->0
  | AsmJl (_)  ->0
  | AsmJle (_) -> 0
  | AsmJg (_)  ->0
  | AsmPush (arg1) ->
    stack_memory_of_argument(arg1)
  | AsmPop (arg1) ->
    stack_memory_of_argument(arg1)
  | AsmCall (_)  -> 0
  | AsmSection (_)->0
  | AsmAlign (_)-> 0
  | AsmDq (_) -> 0
  | AsmRep -> 0
  | AsmSto -> 0
;;

let rec list_max  (list: int list) : int = 


  match list with
  |[] -> 0
  |x::y -> 
    let list_max_y = list_max(y) in 
    if x > list_max_y then
      x
    else
      list_max_y
;;
let rec stack_memory_of_instruction_list (list : instruction list) : int = 
  let new_list = List.map (stack_memory_of_instruction) (list) in
  list_max  new_list
;;

let check_rax_int (): instruction list =
  let error = fresh_name "check_int_error" in
  let end_label = fresh_name "check_int_end_label" in
  [AsmMov (ArgRegister(R11), ArgRegister(RAX));
   AsmAnd (ArgRegister(RAX), ArgConstant(string_of_int 1));
   AsmCmp (ArgRegister(RAX), ArgConstant(string_of_int 0));
   AsmJne error;
   (*this is an int if does not jump*)
   AsmMov (ArgRegister(RAX), ArgRegister(R11));
   AsmJmp end_label;
   AsmLabel error;
   AsmPush(ArgRegister(RAX));
   AsmMov (ArgRegister(RDI), ArgConstant(string_of_int 1));
   AsmCall " stopWithError";
   AsmPop (ArgRegister(RAX));
   AsmLabel end_label
  ]
;; 

let check_rax_bool() : instruction list = 
  let error = fresh_name "check_bool_error" in 
  let end_label = fresh_name "check_bool_end_label" in
  [AsmMov (ArgRegister(R11), ArgRegister(RAX));
   AsmAnd (ArgRegister(RAX), ArgConstant(string_of_int 3));
   AsmCmp (ArgRegister(RAX), ArgConstant(string_of_int 3));
   AsmJne error;
   (*this is a bool if it doesn't jump*)
   AsmMov(ArgRegister(RAX), ArgRegister(R11));
   AsmJmp end_label;
   AsmLabel error;
   AsmPush (ArgRegister(RAX));
   AsmMov (ArgRegister(RDI), ArgConstant(string_of_int 2));
   AsmCall " stopWithError";
   AsmPop (ArgRegister(RAX));
   AsmLabel end_label]
;;

let check_rax_tuple() : instruction list = 
  let error = fresh_name "check_tuple_error" in
  let end_label = fresh_name "check_tuple_end_label" in
  [AsmMov (ArgRegister(R11), ArgRegister(RAX));
   AsmAnd (ArgRegister RAX, ArgConstant "3");
   AsmCmp (ArgRegister RAX, ArgConstant "3");
   AsmJe error;

   (* boolean
      z   1       1   1
      b63 b62 .... b1 b0

      tuple 
      z    z       0   1
      b63 b62 .... b1 b0 

      int 

      z   z        z   0
      b63 b62 .... b1 b0
      also do a check for ints
   *)
   AsmMov (ArgRegister RAX, ArgRegister R11);
   AsmAnd (ArgRegister RAX, ArgConstant "1");
   AsmCmp (ArgRegister RAX, ArgConstant "0");
   AsmJe error;

   (*this is a heap representation if it doesn't jump*)

   AsmMov (ArgRegister RAX, ArgRegister R11);
   AsmXor (ArgRegister RAX, ArgConstant "1");
   AsmMov (ArgRegister R9, ArgMemory(AddrByRegister RAX) );
   AsmShr (ArgRegister R9, ArgConstant(string_of_int 63) );
   AsmCmp (ArgRegister R9, ArgConstant "1");

   AsmJe error;

   (*this is a tuple if it doesn't jump*)
   AsmMov(ArgRegister(RAX), ArgRegister(R11));
   AsmJmp end_label;
   AsmLabel error;
   AsmPush (ArgRegister(RAX));
   AsmMov (ArgRegister(RDI), ArgConstant("3"));
   AsmCall " stopWithError";
   AsmPop (ArgRegister(RAX));
   AsmLabel end_label
  ]
;;

let check_rax_valid_index(addr: argument) : instruction list = 
  (*this function is called under the assumption that 
     check rax tuple was successful and that rax
     currently has the value of the index in it, which 
     has replaced the value of the tuple identifier in rax
     at some point*)
  let error = fresh_name "check_index_error" in 
  let end_label = fresh_name "check_index_end_label" in 
  [(*step 1: check if index is negative*)
    (*mov rax into r11 copies index (0 in tindex_1) into r11*)
    AsmMov (ArgRegister R11, ArgRegister RAX);
    AsmMov (ArgRegister R8,  ArgConstant "0x8000000000000000");
    AsmAnd (ArgRegister RAX, ArgRegister R8);
    AsmXor (ArgRegister RAX, ArgRegister R8);
    AsmMov (ArgRegister R9, ArgConstant "0x8000000000000000");
    AsmCmp (ArgRegister RAX, ArgRegister R9);
    AsmJne error;
    (*this index is positive if it doesn't jump*)
    AsmMov (ArgRegister RAX, ArgRegister R11);

    (*step 2: check if index is too large*)
    (*addr is pointer to tuple*)
    AsmMov (ArgRegister R10, addr);
    AsmXor (ArgRegister R10, ArgConstant "1");
    (*index is subtraced from size. it is a valid index if doesn't jump
       shift right 1 to divide by 1*)
    AsmMov (ArgRegister R9, ArgRegister R11);
    AsmSar (ArgRegister R9, ArgConstant "1");
    AsmCmp (ArgMemory(AddrByRegister R10), ArgRegister R9);
    AsmJle error;
    AsmMov (ArgRegister RAX, ArgRegister R11);
    AsmJmp end_label;

    (*this error sequence can be used for both, since share error code*)
    AsmLabel error;
    AsmPush (ArgRegister RAX);
    AsmMov (ArgRegister RDI, ArgConstant "4");
    AsmCall " stopWithError";
    AsmPop (ArgRegister RAX);
    AsmLabel end_label
  ]
;;   

let check_rax_closure() : instruction list = 
  let error = fresh_name "check_closure_error" in
  let end_label = fresh_name "check_closure_end_label" in
  [
    AsmMov (ArgRegister R11, ArgRegister RAX);
    AsmAnd (ArgRegister R11, ArgConstant "3");
    AsmCmp (ArgRegister R11, ArgConstant "3");
    AsmJe error;
    AsmMov (ArgRegister R11, ArgRegister RAX);
    AsmXor (ArgRegister R11, ArgConstant "1");
    AsmMov (ArgRegister RAX, ArgMemory (AddrByRegister R11));
    AsmOr  (ArgRegister R11, ArgConstant "1");
    AsmShr (ArgRegister RAX, ArgConstant "63");
    AsmAnd (ArgRegister RAX, ArgConstant "1");
    AsmCmp (ArgRegister RAX, ArgConstant "1");
    AsmJne error;
    AsmMov (ArgRegister RAX, ArgRegister R11);
    AsmJmp end_label;
    AsmLabel error;
    AsmPush (ArgRegister RAX);
    AsmMov (ArgRegister RDI, ArgConstant "5");
    AsmCall " stopWithError";
    AsmPop (ArgRegister RAX);
    AsmLabel end_label;
  ]
;;

let memory_check (desired : int) : instruction list = 
  let end_label = fresh_name "enough_memory" in
  [
    (*check to see if there is enough memory available and call gc*)
    AsmMov (ArgRegister R8, ArgConstant(string_of_int desired));
    AsmMov (ArgRegister RAX, ArgMemory (AddrByLabel "end_of_heap"));
    AsmMov(ArgRegister R9, ArgMemory(AddrByLabel "heap_cursor"));
    AsmSub (ArgRegister RAX, ArgRegister R9);
    AsmCmp (ArgRegister RAX, ArgRegister R8);
    AsmJg end_label;
    AsmMov (ArgMemory (AddrByLabel "end_of_stack"), ArgRegister RSP);
    AsmMov (ArgRegister RDI, ArgRegister R8);
    AsmCall " gc";
    AsmLabel end_label;
  ]
;;
let rec compile_expression (env : environment) (e : expr)
  : instruction list =
  match e with
  | ETuple (expr_list) -> 

    let tuple_size = List.length expr_list in

    let rec compute_tuple_arguments (recursive_env: environment) (expr_list : expr list) : instruction list * argument list=
      (match expr_list with
       |[]-> ([],[])
       |x::y ->
         let (addr1, args_env) = allocate_temp_variable recursive_env in
         let (instructions_for_arg) = compile_expression args_env x in
         let (recursive_instructions, recursive_args) = compute_tuple_arguments (args_env) y in
         ((instructions_for_arg @[AsmMov (addr1, ArgRegister RAX)]@ recursive_instructions),(addr1:: recursive_args))
      )
    in

    let (compute_instructions, argument_addresses) = compute_tuple_arguments env expr_list 

    in

    let rec save_tuple_arguments_onto_heap (addr_list : argument list)(offset : int) : instruction list = 
      match addr_list with
      |[] -> []
      |x::y ->
        [AsmMov (ArgRegister R11, x);
         AsmMov (ArgMemory (AddrByRegisterOffset (RAX, offset)), ArgRegister R11);
         AsmMov (ArgRegister R10, ArgConstant "0");
         AsmMov (x, ArgRegister R10)
         (*write 0 to memory location after using temp var*)
        ]
        @
        save_tuple_arguments_onto_heap y (offset + 8)
    in
    compute_instructions
    @
    let mem_desired = (tuple_size * 8) + 16 in  

     memory_check mem_desired 
    @

    [AsmMov (ArgRegister R8, ArgMemory(AddrByLabel "heap_cursor"));
     AsmMov (ArgRegister R9, ArgConstant(string_of_int tuple_size));
     AsmMov (ArgMemory(AddrByRegister R8), ArgRegister R9);
     AsmMov (ArgRegister R9, ArgConstant "0");
     AsmMov (ArgMemory(AddrByRegisterOffset (R8, 8)), ArgRegister R9);

     AsmMov (ArgRegister RAX, ArgMemory(AddrByLabel "heap_cursor"));
     AsmMov (ArgRegister R10, ArgRegister RAX);
     AsmAdd (ArgRegister R10, ArgConstant(string_of_int mem_desired));
     AsmAdd (ArgRegister R10, ArgConstant(string_of_int mem_desired));
     AsmMov (ArgMemory(AddrByLabel "heap_cursor"), ArgRegister R10)]

    @

    save_tuple_arguments_onto_heap argument_addresses 16
    @
    [AsmOr (ArgRegister RAX, ArgConstant "1")] (*set up rax; does this match binary rep?*)

  |EInt (integer)-> 
    [AsmMov(ArgRegister(RAX), ArgConstant(string_of_twice_int(integer)))]
  |EBool (boolean) -> 
    (match boolean with 
     |true -> [AsmMov (ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"))]
     |false ->[AsmMov (ArgRegister (RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"))])


  |EUnaryOp (unop, expression )-> 
    (match unop with
     |OpAfter -> 
       compile_expression (env) (expression)
       @ [AsmAdd(ArgRegister(RAX), ArgConstant(string_of_int 2))]
       @ check_rax_int ()
     |OpBefore -> 
       compile_expression env expression
       @ [AsmSub(ArgRegister(RAX),ArgConstant(string_of_int 2))]
       @check_rax_int ()
     |OpIsBool -> 
       compile_expression env expression
       @ 
       let conflicting_rep = fresh_name "conflicting_rep" in
       let end_label = fresh_name "end_label" in
       [AsmMov (ArgRegister R9, ArgRegister RAX);
        AsmAnd (ArgRegister R9, ArgConstant "3");
        AsmCmp (ArgRegister R9, ArgConstant "3");
        AsmJe conflicting_rep;
        AsmMov (ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF");
        AsmJmp end_label;
        AsmLabel conflicting_rep;
        AsmSal (ArgRegister(RAX), ArgConstant(string_of_int 63));
        AsmMov (ArgRegister(R10), ArgConstant("0x7FFFFFFFFFFFFFFF"));
        AsmOr  (ArgRegister(RAX), ArgRegister(R10));
        AsmLabel end_label;
       ]

     |OpIsInt -> 
       compile_expression env expression
       @
       [AsmXor (ArgRegister(RAX),ArgConstant(string_of_int 1));
        AsmSal (ArgRegister(RAX), ArgConstant(string_of_int 63));
        AsmMov (ArgRegister(R10), ArgConstant("0x7FFFFFFFFFFFFFFF"));
        AsmOr  (ArgRegister(RAX), ArgRegister(R10))]
     |OpPrint ->
       let instructions = compile_expression env expression in




       instructions @
       [AsmPush(ArgRegister(RAX));
        AsmMov (ArgRegister(RDI), ArgRegister(RAX));
        AsmCall " printValue";
        AsmPop (ArgRegister(RAX));

       ] 
     |OpIsTuple ->
       let not_tuple = fresh_name "not_tuple" in
       let endlabel = fresh_name "end" in

       compile_expression env expression
       @

       [
         AsmMov (ArgRegister R11, ArgRegister RAX);
         AsmShl (ArgRegister RAX, ArgConstant "62");
         AsmMov (ArgRegister R8, ArgConstant "0xc000000000000000");
         AsmMov (ArgRegister R9, ArgConstant "0x8000000000000000");
         AsmXor (ArgRegister RAX, ArgRegister R9);
         AsmCmp (ArgRegister RAX, ArgRegister R8);
         AsmJne not_tuple;
         (*this is a heap representation*)
         AsmXor (ArgRegister R11, ArgConstant "1");
         AsmMov (ArgRegister RAX, ArgMemory(AddrByRegister R11));


         AsmShr (ArgRegister RAX, ArgConstant( string_of_int 63));
         AsmMov (ArgRegister R10, ArgConstant "0");
         AsmCmp (ArgRegister R10, ArgRegister RAX);
         AsmJne not_tuple;

         AsmMov (ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF");
         AsmJmp endlabel;
         AsmLabel not_tuple;
         (*this is not a tuple if it runs this code- put binary rep 
            for false into rax*)
         AsmMov (ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF");

         AsmLabel endlabel;]
    )


  | EVar (str) ->
    [AsmMov (ArgRegister(RAX), find_named_variable str env)] 

  | EBinaryOp (biop, expression_1, expression_2) ->
    let (left_address, left_env) = allocate_temp_variable(env) in
    compile_expression left_env expression_1

    @ (match biop with 
        |OpPlus -> check_rax_int()
        |OpMinus -> check_rax_int()
        |OpTimes ->  check_rax_int()@
                     [AsmSar(ArgRegister RAX, ArgConstant (string_of_int (1)))]
        |OpLessThan -> check_rax_int()
        |OpGreaterThan -> check_rax_int()
        |OpEqualTo -> check_rax_int()
        |OpAnd -> check_rax_bool()
        |OpOr -> check_rax_bool()
        |OpTupleIndex -> check_rax_tuple())

    @ [AsmMov (left_address, ArgRegister(RAX))]
    @ let (right_address, _) = allocate_temp_variable(left_env) in
    compile_expression left_env expression_2 
    @ (match biop with 
        |OpPlus -> check_rax_int()
        |OpMinus -> check_rax_int()
        |OpTimes ->  check_rax_int()
        |OpLessThan -> check_rax_int()
        |OpGreaterThan -> check_rax_int()
        |OpEqualTo -> check_rax_int()
        |OpAnd -> check_rax_bool()
        |OpOr -> check_rax_bool()
        |OpTupleIndex -> 
          (*have to check to make sure is integer and in bounds*)
          check_rax_int()
          @
          check_rax_valid_index(left_address) )
    @ [AsmMov (right_address, ArgRegister RAX)]
    @ [AsmMov(ArgRegister(RAX), left_address)]
    @ (match biop with 
        |OpPlus -> [AsmAdd(ArgRegister (RAX), right_address );]
        |OpMinus -> [AsmSub(ArgRegister (RAX), right_address)]
        |OpTimes ->  [AsmIMul(ArgRegister (RAX), right_address)]
        |OpLessThan -> 
          let a_less_than_b = fresh_name "a_less_than_b" in
          let endlabel = fresh_name "end" in
          [
            AsmMov(ArgRegister(R10), right_address);
            AsmCmp(ArgRegister(RAX),ArgRegister(R10));
            AsmJl a_less_than_b;
            AsmMov (ArgRegister(RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"));
            AsmJmp endlabel;
            AsmLabel a_less_than_b;
            AsmMov (ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"));
            AsmLabel endlabel
          ]
        |OpGreaterThan -> 
          let a_greater_than_b = fresh_name "a_greater_than_b" in
          let endlabel = fresh_name "end" in
          [
            AsmMov(ArgRegister(R10), right_address);
            AsmCmp(ArgRegister(RAX),ArgRegister(R10));
            AsmJg a_greater_than_b;
            AsmMov (ArgRegister(RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"));
            AsmJmp endlabel;
            AsmLabel a_greater_than_b;
            AsmMov (ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"));
            AsmLabel endlabel
          ]
        |OpEqualTo -> 
          let equal = fresh_name "equal" in
          let endlabel = fresh_name "end" in
          [AsmMov(ArgRegister(R10),right_address);
           AsmCmp(ArgRegister (RAX), ArgRegister(R10));
           AsmJe equal
          (*not equal- inserting false*)
          ;AsmMov (ArgRegister(R10),ArgConstant("0x7FFFFFFFFFFFFFFF") )
          ;AsmMov (ArgRegister(RAX), ArgRegister(R10))
          ;AsmJmp endlabel
          ;AsmLabel equal
          ;AsmMov (ArgRegister(R10), ArgConstant("0xFFFFFFFFFFFFFFFF"))
          ;AsmMov (ArgRegister(RAX), ArgRegister(R10))
          ;AsmLabel endlabel]
        |OpAnd -> 
          [AsmMov(ArgRegister(R10),right_address)
          ;AsmAnd(ArgRegister (RAX), ArgRegister(R10))]
        |OpOr -> 
          [AsmMov(ArgRegister(R10),right_address)
          ;AsmOr(ArgRegister (RAX), ArgRegister(R10))]
        |OpTupleIndex -> 
          [(*move index into register to use it R10 holds index, R9 holds pointer*)
            AsmMov(ArgRegister R10, right_address);
            AsmMov(ArgRegister R9, left_address);
            AsmXor(ArgRegister R9, ArgConstant "1");
            AsmAdd(ArgRegister R9, ArgConstant "16");
            AsmSar (ArgRegister R10, ArgConstant "1");
            AsmMov(ArgRegister RAX, ArgMemory(AddrbyRegisterProductOffset(R9, R10, 8)))
          ])
    @
    [AsmMov (ArgRegister R10, ArgConstant "0");
     AsmMov (left_address, ArgRegister R10);
     AsmMov (right_address, ArgRegister R10)]
  (*write 0 to memory location after allocating temporary variable*)

  | ELet (str, expression_1, expression_2)-> 
    (*ELet("x",EInt(1),EUnaryOp(OpAfter,EVar("x")))*)
    let recursive_env : environment = (allocate_named_variable(str) (env)) in

    compile_expression recursive_env expression_1
    @[AsmMov(find_named_variable(str) (recursive_env), ArgRegister RAX)]
    @ compile_expression recursive_env expression_2
    @

    [AsmMov (ArgRegister R10, ArgConstant "0");
     AsmMov (find_named_variable(str) (recursive_env),ArgRegister R10)]
  (* write 0 to memory location after stop using a local variable*)

  |EIf (boolean, expr1, expr2) -> 

    let else_label = fresh_name "else" in
    let end_label = fresh_name "end" in
    compile_expression env boolean
    @ check_rax_bool ()
    @[AsmMov(ArgRegister(R10),ArgConstant("0xFFFFFFFFFFFFFFFF"))
     ;AsmCmp (ArgRegister(RAX), (ArgRegister(R10)))
     ;AsmJne else_label]
    @compile_expression env expr1
    @
    [AsmJmp end_label;
     AsmLabel else_label]
    @compile_expression env expr2
    @[(*insert dummy instruction to make sure end labels aren't on same line*)
      AsmMov (ArgRegister(RAX),ArgRegister(RAX)) ;
      AsmLabel end_label]
  |EAppl(expression_1,expression_2, tail_bool) ->
    let () = print_endline(show_expr e) in 
    let () = print_endline("starting eappl") in 
    let grow_closure = fresh_name "grow_closure" in 
    let end_label = fresh_name "end_label" in

    (*compile left subexpression*) 
    let (closure_addr, env') = allocate_temp_variable env in 
    compile_expression env' expression_1
    (*check to make sure first argument is a closure- fail if it is not*)
    @ check_rax_closure() 
    @[AsmMov (closure_addr, ArgRegister RAX)]
    @ 
    let (arg_addr, _) = allocate_temp_variable env' in
    compile_expression env' expression_2
    @
    let (padding,_) = allocate_temp_variable env' in 

    if tail_bool = false then 
      let () = print_endline ("if condition") in 
      ((*compare number of arguments a in closure to number of paremeters for function p*)
      [AsmMov (arg_addr, ArgRegister RAX);
       AsmMov (ArgRegister RAX, closure_addr);
       AsmXor (ArgRegister RAX, ArgConstant "1");
       AsmMov (ArgRegister R11, ArgMemory(AddrByRegister RAX));
       AsmMov (ArgRegister R10, ArgMemory(AddrByRegisterOffset(RAX, 16)));
       AsmMov (ArgRegister R9, ArgConstant "0x8000000000000000");
       AsmXor (ArgRegister R11, ArgRegister R9);
       AsmMov (ArgRegister R9, ArgRegister R11); (*# current args in R9*)

      ]@


       let memory = fresh_name "enough_memory" in
      [
        (*check to see if there is enough memory available and call gc*)
        AsmMov (ArgRegister RSI, ArgRegister R9);
        AsmIMul (ArgRegister RSI, ArgConstant "8");
        AsmAdd (ArgRegister RSI, ArgConstant "40"); 
        (*40 is size of overhead bits plus one addiitonal argument *)
        AsmMov (ArgRegister RDX, ArgMemory (AddrByLabel "end_of_heap"));
        AsmMov(ArgRegister RCX, ArgMemory(AddrByLabel "heap_cursor"));
        AsmSub (ArgRegister RDX, ArgRegister RCX);
        AsmCmp (ArgRegister RDX, ArgRegister RSI);
        AsmJg memory; (* jumps if there is enough memory- val in RDX > Val in RSI*)
        AsmOr (ArgRegister RAX, ArgConstant "1");
      AsmPush (ArgRegister RAX);
        AsmPush (ArgRegister R11);
        AsmPush (ArgRegister R10);
        AsmPush (ArgRegister R9);
        AsmMov (ArgMemory (AddrByLabel "end_of_stack"), ArgRegister RSP);
        AsmMov (ArgRegister RDI, ArgRegister RSI);
        AsmCall " gc";
        AsmPop (ArgRegister R9);
        AsmPop (ArgRegister R10);
        AsmPop (ArgRegister R11);
        AsmPop (ArgRegister RAX);
        AsmSub (ArgRegister RAX, ArgConstant "1");
      AsmLabel memory;
      ]


      @[
        AsmAdd (ArgRegister R11, ArgConstant "1");
        AsmCmp (ArgRegister R11, ArgRegister R10);

        (*overhead bytes*)
        AsmJne grow_closure;
        (*GO TIME*)
        AsmIMul(ArgRegister R11, ArgConstant "8");
        AsmSub (ArgRegister RSP, ArgRegister R11);
        AsmMov (ArgRegister RSI, ArgRegister RAX);
        AsmAdd (ArgRegister RSI, ArgConstant "32");
        AsmMov (ArgRegister RDI, ArgRegister RSP);
        AsmMov (ArgRegister RCX, ArgRegister R9);
        AsmRep;
        AsmMov (ArgRegister R8, arg_addr);
        AsmMov (ArgMemory (AddrByRegister RDI), ArgRegister R8);
        AsmMov (ArgRegister RAX, ArgMemory (AddrByRegisterOffset(RAX, 24)));
        AsmMov (padding, ArgRegister R11);
        AsmCall ("  rax");
        AsmMov (ArgRegister R11, padding);
        AsmSub (ArgRegister RSP, ArgRegister R11);
        AsmJmp end_label;
        AsmLabel grow_closure;


        AsmIMul(ArgRegister R11, ArgConstant "8");
        AsmAdd (ArgRegister R11, ArgConstant "32");
        AsmMov (ArgRegister RSI, ArgRegister RAX);
        AsmMov (ArgRegister RDI, ArgMemory (AddrByLabel "heap_cursor"));
        AsmMov (ArgRegister RCX, ArgRegister R9);
        AsmAdd (ArgRegister RCX, ArgConstant "4"); (*overhead bytes*)
        AsmRep;
        AsmMov (ArgRegister R8, arg_addr);
        AsmMov (ArgMemory (AddrByRegister RDI), ArgRegister R8);
        AsmMov (ArgRegister RAX, ArgMemory (AddrByLabel "heap_cursor"));
        AsmMov (ArgRegister R10, ArgRegister RAX);
        AsmMov (ArgRegister RDI, ArgConstant "0");
      AsmMov (ArgMemory (AddrByRegisterOffset (RAX, 8)), ArgRegister RDI);
      AsmOr  (ArgRegister RAX, ArgConstant "1");
        AsmMov (ArgRegister R9, ArgConstant "1" );
        AsmAdd (ArgMemory (AddrByRegister R10), ArgRegister R9);
        AsmAdd (ArgRegister R10, ArgRegister R11);
        AsmMov (ArgMemory (AddrByLabel "heap_cursor"), ArgRegister R10);
        AsmLabel end_label;
        AsmMov (ArgRegister R10, ArgConstant "0");
        AsmMov (closure_addr, ArgRegister R10);
        AsmMov (arg_addr, ArgRegister R10);
        AsmMov (padding, ArgRegister R10);
        (*write 0 to memory location after allocating temporary variable*)
      ])
    else
      let () = print_endline("else condition ") in 

      let num_params = retrieve_num_parameters env' in 
      (*compare number of arguments a in closure to number of paremeters for function p*)
      [AsmMov (arg_addr, ArgRegister RAX);
       AsmMov (ArgRegister RAX, closure_addr);
       AsmXor (ArgRegister RAX, ArgConstant "1");
       AsmMov (ArgRegister R11, ArgMemory(AddrByRegister RAX));
       AsmMov (ArgRegister R10, ArgMemory(AddrByRegisterOffset(RAX, 16)));
       AsmMov (ArgRegister R9, ArgConstant "0x8000000000000000");
       AsmXor (ArgRegister R11, ArgRegister R9);
       AsmMov (ArgRegister R9, ArgRegister R11); (*# current args in R9*)

      ]@


    (*  let memory = fresh_name "enough_memory" in
      [
        (*check to see if there is enough memory available and call gc*)
        AsmMov (ArgRegister RSI, ArgRegister R9);
        AsmIMul (ArgRegister RSI, ArgConstant "8");
        AsmAdd (ArgRegister RSI, ArgConstant "40"); 
        (*40 is size of overhead bits plus one addiitonal argument *)
        AsmMov (ArgRegister RDX, ArgMemory (AddrByLabel "end_of_heap"));
        AsmMov(ArgRegister RCX, ArgMemory(AddrByLabel "heap_cursor"));
        AsmSub (ArgRegister RDX, ArgRegister RCX);
        AsmCmp (ArgRegister RDX, ArgRegister RSI);
        AsmJg memory; (* jumps if there is enough memory- val in RDX > Val in RSI*)
        AsmPush (ArgRegister RAX);
        AsmPush (ArgRegister R11);
        AsmPush (ArgRegister R10);
        AsmPush (ArgRegister R9);
        AsmMov (ArgMemory (AddrByLabel "end_of_stack"), ArgRegister RSP);
        AsmMov (ArgRegister RDI, ArgRegister RSI);
        AsmCall " gc";
        AsmPop (ArgRegister R9);
        AsmPop (ArgRegister R10);
        AsmPop (ArgRegister R11);
        AsmPop (ArgRegister RAX);
        AsmLabel memory;
      ]


      @*)[
        AsmAdd (ArgRegister R11, ArgConstant "1");
        AsmCmp (ArgRegister R11, ArgRegister R10);

        (*overhead bytes*)
        AsmJne grow_closure;
        (*GO TIME*) 

        (* this section of the code generation has been modified in the Hoopoe branch 
           to include tail call optimization
           If the closure is full, then

           If the number of parameters in the closure is greater than the number of parameters 
           of this function, we perform a normal call 
           (just as we would if the EAppl were not in a tail position).

           Otherwise, we perform a tail call.
            Instead of pushing the arguments onto the stack, we replace our own arguments with them.
            (If we have more parameters than the target function, we must replace our other arguments with zeros.)

            We tear down our stack 
            (in a fashion similar to how we would at the end of a function declaration), 
            returning it to (almost) the same condition as it was in when the original caller called us.
            
            We jmp to the function rather than calling it.
        *)
      ]
      @
      let not_tail_call = fresh_name "not_tail_call" in 
      let equal_params = fresh_name "equal_params" in 
      [
        AsmMov(ArgRegister R8, ArgConstant (string_of_int num_params));
        AsmCmp(ArgRegister R8, ArgRegister R10);
        AsmJl not_tail_call; (*for now, only tail calling w equal param #*)
        AsmJe equal_params;
        AsmMov(ArgRegister RDX, ArgRegister RAX);
        AsmMov(ArgRegister RAX, ArgConstant "0");
        AsmMov(ArgRegister RDI, ArgRegister RBP);
        AsmMov(ArgRegister RCX, ArgRegister R8);
        AsmAdd(ArgRegister RDI, ArgConstant "16");
        AsmSto;
        AsmMov(ArgRegister RAX, ArgRegister RDX);
      ]
      @
      let () = print_endline("tail calling now ") in 
      [ AsmLabel equal_params;
        AsmIMul(ArgRegister R11, ArgConstant "8");
        AsmMov (ArgRegister RSI, ArgRegister RAX);
        AsmAdd (ArgRegister RSI, ArgConstant "32");
        AsmMov (ArgRegister RDI, ArgRegister RBP);
        AsmAdd (ArgRegister RDI, ArgConstant "16");
        AsmMov (ArgRegister RCX, ArgRegister R9);
        AsmRep;
        AsmMov (ArgRegister R8, arg_addr);
        AsmMov (ArgMemory(AddrByRegister RDI), ArgRegister R8);
        AsmMov (ArgRegister RAX, ArgMemory (AddrByRegisterOffset(RAX, 24)));
        AsmMov (ArgRegister RSP, ArgRegister RBP);
        AsmPop (ArgRegister RBP);
        AsmJmp ("  rax");

      ]
      @

      [ AsmLabel not_tail_call;
        AsmIMul(ArgRegister R11, ArgConstant "8");
        AsmSub (ArgRegister RSP, ArgRegister R11);
        AsmMov (ArgRegister RSI, ArgRegister RAX);
        AsmAdd (ArgRegister RSI, ArgConstant "32");
        AsmMov (ArgRegister RDI, ArgRegister RSP);
        AsmMov (ArgRegister RCX, ArgRegister R9);
        AsmRep;
        AsmMov (ArgRegister R8, arg_addr);
        AsmMov (ArgMemory (AddrByRegister RDI), ArgRegister R8);
        AsmMov (ArgRegister RAX, ArgMemory (AddrByRegisterOffset(RAX, 24)));
        AsmMov (padding, ArgRegister R11);
        AsmCall ("  rax");
        AsmMov (ArgRegister R11, padding);
        AsmSub (ArgRegister RSP, ArgRegister R11);
        AsmJmp end_label;
        AsmLabel grow_closure;


        AsmIMul(ArgRegister R11, ArgConstant "8");
        AsmAdd (ArgRegister R11, ArgConstant "32");
        AsmMov (ArgRegister RSI, ArgRegister RAX);
        AsmMov (ArgRegister RDI, ArgMemory (AddrByLabel "heap_cursor"));
        AsmMov (ArgRegister RCX, ArgRegister R9);
        AsmAdd (ArgRegister RCX, ArgConstant "4"); (*overhead bytes*)
        AsmRep;
        AsmMov (ArgRegister R8, arg_addr);
        AsmMov (ArgMemory (AddrByRegister RDI), ArgRegister R8);
        AsmMov (ArgRegister RAX, ArgMemory (AddrByLabel "heap_cursor"));
        AsmMov (ArgRegister R10, ArgRegister RAX);
        AsmOr  (ArgRegister RAX, ArgConstant "1");
        AsmMov (ArgRegister R9, ArgConstant "1" );
        AsmAdd (ArgMemory (AddrByRegister R10), ArgRegister R9);
        AsmAdd (ArgRegister R10, ArgRegister R11);
        AsmMov (ArgMemory (AddrByLabel "heap_cursor"), ArgRegister R10);
        AsmLabel end_label;
        AsmMov (ArgRegister R10, ArgConstant "0");
        AsmMov (closure_addr, ArgRegister R10);
        AsmMov (arg_addr, ArgRegister R10);
        AsmMov (padding, ArgRegister R10);
        (*write 0 to memory location after allocating temporary variable*)
      ]
  |ESet (tuple, index, value) -> 
    let (tuple_addr, env') = allocate_temp_variable env in 
    compile_expression env' tuple 
    @
    check_rax_tuple()
    @
    [AsmMov (tuple_addr, ArgRegister RAX)]
    @
    let (index_addr, env'') = allocate_temp_variable env' in 
    compile_expression env'' index
    @
    check_rax_int()
    @
    check_rax_valid_index(tuple_addr)
    @
    [AsmMov (index_addr, ArgRegister RAX)]
    @
    let (value_addr, env''') = allocate_temp_variable env'' in 
    compile_expression env''' value 
    @
    [AsmMov (value_addr, ArgRegister RAX);
     (*At this point, all three expressions are stored on the stack*)
     AsmMov (ArgRegister RAX, tuple_addr);
     AsmXor (ArgRegister RAX, ArgConstant "1");
     AsmMov (ArgRegister R11, index_addr);
     AsmShr (ArgRegister R11, ArgConstant "1");
     AsmIMul(ArgRegister R11, ArgConstant "8");
     AsmAdd (ArgRegister R11, ArgConstant "16");
     (*R11 holds the amount to advance by*)
     AsmAdd (ArgRegister RAX, ArgRegister R11);
     (*RAX points to index to set*)
     AsmMov (ArgRegister R10, value_addr );
     AsmMov (ArgMemory(AddrByRegister RAX), ArgRegister R10);
     (*move changed value into RAX at end*)
     AsmMov (ArgRegister RAX, ArgRegister R10);
     AsmMov (ArgRegister R10, ArgConstant "0");
     AsmMov (tuple_addr, ArgRegister R10);
     AsmMov (index_addr, ArgRegister R10);
     AsmMov (value_addr, ArgRegister R10);]
  |ELambda (_,_) -> failwith "ELambda in compilation"

;;

let compile_function_declaration (env: environment) (d: declaration): instruction list =
  match d with
    DFunction (identifier, paremeter_list, expression) ->
    let param_env = (match env with 
          _, integer, dictionary -> (List.length paremeter_list, integer, dictionary)) in 
    let () = print_endline (string_of_environment param_env) in 
    (*env with dictionary entries mapping to paremters in it*)
    let paremeter_env = add_parameters_to_environment 0 paremeter_list param_env in 

    let decl_instructions = compile_expression paremeter_env expression in

    let offset: int = 
      stack_memory_of_instruction_list (decl_instructions) in 
    let offset' = if offset mod 16 = 0 then
        offset + 8
      else
        offset
    in

    let rcx = offset' / 8 in
    (*push rbp onto the stack to save it
       copy rsp into rbp*)

    [AsmLabel ("fun_"^identifier);
     AsmPush (ArgRegister(RBP));

     AsmMov (ArgRegister(RBP), ArgRegister(RSP));
     AsmSub (ArgRegister(RSP), ArgConstant(string_of_int offset'));
     (*push callee saved registers*)
     AsmMov (ArgRegister RAX, ArgConstant "0");
     AsmMov (ArgRegister RDI, ArgRegister RSP);
     AsmMov (ArgRegister RCX, ArgConstant (string_of_int rcx));
     AsmSto;

     AsmPush(ArgRegister(RBX));
     AsmPush(ArgRegister(R12));
     AsmPush(ArgRegister(R13));
     AsmPush(ArgRegister(R14));
     AsmPush(ArgRegister(R15))
    ]
    @
    (*leave return value in rax*)
    decl_instructions @ 
    (*restore calle saved registets*)
    [AsmPop(ArgRegister(R15));
     AsmPop(ArgRegister(R14));
     AsmPop(ArgRegister(R13));
     AsmPop(ArgRegister(R12));
     AsmPop(ArgRegister(RBX))]
    @
    (*restore old top of stack mov rsp, rbp*)
    [AsmMov(ArgRegister(RSP),ArgRegister(RBP));
     (*restore old base pointer pop rbp*)

     AsmPop(ArgRegister(RBP));
     (*execute ret instruction*)
     AsmRet]

;;

let rec mark_eappl_position_rec (expr: expr) : expr =

  match expr with
  | EInt x ->  EInt(x)
  | EBool x -> EBool(x)
  | EUnaryOp (op, e) -> EUnaryOp(op,e)
  | EBinaryOp (op, e, e') -> EBinaryOp(op,e,e')
  | ETuple exprs -> 
    ETuple(exprs)
  | ELet (str, e, tail) -> 
    ELet(str, e, mark_eappl_position_rec tail)
  | EVar x -> EVar(x)
  | EIf (e, tail, tail') -> 
    EIf(e, mark_eappl_position_rec tail, mark_eappl_position_rec tail')
  |EAppl (e,e',_) -> 
    EAppl(e,e', true)
  | ESet (e, e', e'') -> 
    ESet(e,e',e'')
  | ELambda (str, e) -> 
    ELambda(str, e)

;;
let mark_tail_positions (prog: program) : program = 
  match prog with 
    Program (decl_list, e) -> 

    let unpack_decl (decl: declaration) : declaration = 

      match (decl : declaration) with DFunction (str, params, expr) -> 
        let converted = mark_eappl_position_rec expr in 
        let () = print_endline(show_expr converted) in 
        DFunction(str, params, converted)

    in

    let marked_declarations = List.map unpack_decl decl_list in 
    Program(marked_declarations,e)

;;
let compile_program (prog: program) : instruction list =

  (match prog with
   |Program (decl_list, e) -> 
     let ids = List.map Wellformedness.get_identifier decl_list in
     let closure_env = allocate_closure_bindings ids empty_environment in
     let decl_list_instructions=  
       List.map (compile_function_declaration closure_env) decl_list
       |>
       List.concat in

     let expr_instructions = compile_expression closure_env e in
     let offset: int = 
       stack_memory_of_instruction_list (expr_instructions) in 
     let offset' = if offset mod 16 = 0 then
         offset + 8
       else
         offset
     in

     let rcx = offset' / 8 in
     (*push rbp onto the stack to save it
        copy rsp into rbp*)
     decl_list_instructions
     @
     [AsmLabel "bird_main";
      AsmPush (ArgRegister(RBP));

      AsmMov (ArgRegister(RBP), ArgRegister(RSP));
      AsmMov (ArgMemory (AddrByLabel ("heap_cursor" )), ArgRegister RDI);
      AsmMov (ArgMemory (AddrByLabel ("start_of_heap" )), ArgRegister RDI);
      AsmMov (ArgMemory (AddrByLabel ("end_of_heap")), ArgRegister RSI);
      AsmMov (ArgMemory (AddrByLabel ("start_of_stack")), ArgRegister RBP);
      AsmSub (ArgRegister(RSP), ArgConstant(string_of_int offset'));

      AsmMov (ArgRegister RAX, ArgConstant "0");
      AsmMov (ArgRegister RDI, ArgRegister RSP);
      AsmMov (ArgRegister RCX, ArgConstant (string_of_int rcx));
      AsmSto;
      (* loop to set all memory locations between RBP and RSP to 0*)
      (*push callee saved registers*)
      AsmPush(ArgRegister(RBX));
      AsmPush(ArgRegister(R12));
      AsmPush(ArgRegister(R13));
      AsmPush(ArgRegister(R14));
      AsmPush(ArgRegister(R15))
     ]
     @
     (*leave return value in rax*)
     expr_instructions @ 
     (*restore calle saved registets*)
     [AsmPop(ArgRegister(R15));
      AsmPop(ArgRegister(R14));
      AsmPop(ArgRegister(R13));
      AsmPop(ArgRegister(R12));
      AsmPop(ArgRegister(RBX))]
     @
     (*restore old top of stack mov rsp, rbp*)
     [AsmMov(ArgRegister(RSP),ArgRegister(RBP));
      (*restore old base pointer pop rbp*)

      AsmPop(ArgRegister(RBP));
      (*execute ret instruction*)
      AsmRet])
;;
(* this function returns a list of pairs where the first element is 
   the id to use for the global closure and the second element
   is the number of paremeters needed for the closure*)
let rec create_globals (decl_list: declaration list) : (string * int) list= 
  match decl_list with 
  |[] -> [] 
  | x :: y -> 
    match x with 
      DFunction (id, parameters, _) -> 
      [(id, List.length parameters)]
      @
      create_globals y
;;

let code_of_closure ((str, integer): (string * int)) : string = 
  "align 8\n"^
  "closure_of_"^str^":\n"^
  "  dq 0x8000000000000000, "^" 0x0000000000000000, "^string_of_int integer ^ ", fun_"^str^"\n"
;;
let compile_to_assembly_code (prog: program) : string =


  let errors = Wellformedness.check_well_formed prog in
  ignore errors;
  let tail_marked = mark_tail_positions prog in 
  let converted = ClosureConversion.closure_convert_program tail_marked in 
  match converted with Program (decl_list, _) -> 
    let pairs : string list = List.map code_of_closure (create_globals decl_list) in
    let globals = String.concat "" pairs in
    let instructions = compile_program tail_marked in
    let instruction_code = code_of_instruction_list instructions in

    "section .data\n" ^
    "align 8\n" ^
    "heap_cursor:\n" ^
    "  dq 0\n"^
    "start_of_heap:\n"^
    "  dq 0\n"^
    "start_of_stack:\n"^
    "  dq 0\n"^
    "end_of_heap:\n"^
    "  dq 0\n"^
    "end_of_stack:\n"^
    "  dq 0\n"^
    globals^
    "section .text\n" ^
    "extern printValue\n"^
    "extern stopWithError\n"^
    "extern gc\n"^
    "global bird_main\n" ^
    "global start_of_heap\n"^
    "global end_of_heap\n"^
    "global start_of_stack\n"^
    "global end_of_stack\n"^
    "global heap_cursor\n"^


    instruction_code ^
    "\n"
;;

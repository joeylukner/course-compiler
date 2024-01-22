open Batteries;;
open HatchLanguage;;

open Printf;;

open Asts;;

exception IllFormed of string list;;

(* This function produces a list of compile-time errors found in a program. *)
let get_identifier (decl: declaration) : string = 
  (match decl with 
     DFunction (id, _, _) -> id);;

let get_identifiers_of_paremeter_list (decl: declaration): string list =
  match decl with 
    DFunction (_, paremeters, _) -> 
    paremeters
;;
let rec count (str: string) (lst: string list) : int =
  match lst with
  |[] -> 0
  | x::y ->
    if x = str then
      1 + count str y
    else
      0 + count str y
;; 
let count_occurrences_in_list (lst: string list)(str: string ) : (string * int) =
  (str, count str lst)
;;

let second_element_plural (pair: (string*int) ) : bool =
  let (_, integer) = pair in
  if integer > 1 then true else false
;;

let rec duplicate_paremeters (decl_list: declaration list) : string list = 

  match decl_list with
  |[]-> []
  |x::y -> 
    duplicate_paremeters y 
    @

    let declaration_id = get_identifier x in
    let parem_ids = get_identifiers_of_paremeter_list x in
    let occurrences_pairs = List.map (count_occurrences_in_list parem_ids) parem_ids in
    let duplicate_pairs = List.filter second_element_plural occurrences_pairs in
    let duplicate_strings: string list = List.map fst duplicate_pairs in
    let offender_functions: string list = List.unique duplicate_strings in

    let rec generate_errors (decl_id : string) (parem_duplicates: string list) : string list = 
      match parem_duplicates with
      |[] -> []
      |x :: y ->
        ["Function "^decl_id^ " declares a duplicate parameter " ^x^ ".\n"]
        @
        generate_errors decl_id y
    in
    generate_errors declaration_id offender_functions;;
let rec string_of_errors (errors: string list) : string = 
  match errors with 
  |[]-> ""
  |x::y -> x ^ string_of_errors y
;;
let rec contains (str: string)(lst: string list): bool = 
  match lst with 
    [] -> false 
  | x :: y -> 
    if str = x then
      true
    else
      false || contains str y

;;
let rec unbound_vars (vars_in_scope: string list) ( expr: expr) : string list =
  match expr with
  | ETuple tuple_args -> 
    let lst = List.map (unbound_vars vars_in_scope ) tuple_args in
    List.concat lst
  | EInt _ -> []
  | EBool _ -> []
  | EUnaryOp (_, e) -> unbound_vars vars_in_scope  e 
  | EBinaryOp (_, e, e2) -> 
    unbound_vars vars_in_scope  e 
    @
    unbound_vars vars_in_scope  e2 
  | ELet (var, e, e2) ->
    let updated_vars_in_scope : string list = [var]@vars_in_scope in 
    unbound_vars vars_in_scope  e 
    @
    unbound_vars updated_vars_in_scope  e2  
  | EVar varname -> 
    if contains (varname) (vars_in_scope) then
      []
    else
      ["Unbound variable " ^varname^".\n"] 
  | EIf (e1, e2, e3) ->
    unbound_vars vars_in_scope  e1 
    @
    unbound_vars vars_in_scope  e2 
    @
    unbound_vars vars_in_scope  e3 
 
  |EAppl(e1,e2, tail_bool) -> 
    ignore tail_bool;
    unbound_vars vars_in_scope e1
    @
    unbound_vars vars_in_scope e2

  |ESet (e1, e2, e3) ->
    unbound_vars vars_in_scope e1
    @
    unbound_vars vars_in_scope e2 
    @
    unbound_vars vars_in_scope e3

   |ELambda (_) -> failwith "lambda"
;;

let duplicate_definitions  (decl_list : declaration list)  : string list = 


  let identifiers :string list= List.map (get_identifier) decl_list in
  let occurrences_pairs : (string*int) list = List.map (count_occurrences_in_list identifiers) identifiers in
  let duplicate_pairs: (string * int) list = List.filter second_element_plural occurrences_pairs in
  let duplicate_strings: string list = List.map fst duplicate_pairs in
  let offender_functions: string list = List.unique duplicate_strings in

  let rec generate_errors (fns: string list) : string list =
    match (fns : string list) with
      [] -> [] 
    | x :: y -> 
      ["Duplicate definition of function "^x^".\n"]
      @
      generate_errors y

  in
  generate_errors offender_functions
  (* decl_list
     |> List.map get_identifier 
     |> List.map count_occurrences_in_list
     |> List.filter second_element_plural
     |> List.map fst
     |> List.unique *)
;;
(*check Dove branch for implementation of error checking funciton that determines if the correct
   number of function arguments are provided*)

(*undefined functions become undefined variables in falcon, a change from dove due to the nature
   of functions being bound to closures*)

let check_program_for_errors (p : program) : string list =
  match p with 
    Program (decl_list, expr) -> 
    let functions : string list = List.map get_identifier decl_list in
    (*check unbound vars for dove declarations*)  
    let rec call_unbound_on_decls (declarations: declaration list): string list = 
      match declarations with
      |[] -> []
      |x::y ->
        match x with
          DFunction (_, parems, expr) ->
          unbound_vars (functions @ parems) expr
          @
          call_unbound_on_decls
            y
    in



    call_unbound_on_decls decl_list
    @
    unbound_vars functions expr
    @
    
    duplicate_definitions decl_list
    @

    duplicate_paremeters decl_list


;;

(* This function will check a program for compile-time errors.  If any errors
   are found, an IllFormed exception is thrown.  Otherwise, unit is returned. *)
let check_well_formed (p : program) : unit =
  let errors = check_program_for_errors p in
  if List.is_empty errors then () else raise (IllFormed errors);
;;

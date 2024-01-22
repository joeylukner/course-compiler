(* This file contains the data structure and functions we will use to manage
   the environment as we compile our code. *)

open Batteries;;
open Printf;;
open Assembly;;

(* This data type represents the kind of environment we will use in our
   compiler.  This is a triple between the number of parameters of a specific
   function, the next free stack address and a
   dictionary mapping variable names to their offsets. *)
type environment = int * int * argument Map.String.t;;

(* This defines an exception which should be thrown if we try to read from an
   environment something that doesn't exist. *)
exception UnboundVariable of string * environment;;

(* The value representing an empty environment.  The map is empty and zero
   temporary locations are in use. *)
let empty_environment : environment = (0, -8, Map.String.empty);;

(* A function to allocate a space for a named variable from the environment.
   This function takes a variable name and an environment.  It returns the
   new environment after that allocation.
*)

let retrieve_num_parameters (env: environment) : int = 
   match env with num_params, _, _ -> num_params
;;
let allocate_named_variable (name : string) (env : environment) : environment =
  let (num_params, address, dictionary) = env in
  let whatever = ArgMemory(AddrByRegisterOffset (RBP , address) ) in
  let dictionary' = Map.String.add name whatever dictionary  in
  let my_env = (num_params, address -8, dictionary') in
  my_env
;;



(* A function to find a previously-allocated space in an environment based upon
   the name of the variable for which that space was allocated.  If the variable
   was not previously allocated, then UnboundVariable is raised.
*)
let find_named_variable (name : string) (env : environment) : argument =
  let (_,_, dictionary) = env in
  try Map.String.find name dictionary with
  |Not_found -> raise (UnboundVariable (name, env))
;;

(* A function to allocate space for a temporary variable from the environment.
   This function does not require a variable name because the temporary space is
   being allocated for the compiler and will not be associated with a name.
   Given an environment, it returns a new, temporary address as well as a new
   environment after that allocation.
*)
let allocate_temp_variable (env : environment) : argument * environment =
  let (num_params, address, dictionary) = env in 
  let whatever = ArgMemory(AddrByRegisterOffset (RBP, address)) in
  let my_env = (num_params, address -8, dictionary) in
  (whatever, my_env)
;;

(* A function to print an environment.  This is provided to you for debugging
   purposes. *)
let string_of_environment (env : environment) : string =
  let (num_params, next_free, dict) = env in
  let mappings = List.of_enum (Map.String.enum dict) in
  let rec string_of_mappings ms =
    match ms with
    | [] -> ""
    | (name, address)::ms' ->
      (Printf.sprintf
         "%s stored at %s\n"
         name
         (code_of_argument address)
      ) ^ string_of_mappings ms'
  in
  (Printf.sprintf "Number of params: %d\n" num_params) ^
  (Printf.sprintf "Next free offset: %d\n" next_free) ^
  string_of_mappings mappings
;;

(*offset should be 0 on first call to make address + 16*)
let rec add_parameters_to_environment (offset: int )(lst : string list) (env : environment):
  environment =
  match lst with 
  |[] -> env
  |x ::y -> 

    let (num_params, address, dictionary) = env in 
    let whatever = ArgMemory(AddrByRegisterOffset (RBP , 
    (*rbp plus 16 for first paremeter*)
    (16 + (offset))) ) in
    let dictionary' = Map.String.add x whatever dictionary in
    (* let () = printf "paremeter added to dictionary: %s located at offset+16 below rbp: %d\n" x offset in *)
   let my_env = (num_params, address, dictionary') in
   add_parameters_to_environment (offset + 8) y my_env
;; 

let rec allocate_closure_bindings (names: string list) (env: environment): environment = 
   (* let () = printf "adding prod to dictionary" in *)
   match names with
   |[] -> env
   | identifier :: rest ->
   let (num_params, address, dictionary) = env in
   let whatever = ArgLabelOffset("closure_of_" ^ identifier, "1") in
   let dictionary' = Map.String.add identifier whatever dictionary in
   let my_env = (num_params, address, dictionary') in
   allocate_closure_bindings rest my_env
   
;;

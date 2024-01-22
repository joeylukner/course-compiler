(** This file contains the closure conversion algorithm as part of the
    Finch branch, an extension of Falcon*)

open Batteries;;
open HatchLanguage;;

open Assembly;;
open Asts;;
open Environment;;
open Printf;;
open Freshening;;

let rec free_vars (expr: expr) : Set.String.t = 
  match expr with
  (*base cases*)
  | EInt _ -> Set.String.empty
  | EBool _ -> Set.String.empty
  | EVar variable -> Set.String.add variable Set.String.empty

  (*recursive cases*)
  | EUnaryOp (_, expression) -> free_vars expression
  | EBinaryOp (_, expression, expression') -> 
    Set.String.union (free_vars expression) (free_vars expression')
  | ETuple expressionlst ->
    List.map free_vars expressionlst |> 
    List.fold_left Set.String.union Set.String.empty
  | EIf (expression, expression', expression'') -> 
    Set.String.union 
      (Set.String.union(free_vars expression) (free_vars expression'))
      (free_vars expression'')
  | EAppl (expression, expression', tail_bool) ->
    ignore tail_bool; 
    Set.String.union (free_vars expression) (free_vars expression')
  | ELambda (_, expression) -> 
    free_vars expression
  | ESet (expression, expression', expression'') ->
    Set.String.union 
      (Set.String.union(free_vars expression) (free_vars expression'))
      (free_vars expression'')
      
  (*case to remove variables that are in scope*)
  | ELet (name, expression, expression') -> 
    let union = Set.String.union (free_vars expression) (free_vars expression') in
    (match (Set.String.find_opt name union )with
     |Some(found_element) -> Set.String.remove found_element union
     |None -> union)
  
;;

let rec closure_convert_expression (expr: expr) : expr * declaration list = 
  match expr with
  (*base cases*)
  | EInt _ -> (expr, [])
  | EBool _ -> (expr, [])
  | EVar _ -> (expr, [])

  (*recursive cases*)
  | EUnaryOp (operator, expression) -> 
    let (recursive_expression, lst) = closure_convert_expression expression in 
    (EUnaryOp (operator, recursive_expression), lst)
  | EBinaryOp (operator, expression, expression') -> 
    let (recursive_expression, lst) = closure_convert_expression expression in 
    let (recursive_expression', lst') = closure_convert_expression expression' in 
    (EBinaryOp(operator, recursive_expression, recursive_expression'), lst @ lst')
  | ETuple expressionlst -> 
    let (expressions, declaration_lists) = 
    List.map closure_convert_expression expressionlst |>
    List.split in

    let concatenated = List.concat declaration_lists in 
    (ETuple(expressions), concatenated)
  | ELet (identifier, expression, expression') -> 
    let (recursive_expression, lst) = closure_convert_expression expression in 
    let (recursive_expression', lst') = closure_convert_expression expression' in 
    (ELet(identifier, recursive_expression, recursive_expression'), lst @ lst')
  | EIf (expression, expression', expression'') -> 
    let (recursive_expression, lst) = closure_convert_expression expression in 
    let (recursive_expression', lst') = closure_convert_expression expression' in 
    let (recursive_expression'', lst'') = closure_convert_expression expression'' in 
    (EIf(recursive_expression, recursive_expression', recursive_expression''), lst @ lst' @ lst'')
  | EAppl (expression, expression', tail_bool) -> 
   ignore tail_bool;
    let (recursive_expression, lst) = closure_convert_expression expression in 
    let (recursive_expression', lst') = closure_convert_expression expression' in 
    (EAppl (recursive_expression, recursive_expression' ,false), lst @ lst')
  | ESet (expression,expression',expression'') -> 
    let (recursive_expression, lst) = closure_convert_expression expression in 
    let (recursive_expression', lst') = closure_convert_expression expression' in 
    let (recursive_expression'', lst'') = closure_convert_expression expression'' in 
    (ESet(recursive_expression, recursive_expression', recursive_expression''), lst @ lst' @ lst'') 

  (*case for ELambda- replace each ELambda with a reference to a new declaration *)
  | ELambda (identifier, expression) -> 
    let parameters = Set.String.to_list (free_vars expression) in 
    let (subbody, declarations) = closure_convert_expression expression in
    let fresh_identifier = fresh_name "$lambda$" in 
    let converted_decl = DFunction(fresh_identifier, parameters @ [identifier], subbody ) in 


    let rec create_reference (variables: string list): expr = 
      
      match variables with 
      [] -> EVar(fresh_identifier)
      | x :: y -> 
        EAppl(create_reference y , EVar(x), false)
      
    in
    
    ( create_reference parameters, declarations @ [converted_decl])

;;

(*this function takes in a declaration and returns an updated declaration
   that has been closure converted along with a declaration list that 
   contains all of the additional declarations that were created in the process
   of the conversion*)
let closure_convert_declaration (decl: declaration) : declaration * declaration list =
  match decl with DFunction (id, parameters, body) -> 
    let (new_body, declarations) = closure_convert_expression body in 
    let replacement_decl: declaration = DFunction(id, parameters, new_body) in
    (replacement_decl, declarations)

;;


(*this function takes in a program, closure converts each of that programs
   declarations, and closure converts the programs main body expression. it 
   returns a new program that contains a list of declarations that has
   all of the new converted declarations, and the converted body expression*)
let closure_convert_program (prog: program) : program = 
  match prog with
  | Program (declaration_list, expr) -> 
    let (replacements_declarations, extra_declarations) =
      List.map closure_convert_declaration declaration_list
      |> List.split 
    in
    let combined_extra_declarations = List.concat extra_declarations in 

    let (new_body, more_declarations) = closure_convert_expression expr in 
    Program(combined_extra_declarations @ replacements_declarations
            @ more_declarations, new_body) 

;;

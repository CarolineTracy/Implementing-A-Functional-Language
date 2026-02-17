(* Ocaml- interpreter.
 *
 * N. Danner
 *)

module Ast = Challenge_ast

(* UndefinedFunction f is raised when f is called but not defined.
 *)
exception UndefinedFunction of Ast.Id.t

(* UnboundVariable x is raised when x is used but not declared.
 *)
exception UnboundVariable of Ast.Id.t

(* TypeError s is raised when an operator or function is applied to operands
 * of the incorrect type.  s is any (hopefuly useful) message.
 *)
exception TypeError of string

(* Values.
 *)
module Value = struct
  type t = 
    | V_Int of int
    | V_Bool of bool
    | V_Fun of Ast.Id.t list * Ast.Expr.t * (Ast.Id.t*t) list
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
    | V_Fun (_, _, _) -> "function"
end

(* Environments.  An environment is a finite map from identifiers to values.
 * We will interchangeably treat environments as functions or sets or lists
 * of pairs in documentation.  We will use ρ as a metavariable over
 * environments.
 *)
module Env = struct

  type t = (Ast.Id.t * Value.t) list
  [@@deriving show]

  (*  empty = ρ, where dom ρ = ∅.
   *)
  let empty : t = []

  (* from_list xsvs = xsvs.
   *)
  let from_list : t -> t = fun rho -> rho

  (* join ρ₀ ρ₁ = ρ, where
   *   dom ρ = dom ρ₀ ∪ dom ρ₁
   *   ρ(x) = ρ₀(x), x ∈ dom ρ₀ - dom ρ₁
   *          ρ₁(x), x ∈ dom ρ₁.
   *)
  let join (rho0 : t) (rho1 : t) : t =
    List.append (
      List.filter(
        fun (x, _) -> not @@ List.mem_assoc x rho1
      ) rho0
    ) rho1

  (*  If x is in ρ, lookup ρ x b = ρ(x). 
   *  If b = true, then x is a variable. If b = false, then x is a function name.
   *  If x is not in ρ and b is true (which means x is a variable), lookup ρ x raises an UnboundVariable error.
   *  If x is not in ρ and b is false (which means x is a function name), lookup ρ x raises an UndefinedFunction error.
   *)
  let lookup (rho : t) (x : Ast.Id.t) (b : bool) : Value.t = 
    match (List.assoc_opt x rho) with
    | Some Value.V_Bool b -> Value.V_Bool b
    | Some Value.V_Int n -> Value.V_Int n
    | Some Value.V_Fun (param_l, e, bound_so_far) -> Value.V_Fun (param_l, e, bound_so_far)
    | None -> (match b with
              | true -> raise(UnboundVariable x)
              | false -> raise(UndefinedFunction x)
              )

  (*  update ρ x v = ρ{x → v}.
   *)
  let update (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    (x, v) :: List.remove_assoc x rho

end

(*  func_body is the body of a given function, param_l is a list consisting of that function's parameters, and rho is the current 
 *  environment when that function was declared. 
 *  If all variables in e are either in rho or in param_l, then no_unbound_var rho param_l e = true. 
 *  If there is at least one variable in e that is not in rho and not in param_l, then no_unbound_var rho param_l e raises an UnboundVariable error.
 *)
let rec no_unbound_var (rho : Env.t) (param_l : Ast.Id.t list) (func_body : Ast.Expr.t) : bool =
  match func_body with
  | Ast.Expr.Var x -> 
    (match (List.mem x param_l) with
    | true -> true
    | false -> (let _ = (Env.lookup rho x true) in
               true
              )
    )
  | Ast.Expr.Num _ -> true
  | Ast.Expr.Bool _ -> true
  | Ast.Expr.Unop (_, e) -> (no_unbound_var rho param_l e)
  | Ast.Expr.Binop (_, e, e') -> (no_unbound_var rho param_l e) && (no_unbound_var rho param_l e')
  | Ast.Expr.If (e, e', e'') -> (no_unbound_var rho param_l e) && (no_unbound_var rho param_l e') && (no_unbound_var rho param_l e'')
  | Ast.Expr.Let (_, e', e) -> (no_unbound_var rho param_l e') && (no_unbound_var rho param_l e)
  | Ast.Expr.Call (_, call_l) -> 
    let fold_func = (fun acc e0 -> acc && (no_unbound_var rho param_l e0)) in
    List.fold_left fold_func true call_l
  | Ast.Expr.Fun (anon_param_l, e) ->
    let all_params_l = param_l @ anon_param_l in
    no_unbound_var rho all_params_l e

(*  binop op v v' = v'', where v'' is the result of applying the semantic
 *  denotation of `op` to `v` and `v''`.
 *)
let binop (op : Ast.Expr.binop) (v : Value.t) (v' : Value.t) : Value.t =
  match (op, v, v') with
  | (Ast.Expr.Plus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n + n')
  | (Ast.Expr.Minus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n - n')
  | (Ast.Expr.Times, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n * n')
  | (Ast.Expr.Div, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n / n')
  | (Ast.Expr.Mod, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n mod n')
  | (Ast.Expr.And, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b && b')
  | (Ast.Expr.Or, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b || b')
  | (Ast.Expr.Eq, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n = n')
  | (Ast.Expr.Eq, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b = b')
  | (Ast.Expr.Ne, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n != n')
  | (Ast.Expr.Ne, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b != b')
  | (Ast.Expr.Lt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n < n')
  | (Ast.Expr.Lt, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b < b')
  | (Ast.Expr.Le, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n <= n')
  | (Ast.Expr.Le, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b <= b')
  | (Ast.Expr.Gt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n > n')
  | (Ast.Expr.Gt, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b > b')
  | (Ast.Expr.Ge, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n >= n')
  | (Ast.Expr.Ge, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b >= b')
  | _ -> raise(TypeError "Binary operation (binop) applied to operands of the incorrect type.")

(*  unop op v = v', where v' is the result of applying the semantic
 *  denotation of `op` to `v`.
 *)
let unop (op : Ast.Expr.unop) (v : Value.t) : Value.t =
  match (op, v) with
  | (Ast.Expr.Neg, Value.V_Int n) -> Value.V_Int (-n)
  | (Ast.Expr.Not, Value.V_Bool b) -> Value.V_Bool (not b)
  | _ -> raise(TypeError "Unary operation (unop) applied to operand of the incorrect type.")

(*  eval ρ e = v, where ρ ├ e ↓ v according to our evaluation rules. 
 *)
let rec eval (rho : Env.t) (e : Ast.Expr.t) : Value.t =
  match e with
  | Ast.Expr.Var x -> Env.lookup rho x true
  | Ast.Expr.Num n -> Value.V_Int n
  | Ast.Expr.Bool b -> Value.V_Bool b
  | Ast.Expr.Unop (op, e) ->
    let v = eval rho e in
    unop op v
  | Ast.Expr.Binop (op, e, e') ->
    let v = eval rho e in
    let v' = eval rho e' in
    binop op v v'
  | Ast.Expr.If (e, e', e'') -> 
    let v = eval rho e in
    (match v with
      | Value.V_Bool true -> eval rho e'
      | Value.V_Bool false -> eval rho e''
      | _ -> raise(TypeError "Expected a bool. Did not receive a bool.")
    )
  | Ast.Expr.Let (x, e', e) ->
    let v' = eval rho e' in
    eval (Env.update rho x v') e
  | Ast.Expr.Call (f', call_l) ->
    (match f' with
    | Ast.Expr.Var f -> 
      let (param_l, func_e, bound_so_far) = Env.lookup rho f false in
      (match (List.length param_l) with
      | 0 -> 
        (match (List.length call_l) with
        | 0 ->
          let (params, _) = List.split bound_so_far in
          let _ = Env.update rho f (params, func_e, []) in
          let func_rho = Env.from_list bound_so_far in
          let new_rho = Env.join rho func_rho in
          eval new_rho func_e
        | _ -> raise(TypeError "Function called with too many arguments.")
        )
      | _ -> 
        (match (List.length call_l) with
        | 0 -> Value.V_Fun (param_l, func_e, bound_so_far)
        | _ -> 
          let curr_param = List.hd param_l in
          let param_l_minus_1 = List.tl param_l in
          let curr_call = List.hd call_l in
          let call_l_minus_1 = List.tl call_l in
          let eval_curr_call = eval rho curr_call in
          let _ = Env.update rho f (param_l_minus_1, func_e, ((curr_param, eval_curr_call) :: bound_so_far)) in
          eval rho (Ast.Expr.Call (f, call_l_minus_1))
        )
      )
    | Ast.Expr y -> (match (eval rho y) with
                    | Value.V_Fun (param_l', func_e', bound_so_far') ->
                      (match (List.length param_l') with
                      | 0 -> 
                        (match (List.length call_l) with
                        | 0 ->
                          let func_rho = Env.from_list bound_so_far' in
                          let new_rho = Env.join rho func_rho in
                          eval new_rho func_e'
                        | _ -> raise(TypeError "Function called with too many arguments.")
                        )
                      | _ -> 
                        (match (List.length call_l) with
                        | 0 -> Value.V_Fun (param_l', func_e', bound_so_far)
                        | _ -> 
                          let curr_param = List.hd param_l' in
                          let param_l_minus_1 = List.tl param_l' in
                          let curr_call = List.hd call_l in
                          let call_l_minus_1 = List.tl call_l in
                          let eval_curr_call = eval rho curr_call in
                          let _ = Env.update rho curr_param eval_curr_call in
                          eval rho (Ast.Expr.Call (Ast.Expr.Fun (param_l_minus_1, func_e'), call_l_minus_1))
                        )
                      )
                    | _ -> raise(TypeError "Function call expression is not well typed.")
                    )
    )

    

      When a function is fully evaluated, its bound_so_far (that records the values bound to the parameters so far) goes back to empty, and its parameters equal the vars in bound_so_far
      I SHOULD EVALUATE IT LIKE THIS: f 3 4 5 6 IS ((((f 3) 4) 5) 6). SO LIKE EVALUATE IT ONE BY ONE. 

  | Ast.Expr.Fun (param_l, e') ->
    PROBABLY WILL BE SIMILAR TO CALL EXPRESSIONS?
    NOTE THAT E IS BODY
    For anonymous functions: since they can be defined in the expression part of the script, you should make sure that variables that are not parameters are already defined BEFORE the anonymous function is defined. MAYBE DO THIS USING NO_UNBOUND_VAR FUNCTION. If thats not the case, then automatically raise unboundvariable error. Maybe you can do this by doing lookup for all the varialbes in the body of the anonymous function? or maybe there's an easier way (figure that out)
    When a function is fully evaluated, its bound_so_far (that records the values bound to the parameters so far) goes back to empty, and its parameters equal the vars in bound_so_far
  
  
  DO STEPS IN THE DOC
  In the challenge interpreter, you can keep function definitions as parts of the environment, as opposed to doing fundef_l like I did in the core problem (that’s one way to do the challenge problem)

(*  eval func_env e = v, where _ ├ e ↓ v. fundef_l is a list of all the 
 *  function definitions in the script, while e is the expression in the script.
 *
 *  Because later declarations shadow earlier ones, this is the `eval`
 *  function that is visible to clients.
 *)
let eval (fundef_l : Ast.Script.fundef list) (e : Ast.Expr.t) : Value.t =
  let map_func = (fun fundef0 -> let (name0, param_l0, e0) = fundef in (name0, (param_l0, e0, Env.empty))) in
  let fundef_l_mapped = List.map map_func fundef_l in
  let func_env = Env.from_list fundef_l_mapped in
  eval func_env e

(* exec p = v, where `v` is the result of executing `p`.
 *)
let exec (p : Ast.Script.t) : Value.t =
  let Pgm (fundef_l, e) = p in
  let fold_func = (fun acc fundef0 -> let (_, param_l, e0) = fundef0 in acc && (no_unbound_var Env.empty param_l e0)) in
  let _ = List.fold_left fold_func true fundef_l in
  eval fundef_l e
    


  ADD ALL TESTS IN DOC
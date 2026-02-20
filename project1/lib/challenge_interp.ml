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

  (*  only_funcs ρ = ρ₀, where ρ₀ is an environment that contains 
   *  all the elements of ρ whose value is type Value.V_Fun.
   *)
  let only_funcs (rho : t) : t =
    List.filter (fun (_, value) -> match value with | Value.V_Bool _ -> false | Value.V_Int _ -> false | Value.V_Fun _ -> true) rho

  (*  If x is in ρ, lookup ρ x = ρ(x). If x is not in ρ, lookup ρ x raises an UnboundVariable error.
   *)
  let lookup (rho : t) (x : Ast.Id.t) : Value.t = 
    match (List.assoc_opt x rho) with
    | Some Value.V_Bool b -> Value.V_Bool b
    | Some Value.V_Int n -> Value.V_Int n
    | Some Value.V_Fun (param_l, e, bound_so_far) -> Value.V_Fun (param_l, e, bound_so_far)
    | None -> raise(UnboundVariable x)

  (*  update ρ x v = ρ{x → v}.
   *)
  let update (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    (x, v) :: List.remove_assoc x rho

end

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
  | Ast.Expr.Var x -> Env.lookup rho x
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
      (match (Env.lookup rho f) with
      | Value.V_Bool _ -> raise(TypeError "Function definition stored incorrectly")
      | Value.V_Int _ -> raise(TypeError "Function definition stored incorrectly")
      | Value.V_Fun (param_l, func_e, bound_so_far) ->
        (match (List.length param_l) with
        | 0 -> 
          (match (List.length call_l) with
          | 0 ->
            let only_func_defs = Env.only_funcs rho in
            let func_rho = Env.from_list bound_so_far in
            let new_rho = Env.join only_func_defs func_rho in
            eval new_rho func_e
          | _ -> 
            let only_func_defs = Env.only_funcs rho in
            let func_rho = Env.from_list bound_so_far in
            let new_rho = Env.join only_func_defs func_rho in 
            let call_expr = Ast.Expr.Call (func_e, call_l) in
            eval new_rho call_expr
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
            let func_in_env = Value.V_Fun (param_l_minus_1, func_e, ((curr_param, eval_curr_call) :: bound_so_far)) in
            let new_rho = Env.update rho f func_in_env in
            eval new_rho (Ast.Expr.Call (f', call_l_minus_1))
          )
        )
      )
    | _ -> (match (eval rho f') with
            | Value.V_Fun (param_l', func_e', bound_so_far') ->
              (match (List.length param_l') with
              | 0 -> 
                (match (List.length call_l) with
                | 0 ->
                  let only_func_defs = Env.only_funcs rho in
                  let func_rho = Env.from_list bound_so_far' in
                  let new_rho = Env.join only_func_defs func_rho in
                  eval new_rho func_e'
                | _ -> 
                  let only_func_defs = Env.only_funcs rho in
                  let func_rho = Env.from_list bound_so_far' in
                  let new_rho = Env.join only_func_defs func_rho in 
                  let call_expr = Ast.Expr.Call (func_e', call_l) in
                  eval new_rho call_expr
                )
              | _ -> 
                (match (List.length call_l) with
                | 0 -> Value.V_Fun (param_l', func_e', bound_so_far')
                | _ -> 
                  let curr_param = List.hd param_l' in
                  let param_l_minus_1 = List.tl param_l' in
                  let curr_call = List.hd call_l in
                  let call_l_minus_1 = List.tl call_l in
                  let eval_curr_call = eval rho curr_call in
                  let new_rho = Env.update rho curr_param eval_curr_call in
                  eval new_rho (Ast.Expr.Call (Ast.Expr.Fun (param_l_minus_1, func_e'), call_l_minus_1))
                )
              )
            | _ -> raise(TypeError "Function call expression is not well typed.")
            )
    )
  | Ast.Expr.Fun (param_l, e') -> Value.V_Fun (param_l, e', rho)

(*  eval start_env e = v, where _ ├ e ↓ v. start_env is an environment containing all the 
 *  function definitions in the script, while e is the expression in the script.
 *
 *  Because later declarations shadow earlier ones, this is the `eval`
 *  function that is visible to clients.
 *)
let eval (start_env : Env.t) (e : Ast.Expr.t) : Value.t =
  eval start_env e

(* exec p = v, where `v` is the result of executing `p`.
 *)
let exec (p : Ast.Script.t) : Value.t =
  let Pgm (fundef_l, e) = p in
  let map_func = (fun fundef0 -> let (name0, param_l0, e0) = fundef0 in let vfun_in_env = Value.V_Fun (param_l0, e0, Env.empty) in (name0, vfun_in_env)) in
  let fundef_l_mapped = List.map map_func fundef_l in
  let start_env = Env.from_list fundef_l_mapped in
  eval start_env e
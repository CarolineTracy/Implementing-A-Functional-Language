(* Ocaml- interpreter.
 *
 * N. Danner
 *)

module Ast = Core_ast

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
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
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

end

<<<<<<< HEAD

(* exec p = v, where `v` is the result of executing `p`.
 *)
let exec (_ : Ast.Script.t) : Value.t =
  failwith "Unimplemented:  Core.Interp.exec"

=======
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
  | (Ast.Expr.Le, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n <= n')
  | (Ast.Expr.Gt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n > n')
  | (Ast.Expr.Ge, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n >= n')
  | _ -> raise(TypeError "Binary operation (binop) applied to operands of the incorrect type.")

(*  unop op v = v', where v' is the result of applying the semantic
 *  denotation of `op` to `v`.
 *)
let unop (op : Ast.Expr.unop) (v : Value.t) : Value.t =
  match (op, v) with
  | (Ast.Expr.Neg, Value.V_Int n) -> Value.V_Int (-n)
  | (Ast.Expr.Not, Value.V_Bool b) -> Value.V_Bool (not b)
  | _ -> raise(TypeError "Unary operation (unop) applied to operand of the incorrect type.")

(*  eval fundef_l ρ e = v, where ρ ├ e ↓ v according to our evaluation rules. fundef_l is a list of 
    all the function definitions in the script, while e is the expression in the script.
 *)
let rec eval (fundef_l : Ast.Script.fundef list) (rho : Env.t) (e : Ast.Expr.t) : Value.t =
  match e with
  | Ast.Expr.Var x -> Env.lookup rho x
  | Ast.Expr.Num n -> Value.V_Int n
  | Ast.Expr.Bool b -> Value.V_Bool b
  | Ast.Expr.Unop (op, e) ->
    let v = eval fundef_l rho e in
    unop op v
  | Ast.Expr.Binop (op, e, e') ->
    let v = eval fundef_l rho e in
    let v' = eval fundef_l rho e' in
    binop op v v'
  | Ast.Expr.If (e, e', e'') -> 
    let v = eval fundef_l rho e in
    (match v with
      | Value.V_Bool true -> eval fundef_l rho e'
      | Value.V_Bool false -> eval fundef_l rho e''
      | _ -> raise(TypeError "Expected a bool. Did not receive a bool.")
    )
  | Ast.Expr.Let (x, e', e) ->
    let v' = eval fundef_l rho e' in
    eval fundef_l (Env.update rho x v') e
  | Ast.Expr.Call (f, call_l) ->
    let find_f_func = (fun f_def -> let (f', _, _) = f_def in (f' = f)) in
    (match (List.find_opt find_f_func fundef_l) with
    | Some f_def -> 
      let (_, param_l, e') = f_def in
      (match ((List.length param_l) = (List.length call_l)) with
      | true -> 
        let fold_func = (fun curr_env param' call' -> let v = eval fundef_l curr_env call' in Env.update curr_env param' v) in
        eval fundef_l (List.fold_left2 fold_func rho param_l call_l) e'
      | false -> raise(TypeError "Function called with the wrong number of arguments.")
      )
    | None -> raise(UndefinedFunction f)
    )

(*  eval e = v, where _ ├ e ↓ v.
 *
 *  Because later declarations shadow earlier ones, this is the `eval`
 *  function that is visible to clients.
 *)
let eval (fundef_l : Ast.Script.fundef list) (e : Ast.Expr.t) : Value.t =
  eval fundef_l Env.empty e

(* exec p = v, where `v` is the result of executing `p`.
 *)
let exec (p : Ast.Script.t) : Value.t =
  match p with
  | Ast.Script.Pgm (fundef_l, e) -> eval fundef_l e
>>>>>>> 0fab27c9b132d6d1f19ea9fa686587fc9948b787

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

  (*  If x is in ρ, lookup ρ x = ρ(x). If x is not in ρ, lookup ρ x raises an UnboundVariable error.
   *)
  let lookup (rho : t) (x : Ast.Id.t) : Value.t = 
    match (List.assoc_opt x rho) with
    | Some Value.V_Bool b -> Value.V_Bool b
    | Some Value.V_Int n -> Value.V_Int n
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
  | DO ALL OF THEM, THERE ARE MORE IN AST

(*  unop op v = v', where v' is the result of applying the semantic
 *  denotation of `op` to `v`.
 *)
let unop (op : Ast.Expr.unop) (v : Value.t) : Value.t =
  match (op, v) with
  | (Ast.Expr.Neg, Value.V_Int n) -> Value.V_Int (-n)
  | (Ast.Expr.Not, Value.V_Bool b) -> (match b with
                                       | Value.V_Bool true -> Value.V_Bool false
                                       | Value.V_Bool false -> Value.V_Bool true
                                      )
  |

ADD EXCEPTIONS!! NOTE THAT THE SAMPLE CODE DIDN'T INCLUDE EXCEPTIONS SO I HAVE TO ADD THEM MYSELF!! 
HAVE THE TYPEERROR EXCEPTION HAVE STRINGS

(*  eval fundef_l ρ e = v, where ρ ├ e ↓ v according to our evaluation rules. fundef_l is a list of 
    all the function definitions in the script, while e is the expression in the script.
 *)
let rec eval (fundef_l : Ast.Script.fundef list) (rho : Env.t) (e : Ast.Expr.t) : Value.t =
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
  | Ast.Expr.Call (f, call_l) ->
    let find_f_func = (fun f_def -> match f_def with | (f, _, _) -> true | _ -> false) in
    (match (List.find_opt find_f_func fundef_l) with
    | Some Ast.Script.fundef f_def -> 
      let (f', param_l, e') = f_def in
      (match ((List.length param_l) = (List.length call_l)) with
      | Value.V_Bool true -> 
        let fold_func = (fun curr_env param' call' -> let v = eval fundef_l curr_env call' in Env.update curr_env param' v) in
        fold_left2 fold_func rho param_l call_l
      | Value.V_Bool false -> raise(TypeError "Function called with wrong number of arguments.")
      )
    | None -> raise(UndefinedFunction f)
    )

      make new envrionment which is combo of exisitn envrionment and added evaluations of parameters
      for each elem in call_l: evaluate it, then add it to the envrionment
      find a way to check the number of arguments. if the number of arguments that its called with is not equal to the number of arguments the function takes, then it raises an error
      steps: use the envrionment to evaluate all the arguments in the call expression and in the function's envrionment bind the arguments to the evaluated values, then compute the functions output

(*  eval e = v, where _ ├ e ↓ v.
 *
 *  Because later declarations shadow earlier ones, this is the `eval`
 *  function that is visible to clients.
 *)
let eval (e : Ast.Expr.t) : Value.t =
  eval Env.empty e

(* exec p = v, where `v` is the result of executing `p`.
 *)
let exec (p : Ast.Script.t) : Value.t =
  let (fundef_l, e) = p in

  failwith "Unimplemented:  Core.Interp.exec"
note: this is a short function

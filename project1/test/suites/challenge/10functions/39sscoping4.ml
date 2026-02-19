(*!tests!
*
* { "exception": "UnboundVariable"}
*
*
*)
let f = fun x -> x + a in let a = 7 in f 1 ;; 
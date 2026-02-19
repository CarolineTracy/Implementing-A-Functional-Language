(*!tests!
*
*
* {"output": ["4"]}
*
*)

let a = 3 in let f = fun x -> x + a in let a = 7 in f 1 ;; 
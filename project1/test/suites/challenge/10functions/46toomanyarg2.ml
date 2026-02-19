(*!tests!
*
* {"output": ["7"]}
*
*
*)
let f = fun x -> (fun y -> x+y) in f 3 4 ;; 
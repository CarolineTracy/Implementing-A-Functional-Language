(*!tests!
*
* {"exception": "TypeError"}
*
*)

let f = fun x -> (fun y -> x+y) in f 3 4 5 ;; 
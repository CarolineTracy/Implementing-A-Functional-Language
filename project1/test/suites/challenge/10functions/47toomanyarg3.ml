(*!tests!
*
* {"output": ["7"]}
*
*
*)

(fun f x -> f(f x)) (fun y -> y + 1) 5 ;; 
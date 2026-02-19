(*!tests!
*
* {"output": ["8"]}
*
*)
let f = (let g = fun x -> x * 2 in g) in 
f 4 ;; 
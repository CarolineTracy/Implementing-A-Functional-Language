(*!tests!
*
* {"output": ["12"]}
*
*)

let f = fun x y z -> x + y + z in 
let g = f 3 in 
g 4 5 ;; 

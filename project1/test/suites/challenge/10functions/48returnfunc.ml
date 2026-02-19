(*!tests!
*
* {"output": ["12"]}
*
*
*)

let f = fun x -> fun y -> fun z -> x + y + z in 
let g = f 2 in 
let h = g 4 in 
h 6 ;; 


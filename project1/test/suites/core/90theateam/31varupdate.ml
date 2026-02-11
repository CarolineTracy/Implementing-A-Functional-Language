(*!tests!
* 
* {"output": ["11"]}
*
*
*)

let x = 9 in 
let rec f y = x + y in 
let x = 100 in 
f 2  + (x -100) ;; 



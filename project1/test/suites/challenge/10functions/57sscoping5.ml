(*!tests!
*
* {"output": ["11"]}
*
*)
let x = 1 in let f = (fun y -> x + y) in let x = 100 in f 10 ;;
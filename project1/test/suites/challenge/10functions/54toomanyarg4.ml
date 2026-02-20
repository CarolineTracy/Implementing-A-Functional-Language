(*!tests!
*
* {"output": ["33"]}
*
*
*)
let f = fun x -> fun y -> fun z -> fun a -> fun b -> z + (a * b) - x + y in f 5 7 1 3 10 ;;
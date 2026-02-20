(*!tests!
*
* {"output": ["25"]}
*
*
*)
let f = fun x -> fun y -> fun z -> z + x + y + x in f 10 2 3 ;;
(*!tests!
*
* {"output": ["31"]}
*
*
*)
let rec f x = x + 3 ;;
f 5 + f 7 + f 10 ;;
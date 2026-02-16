(*!tests!
*
* {"exception": "UnboundVariable"}
*
*
*)
let rec f x = x + y
and g y = f 3 ;;
g 10 ;;
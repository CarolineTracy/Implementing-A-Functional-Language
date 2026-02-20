(*!tests!
*
* {"exception" : "UnboundVariable"}
*
*
*)
let rec g x y = x + y
and h z = z + 1 
and f x = x + y + z ;;
g 1 2 + h 4 + f 3 ;;
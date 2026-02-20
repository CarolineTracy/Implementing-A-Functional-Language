(*!tests!
*
* {"exception" : "UnboundVariable"}
*
*
*)
let rec g x = o + x ;;
let o = 10 in g 2 ;;
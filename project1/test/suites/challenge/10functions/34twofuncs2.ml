(*!tests!
*
* {"output": ["11"]}
*
*
*)

let rec f x = (g 5) + x
and g y = y + 4 ;;
f 2 ;;
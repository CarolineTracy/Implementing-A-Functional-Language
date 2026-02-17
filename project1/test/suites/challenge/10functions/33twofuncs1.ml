(*!tests!
*
* {"output": ["11"]}
*
*
*)

let rec f x = x + 4 
and g y = (f 5) + y ;;
g 2 ;;
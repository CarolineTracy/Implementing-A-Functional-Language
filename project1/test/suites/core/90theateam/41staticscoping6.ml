(*!tests!
*
* {"output": ["24"]}
*
*
*)
let rec g x y = x + y
and h z = z + 1 
and f x y z = x + y + z ;;
g 1 2 + h 4 + f 3 6 7 ;;
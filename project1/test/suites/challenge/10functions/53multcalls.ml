(*!tests!
*
* {"output": ["23"]}
*
*)
let rec f x = x + 3 ;;
f 1 + f 5 + f 8 ;; 
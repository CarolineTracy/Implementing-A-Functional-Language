(*!tests!
*
* {"output": ["28"]}
*
*)
let rec f g x = g 7 ;;
f (fun x -> x * 4) ((fun u -> u + u) 3) ;; 
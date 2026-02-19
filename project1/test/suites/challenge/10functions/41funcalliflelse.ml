(*!tests!
*
*
* {"output": ["13"]}
*
*)

let f = (if true then (fun x -> x + 10) else (fun x -> x - 10)) in f 3 ;; 
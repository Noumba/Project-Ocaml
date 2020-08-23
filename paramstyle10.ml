
type time = {ent:int ; context: int; eval: int; typ: int }
let record = {ent = -1 ; context = -1; eval = -1; typ = -1}

let help = fun () -> print_string "\nAvailable factors for passing styles\n
1 - Entity Passed\n
2 - Context\n
3 - Evaluation Strategy\n
4 - Typing
\n\n
Enter number for the selected factor for the passing style: "

let rec name record = let () = help() in let t = read_int ()
in if t = 1 then 
 let () = print_string "Initialise selected factor: " in let y = read_int() in 
    name ({record with ent = y }) 
  else if t = 2 then 
  let () = print_string "Initialise selected factor: " in let y = read_int() in 
    name ({record with context = y})
  else if t = 3 then
  let () = print_string "Initialise selected factor: " in let y = read_int() in  
    name ({record with eval = y})
  else if t = 4 then 
  let () = print_string "Initialise selected factor: " in let y = read_int() in 
    name ({record with typ = y})
  else 
    record
  

   let record = name record
   ;;
   
let user_facing_information = 
  fun () -> print_endline "This table represents information the user faces concerning factors of parameter passing styles
  Factors ******************Instance values *********************Meaning

  Entity passed                   1                              Value
                                  2                              Reference
                                  3                              Computation
                                  4                              Environment
                                  5                              Continuation
                                  6                              Denotation
  
  Context                         1                              Calling method
                                  2                              Called method
                                  3                              Calling and called
                                  4                              Other

  Evaluation                      1                              Strict                              
  Strategy                                                                
                                  2                              Lazy
                                  3                              Non
                                  4                              Manual

  Typing                          1                              Yes
                                  2                              No
                                  3                              Non
  "   

;;
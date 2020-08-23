
let name = "Pass by Name"
in


let effect_factor name  = 
		   match name with 
		   |"Pass by Value" -> let no_mut   =  let () = print_string "Enter value for variable t: " in 
												let t = read_int() in
												let () = print_string "No mutation since t is: " in
												let add2 one = one + 2 in let () = Printf.printf "%d\n" t	in add2 (t)													
												in no_mut 									 		
		   |"Pass by Reference" -> let new_add =
									let () = print_string "Enter value for reference variable: " in 
										let t = read_int() in
									let r = ref t in 								 
									let v = !r in (r := v+3; !r)
									in new_add									
		   |"Pass by Name" -> let add_2_var = 
								let () = print_string "Enter value for first variable x: " in 
								let x = read_int() in
								let () = print_string "Enter value for second variable y: " in 
								let y = read_int() in
								let add x y = 
								let f = fun a y -> a + y
								in let a = x 
								in f a y  (*add (x+y) (x+y) *)
								in add (x) (y)
								in add_2_var
		   |"Pass by Copy-Restore" ->2		   
		   |"Pass by Need" ->3		   
		   | _ ->4  
		   
 in effect_factor name 

 ;;
  
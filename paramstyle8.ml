
(* C:\Users\user\Desktop\Projectdocs\prog\paramstyle8.ml *)

(**Assuming the factors that effect parameter passing include entity passed, context, evaluation and typing *)

type entity_type =   (*the variouse types of entities *)
Entity_type of int       (*each integer corresponds to a type of entity*)   

type context_type =  (*the various types of context*)
Context_type of int         
     

type evaluation_strat =    (*the various evaluation strategies*)
Evaluation_strat of int                 


type typing =               (*the various types*)
Correct_type of int        
 

 type passing_style =            (* various passing styles names*)
 Passing_style of string 
 
 let setName name =              (**Method to set the name of a passing style *)
 let n = Passing_style name in 
 match n with 
 Passing_style name -> Passing_style name
 
let f x = match x with 			(**Method used to output record of factors*)
x -> x 

let rec return_inserted_style user_styles x y =    (** returns a specific style from the list of passing styles*)
				match user_styles with 
				[] -> y
				|h :: t ->if h = y then h else return_inserted_style t x y
				
let rec insert_new_style user_styles x y =       (**Inserts a new passing style on the initial list of of passing style(known passing styles) *)
				match user_styles with 
				[] -> [y]
				|h :: t ->if h = y then user_styles else h :: insert_new_style t x y		

	let rec remove_passing_style styles y =   (** Method used by the method "removal" to remove a passing style from the list of passing styles *)
		match styles with 
		| [] -> []
		| h :: t -> if h = y 
								then t 
					else h :: remove_passing_style t y  

type factors = {fac1 : entity_type; fac2 : context_type; fac3 : evaluation_strat; fac4 : typing}  (** Record type to hold known factors that effect the various passing styles *)
type by_val = {init_var: int; eval: int; final_var : int }        (**Record type to hold the effects that the specific values of factors induce*)
type style_factors = {name : passing_style; factor : factors}      (** Record type to hold the properties of a passing style, consist of passing style name and factors*)

type time = {ent:int ; context: int; eval: int; typ: int }    (**Record for holding instances of parameter passing style factors*)
let record = {ent = 5 ; context = 5; eval = 5; typ = 5}      (**Default instances(value) for parameter passing style factors*)

let user_styles = [{name = Passing_style "Pass by Value";				(**List of  Known(hard-coded) passing styles   *)
  factor =
   {fac1 = Entity_type 1; fac2 = Context_type 1; fac3 = Evaluation_strat 1;
    fac4 = Correct_type 1}}; 
	{name = Passing_style "Pass by Reference";
  factor =
   {fac1 = Entity_type 1; fac2 = Context_type 2; fac3 = Evaluation_strat 3;
    fac4 = Correct_type 3}}; 
	{name = Passing_style "Pass by Name";
  factor =
   {fac1 = Entity_type 2; fac2 = Context_type 2; fac3 = Evaluation_strat 2;
    fac4 = Correct_type 2}}; 
	{name = Passing_style "Pass by Copy_restore";
  factor =
   {fac1 = Entity_type 3; fac2 = Context_type 3; fac3 = Evaluation_strat 3;
    fac4 = Correct_type 3}}; {name = Passing_style "Pass by Need";
  factor =
   {fac1 = Entity_type 1; fac2 = Context_type 2; fac3 = Evaluation_strat 3;
		fac4 = Correct_type 4}} ] 

	(**Method that checks if passing style is in list, adds it if not and returns the list*)
let sanitize entity_type context_type evaluation_strat typing user_styles name=     
  match (entity_type, context_type, evaluation_strat, typing) with      
  (Entity_type 1,Context_type 1,Evaluation_strat 1,Correct_type 1) -> user_styles
  |(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 3) -> user_styles																										
  |(Entity_type 2,Context_type 2,Evaluation_strat 2,Correct_type 2) -> user_styles 		 
  |(Entity_type 3,Context_type 3,Evaluation_strat 3,Correct_type 3) -> user_styles
	|(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 4) -> user_styles																				
  |(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let new_style = 
								let  x = {fac1 = entity_type; fac2 = context_type; fac3 = evaluation_strat; fac4 = typing}
								in 
								
								let y = {name = (setName name); factor = x }
								in
								let () = print_endline("passing style is :" ^ name)
								in																																					
							insert_new_style user_styles x y 	
							in new_style 
;;

let get_name entity_type context_type evaluation_strat typing  =   (**Method that Gets the name of a passing style or takes name from user if user defined style*)
  match (entity_type, context_type, evaluation_strat, typing) with
  (Entity_type 1,Context_type 1,Evaluation_strat 1,Correct_type 1) -> "Pass by Value"
  |(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 3) -> "Pass by Reference"																										
  |(Entity_type 2,Context_type 2,Evaluation_strat 2,Correct_type 2) -> "Pass by Name" 		 
  |(Entity_type 3,Context_type 3,Evaluation_strat 3,Correct_type 3) -> "Pass by Copy_restore"
|(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 4) -> "Pass by Need"																				
	|(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let () = print_string "Enter name for your passing style: " 
																																								in read_line()

;;

let effect_factor entity_type context_type evaluation_strat typing =    (**Method that simulates the effects of the various known passing styles*)
		   match (entity_type, context_type, evaluation_strat, typing) with 
		   (Entity_type 1,Context_type 1,Evaluation_strat 1,Correct_type 1) -> let no_mut   =  (**For call by value*)
									let () = print_string "Enter value for a variable t: " in 
									let t =  read_int() in
									let r = ref t in 								 
									let v = r in print_endline "After computation, variable is now:"; 
									(let f = fun r -> r := !v+3; !r  
									in let b = {init_var = t; eval = f (v); final_var = !v }
									in let ft = fun x -> x in ft b   )
		in no_mut 									 		
		   |(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 3) -> let new_add = (**For call by reference*)
									let () = print_string "Enter value for reference variable: " in 
										let t =  read_int() in
									let r = ref t in 								 
									let v = r in print_endline "After computation, Reference variable is now:  ";
									 (let () = r := !v+3;  
									 in let b = {init_var = t; eval = !r; final_var = !v } 
									 in let ft = fun x -> x in ft b  )
									
									in new_add									
		   |(Entity_type 2,Context_type 2,Evaluation_strat 2,Correct_type 2) -> let add_2_var =  (**For call by Name *)
								let () = print_string "Enter value for first variable x: " in 
								let   p =  (read_int()) in
								let () = print_string "Enter value for second variable y: " in 
								let  q =  (read_int()) in let x = lazy p in let y = lazy (q/0) in 
								
								let f a y = (a + y)	in let add  (lazy (x)) (lazy (y))= 						
							  let b = {init_var = x; eval = f x y; final_var = x } in let ft = fun x -> x in ft b
								in add (x) (x)
								in add_2_var
			|(Entity_type 3,Context_type 3,Evaluation_strat 3,Correct_type 3) -> let copy_rest =  (**For call by copy-restore*)
							let () = print_string "Enter value for a reference variable: " in 
							let var1 = read_int() in 																													
							let r = ref var1 in 																																																																				
							let call_copy_restore v  = 						 
							let v = r in let copy_var = !v ; in
							(let () = r := !v+5;  
							in let b = {init_var = copy_var; eval = !r; final_var = !v } 
							in let ft = fun x -> x in ft b   )																																																																																																																	
							in call_copy_restore r  
							in copy_rest 
			|(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 4) -> let new_add =  (**For call by Need*)
																													
							let () = print_string "Enter value for first variable x: " in 
							let   p =  (read_int()) in let x = lazy (p+p) in 
							let () = print_string "Enter value for second variable t: " in 																															
							let t = read_int() in																															
							let r = ref t in 								 																															
							let by_need  (lazy (x)) = 
							let () = r := x ;							
							in let b = {init_var = x; eval = !r ; final_var = x } in let ft = fun x -> x in ft b
							in by_need (x) 
							in new_add									
			 |(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _)->
			 														{ init_var = 4 ; eval = 5; final_var = 6}  (**For userdefined*)

(**Method that returns a specific passing style from list of styles*)
let paramstyle entity_type context_type evaluation_strat typing user_styles name= 
 match (entity_type, context_type, evaluation_strat, typing) with     
  (Entity_type 1,Context_type 1,Evaluation_strat 1,Correct_type 1) -> let rec get_style_list =  
					let name = name 																				
					in					
					let  x = {fac1 = entity_type; fac2 = context_type; fac3 = evaluation_strat; fac4 = typing}
					in 
					let y = {name = (setName name); factor = x }
					in
						let () = print_endline("passing style is: " ^ name)
					in																				
					return_inserted_style user_styles x y 
				in get_style_list
  |(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 3) -> let rec get_style_list  =  
				let name = name
				in	 																	
				let  x = {fac1 = entity_type; fac2 = context_type; fac3 = evaluation_strat; fac4 = typing}
				in 
				let y = {name = (setName name); factor = x }
				in
					let () = print_endline("passing style is: " ^ name)
				in
				return_inserted_style user_styles x y 
			in get_style_list 																												
  |(Entity_type 2,Context_type 2,Evaluation_strat 2,Correct_type 2) -> let rec get_style_list  =  
				let name = name
				in
				let  x = {fac1 = entity_type; fac2 = context_type; fac3 = evaluation_strat; fac4 = typing}
				in 
				let y = {name = (setName name); factor = x }
				in
					let () = print_endline("passing style is: " ^ name)
				in 				
				return_inserted_style user_styles x y
			 in get_style_list 		 
  |(Entity_type 3,Context_type 3,Evaluation_strat 3,Correct_type 3) -> let rec get_style_list  =  
				let name = name
				in																			
				let  x = {fac1 = entity_type; fac2 = context_type; fac3 = evaluation_strat; fac4 = typing}
				in 
				let y = {name = (setName name); factor = x }
				in
				let () = print_endline("passing style is :" ^ name)
				in
				return_inserted_style user_styles x y
			in get_style_list 
|(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 4) -> let rec get_style_list  =  
																		
				let name = name
				in																			
				let  x = {fac1 = entity_type; fac2 = context_type; fac3 = evaluation_strat; fac4 = typing}
				in 
				let y = {name = (setName name); factor = x }
				in
					let () = print_endline("passing style is: " ^ name)
				in
				return_inserted_style user_styles x y
			in get_style_list 																					
  |(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let new_style = 
						let  x = {fac1 = entity_type; fac2 = context_type; fac3 = evaluation_strat; fac4 = typing}
						in 
						let y = {name = (setName name); factor = x } 
						in
						let () = print_endline("passing style is :" ^ name)
						in																																																								
					return_inserted_style user_styles x y 	
					in new_style  

	(**Help method that display the various factors for parameter passing*)

let help = fun () -> print_string "\nAvailable factors for passing styles\n
	1 - Entity Passed
	2 - Context
	3 - Evaluation Strategy
	4 - Typing
\n
 "

(**Method that display the specific factor instances and their meaning *)
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
                          
let rec name record = let () = print_string "Enter number for the selected factor type for the passing style:" 
in let t = read_int ()
in if t = 1 then 
 let () = print_string "Initialise selected factor: " in let y = read_int() in 
 let () = print_string "More factors? (y/n): " in let opt = read_line() in 
		if opt.[0] = 'y' then
    name ({record with ent = y })  else {record with ent = y}
  else if t = 2 then 
  	let () = print_string "Initialise selected factor: " in let y = read_int() in 
		let () = print_string "More factors? (y/n): " in let opt = read_line() in 
			if opt.[0] = 'y' then
		name ({record with context = y })  else {record with context = y}
  else if t = 3 then
  	let () = print_string "Initialise selected factor: " in let y = read_int() in  
		let () = print_string "More factors? (y/n): " in let opt = read_line() in 
			if opt.[0] = 'y' then
		name ({record with eval = y })  else {record with eval = y}
  else if t = 4 then 
  	let () = print_string "Initialise selected factor: " in let y = read_int() in 
		let () = print_string "More factors? (y/n): " in let opt = read_line() in 
			if opt.[0] = 'y' then
		name ({record with typ = y })  else {record with typ = y}
  else 
    record

(**Method to Insert passing style into updated list of styles at runtime*)
	let insert_new_passing_style styles = 
  let record = name record in 
	let name = get_name (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ) ; in 
	let styles = sanitize (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ) styles name
	 in styles 	
;;
(**Method to Remove a passing style form list of passing style*)
let rec removal styles = 
let record = name record in 	
let name = get_name (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ) in
	let remove_style entity_type context_type evaluation_strat typing styles name =     
		match (entity_type, context_type, evaluation_strat, typing) with
		|(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let new_style = 
		let  x = {fac1 = entity_type; fac2 = context_type; fac3 = evaluation_strat; fac4 = typing}
		in 
		
		let y = {name = (setName name); factor = x }
		in																	
		remove_passing_style styles y
	in new_style 
	in let styles =  remove_style (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ) styles name 
	in styles 																																								

;;

let () = print_string "\nWelcome\nTo test for known style(hardcoded), enter values for it factors\n 
Else other values defines a new style\n" in
let () = help() in
user_facing_information()
let record = name record 
;;
let name = get_name (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ) (**Variable "name" holds name of a passing style*)

let styles = sanitize (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ) user_styles name (**Holding updated list of passing style in "styles"*)
 let test = paramstyle (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ)  styles name (**Returns the specific passing style induced by values of the various factorsand hloding it in "test"*)
 let fac = print_endline ("Factors that effects it and with their value are:\n ") ; f test.factor (**Displaying a record that holds the factors for this passing style*)

let () = print_endline "Effects Induced by specific values of factors is:\n "  
let effect = effect_factor (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ)
;;

(**Method that gives users options on what next they may wish to do*)
let rec usermind styles = let () = print_endline "More parameter passing styles?? (y/n) : " 
in let opinion = read_line() in 
 if opinion.[0] = 'y' then 
  (let operation =  let () = print_endline "What do u want to do??\nEnter 1 to add new style or test known style\nEnter 2 to remove a passing style\nEnter 3 to view all current passing styles\n "
		 in let opinion2 = read_int() in 
				 (if opinion2 = 1 then 
						usermind (insert_new_passing_style styles)
					else if opinion2 = 2 then
						usermind (removal styles)
					else if opinion2 = 3 then 
					   styles					  
					else styles ) in operation )
 else styles 


let styles = usermind styles 
;;


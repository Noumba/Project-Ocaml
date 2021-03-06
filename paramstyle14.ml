
(* C:\Users\user\Desktop\Projectdocs\prog\paramstyle12.ml *)

(**Assuming the factors that effect parameter passing include entity passed, context, evaluation and typing *)

type factors_instance_list = int list (**List of integers for holding various instances for factors *)

type entity_type =   (*the variouse types of entities *)
Entity_type of factors_instance_list   (* list of integers holds the various type of entity for a passing style and each integer corresponds to a specific type of entity*)   

type context_type =  (*the various types of context*)
Context_type of factors_instance_list         

type evaluation_strat =    (*the various evaluation strategies*)
Evaluation_strat of factors_instance_list                 


type typing =               (*the various types*)
Correct_type of factors_instance_list        
 

 type passing_style =            (* various passing styles names*)
 Passing_style of string 
 
 let setName name =              (**Method to set the name of a passing style *)
 let n = Passing_style name in 
 match n with 
 Passing_style name -> Passing_style name

(*Method to get the passing style name
@param name : name of passing style with defined constructor
**)
 let getName name = 			
	match name with 
	Passing_style a -> a 
 
let f x = match x with 			(**Method used to output record of factors*)
x -> x 
(**
 returns a specific style from the list of passing styles
 @param user_style: list from which we want to return the passing style
 @param y; passing style to be returned
 *)
let rec return_inserted_style user_styles y =    
				match user_styles with 
				[] -> y
				|h :: t ->if h = y then h else return_inserted_style t y 

(**
Inserts a new passing style on the initial list of of passing style(known passing styles)
@param user_styles: the list from which to add new passing style
@param y : passing style to be added to list
*)
let rec insert_new_style user_styles y =   
				match user_styles with 
				[] -> let () = print_string " successfully added.\n\n" in  [y]
				|h :: t ->if h = y then let () = print_string "Passing style already exists. " in  user_styles else h :: insert_new_style t y

(** Method that inserts intances into factors list of instances
@param  instance_list: instance list as defined by the type factors_instance_list above
@param  y : specific instance of a factor to be added in instance list from user for that factor
*)		
let rec insert_instance instance_list y = 
	match instance_list with 
	[] -> [y]
	|h :: t -> if h = y then instance_list else h :: insert_instance t y 

(**Method used by the method "removal"(line 284) to remove a passing style from the list of passing styles
	@param styles: list to remove passing style from
	@param y : passing style to be removed
*)	
let rec remove_passing_style styles y =
	match styles with 
	| [] -> let () = print_string "\nNo such passing style in the structure. \n\n" in []
	| h :: t -> if h = y 
							then 
							let () = print_string " deleted successfully." in t 
				else h :: remove_passing_style t y  


let validate_input () = let () = print_string " " in 
let rec check_int () =  
try read_int() with | _ -> let () = print_string "INVALID input enter number: " in   check_int () 
in check_int ()

(** Record type to hold known factors that effect the various passing styles
@param entity, context, evaluation, typing: various factors that effect parameter passing
*)
type factors = {entity : entity_type; context : context_type; evaluation : evaluation_strat; typing : typing} 

(**Record type to hold the effects that the specific values of factors induce
@param init_var, final_var: the initial and final values of a variable before and after conputation respectively
@param eval: result of the computation
*)
type by_val = {init_var: int; eval: int; final_var : int }        
type style_factors = {name : passing_style; factor : factors} (** Record type to hold the properties of a passing style, consist of passing style name and factors*)


type interpret_rec = {styles : style_factors list ; interp : (passing_style * string) list }
type interpret_rec1 = {styles : style_factors list ; interp : (passing_style * string) list; new_entity_instance_list:(string*int) list;
new_context_instance_list:(string*int) list;new_eval_instance_list:(string*int) list }
type new_factor_instance1 = {fact_name:string ; num:int ; new_entity_instance_list:(string*int) list}	
type new_factor_instance2 = {fact_name:string ; num:int ; new_context_instance_list:(string*int) list}
type new_factor_instance3 = {fact_name:string ; num:int ; new_eval_instance_list:(string*int) list}

type time = {ent: factors_instance_list ; context: factors_instance_list; eval: factors_instance_list; typ: factors_instance_list;
new_entity_instance_list:(string*int) list; new_context_instance_list:(string*int) list; new_eval_instance_list:(string*int) list }    (**Record type for holding instances of parameter passing style factors*)

(**Default instances(value) for parameter passing style factors
@value [5] : default instance for factors not of interest to the users
*)
let record = {ent = [5] ; context = [5]; eval = [5]; typ = [5]; new_entity_instance_list = [("new",12)]; new_context_instance_list = [("new",12)];
new_eval_instance_list = [("new",12)] } 

let trash = record.new_entity_instance_list

(**List of  Known(hard-coded) passing styles   *)
let user_styles = [{name = Passing_style "Pass by Value";				
  factor =
   {entity = Entity_type [1]; context = Context_type [1]; evaluation = Evaluation_strat [1];
    typing = Correct_type [1]}}; 
	{name = Passing_style "Pass by Reference";
  factor =
   {entity = Entity_type [1]; context = Context_type [2]; evaluation = Evaluation_strat [3];
    typing = Correct_type [3]}}; 
	{name = Passing_style "Pass by Name";
  factor =
   {entity = Entity_type [2]; context = Context_type [2]; evaluation = Evaluation_strat [2];
    typing = Correct_type [2]}}; 
	{name = Passing_style "Pass by Copy_restore";
  factor =
   {entity = Entity_type [3]; context = Context_type [3]; evaluation = Evaluation_strat [3];
    typing = Correct_type [3]}}; {name = Passing_style "Pass by Need";
  factor =
   {entity = Entity_type [1]; context = Context_type [2]; evaluation = Evaluation_strat [3];
		typing = Correct_type [4]}} ] 

let interpretation_list = [(Passing_style "Pass by value", "Here, entity passed evaluated from left to right")]

	(**Method(sanitize) that checks if passing style is in list, adds it if not and returns the list
	@param	user_styles, name: list of known passing styles and name of passing styles recpectively
	@param entity_type, context_type, evaluation_strat, typing: factors that effect parameter passing
	@param x: record for passing style factors
	@param y: passing style
	*)
let sanitize entity_type context_type evaluation_strat typing user_styles name=     
  match (entity_type, context_type, evaluation_strat, typing) with      
  (Entity_type [1],Context_type [1],Evaluation_strat [1],Correct_type [1]) -> user_styles
  |(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [3]) -> user_styles																										
  |(Entity_type [2],Context_type [2],Evaluation_strat [2],Correct_type [2]) -> user_styles 		 
  |(Entity_type [3],Context_type [3],Evaluation_strat [3],Correct_type [3]) -> user_styles
	|(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [4]) -> user_styles																				
  |(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let new_style = 
								let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
								in 								
								let y = {name = (setName name); factor = x }
								in
								let () = print_string "\npassing style " in let () = print_string "''" in 
								let () = print_string name in let () = print_string "''"
								in																																					
							insert_new_style user_styles y 	
							in new_style 
;;
(**Method that Gets the name of a passing style or takes name from user if user defined style*)
let get_name entity_type context_type evaluation_strat typing  =   
  match (entity_type, context_type, evaluation_strat, typing) with
  (Entity_type [1],Context_type [1],Evaluation_strat [1],Correct_type [1]) -> "Pass by Value"
  |(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [3]) -> "Pass by Reference"																										
  |(Entity_type [2],Context_type [2],Evaluation_strat [2],Correct_type [2]) -> "Pass by Name" 		 
  |(Entity_type [3],Context_type [3],Evaluation_strat [3],Correct_type [3]) -> "Pass by Copy_restore"
|(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [4]) -> "Pass by Need"																				
	|(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let () = print_string "Enter name for your passing style: " 
																																								in read_line()
;;
 (**Method used to simulate the effects of the various known passing styles*)
let effect_factor entity_type context_type evaluation_strat typing =   
		   match (entity_type, context_type, evaluation_strat, typing) with 
		   (Entity_type [1],Context_type [1],Evaluation_strat [1],Correct_type [1]) -> let no_mut   =  (**For call by value*)
									let () = print_string "Enter value for a variable t: " in 
									let t =  validate_input() in
									let r = ref t in 								 
									let v = r in print_endline "After computation, variable is now:"; 
									(let f = fun r -> r := !v+3; !r  
									in let b = {init_var = t; eval = f (v); final_var = !v }
									in let ft = fun x -> x in ft b   )
		in no_mut 									 		
		   |(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [3]) -> let new_add = (**For call by reference*)
									let () = print_string "Enter value for reference variable: " in 
										let t =  validate_input() in
									let r = ref t in 								 
									let v = r in print_endline "After computation, Reference variable is now:  ";
									 (let () = r := !v+3;  
									 in let b = {init_var = t; eval = !r; final_var = !v } 
									 in let ft = fun x -> x in ft b  )
									
									in new_add									
		   |(Entity_type [2],Context_type [2],Evaluation_strat [2],Correct_type [2]) -> let add_2_var =  (**For call by Name *)
								let () = print_string "Enter value for first variable x: " in 
								let   p =  (validate_input()) in
								let () = print_string "Enter value for second variable y: " in 
								let  q =  (validate_input()) in let x = lazy p in let y = lazy (q/0) in 
								
								let f a y = (a + y)	in let add  (lazy (x)) (lazy (y))= 						
							  let b = {init_var = x; eval = f x y; final_var = x } in let ft = fun x -> x in ft b
								in add (x) (y)
								in add_2_var
			|(Entity_type [3],Context_type [3],Evaluation_strat [3],Correct_type [3]) -> let copy_rest =  (**For call by copy-restore*)
							let () = print_string "Enter value for a reference variable: " in 
							let var1 = validate_input() in 																													
							let r = ref var1 in 																																																																				
							let call_copy_restore v  = 						 
							let v = r in let copy_var = !v ; in
							(let () = r := !v+5;  
							in let b = {init_var = copy_var; eval = !r; final_var = !v } 
							in let ft = fun x -> x in ft b   )																																																																																																																	
							in call_copy_restore r  
							in copy_rest 
			|(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [4]) -> let new_add =  (**For call by Need*)
																													
							let () = print_string "Enter value for first variable x: " in 
							let   p =  (validate_input()) in let x = lazy (p+p) in 
							let () = print_string "Enter value for second variable t: " in 																															
							let t = validate_input() in																															
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
  (Entity_type [1],Context_type [1],Evaluation_strat [1],Correct_type [1]) -> let rec get_style_list =  
					let name = name 																				
					in					
					let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
					in 
					let y = {name = (setName name); factor = x }
					in																				
					return_inserted_style user_styles y 
				in get_style_list
  |(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [3]) -> let rec get_style_list  =  
				let name = name
				in	 																	
				let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
				in 
				let y = {name = (setName name); factor = x }
				in
				return_inserted_style user_styles y 
			in get_style_list 																												
  |(Entity_type [2],Context_type [2],Evaluation_strat [2],Correct_type [2]) -> let rec get_style_list  =  
				let name = name
				in
				let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
				in 
				let y = {name = (setName name); factor = x }
				in 				
				return_inserted_style user_styles y
			 in get_style_list 		 
  |(Entity_type [3],Context_type [3],Evaluation_strat [3],Correct_type [3]) -> let rec get_style_list  =  
				let name = name
				in																			
				let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
				in 
				let y = {name = (setName name); factor = x }
				in
				return_inserted_style user_styles y
			in get_style_list 
|(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [4]) -> let rec get_style_list  =  
																		
				let name = name
				in																			
				let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
				in 
				let y = {name = (setName name); factor = x }
				in
				return_inserted_style user_styles y
			in get_style_list 																					
  |(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let new_style = 
						let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
						in 
						let y = {name = (setName name); factor = x } 
						in																																																								
					return_inserted_style user_styles y 	
					in new_style  

	(**Help method that display the various factors available for parameter passing*)

let help = fun () -> print_string "\nAvailable factors for passing styles
	1 - entity
	2 - context
	3 - evaluation 
	4 - typing
 "

let display_menu = 
	fun() -> print_endline "
	PASSING STYLE ACTION MENU 
	1. Add new passing style
	2. Delete passing style
	3. See all available passing style
	4. Select passing style from structure
	5. Help
	6. Exit system
	 "


(**Method that display the specific factor instances and their meaning *)
let user_facing_information styles interpretation_list new_entity_instance_list new_context_instance_list a= 
	let () =  print_string "This table represents information the user faces concerning factors of parameter passing styles
Factors *Meaning**Instances

Entity     0      New_instance
           1      Value
           2      Reference
           3      Computation
           4      Environment
           5      Continuation
           6      Denotation
  
Context    0      New_instance
           1      Calling method
           2      Called method
           3      Calling and called
           4      Other

Eval       0      New_instance 
           1      Strict                                                                                              
           2      Lazy
           3      Non
           4      Manual

Typing     0      New_instance 
           1      Yes
           2      No
           3      Non
" 
in let styles = styles
in let interp = interpretation_list 
in {styles = styles; interp = interp; new_entity_instance_list=new_entity_instance_list;new_context_instance_list=new_context_instance_list;
new_eval_instance_list = a}


(* 
	method that Removes the head from a list
	@param l: the list from which to remove head
*)
let remove_head l = 
	if List.length l > 1 then
		match l with
			[] -> []
			|h ::t -> t
	else	
		l

(**Method that removes default value([5]) from instance list for factors if factor is of interest to the user.*)
let preserve_default = fun record -> 
	{
		ent = remove_head record.ent; 
		eval = remove_head record.eval; 
		context = remove_head record.context;  
		typ = remove_head record.typ;
		new_entity_instance_list = record.new_entity_instance_list;
		new_context_instance_list = record.new_context_instance_list;
		new_eval_instance_list = record.new_context_instance_list 
	} 

(***)

let factor_name fac_name =
	match String.lowercase_ascii fac_name with 
	"entity" -> 1
	|"context" -> 2
	|"evaluation" -> 3
	|"typing" -> 4
	|_->100






let rec insert_new_instance new_instance new_instance_list = 
	match new_instance_list with
	[] -> let () = print_string "\nNew instance successfully added\n" in  new_instance :: []
	|(a,b) :: t -> if (a,b) = new_instance then 
																	let () = print_string "\nalready exist" 
																	in new_instance_list else (a,b) :: insert_new_instance new_instance t

let ent_name val_name new_entity_instance_list=
	match String.lowercase_ascii val_name with
		"value" -> {fact_name = val_name ; num=1 ; new_entity_instance_list=new_entity_instance_list}
    |"reference" -> {fact_name = val_name ; num=2 ; new_entity_instance_list=new_entity_instance_list}
    |"computation" ->{fact_name = val_name ; num=3 ; new_entity_instance_list=new_entity_instance_list}
    |"environment" -> {fact_name = val_name ; num=4 ; new_entity_instance_list=new_entity_instance_list}
    |"continuation" -> {fact_name = val_name ; num=5 ; new_entity_instance_list=new_entity_instance_list}
		|"denotation" -> {fact_name = val_name;num=6;new_entity_instance_list=new_entity_instance_list}
		|_-> let ret new_entity_instance_list= let () = print_string "Give name for factor: " in let newfac_name = read_line() in 
									 let () = print_string "Give value for factor: "
														in let num = validate_input()
														in let new_instance = (newfac_name,num) 
														in let new_entity_instance_list = insert_new_instance new_instance new_entity_instance_list 
														in {fact_name = newfac_name;num=num; new_entity_instance_list=new_entity_instance_list}
                            in ret new_entity_instance_list
				
let return_factor_instance num new_entity_instance_list= 
	match num with 
	1-> "value"
	|2-> "reference"
	|3-> "computation"
	|4-> "environment"
	|5-> "continuation"
	|6-> "denotation"
	|_-> let rec get_instance new_entity_instance_list=
				match new_entity_instance_list with 
				[] -> "none"
				|(a,b) :: tl -> if b = num then a else get_instance tl 
				in get_instance new_entity_instance_list 


let context_name val_name new_context_instance_list =
	match String.lowercase_ascii val_name with
		"calling" ->{fact_name = val_name ; num=1 ; new_context_instance_list=new_context_instance_list}
		|"called" ->{fact_name = val_name ; num=2 ; new_context_instance_list=new_context_instance_list}
		|"callboth" -> {fact_name = val_name ; num=3 ; new_context_instance_list=new_context_instance_list}
		|"other" ->{fact_name = val_name ; num=4 ; new_context_instance_list=new_context_instance_list}
		|_-> let ret new_entity_instance_list= let () = print_string "Give name for factor: " in let newfac_name = read_line() in 
		let () = print_string "Give value for factor: "
						 in let num = validate_input()
						 in let new_instance = (newfac_name,num) 
						 in let new_context_instance_list = insert_new_instance new_instance new_context_instance_list 
						 in {fact_name = newfac_name;num=num; new_context_instance_list=new_context_instance_list}
						 in ret new_context_instance_list


let return_factor_context_instance num new_context_instance_list= 
match num with 
1-> "calling"
|2-> "called"
|3-> "callboth"
|4-> "other"
|_-> let rec get_instance new_context_instance_list=
			match new_context_instance_list with 
			[] -> "none"
			|(a,b) :: tl -> if b = num then a else get_instance tl 
			in get_instance new_context_instance_list 
						

let eval_name val_name new_eval_instance_list =
	match String.lowercase_ascii val_name with
		"strict" -> {fact_name = val_name ; num=1 ; new_eval_instance_list=new_eval_instance_list}
		|"lazy" -> {fact_name = val_name ; num=2 ; new_eval_instance_list=new_eval_instance_list}
		|"non" -> {fact_name = val_name ; num=3 ; new_eval_instance_list=new_eval_instance_list}
		|"manual" -> {fact_name = val_name ; num=4 ; new_eval_instance_list=new_eval_instance_list}
		|_->  let ret new_eval_instance_list= let () = print_string "Give name for factor: " in let newfac_name = read_line() in 
						let () = print_string "Give value for factor: "
						 in let num = validate_input()
						 in let new_instance = (newfac_name,num) 
						 in let new_eval_instance_list = insert_new_instance new_instance new_eval_instance_list 
						 in {fact_name = newfac_name;num=num; new_eval_instance_list=new_eval_instance_list}
						 in ret new_eval_instance_list


let return_factor_eval_instance num new_eval_instance_list= 
match num with 
1-> "strict"
|2-> "lazy"
|3-> "non"
|4-> "manual"
|_-> let rec get_instance new_eval_instance_list=
			match new_eval_instance_list with 
			[] -> "none"
			|(a,b) :: tl -> if b = num then a else get_instance tl 
			in get_instance new_eval_instance_list


let typing_name val_name =
	match String.lowercase_ascii val_name with
		"yes" -> 1
		|"no" -> 2
		|"non" -> 3
		|_-> let ret = let () = print_string "Give value for the factor: "
														in let num = validate_input()
														in f num
														in ret
	
													
														


(**Method used to initialise selected factors of interest from the user 
@param record : record that holds Default instances(value) for parameter passing style factors
*)

let rec factor_initializer record = let () = help() in let () = print_string "Give selected factor type for the passing style: " 
in let t1 = read_line ()
in let t = (factor_name t1)
in if t = 1 then 
 let () = print_string "Initialise selected factor(see help menu for possible value): " in let y1 = read_line()
 in let new_entity_instance_list1 = (ent_name y1 (record.new_entity_instance_list))
 in let new_entity_instance_list2 = new_entity_instance_list1.new_entity_instance_list
 in let y = new_entity_instance_list1.num
 in 
 let () = print_string "More factors? (y/n): " in let opt = read_line() in 
		if opt.[0] = 'y' then
				factor_initializer ({record with ent = insert_instance (record.ent) y;new_entity_instance_list = new_entity_instance_list2}) 
		else 
			preserve_default {record with ent = insert_instance (record.ent) y; new_entity_instance_list = new_entity_instance_list2 }
		else 
	if t = 2 then 
		let () = print_string "Initialise selected factor(see help menu for possible value): " in let y1 = read_line()
		in let new_context_instance_list1 = (context_name y1 (record.new_context_instance_list))
 		in let new_context_instance_list2 = new_context_instance_list1.new_context_instance_list 
		in let y = new_context_instance_list1.num in 
		let () = print_string "More factors? (y/n): " in let opt = read_line() in 
			if opt.[0] = 'y' then
				factor_initializer ({record with context = insert_instance (record.context) y; new_context_instance_list = new_context_instance_list2 })  
			else 
				preserve_default	{record with context = insert_instance (record.context) y ; new_context_instance_list = new_context_instance_list2}
	else 
		if t = 3 then
			let () = print_string "Initialise selected factor(see help menu for possible value): " in let y1 = read_line() 
			in let new_eval_instance_list1 = (eval_name y1 (record.new_eval_instance_list))
			in let new_eval_instance_list2 = new_eval_instance_list1.new_eval_instance_list 
			in let y = new_eval_instance_list1.num in 
			let () = print_string "More factors? (y/n): " in let opt = read_line() in 
			if opt.[0] = 'y' then
				factor_initializer ({record with eval =  insert_instance (record.eval) y; new_eval_instance_list = new_eval_instance_list2 })  
			else 
			 preserve_default {record with eval = insert_instance (record.eval) y ; new_eval_instance_list = new_eval_instance_list2 }
		else 
	if t = 4 then 
  	let () = print_string "Initialise selected factor(see help menu for possible value): " in let y1 = read_line() 
		in let y = (typing_name y1) in 
		let () = print_string "More factors? (y/n): " in let opt = read_line() in 
			if opt.[0] = 'y' then
				factor_initializer ({record with typ =  insert_instance (record.typ) y})  
			else
			preserve_default {record with typ = insert_instance (record.typ) y }
  else 
		preserve_default record


let rec insert_interpretation (k,v) interpret_list = 
	match interpret_list with
	[] -> let () = print_string "\nInterpretation successfully added\n" in  (k,v) :: []
	|(a,b) :: t -> if (a,b) = (k,v) then interpret_list else (a,b) :: insert_interpretation (k,v) t 
	
let rec delete_interpretation name interpret_list = 
	match interpret_list with 
	[] ->let () = print_string "\nNo interpretation for the passing style\n" in  []
	|(a,b) :: t -> if String.lowercase_ascii (getName a) = (String.lowercase_ascii name) then 
																					let () = print_string "\n\n Interpretation deleted successfully" 
																					in t 
																				else 
																					(a,b) :: delete_interpretation name t 

(*let rec modify_interpretation interpret_list =
	let () = print_string " Give the name of the passing style you want to modify it interpreteation: "
	in let style_name = read_line()
	in let () = print_string "Give the modified interpretation: "
		in let modified = read_line()
		in let rec modify style_name interpret_list =
		match interpret_list with 
		[] ->  []
		|(a,b) :: t -> if a = (setName style_name) then (a, modified) :: t else (a,b) :: modify style_name t 
		in modify style_name*)

	(*Insert an interpretation for a passing style 
	@param name : name of the passing style
	@param : interpretation list
	**)	

let insert_new_interpretation name interpretation_list = 
	let () = print_string "Give interpretation for this passing style: "
	in let interpretation = read_line()
	in let interpreter = (Passing_style name, interpretation)
	in insert_interpretation interpreter interpretation_list

	(*retieves an interpretation for a passing style 
	@param name : name of the passing style
	@param : interpretation list
	**)	
let rec look_up_interpretation name interpret_list = 
	match interpret_list with
	[] -> "No such passing style interpretation"
	|(a,b) :: t -> if String.lowercase_ascii (getName name) = String.lowercase_ascii (getName a) then b else look_up_interpretation name t 

(**Method to Insert passing style into updated list of styles at runtime
@param styles: Updated list of passing styles we want to add new passing style
*)
	let insert_new_passing_style styles interpretation_list = 
  let record = factor_initializer record in 
	let name = get_name (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ) ; in 
	let styles = sanitize (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ) styles name
	in let interp = insert_new_interpretation name interpretation_list
	in let new_entity_instance_list = record.new_entity_instance_list
	in let new_context_instance_list  = record.new_context_instance_list
	in let new_eval_instance_list  = record.new_eval_instance_list
	in { styles = styles ; interp = interp; new_entity_instance_list = new_entity_instance_list; new_context_instance_list  = new_context_instance_list;
	new_eval_instance_list = new_eval_instance_list  }
;;
(**Method to Remove a passing style from list of passing style
@param styles: list of passing style
*)
let rec delete_passing_style styles interpretation_list new_entity_instance_list new_context_instance_list new_eval_instance_list= 
	let () = print_string "Enter name for the style: " in let name = read_line() in
	let rec removal styles interpretation_list= 	   
		match styles with
		[] -> let () = print_string "Style don't exist" in [] 
		|hd :: tl -> if String.lowercase_ascii (name)= String.lowercase_ascii (getName hd.name) 
												then let () = print_string "Passing style " in  let () = print_string "''" in 
												let () = print_string name in  let () = print_string "''" in
												remove_passing_style styles hd else 
												hd::removal tl interpretation_list
	in let styles = removal styles interpretation_list  
	in let interp = delete_interpretation name interpretation_list
	in { styles = styles ; interp = interp; new_entity_instance_list= new_entity_instance_list; new_context_instance_list=new_context_instance_list;
	new_eval_instance_list = new_eval_instance_list } 	

type view_interpret_rec = {styles : style_factors list ; interp : (passing_style * string) list ; known: unit; new_entity_instance_list:(string*int) list;
new_context_instance_list:(string*int) list; new_eval_instance_list:(string*int) list}

	(*Function to print list 
	@param : interpretation list
	**)	
let print_list_li new_entity_instance_list li =
let rec print_list new_entity_instance_list li = 
	match li with 
	[] -> ()
	|hd :: tl ->  print_string (return_factor_instance hd new_entity_instance_list) ; print_string " "; print_list new_entity_instance_list tl 
	in 
	print_string "[";
	print_list new_entity_instance_list li ;
	print_string "]"


let print_list_li_context new_context_instance_list li =
	let rec print_list new_context_instance_list li = 
		match li with 
		[] -> ()
		|hd :: tl ->  print_string (return_factor_context_instance hd new_context_instance_list) ; print_string " "; print_list new_context_instance_list tl 
		in 
		print_string "[";
		print_list new_context_instance_list li ;
		print_string "]"


let print_list_li_eval new_eval_instance_list li =
	let rec print_list new_eval_instance_list li = 
		match li with 
		[] -> ()
		|hd :: tl ->  print_string (return_factor_eval_instance hd new_eval_instance_list) ; print_string " "; print_list new_eval_instance_list tl 
		in 
		print_string "[";
		print_list new_eval_instance_list li ;
		print_string "]"

(*Method to print a list of passing style 
	@param styles : list of styles
	**)	
let print_styles styles new_entity_instance_list new_context_instance_list new_eval_instance_list=
let rec print_style_list styles=
	match styles with 
	[] -> ()
	|{name = Passing_style a;	factor ={entity = Entity_type b; context = Context_type c; evaluation = Evaluation_strat d;
		typing = Correct_type e}} :: tl
		 -> print_endline "{name = "; print_string a; print_endline ";"; 
										print_string "factors = {entity = ";print_list_li new_entity_instance_list b;print_string "; context = ";print_list_li_context new_context_instance_list c;
										print_string "; evaluation = ";print_list_li_eval new_eval_instance_list d;print_string " typing = "; print_list_li new_entity_instance_list e;print_endline "}};";
										print_style_list tl
						in print_string "[";
						 print_style_list styles;
						 print_string "]"

(*Method to print a specific style passing style 
	@param styles : list of styles
	**)					 
let print_specific_style record new_entity_instance_list new_context_instance_list new_eval_instance_list= 
match record with 
{name = Passing_style a;	factor ={entity = Entity_type b; context = Context_type c; evaluation = Evaluation_strat d;
		typing = Correct_type e}} 
		 -> print_endline "{name = "; print_string a; print_endline ";"; 
												 print_string "factors = {entity = ";print_list_li new_entity_instance_list b;print_string "; context = ";print_list_li_context new_context_instance_list c;
												 print_string "; evaluation = ";print_list_li_eval new_eval_instance_list d;print_string " typing = "; print_list_li new_entity_instance_list e;print_endline "}}"

(*Method to print specific factors of a passing style
			@param record : passing style
			**)	
let print_factors record new_entity_instance_list new_context_instance_list new_eval_instance_list= 
	match record.factor with
	{entity = Entity_type b; context = Context_type c; evaluation = Evaluation_strat d;typing = Correct_type e}
	->  print_string "factors = {entity = ";print_list_li new_entity_instance_list b;print_string "; context = ";print_list_li_context new_context_instance_list c;
	print_string "; evaluation = ";print_list_li_eval new_eval_instance_list d;print_string " typing = "; print_list_li new_entity_instance_list e;print_endline "}"

(*Method to print an effect for a passing style 
			@param record : factors for the passing style
			**)	
let print_effect record = 
	match record with
	{init_var = a; eval = b; final_var = c }
	->  print_string "effects = {init_var = ";print_int a;print_string "; eval = ";print_int b;
	print_string "; final_var = ";print_int c; print_endline "}"


(*Method to view all passing styles in structure 
			@param styles : list of styles
			@param interpretation: 
			**)		
let rec view_style styles interpretation_list new_entity_instance_list new_context_instance_list new_eval_instance_list=
let () = print_string "Give passing style name: "
in let ps_name = read_line() 
in
let rec view styles interpretation_list new_entity_instance_list= 
match styles  with 
[] -> let () = print_string "No such passing style in structure, try again\n" in ()
|hd :: t -> 
				if String.lowercase_ascii (ps_name)= String.lowercase_ascii (getName hd.name)
				then 
(						
let () = print_specific_style (paramstyle (hd.factor.entity) (hd.factor.context) (hd.factor.evaluation) (hd.factor.typing)  styles ps_name) new_entity_instance_list new_context_instance_list new_eval_instance_list
in let () = print_string "\nInterpretation =  "																	
in let () =	 print_string (look_up_interpretation (setName ps_name) interpretation_list)
in let () = print_string "\n"
in let () = print_factors (paramstyle (hd.factor.entity) (hd.factor.context) (hd.factor.evaluation) (hd.factor.typing)  styles ps_name) new_entity_instance_list new_context_instance_list new_eval_instance_list

in let () = print_string "\nEffects = " in 
	print_effect (effect_factor (hd.factor.entity) (hd.factor.context) (hd.factor.evaluation) (hd.factor.typing)) 
			
)											
				else view t interpretation_list new_entity_instance_list
				in let styles = styles
				in let new_entity_instance_list = new_entity_instance_list																	
				in view styles interpretation_list new_entity_instance_list


let view_specific_passing_style style interpretation_list new_entity_instance_list new_context_instance_list new_eval_instance_list=
	let known = view_style style interpretation_list new_entity_instance_list new_context_instance_list new_eval_instance_list
	in let styles = style 
	in let interp = interpretation_list
	in {styles = styles;interp=interp;known= known; new_entity_instance_list = new_entity_instance_list; new_context_instance_list = new_context_instance_list;
	new_eval_instance_list = new_eval_instance_list}

let display_styles styles interpretation_list new_entity_instance_list new_context_instance_list new_eval_instance_list= 
let styles =  styles
in let () = print_styles styles new_entity_instance_list new_context_instance_list new_eval_instance_list
in let interp = interpretation_list 
in  { styles = styles ; interp = interp;new_entity_instance_list=new_entity_instance_list; new_context_instance_list=new_context_instance_list;
new_eval_instance_list = new_eval_instance_list }
;;

let () = print_string "\nWelcome\nTo test for known style(hardcoded), enter values for it factors\n 
Else other values defines a new style\n" 


(**Method that gives users options on they may wish to do
	@param user_styles : list of passing style to work with
	@param interpretation_list: 	
*)
let rec usermind user_styles interpretation_list new_entity_instance_list new_context_instance_list new_eval_instance_list= let () = display_menu () 
in let () = print_string " Enter your choice (1-5): "
in let opinion  = validate_input() in 
						match opinion with
				  |1 -> let update = (insert_new_passing_style user_styles interpretation_list) in
						 		usermind update.styles update.interp update.new_entity_instance_list update.new_context_instance_list update.new_eval_instance_list 
					|2 ->let update = (delete_passing_style user_styles interpretation_list new_entity_instance_list new_context_instance_list new_eval_instance_list) in								
								usermind  update.styles update.interp update.new_entity_instance_list update.new_context_instance_list update.new_eval_instance_list 
					|3 ->let update = (display_styles user_styles interpretation_list new_entity_instance_list new_context_instance_list new_eval_instance_list) in 
								usermind update.styles update.interp update.new_entity_instance_list update.new_context_instance_list update.new_eval_instance_list 
					|4 ->let update = (view_specific_passing_style user_styles interpretation_list new_entity_instance_list new_context_instance_list new_eval_instance_list)	in 
								usermind update.styles update.interp update.new_entity_instance_list	update.new_context_instance_list update.new_eval_instance_list 
					|5 ->let update = (user_facing_information user_styles interpretation_list new_entity_instance_list new_context_instance_list new_eval_instance_list) in
								usermind update.styles update.interp update.new_entity_instance_list update.new_context_instance_list update.new_eval_instance_list 
					|_-> ()				  
	 
;;

usermind user_styles interpretation_list record.new_entity_instance_list record.new_context_instance_list record.new_eval_instance_list
;;


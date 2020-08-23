
(* C:\Users\user\Desktop\Projectdocs\prog\paramstyle4.ml *)

(**Assuming the factors that effect parameter passing include entity passed, context, evaluation and typing *)

type entity_type =   (*the variouse types of entities *)
Entity_type of int       (*each integer corresponds to a type of entity*)   

type context_type =  (*the various types of context*)
Context_type of int         
     

type evaluation_strat =    (*the various evaluation strategies*)
Evaluation_strat of int                 


type typing =               (*the various types*)
Correct_type of int        
 

 type passing_style =            (*various passing styles*)
 Passing_style of string

 let get_style nm =    (*getting the passing style*)
 match nm with 
 Passing_style a -> a  
 
 let setName name = 
 let n = Passing_style name in 
 match n with 
 Passing_style name -> Passing_style name
 
 let rec removal passing_type styles = 
 let s = Passing_style passing_type in 
 match styles with 
 | [] -> []
 | h :: t -> if h = s 
 then t else h :: removal passing_type t 

  
let setEnt ent =
let c = Entity_type ent in
 match c with 
|Entity_type ent -> Entity_type ent

let setContext cont = 
let con = Context_type cont in 
match con with 
|Context_type cont -> Context_type cont

let setEval_strat evl = 
let ev = Evaluation_strat evl in 
match ev with 
|Evaluation_strat evl -> Evaluation_strat evl

let setTyping typ = 
let typx = Correct_type typ in 
match typx with 
|Correct_type typ -> Correct_type typ

let rec insert_style user_styles x y =
				match user_styles with 
				[] -> [y]
				|h :: t ->if h = y then [h] else insert_style t x y
				
let rec insert_new_style user_styles x y =
				match user_styles with 
				[] -> [y]
				|h :: t ->if h = y then user_styles else h :: insert_new_style t x y				

type factors = {fac1 : entity_type; fac2 : context_type; fac3 : evaluation_strat; fac4 : typing}

type style_factors = {name : passing_style; id : string; factor : factors}

let user_styles = [{name = Passing_style "Pass by Value"; id = "1111";
  factor =
   {fac1 = Entity_type 1; fac2 = Context_type 1; fac3 = Evaluation_strat 1;
    fac4 = Correct_type 1}}; 
	{name = Passing_style "Pass by Reference"; id = "1233";
  factor =
   {fac1 = Entity_type 1; fac2 = Context_type 2; fac3 = Evaluation_strat 3;
    fac4 = Correct_type 3}}; 
	{name = Passing_style "Pass by Name"; id = "2222";
  factor =
   {fac1 = Entity_type 2; fac2 = Context_type 2; fac3 = Evaluation_strat 2;
    fac4 = Correct_type 2}}; 
	{name = Passing_style "Pass by Copy_restore"; id = "3333";
  factor =
   {fac1 = Entity_type 3; fac2 = Context_type 3; fac3 = Evaluation_strat 3;
    fac4 = Correct_type 3}}; {name = Passing_style "Pass by Need"; id = "1234";
  factor =
   {fac1 = Entity_type 1; fac2 = Context_type 2; fac3 = Evaluation_strat 3;
    fac4 = Correct_type 4}} ] 
	
let sanitize a b c d user_styles  =  
 
	let e = setEnt a  and 
	 co = setContext b and 
	 el = setEval_strat c  and 
	 ty = setTyping d 

in 
match
{fac1 = e ; fac2 = co; fac3=el; fac4=ty} with
{fac1 = x ; fac2 = y; fac3=z; fac4=q} -> let paramstyle2 x y z q =   
  match x,y,z,q with      
  (Entity_type 1,Context_type 1,Evaluation_strat 1,Correct_type 1) -> user_styles
  |(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 3) -> user_styles																										
  |(Entity_type 2,Context_type 2,Evaluation_strat 2,Correct_type 2) -> user_styles 		 
  |(Entity_type 3,Context_type 3,Evaluation_strat 3,Correct_type 3) -> user_styles
|(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 4) -> user_styles																				
  |(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let new_style = 
																			let concat_id = Printf.sprintf "%d%d%d%d" a b c d
																			in 
																			let () = print_string "Enter style name: " in
																				let name =  read_line()																				
																				in 					
																				let  x = {fac1 = setEnt a; fac2 = setContext b; fac3 = setEval_strat c; fac4 = setTyping d}
																				in 
																				let y = {name = (setName name); id = concat_id; factor = x }
																				in
																				let () = print_endline("passing style is :" ^ name)
																				in
																				let print_fact x = 
																				match x with 
																				x -> x.fac1
																				in
																				(*let f = fun x -> x in f y*)																		
																			insert_new_style user_styles x y 	
																			in new_style 
  in paramstyle2 x y z q 
;;

let paramstyle a b c d user_styles  =  
 
	let e = setEnt a  and 
	 co = setContext b and 
	 el = setEval_strat c  and 
	 ty = setTyping d 

in 
match
{fac1 = e ; fac2 = co; fac3=el; fac4=ty} with
{fac1 = x ; fac2 = y; fac3=z; fac4=q} -> let paramstyle2 x y z q =   
  match x,y,z,q with      
  (Entity_type 1,Context_type 1,Evaluation_strat 1,Correct_type 1) -> let rec get_style_list =  
																		 let concat_id = Printf.sprintf "%d%d%d%d" a b c d
																		 in 
																				let name = "Pass by Value" 																				
																				in					
																				let  x = {fac1 = setEnt a; fac2 = setContext b; fac3 = setEval_strat c; fac4 = setTyping d}
																				in 
																				let y = {name = (setName name); id = concat_id; factor = x }
																				in
																				 let () = print_endline("passing style is: " ^ name)
																				in
																				(*let f = fun x -> x in f y*)
																				insert_style user_styles x y 
																			in get_style_list
  |(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 3) -> let rec get_style_list  =  
																		let concat_id = Printf.sprintf "%d%d%d%d" a b c d
																		 in 
																				let name = "Pass by Reference" 
																				in																			
																				let  x = {fac1 = setEnt a; fac2 = setContext b; fac3 = setEval_strat c; fac4 = setTyping d}
																				in 
																				let y = {name = (setName name); id = concat_id; factor = x }
																				in
																				 let () = print_endline("passing style is: " ^ name)
																				in
																				(*let f = fun x -> x in f y*)
																				insert_style user_styles x y 
																			in get_style_list 																												
  |(Entity_type 2,Context_type 2,Evaluation_strat 2,Correct_type 2) -> let rec get_style_list  =  
																		let concat_id = Printf.sprintf "%d%d%d%d" a b c d
																		 in 
																				let name = "Pass by Name" 
																				in
																				let  x = {fac1 = setEnt a; fac2 = setContext b; fac3 = setEval_strat c; fac4 = setTyping d}
																				in 
																				let y = {name = (setName name); id = concat_id; factor = x }
																				in
																			    let () = print_endline("passing style is: " ^ name)
																				in 
																				(*let f = fun x -> x in f y*)
																				insert_style user_styles x y
																			in get_style_list 		 
  |(Entity_type 3,Context_type 3,Evaluation_strat 3,Correct_type 3) -> let rec get_style_list  =  
																		let concat_id = Printf.sprintf "%d%d%d%d" a b c d
																		 in 
																				let name = "Pass by Copy_restore" 
																				in																			
																				let  x = {fac1 = setEnt a; fac2 = setContext b; fac3 = setEval_strat c; fac4 = setTyping d}
																				in 
																				let y = {name = (setName name); id = concat_id; factor = x }
																				in
																				let () = print_endline("passing style is :" ^ name)
																				in
																				(*let f = fun x -> x in f y*)
																				insert_style user_styles x y
																			in get_style_list 
|(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 4) -> let rec get_style_list  =  
																		let concat_id = Printf.sprintf "%d%d%d%d" a b c d
																		 in 
																				let name = "Pass by Need" 
																				in																			
																				let  x = {fac1 = setEnt a; fac2 = setContext b; fac3 = setEval_strat c; fac4 = setTyping d}
																				in 
																				let y = {name = (setName name); id = concat_id; factor = x }
																				in
																				 let () = print_endline("passing style is: " ^ name)
																				in
																				(*let f = fun x -> x in f y*)
																				insert_style user_styles x y
																			in get_style_list 																					
  |(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let new_style = 

																				let  x = {fac1 = setEnt a; fac2 = setContext b; fac3 = setEval_strat c; fac4 = setTyping d}
																				in 
																				let y = {name = (setName name); id = concat_id; factor = x }
																				in															
																				(*let f = fun x -> x in f y*)																		
																			insert_style user_styles x y 	
																			in new_style 
  in paramstyle2 x y z q 
;;
print_string "Enter value for the first factor"
let a =  read_int();; 
print_string "Enter value for the second factor"
let	b = read_int() ;;
print_string "Enter value for the third factor"
let	c = read_int() ;;
print_string "Enter value for the fouth factor"
let	d = read_int();;

let styles = sanitize a b c d user_styles
let values = paramstyle a b c d styles

;;
print_string "Enter value for the first factor"
let a =  read_int();; 
print_string "Enter value for the second factor"
let	b = read_int() ;;
print_string "Enter value for the third factor"
let	c = read_int() ;;
print_string "Enter value for the fouth factor"
let	d = read_int();;

let styles = sanitize a b c d styles
let values = paramstyle a b c d styles

;;









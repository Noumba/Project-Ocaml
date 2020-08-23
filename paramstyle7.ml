 
 
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

 let get_style styless =    (*getting the passing style*)
 match styless with 
 Passing_style a -> a  
 
 let setName name = 
 let n = Passing_style name in 
 match n with 
 Passing_style name -> Passing_style name
 
 let rec insert new_passing_style styles = 
 let s = Passing_style new_passing_style in
 match styles with 
 | [] -> [s]
 | h :: t -> if h = s  
 then styles else h :: insert new_passing_style t 
 
 let rec removal passing_type styles = 
 let s = Passing_style passing_type in 
 match styles with 
 | [] -> []
 | h :: t -> if h = s 
 then t else h :: removal passing_type t 

  
let setEnt ent =
let c = Entity_type ent in
 match c with 
Entity_type 1 -> c
|Entity_type 2 -> c
|Entity_type 3 -> c
|Entity_type ent -> Entity_type ent

let setContext cont = 
let con = Context_type cont in 
match con with 
Context_type 1 -> con 
|Context_type 2 -> con 
|Context_type 3 -> con
|Context_type cont -> Context_type cont

let setEval_strat evl = 
let ev = Evaluation_strat evl in 
match ev with 
Evaluation_strat 1 -> ev
|Evaluation_strat 2 -> ev
|Evaluation_strat 3 -> ev 
|Evaluation_strat evl -> Evaluation_strat evl

let setTyping typ = 
let typx = Correct_type typ in 
match typx with 
Correct_type 1 -> typx
|Correct_type 2 -> typx 
|Correct_type 3 -> typx
|Correct_type typ -> Correct_type typ

 
 
 
 let rec insert_new_style user_styles x y =
				match user_styles with 
				[] -> [y]
				|h :: t ->if h = y then user_styles else h :: insert_new_style t x y
				
				
 type factor = {fac1 : int; fac2 : int; new_fac: string}
 type style_fac = {id : int; name : factor}
 let  u = {id = 1; name = {fac1 = 1; fac2 = 2; new_fac="sawws"}}
 let user_styles = [u]
 let time = 

				(*let concat_id = Printf.sprintf "%d%d%d%d" 1 2 3 4
				in 
				let () = print_string "Enter style name" in
					let name = read_line() in *)

					let  x = {fac1 = 1; fac2 = 2; new_fac = "dddw"}
					in 
					let y = {id = 1 ; name = x}
					in
					let new_fac = "me" in
					let new_fac_value = "Leonardo" in
				    let ttt x new_fac new_fac_value = fun x new_fac new_fac_value -> {x with new_fac = new_fac_value} in ttt x new_fac
;;																			
time
x new_fac new_fac_value
;;																		
																			
																			
																			
																			
																			
																			
																			
																			
																			
																			

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
 
 let styles = [Passing_style "Pass by value"; Passing_style "Pass by reference"; Passing_style "Pass by Name"; Passing_style "Pass by Copy-Restore"; Passing_style "Pass by Need" ]  (*array of passing styles*)

 let get_style styless =    (*getting the passing style*)
 match styless with 
 Passing_style a -> a  
 
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

type style_factors = {fac1 : entity_type; fac2 : context_type; fac3 : evaluation_strat; fac4 : typing}

let paramstyle a b c d  =  
 
	let e = setEnt a  and 
	 co = setContext b and 
	 el = setEval_strat c  and 
	 ty = setTyping d 

in 
match
{fac1 = e ; fac2 = co; fac3=el; fac4=ty} with
{fac1 = x ; fac2 = y; fac3=z; fac4=q} -> let paramstyle2 x y z q =   
  match x,y,z,q with      
  (Entity_type 1,Context_type 1,Evaluation_strat 1,Correct_type 1) -> let rec get_style_list styles a =  
																		match styles with 
																			|[] -> "Empty List"
																			|h::t -> if a = 1 then (get_style h) else get_style_list t (a-1)
																			in get_style_list styles a 
  |(Entity_type 1,Context_type 2,Evaluation_strat 3,Correct_type 3) -> let rec get_style_list styles a =  
																		match styles with 
																			|[] -> "Empty List"
																			|h::t -> if a = 1 then (get_style h) else get_style_list t (a-1)
																			in get_style_list styles a 																												
  |(Entity_type 2,Context_type 2,Evaluation_strat 2,Correct_type 2) -> "pass3" 
  |(Entity_type 3,Context_type 3,Evaluation_strat 3,Correct_type 3) -> "pass4"
  |(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> "Define ur passing style" 
  
  
  in paramstyle2 x y z q 
;;
print_string "Enter value for the first factor"
let a =  read_int();; 
print_string "Enter value for the first factor"
let	b = read_int() ;;
print_string "Enter value for the first factor"
let	c = read_int() ;;
print_string "Enter value for the first factor"
let	d = read_int();;

paramstyle a b c d
;;





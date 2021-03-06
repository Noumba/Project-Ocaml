(* C:\Users\user\Desktop\Projectdocs\prog\paramstyle3.ml *)

(**Assuming the factors that effect parameter passing include entity passed, context, evaluation and typing *)


type entity_type =   (*defining a new type to hold various types of entities which are assumed to be of type int *)
Value of int          (*We use uniion type because we want to hold only one type of entity for each evaluation. *)
|Expr of int 
|Ref of int 
|Den of int 
|Cont of int 
|Env of int ;;

type entity = { tag : int;  (*creating a record with two fields tag field for disjoint unioin  and entitypassed*)
entitypassed : entity       (*creating factor entitypassed of type entity *)
} ;;

type context_type =  (*defining a new type for holding context types*)
Called of int         (*refers to context of called procedure and we assume is of type int *)
|Calling of int ;;     (*refers to context of calling procedure and we assume is of type int *)

type rec_context =         (* defining a record for holding the context of each passing style *)
{  context : context_type   (*variable that represents the context of a passing style which is of type context_type *)
};;

type evaluation_strat =     (*defining a new type for holding the various types of evaluation strategy which are strict and lazy *)
Strict of int                 (*We assume they are of type int *)
|Lazy of int ;;

type valuation =            (* definfing  a new record that holds the evaluation factor *)
{ eval : evaluation_strat    (*evaluation factor *)
};;

type typing =               (*union for holding values of the typing factor *)
Correct_type of int           (*assume to be of type int *)
|Non of int ;;

type test_type =             (*record for holding the typing factor *)
{ types : typing
};;

let paramstyle entitypassed context eval types =   (*method paramstyle that prints a passing style depending on the values of the combination of the four factors assumed *)
  match (entitypassed,context,eval,types) with      (*matching values of factors with sets of number patterns which are constants *)
  (1,1,1,1) -> "passing style is Style1"            (*For all factors value 1, passing style is Style1 *)
  |(1,1,1,2) -> "passing style is Style2"
  |(1,1,2,2) -> "passing style is Style3"
  |(2,1,1,2) -> "passing style is Style4"
  |(2,2,1,2) -> "passing style is Style5"
  |(3,1,1,2) -> "passing style is Style6"
  |(3,3,1,2) -> "passing style is Style7"
  |(4,1,3,2) -> "passing style is Style8"
  |(4,2,1,2) -> "passing style is Style9"
  |(3,4,1,2) -> "passing style is Style10"
  |(1,2,3,2) -> "passing style is Style11"
  |(1,2,3,4) -> "passing style is Style12"
  |(1,3,1,2) -> "passing style is Style13"
  |(_,_,_,_) -> "passing style is Style100";;     (*Default case *)












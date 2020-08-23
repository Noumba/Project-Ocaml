(* C:\Users\user\Desktop\Projectdocs\prog\paramstyle2.ml *)
let paramstyle a b c d =         (*procedure paramstyle, values of a,b,c,d are assumed to be of type int  *)
match (a,b,c,d) with              (* a, b, c, and d are factors that effect parameter passing *)
(1,1,1,1) -> "parameter passing style is Style1"  (*Printing out a parameter passing style base on the values of the assume factors *)
|(1,2,2,1) -> "parameter passing style is Style2"
|(1,2,2,2) -> "parameter passing style is Style3"
|(1,1,2,2) -> "parameter passing style is Style4"
|(1,1,1,2) -> "parameter passing style is Style5"
|(2,2,2,1) -> "parameter passing style is Style6"
|(2,1,2,1) -> "parameter passing style is Style7"
|(2,2,2,3) -> "parameter passing style is Style8"
|(3,1,2,1) -> "parameter passing style is Style9"
|(3,2,2,1) -> "parameter passing style is Style10"
|(3,2,2,2) -> "parameter passing style is Style11"
|(_,_,_,_) -> "parameter passing style is Style100";;  (*Default case *)
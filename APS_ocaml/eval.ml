type injonctions = INZ of int | INF of expr * string list * ident list | INFR of value
and ident = Pair of string * value

let integer i = 
    match i with
    INF


let eval_unary operator e1 = 
    match operator with
    Not -> if (integer(e1) && e1==0) then 1
        else if (integer(e1) && e1==1) then 0
            else "ERROR PAS LES BONS ARGUMEMENTS !"

Let eval_binary operator e1 e2 =
    match operator with
    EQ -> 
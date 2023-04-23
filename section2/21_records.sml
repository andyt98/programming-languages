(* {bar:int * bool, baz:bool * int, foo:int} *)
val x = {bar=(1 + 2, true andalso true) , foo= 3 + 4 , baz=(false, 9) };

(* {id:int, name:string} *)
val my_niece = {name = "Amelia", id = 41123 - 12}

val my_niece_id = #id my_niece

val brain_part = {id = true, ego = false, superego = false}
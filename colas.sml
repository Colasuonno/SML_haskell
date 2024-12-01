(* L'env è 

    [
        (Var, Val) // (x, 10)
        (Var, Func) // (fact, (n) => ..... )
    ]

    Var = K of string
    Val = HskVal of HskType

 *)
type HskVar = string


datatype HskType =
    HskInt of int

datatype HskValue =
    HskType of HskType |
    Variable of HskVar |
    Bind of HskVar * HskValue |
    Plus of HskValue * HskValue |
    Value of HskType 


datatype EntryEnv = EntryVariable of HskVar * HskValue




fun eval(HskType value, _) = SOME value |
   eval(Bind(var, value), environment) = eval(value, EntryVariable(var, value) :: environment ) |
   eval(Plus(a,b), environment) = 
    (case (eval(a, environment), eval(b, environment)) of
       (SOME (HskInt a1), SOME (HskInt b1)) => SOME (HskInt (a1 + b1)) |
        _ => raise Fail "No" )



val DIECI = HskType(HskInt 10)

val MY_ENV = []

val COMP = eval(
    Bind("x", Plus(DIECI, DIECI)), MY_ENV
)
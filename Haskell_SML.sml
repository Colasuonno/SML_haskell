datatype HskType = 
    HskInt of int |
    HskBool of bool |
    HskChar of char

type String = string
type Env = (String  * HskType) list;

fun gen_env() : Env = []

val GLOBAL_ENV : Env = gen_env() 

datatype HskExpr =
    HskType of HskType |
    V of String  | 
    Plus of HskExpr * HskExpr |
    Times of HskExpr * HskExpr |
    If of HskExpr * HskExpr * HskExpr |
    Eq of HskExpr * HskExpr |
    Bind of String * HskType 

(*Let x = 1 in scope*)



fun set_var (e : Env, name: String, value: HskType) : Env = (name, value) :: e

fun get_var(e : Env, name: String) : HskType option = 
    case e of 
        [] => NONE
    | (n,v) :: tail =>
        if n = name then SOME v
        else get_var(tail, name)

fun num_i(n : int) = HskType( HskInt n)

fun var(v : string) = V (v)

fun eval(HskType a, _) = a
    | eval(V var, env) =
        (case get_var(env,var) of
            SOME v => v
            | NONE => raise Fail "no"
        ) 
    | eval(Eq(a,b), env) =
        (case (eval(a, env), eval(b, env)) of
            (HskInt a1, HskInt b1) => if Int.compare(a1, b1) = EQUAL then HskBool(true) else HskBool(false)
            | _  => raise Fail "No")
    | eval(If(se,allora,altrimenti), env) = 
        (case (eval(se, env)) of
            (HskBool res) => if res = true then eval(allora, env) else eval(altrimenti, env)
            | _ => raise Fail "No")
    | eval(Plus(m,n), env) = 
        (case(eval (m, env), eval (n,env)) of
            (HskInt m1, HskInt n1) => HskInt(m1 + n1)
            | _ => raise Fail "No")
    | eval(Times(m,n), env) = 
        (case(eval (m, env), eval (n,env)) of
            (HskInt m1, HskInt n1) => HskInt(m1 * n1)
            | _ => raise Fail "No")
    | eval(Bind(var,value), env) =
        let
            val new_env = set_var(env, var, value)
        in
            eval(HskType(value),new_env)
        end
    

val one = eval (num_i(1), GLOBAL_ENV)

val MY_ENV = set_var(GLOBAL_ENV, "a", one);

val try_if = eval( If( Eq(var("a"), num_i(4)), Times(num_i(4), num_i(4)), Plus(num_i(1), num_i(1))), MY_ENV)


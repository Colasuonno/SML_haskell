signature SML_HASKELL  = 
sig 

end



(* lazy fact

    lazy_fact(0) = 1
    lazy_fact(1) = [*, 1, lazy_fact(0)]
    lazy_fact(2) = [*, 2, [*, 1, lazy_fact(0)]]

 *)


datatype 'a lazylist = Nil | Cons of 'a * (unit -> 'a lazylist);


(* Lazy fib non funziona*)
fun lazy_fib (1) = Cons(1, fn() => Nil) |
    lazy_fib(n) = 
        let 
            val Cons( prev_val, tail) = lazy_fib(n-1)
        in 
            Cons( n + prev_val, fn () => Cons(prev_val, tail)  )
        end;







fun lazy_fact_steps_with_results 0 = Cons (1, fn () => Nil)
  | lazy_fact_steps_with_results n = 
      let
          val Cons (prev_result, tail) = lazy_fact_steps_with_results (n-1)
      in
          Cons (n * prev_result, fn () => Cons (prev_result, tail))
      end;

fun eval_lazy_steps Nil = []  
  | eval_lazy_steps (Cons (x, xs)) = 
      x :: eval_lazy_steps (xs ()); 

fun lazy_fact n = eval_lazy_steps (lazy_fact_steps_with_results n);
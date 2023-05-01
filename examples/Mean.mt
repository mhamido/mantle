module Mean with

fun mean (a: Int) (b: Int): Int =
  let 
     fun start (a: Int) (b: Int): Int = 
        let val sum:  Int = 0 
            val size: Int = 0     
            val i:    Int = a     
            val lt:  Bool = a < b 
        in if lt then 
           loop sum size i 
        else 
           exit sum size i

     fun loop (sum: Int) (size: Int) (i: Int): Int = 
        let val sum:  Int = sum + i
            val size: Int = size + 1
            val i:    Int = i + 1
            val t1:   Bool = a < b
        in if lt then
           loop sum size i
        else
           exit sum size i

     fun exit (sum: Int) (size: Int) (i: Int): Int = 
        let val t2 = sum / size in t2
  in 
     start a b


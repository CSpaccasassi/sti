let coord = 
  fix coord (z) => 
    let p1 = acc-swap () in  
    let v1 = recv p1     in  
    let p2 = acc-swap () in 
    let v2 = recv p2     in
    send (p2, v1); 
    send (p1, v2); 
    coord ()
in let swap = 
    fun x => 
      let p = req-swap () in 
      send (p, x); recv p  
in spawn coord; 
   spawn (fun z => swap 1); 
   spawn (fun z => swap 2)
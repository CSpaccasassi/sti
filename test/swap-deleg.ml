let coord = 
  fix coord (z) =>  
    let p1 = acc-swap () in 
    sel-SWAP p1;
    let p2 = acc-swap () in 
    sel-LEAD p2; 
		deleg (p2, p1); 
		coord () 
in 
let swap = 
  fun x => 
    let p = req-swap () in  
    case p { 
        SWAP: send (p, x); recv p 
      | LEAD: let q = resume p in 
              let y = recv q in  
              send (q, x); y 
                      } 
in spawn coord;
	spawn (fun z => swap 1);
  spawn (fun z => swap 2)
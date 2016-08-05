let process = fun x => x in 
  let coord = (fix coord (z) => 
    let p = acc-db () in 
    let loop = fix loop(z) => 
      case p { 
        QRY: let sql = recv p in 
             let res = process sql in 
             send (p, res); 
             loop () 
        | END: () } 
    in spawn coord; loop ()) 
in spawn coord; 
let clientInit = 
  fun z => 
    let con = req-db () in
    let f1 = (fun sql => 
                sel-QRY con; 
                send (con, sql); 
                recv con) in
    let f2 = (fun z => sel-END con) in 
    (f1, f2) 
in
let dbInit = clientInit () in
let qry    = fst dbInit in
let close  = snd dbInit in 
  qry 1; 
  qry 2; 
  close ()
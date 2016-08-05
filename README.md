# Session Types Inference tool
## Synopsis
This project implements the inference algorithm for finite session types in MLs, an ML-like concurrent functional language, as described in [1]. A session type describes the protocol according to which two processes interact at the two endpoints of a channel. 
Combining and extending well-known techniques [2, 3], we define a type-based static analysis to discover the session types associated to each channel in  unannotated MLs code. Our framework separates the ML type system from the typing of sessions; as a result the inference is performed in two steps. The first step infers the ML type of the source code, together with its communication effects [2]. The second phase finds the protocols that the MLs code's communication effects obey, if any exist. In addition to this, our tool also guarantees partial lock freedom.

## Code Example
The following is the peer-to-peer swap program presented in Sec. 2.3 of [1], which is in ./test/swap-deleg.sml:
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

Running our analysis tool with the options "-s":
```
./SessionTypeInference -h -s -f test/swap-deleg.ml
```
the result is:
```
Session types: 
Cswap1' ~ (+) {$lLEAD$; ! (? int ! int end) end, $lSWAP$; ? int ! int end},
Cswap2' ~ (+) {$lLEAD$; ! (? int ! int end) end, $lSWAP$; ? int ! int end},
Cswap3 ~ + {$lLEAD$; ? (? int ! int end) end, $lSWAP$; ! int ? int end} {}
```

where the session type associated to the "swap" channel is ..., as expected. More explanations can be found in [1].
All labels are enclosed by dollar ($) signs and prepended with the letter 'l'. The three constraints on labels Cswap1', Cswap2' and Cswap3 all refer to the same channel named "swap" in the source code; the session type in Cswap3 is the dual of the session type Cswap1' and Cswap2' (the apostrophe indicates duality). The empty braces in Cswap3 indicate that there are no inactive labels in the internal choice '+', as per [1] (the symbol '(+)' indicates the internal choice).

## Installation
The project can be built using Cabal as follows:
```
cabal configure
cabal build
```

## References
[1] C. Spaccasassi, V. Koutavas, "Type-Based Analysis for Session Inference (Extended Abstract)". FORTE 2016

[2] T. Amtoft, H. R. Nielson, F. Nielson: "Type and effect systems - behaviours for concurrency". Imperial College Press 1999

[3] G. Castagna, M. Dezani-Ciancaglini, E. Giachino, L. Padovani. "Foundations of session types". PPDP 2009: 219-230


## License
MIT License

Copyright (c) 2016 Carlo Spaccasassi

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

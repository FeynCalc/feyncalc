##  FCLoopBasisIntegralToPropagators 

FCLoopBasisIntegralToPropagators[int, {q1, q2, ...}] is an auxiliary function that converts the loop integral int that depends on the loop momenta q1, q2, ... to a list of propagators and scalar products. All propagators and scalar products that do not depend on the loop momenta are discarded, unless the Rest option is set to True..

###  Examples 

```mathematica
SFAD[p1] 
 
FCLoopBasisIntegralToPropagators[%, {p1}] 
 
SFAD[p1, p2] 
 
FCLoopBasisIntegralToPropagators[%, {p1, p2}] 
 
SPD[q, p] SFAD[q, q - p, q - p] 
 
FCLoopBasisIntegralToPropagators[%, {q}]
```

$$![05h81y196ibaj](img/05h81y196ibaj.png)$$

$$![14puvat5dwhz3](img/14puvat5dwhz3.png)$$

$$![1htwv9io5o929](img/1htwv9io5o929.png)$$

$$![059iex2rxo0vc](img/059iex2rxo0vc.png)$$

$$![1fgq8iiziefty](img/1fgq8iiziefty.png)$$

$$![10459cnec7htj](img/10459cnec7htj.png)$$
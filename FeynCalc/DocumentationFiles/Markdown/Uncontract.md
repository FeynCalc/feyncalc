##  Uncontract 

Uncontract[exp, q1, q2, ...] uncontracts Eps and DiracGamma. Uncontract[exp, q1, q2, Pair -> {p}] uncontracts also p.q1 and p.q2; the option Pair -> All uncontracts all momenta except OPEDelta..

###  See also 

Contract.

###  Examples 

```mathematica
 LC[\[Mu], \[Nu]][p, q] 
  
  Uncontract[%, p] 
  
  GS[p] 
  
  Uncontract[%, p] 
  
  Uncontract[LC[\[Mu], \[Nu]][p, q], p, q]
```

$$\bar{\epsilon }^{\mu \nu \overline{p}\overline{q}}$$

$$\overline{p}^{\text{$\$$AL}(\text{$\$$24})} \bar{\epsilon }^{\mu \nu \text{$\$$AL}(\text{$\$$24})\overline{q}}$$

$$\bar{\gamma }\cdot \overline{p}$$

$$\bar{\gamma }^{\text{$\$$AL}(\text{$\$$25})} \overline{p}^{\text{$\$$AL}(\text{$\$$25})}$$

$$\overline{p}^{\text{$\$$AL}(\text{$\$$27})} \overline{q}^{\text{$\$$AL}(\text{$\$$26})} \left(-\bar{\epsilon }^{\mu \nu \text{$\$$AL}(\text{$\$$26})\text{$\$$AL}(\text{$\$$27})}\right)$$

By default scalar products are not uncontracted.

```mathematica
Uncontract[SP[p, q], q]
```

$$\overline{p}\cdot \overline{q}$$

With the option PairAll they are “uncontracted ”.

```mathematica
Uncontract[SP[p, q], q, Pair -> All] 
 
Uncontract[SP[p, q]^2, q, Pair -> All]
```

$$\overline{p}^{\text{$\$$AL}(\text{$\$$28})} \overline{q}^{\text{$\$$AL}(\text{$\$$28})}$$

$$\overline{p}^{\text{$\$$AL}(\text{$\$$29})} \overline{p}^{\text{$\$$AL}(\text{$\$$30})} \overline{q}^{\text{$\$$AL}(\text{$\$$29})} \overline{q}^{\text{$\$$AL}(\text{$\$$30})}$$
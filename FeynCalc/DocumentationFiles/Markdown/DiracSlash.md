##  DiracSlash 

DiracSlash[p] is the contraction $p^{\mu }\gamma _{\mu  }$(FV[p, $\mu$] GA[$\mu$]). Products of those can be entered in the form GS[p1, p2, ...].The shortcut DiracSlash is deprecated, please use GS instead!.

###  See also 

GS, FCI.

###  Examples 

This is q-slash, i.e., $gamma ^{mu }q_{mu }.$

```mathematica
DiracSlash[q] 
 
DiracSlash[p] . DiracSlash[q] 
 
DiracSlash[p, q]
```

$$\bar{\gamma }\cdot \overline{q}$$

$$\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right)$$

$$\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right)$$

DiracSlash is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use GS.

```mathematica
GS[p] 
 
GSD[p] 
 
FCI[GS[p]] === DiracSlash[p] 
 
FCI[GSD[p]] === DiracSlash[p, Dimension -> D]
```

$$\bar{\gamma }\cdot \overline{p}$$

$$\gamma \cdot p$$

$$\text{True}$$

$$\text{True}$$
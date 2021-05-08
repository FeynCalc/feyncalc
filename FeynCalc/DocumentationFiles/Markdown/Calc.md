##  Calc 

Calc[exp]  performs several simplifications that involve Contract, DiracSimplify SUNSimplify, DotSimplify, EpsEvaluate, ExpandScalarProduct, PowerSimplify, Expand2 and Trick..

###  See also 

Trick, DiracSimplify, DiracTrick.

###  Examples 

This calculates $gamma ^{mu }gamma _{mu }$ in $4$ dimensions and $g_{nu }^{nu }$ in $text{D}$ dimensions.

```mathematica
Calc[GA[\[Mu], \[Mu]]] 
 
Calc[ MTD[\[Nu], \[Nu]]]
```

$$4$$

$$D$$

This simplifies $f_{text{abc}} f_{text{abe}}.$

```mathematica
Calc[SUNF[a, b, c] SUNF[a, b, e]] 
 
FV[p + r, \[Mu]] MT[\[Mu], \[Nu]] FV[q - p, \[Nu]] 
 
Calc[%] 
 
GluonVertex[{p, li1}, {q, li2}, {-p - q, li3}] 
 
Calc[% FVD[p, li1] FVD[q, li2] FVD[-p - q, li3]]
```

$$C_A \delta ^{ce}$$

$$\bar{g}^{\mu \nu } \left(\overline{q}-\overline{p}\right)^{\nu } \left(\overline{p}+\overline{r}\right)^{\mu }$$

$$\overline{p}\cdot \overline{q}-\overline{p}\cdot \overline{r}-\overline{p}^2+\overline{q}\cdot \overline{r}$$

$$V^{\text{li1}\text{li2}\text{li3}}(p\text{, }q\text{, }-p-q)$$

$$0$$
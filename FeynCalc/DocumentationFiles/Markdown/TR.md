##  TR 

TR[exp] calculates the Dirac trace of exp. Depending on the setting of the option SUNTrace also a trace over SU(N) objects is performed. The Mathematica build-in function Tr is overloaded to call TR if any of DiracGamma, GA, GAD, GS or GSD are in the expression.Tr[list] finds the trace of the matrix or tensor list. Tr[list, f] finds a generalized trace, combining terms with f instead of Plus. Tr[list, f, n] goes down to level n in list.Tr[ expression ] calculates the DiracTrace, i.e.,  TR[ expression ] if any of DiracGamma, GA, GAD, GS or GSD is present in expression..

###  See also 

DiracSimplify, DiracTrace, FermionSpinSum, SUNTrace.

###  Examples 

```mathematica
GA[\[Mu], \[Nu]] 
 
TR[%] 
 
TR[(GSD[p] + m) . GAD[\[Mu]] . (GSD[q] - m) . GAD[\[Nu]]] 
 
TR[GA[\[Mu], \[Nu], \[Rho], \[Sigma], 5]] 
 
TR[GS[p, q, r, s]] 
 
TR[(GS[p] + m) . GA[\[Mu]] . (GS[q] + m) . GA[\[Mu]], Factoring -> True] 
 
TR[GA[\[Alpha], \[Beta]], FCE -> True] 
 
GA[\[Mu], \[Nu]] SUNT[b] . SUNT[c] SUNDelta[c, b] 
 
TR[%, SUNTrace -> False, SUNNToCACF -> True] 
 
TR[%%, SUNTrace -> True, SUNNToCACF -> True] 
 
TR[1, SUNTrace -> False, SUNNToCACF -> True] 
 
TR[1, SUNTrace -> True, SUNNToCACF -> True] 
 
Tr[ GA[m, n]]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$4 \bar{g}^{\mu \nu }$$

$$-4 \left(m^2 g^{\mu \nu }+g^{\mu \nu } (p\cdot q)-p^{\nu } q^{\mu }-p^{\mu } q^{\nu }\right)$$

$$-4 i \bar{\epsilon }^{\mu \nu \rho \sigma }$$

$$4 \left(\left(\overline{p}\cdot \overline{s}\right) \left(\overline{q}\cdot \overline{r}\right)-\left(\overline{p}\cdot \overline{r}\right) \left(\overline{q}\cdot \overline{s}\right)+\left(\overline{p}\cdot \overline{q}\right) \left(\overline{r}\cdot \overline{s}\right)\right)$$

$$8 \left(2 m^2-\overline{p}\cdot \overline{q}\right)$$

$$4 \bar{g}^{\alpha \beta }$$

$$\delta ^{bc} T^b.T^c \bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$4 C_F \bar{g}^{\mu \nu }$$

$$4 C_F \bar{g}^{\mu \nu }$$

$$4$$

$$4$$

$$4 \bar{g}^{mn}$$
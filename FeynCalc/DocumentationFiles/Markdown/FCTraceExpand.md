##  FCTraceExpand 

FCTraceExpand[exp] expands traces of Dirac and $\text{SU}(N)$ matrices using linearity of the trace. The traces themselves are not evaluated..

###  See also 

DiracTrace, SUNTrace.

###  Examples 

```mathematica
DiracTrace[GA[\[Mu]] . (GS[p1] + m1) . GA[\[Nu]] . (GS[p2] + m2) . GA[\[Rho]] +x] 
 
FCTraceExpand[%] 
 
FCTraceExpand[%%, DotSimplify -> False] 
 
FCTraceExpand[%%%, DiracTrace -> False] 
 
a*DiracTrace[GA[\[Mu]] . (GS[p1] + m1) . GA[\[Nu]]] + b*DiracTrace[GA[\[Mu]] . (GS[p2] + m2) . GA[\[Nu]]] 
 
FCTraceExpand[%, Momentum -> {p1}]
```

$$\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}+\text{m1}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}+\text{m2}\right).\bar{\gamma }^{\rho }+x\right)$$

$$\text{m1} \text{m2} \text{tr}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }\right)+\text{m1} \text{tr}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}\right).\bar{\gamma }^{\rho }\right)+\text{m2} \text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}\right).\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }\right)+\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}\right).\bar{\gamma }^{\rho }\right)+\text{tr}(1) x$$

$$\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}+\text{m1}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}+\text{m2}\right).\bar{\gamma }^{\rho }\right)+\text{tr}(1) x$$

$$\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}+\text{m1}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}+\text{m2}\right).\bar{\gamma }^{\rho }+x\right)$$

$$a \text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}+\text{m1}\right).\bar{\gamma }^{\nu }\right)+b \text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}+\text{m2}\right).\bar{\gamma }^{\nu }\right)$$

$$a \left(\text{m1} \text{tr}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }\right)+\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}\right).\bar{\gamma }^{\nu }\right)\right)+b \text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}+\text{m2}\right).\bar{\gamma }^{\nu }\right)$$

At the moment SUNTrace automatically expands its content, so here FCTraceExpand is not needed. However, this may change in the future.

```mathematica
SUNTrace[SUNT[i, j, k] + SUNT[l, m, n]] 
 
FCTraceExpand[%] 
 
FCTraceExpand[%%, SUNTrace -> False]
```

$$\text{tr}(T^i.T^j.T^k)+\text{tr}(T^l.T^m.T^n)$$

$$\text{tr}(T^i.T^j.T^k)+\text{tr}(T^l.T^m.T^n)$$

$$\text{tr}(T^i.T^j.T^k)+\text{tr}(T^l.T^m.T^n)$$
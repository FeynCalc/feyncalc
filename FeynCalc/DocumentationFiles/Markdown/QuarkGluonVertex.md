##  QuarkGluonVertex 

QuarkGluonVertex[μ, a] gives the Feynman rule for the quark-gluon vertex. $text{QGV}$ can be used as an abbreviation of QuarkGluonVertex.The dimension and the name of the coupling constant are determined by the options Dimension and CouplingConstant..

###  See also 

GluonVertex.

###  Examples 

```mathematica
QuarkGluonVertex[\[Mu], a, Explicit -> True] 
 
QGV[\[Mu], a] 
 
Explicit[%] 
 
QuarkGluonVertex[\[Mu], a, CounterTerm -> 1, Explicit -> True] 
 
QuarkGluonVertex[\[Mu], a, CounterTerm -> 2, Explicit -> True] 
 
QuarkGluonVertex[\[Mu], a, CounterTerm -> 3, Explicit -> True] 
 
QuarkGluonVertex[{p, \[Mu], a}, {q}, {k}, OPE -> True, Explicit -> True] 
 
QuarkGluonVertex[{p, \[Mu], a}, {q}, {k}, OPE -> False, Explicit -> True]
```

$$i g_s T^a.\gamma ^{\mu }$$

$$Q_a^{\mu }$$

$$i g_s T^a.\gamma ^{\mu }$$

$$\frac{2 i g_s^3 S_n \left(C_F-\frac{C_A}{2}\right) T^a.\gamma ^{\mu }}{\varepsilon }$$

$$\frac{3 i C_A g_s^3 S_n T^a.\gamma ^{\mu }}{\varepsilon }$$

$$\frac{2 i g_s^3 S_n \left(C_A+C_F\right) T^a.\gamma ^{\mu }}{\varepsilon }$$

$$\Omega  \Delta ^{\mu } g_s (\gamma \cdot \Delta ).T^a \left(\sum _{i=0}^{-2+m} (-1)^i (k\cdot \Delta )^i (\Delta \cdot q)^{-2-i+m}\right)+i g_s T^a.\gamma ^{\mu }$$

$$i g_s T^a.\gamma ^{\mu }$$
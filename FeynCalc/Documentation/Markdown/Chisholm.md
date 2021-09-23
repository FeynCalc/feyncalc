## Chisholm

`Chisholm[exp]` substitutes products of three Dirac matrices or slashes by the Chisholm identity.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
GA[\[Mu], \[Nu], \[Rho]]
EpsChisholm[%]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }$$

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }$$

Notice that the output contains dummy indices.

```mathematica
GA[\[Alpha], \[Beta], \[Mu], \[Nu]]
Chisholm[%]
```

$$\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }.\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$i \bar{\gamma }^{\alpha }.\bar{\gamma }^{\text{\$MU}(\text{\$27})}.\bar{\gamma }^5 \bar{\epsilon }^{\beta \mu \nu \;\text{\$MU}(\text{\$27})}+\bar{\gamma }^{\alpha }.\bar{\gamma }^{\nu } \bar{g}^{\beta \mu }-\bar{\gamma }^{\alpha }.\bar{\gamma }^{\mu } \bar{g}^{\beta \nu }+\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta } \bar{g}^{\mu \nu }$$

Dummy Lorentz indices may also appear as FCGV.

```mathematica
SpinorVBar[p1, m1] . GA[\[Alpha], \[Beta], \[Mu], \[Nu]] . SpinorU[p2, m2]
Chisholm[%]
```

$$\bar{v}(\text{p1},\text{m1}).\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }.\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.u(\text{p2},\text{m2})$$

$$i \bar{\epsilon }^{\beta \mu \nu \;\text{\$MU}(\text{\$36})} \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^{\alpha }.\bar{\gamma }^{\text{\$MU}(\text{\$36})}.\bar{\gamma }^5.\left(\varphi (\overline{\text{p2}},\text{m2})\right)+\bar{g}^{\beta \mu } \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^{\alpha }.\bar{\gamma }^{\nu }.\left(\varphi (\overline{\text{p2}},\text{m2})\right)-\bar{g}^{\beta \nu } \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^{\alpha }.\bar{\gamma }^{\mu }.\left(\varphi (\overline{\text{p2}},\text{m2})\right)+\bar{g}^{\mu \nu } \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }.\left(\varphi (\overline{\text{p2}},\text{m2})\right)$$

Chisholm only works with Dirac matrices in $4$ dimensions, $D$-dimensional objects are ignored.

```mathematica
Chisholm[GAD[\[Mu], \[Nu], \[Rho]]]
```

$$\gamma ^{\mu }.\gamma ^{\nu }.\gamma ^{\rho }$$

```mathematica
Chisholm[GA[\[Alpha], \[Beta], \[Mu]]] . Chisholm[GA[\[Alpha], \[Beta], \[Mu]]]
DiracSimplify[%]
```

$$\left(i \bar{\gamma }^{\text{\$MU}(\text{\$63})}.\bar{\gamma }^5 \bar{\epsilon }^{\alpha \beta \mu \;\text{\$MU}(\text{\$63})}+\bar{\gamma }^{\mu } \bar{g}^{\alpha \beta }-\bar{\gamma }^{\beta } \bar{g}^{\alpha \mu }+\bar{\gamma }^{\alpha } \bar{g}^{\beta \mu }\right).\left(i \bar{\gamma }^{\text{\$MU}(\text{\$72})}.\bar{\gamma }^5 \bar{\epsilon }^{\alpha \beta \mu \;\text{\$MU}(\text{\$72})}+\bar{\gamma }^{\mu } \bar{g}^{\alpha \beta }-\bar{\gamma }^{\beta } \bar{g}^{\alpha \mu }+\bar{\gamma }^{\alpha } \bar{g}^{\beta \mu }\right)$$

$$16$$

```mathematica
Chisholm[GA[\[Alpha], \[Beta], \[Mu], \[Nu]]] . Chisholm[GA[\[Alpha], \[Beta], \[Mu], \[Nu]]]
DiracSimplify[%]
```

$$\left(i \bar{\gamma }^{\alpha }.\bar{\gamma }^{\text{\$MU}(\text{\$86})}.\bar{\gamma }^5 \bar{\epsilon }^{\beta \mu \nu \;\text{\$MU}(\text{\$86})}+\bar{\gamma }^{\alpha }.\bar{\gamma }^{\nu } \bar{g}^{\beta \mu }-\bar{\gamma }^{\alpha }.\bar{\gamma }^{\mu } \bar{g}^{\beta \nu }+\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta } \bar{g}^{\mu \nu }\right).\left(i \bar{\gamma }^{\alpha }.\bar{\gamma }^{\text{\$MU}(\text{\$95})}.\bar{\gamma }^5 \bar{\epsilon }^{\beta \mu \nu \;\text{\$MU}(\text{\$95})}+\bar{\gamma }^{\alpha }.\bar{\gamma }^{\nu } \bar{g}^{\beta \mu }-\bar{\gamma }^{\alpha }.\bar{\gamma }^{\mu } \bar{g}^{\beta \nu }+\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta } \bar{g}^{\mu \nu }\right)$$

$$-128$$

```mathematica
GS[p, q, r]
Chisholm[%]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{r}\right)$$

$$-i \bar{\gamma }^{\text{\$MU}(\text{\$121})}.\bar{\gamma }^5 \bar{\epsilon }^{\text{\$MU}(\text{\$121})\overline{p}\overline{q}\overline{r}}+\left(\overline{p}\cdot \overline{q}\right) \bar{\gamma }\cdot \overline{r}-\left(\overline{p}\cdot \overline{r}\right) \bar{\gamma }\cdot \overline{q}+\bar{\gamma }\cdot \overline{p} \left(\overline{q}\cdot \overline{r}\right)$$

```mathematica
GA[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Kappa]]
Chisholm[%]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\sigma }.\bar{\gamma }^{\tau }.\bar{\gamma }^{\kappa }$$

$$i \bar{g}^{\nu \rho } \bar{\gamma }^{\mu }.\bar{\gamma }^{\text{\$MU}(\text{\$130})}.\bar{\gamma }^5 \bar{\epsilon }^{\kappa \sigma \tau \;\text{\$MU}(\text{\$130})}-i \bar{g}^{\kappa \sigma } \bar{\gamma }^{\mu }.\bar{\gamma }^{\text{\$MU}(\text{\$132})}.\bar{\gamma }^5 \bar{\epsilon }^{\nu \rho \tau \;\text{\$MU}(\text{\$132})}+i \bar{g}^{\kappa \tau } \bar{\gamma }^{\mu }.\bar{\gamma }^{\text{\$MU}(\text{\$133})}.\bar{\gamma }^5 \bar{\epsilon }^{\nu \rho \sigma \;\text{\$MU}(\text{\$133})}+i \bar{g}^{\sigma \tau } \bar{\gamma }^{\mu }.\bar{\gamma }^{\text{\$MU}(\text{\$134})}.\bar{\gamma }^5 \bar{\epsilon }^{\kappa \nu \rho \;\text{\$MU}(\text{\$134})}-i \bar{\gamma }^{\mu }.\bar{\gamma }^{\rho }.\bar{\gamma }^5 \bar{\epsilon }^{\kappa \nu \sigma \tau }+i \bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^5 \bar{\epsilon }^{\kappa \rho \sigma \tau }-\bar{\gamma }^{\mu }.\bar{\gamma }^{\tau } \bar{g}^{\kappa \sigma } \bar{g}^{\nu \rho }+\bar{\gamma }^{\mu }.\bar{\gamma }^{\sigma } \bar{g}^{\kappa \tau } \bar{g}^{\nu \rho }+\bar{\gamma }^{\mu }.\bar{\gamma }^{\tau } \bar{g}^{\kappa \rho } \bar{g}^{\nu \sigma }-\bar{\gamma }^{\mu }.\bar{\gamma }^{\rho } \bar{g}^{\kappa \tau } \bar{g}^{\nu \sigma }-\bar{\gamma }^{\mu }.\bar{\gamma }^{\sigma } \bar{g}^{\kappa \rho } \bar{g}^{\nu \tau }+\bar{\gamma }^{\mu }.\bar{\gamma }^{\rho } \bar{g}^{\kappa \sigma } \bar{g}^{\nu \tau }-\bar{\gamma }^{\mu }.\bar{\gamma }^{\tau } \bar{g}^{\kappa \nu } \bar{g}^{\rho \sigma }+\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu } \bar{g}^{\kappa \tau } \bar{g}^{\rho \sigma }+\bar{\gamma }^{\mu }.\bar{\gamma }^{\kappa } \bar{g}^{\nu \tau } \bar{g}^{\rho \sigma }+\bar{\gamma }^{\mu }.\bar{\gamma }^{\sigma } \bar{g}^{\kappa \nu } \bar{g}^{\rho \tau }-\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu } \bar{g}^{\kappa \sigma } \bar{g}^{\rho \tau }-\bar{\gamma }^{\mu }.\bar{\gamma }^{\kappa } \bar{g}^{\nu \sigma } \bar{g}^{\rho \tau }-\bar{\gamma }^{\mu }.\bar{\gamma }^{\rho } \bar{g}^{\kappa \nu } \bar{g}^{\sigma \tau }+\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu } \bar{g}^{\kappa \rho } \bar{g}^{\sigma \tau }+\bar{\gamma }^{\mu }.\bar{\gamma }^{\kappa } \bar{g}^{\nu \rho } \bar{g}^{\sigma \tau }$$

Check the equality of the expressions before and after applying `Chisholm`.

```mathematica
DiracSimplify[GA[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Kappa]] . GA[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Kappa]]]
```

$$-2048$$

```mathematica
DiracSimplify[Chisholm[GA[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Kappa]]] . Chisholm[GA[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Kappa]]]]
```

$$-2048$$

```mathematica
DiracReduce[GA[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Kappa]] . Chisholm[GA[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Kappa]]]]
```

$$-2048$$

Older FeynCalc versions had a function called `Chisholm2` that acted on expressions like $\gamma^{\mu} \gamma^{\nu} \gamma^5$. This functionality is now part of `Chisholm` and can be activated by setting the option `Mode` to `2`.

```mathematica
GA[\[Mu], \[Nu], 5]
Chisholm[%, Mode -> 2] 
  
 

```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^5$$

$$\frac{1}{2} \sigma ^{\text{\$MU}(\text{\$1027})\text{\$MU}(\text{\$1028})} \bar{\epsilon }^{\mu \nu \;\text{\$MU}(\text{\$1027})\text{\$MU}(\text{\$1028})}+\bar{\gamma }^5 \bar{g}^{\mu \nu }$$

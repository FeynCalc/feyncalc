## FeynRule

`FeynRule[lag, {fields}]` derives the Feynman rule corresponding to the field configuration `fields` of the Lagrangian `lag`.

`FeynRule` does not calculate propagator Feynman rules.

`FeynRule` is not very versatile and was primarily developed for QCD calculations. It is often more useful when dealing with bosonic fields than with fermions. If you need a more powerful and universal solution for deriving Feynman rules, have a look at the standalone Mathematica Package FeynRules (not related to FeynCalc).

### See also

[Overview](Extra/FeynCalc.md)

### Examples

$\phi ^4$ Feynman rule

```mathematica
- \[Lambda]/4! QuantumField[\[Phi]]^4 
 
FeynRule[%, {QuantumField[\[Phi]][p1], QuantumField[\[Phi]][p2], 
   QuantumField[\[Phi]][p3], QuantumField[\[Phi]][p4]}]
```

$$-\frac{\lambda  \phi ^4}{24}$$

$$-i \lambda$$

Quark-gluon vertex Feynman rule

```mathematica
I QuantumField[AntiQuarkField] . GA[\[Mu]] . CovariantD[\[Mu]] . QuantumField[QuarkField] 
 
FeynRule[%, {QuantumField[GaugeField, {\[Mu]}, {a}][p1], 
   QuantumField[QuarkField][p2], QuantumField[AntiQuarkField][p3]}]
```

$$i \bar{\psi }.\bar{\gamma }^{\mu }.D_{\mu }.\psi$$

$$i T^a g_s \bar{\gamma }^{\mu }$$

4-gluon vertex Feynman rule

```mathematica
-(1/4) FieldStrength[\[Alpha], \[Beta], i] . FieldStrength[\[Alpha], \[Beta], i] 
 
FeynRule[%, {QuantumField[GaugeField, {\[Mu]}, {a}][p1], QuantumField[GaugeField, {\[Nu]}, {b}][p2], 
    QuantumField[GaugeField, {\[Rho]}, {c}][p3], QuantumField[GaugeField, {\[Sigma]}, {d}][p4]}] 
 
GluonVertex[{p, \[Mu], a}, {q, \[Nu], b}, {r, \[Rho], c}, {s, \[Sigma], d}, Dimension -> 4, Explicit -> True] 
 
FCCanonicalizeDummyIndices[% - %%] // Factor
```

$$-\frac{1}{4} F_{\alpha \beta }^i.F_{\alpha \beta }^i$$

$$i g_s^2 \bar{g}^{\mu \rho } \bar{g}^{\nu \sigma } f^{ad\text{FCGV}(\text{sun921})} f^{bc\text{FCGV}(\text{sun921})}-i g_s^2 \bar{g}^{\mu \nu } \bar{g}^{\rho \sigma } f^{ad\text{FCGV}(\text{sun921})} f^{bc\text{FCGV}(\text{sun921})}+i g_s^2 \bar{g}^{\mu \sigma } \bar{g}^{\nu \rho } f^{ac\text{FCGV}(\text{sun921})} f^{bd\text{FCGV}(\text{sun921})}-i g_s^2 \bar{g}^{\mu \nu } \bar{g}^{\rho \sigma } f^{ac\text{FCGV}(\text{sun921})} f^{bd\text{FCGV}(\text{sun921})}+i g_s^2 \bar{g}^{\mu \sigma } \bar{g}^{\nu \rho } f^{ab\text{FCGV}(\text{sun921})} f^{cd\text{FCGV}(\text{sun921})}-i g_s^2 \bar{g}^{\mu \rho } \bar{g}^{\nu \sigma } f^{ab\text{FCGV}(\text{sun921})} f^{cd\text{FCGV}(\text{sun921})}$$

$$-i g_s^2 \left(f^{ad\text{FCGV}(\text{u96})} f^{bc\text{FCGV}(\text{u96})} \left(\bar{g}^{\mu \nu } \bar{g}^{\rho \sigma }-\bar{g}^{\mu \rho } \bar{g}^{\nu \sigma }\right)+f^{ac\text{FCGV}(\text{u96})} f^{bd\text{FCGV}(\text{u96})} \left(\bar{g}^{\mu \nu } \bar{g}^{\rho \sigma }-\bar{g}^{\mu \sigma } \bar{g}^{\nu \rho }\right)+f^{ab\text{FCGV}(\text{u96})} f^{cd\text{FCGV}(\text{u96})} \left(\bar{g}^{\mu \rho } \bar{g}^{\nu \sigma }-\bar{g}^{\mu \sigma } \bar{g}^{\nu \rho }\right)\right)$$

$$0$$

3-gluon vertex Feynman rule

```mathematica
-(1/4) FieldStrength[\[Alpha], \[Beta], i] . FieldStrength[\[Alpha], \[Beta], i] 
 
FeynRule[%, {QuantumField[GaugeField, {\[Mu]}, {a}][p], QuantumField[GaugeField, {\[Nu]}, {b}][q], 
    QuantumField[GaugeField, {\[Rho]}, {c}][r]}] 
 
GluonVertex[{p, \[Mu], a}, {q, \[Nu], b}, {r, \[Rho], c}, Dimension -> 4, Explicit -> True] 
 
ExpandScalarProduct[% - %%] // Factor
```

$$-\frac{1}{4} F_{\alpha \beta }^i.F_{\alpha \beta }^i$$

$$-g_s \overline{p}^{\nu } \bar{g}^{\mu \rho } f^{abc}+g_s \overline{p}^{\rho } \bar{g}^{\mu \nu } f^{abc}+g_s \overline{q}^{\mu } \bar{g}^{\nu \rho } f^{abc}-g_s \overline{q}^{\rho } \bar{g}^{\mu \nu } f^{abc}-g_s \overline{r}^{\mu } \bar{g}^{\nu \rho } f^{abc}+g_s \overline{r}^{\nu } \bar{g}^{\mu \rho } f^{abc}$$

$$g_s f^{abc} \left(\bar{g}^{\mu \nu } \left(\overline{p}-\overline{q}\right)^{\rho }+\bar{g}^{\mu \rho } \left(\overline{r}-\overline{p}\right)^{\nu }+\bar{g}^{\nu \rho } \left(\overline{q}-\overline{r}\right)^{\mu }\right)$$

$$0$$

Higgs EFT interaction vertex

```mathematica
heftInt = -(1/4) CH FieldStrength[mu, nu, a] . FieldStrength[mu, nu, a] . QuantumField[H]
```

$$-\frac{1}{4} \;\text{CH} F_{\text{mu}\;\text{nu}}^a.F_{\text{mu}\;\text{nu}}^a.H$$

$Hgg$ vertex Feynman rules

```mathematica
FeynRule[heftInt, {QuantumField[GaugeField, {i}, {a}][p1], QuantumField[GaugeField, 
     {j}, {b}][p2], QuantumField[H][p3]}]
```

$$i \;\text{CH} \delta ^{ab} \bar{g}^{ij} \left(\overline{\text{p1}}\cdot \overline{\text{p2}}\right)-i \;\text{CH} \overline{\text{p2}}^i \overline{\text{p1}}^j \delta ^{ab}$$

$Hggg$ vertex Feynman rules

```mathematica
FeynRule[heftInt, {QuantumField[GaugeField, {i}, {a}][p1], QuantumField[GaugeField, 
      {j}, {b}][p2], QuantumField[GaugeField, {k}, {c}][p3], QuantumField[H][p4]}] // Simplify
```

$$\text{CH} g_s f^{abc} \left(-\overline{\text{p1}}^j \bar{g}^{ik}+\overline{\text{p1}}^k \bar{g}^{ij}+\overline{\text{p2}}^i \bar{g}^{jk}-\overline{\text{p2}}^k \bar{g}^{ij}-\overline{\text{p3}}^i \bar{g}^{jk}+\overline{\text{p3}}^j \bar{g}^{ik}\right)$$

$Hgggg$ vertex Feynman rules

```mathematica
FeynRule[heftInt, {QuantumField[GaugeField, {i}, {a}][p1], QuantumField[GaugeField, {j}, 
       {b}][p2], QuantumField[GaugeField, {k}, {c}][p3], 
     QuantumField[GaugeField, {l}, {d}][p4], QuantumField[H][p5]}] // 
   FCCanonicalizeDummyIndices[#, SUNIndexNames -> {e}] & // Collect2[#, SUNF, 
    FCFactorOut -> I CH SMP["g_s"]^2] &
```

$$i \;\text{CH} g_s^2 \left(f^{ade} f^{bce} \left(\bar{g}^{ik} \bar{g}^{jl}-\bar{g}^{ij} \bar{g}^{kl}\right)+f^{ace} f^{bde} \left(\bar{g}^{il} \bar{g}^{jk}-\bar{g}^{ij} \bar{g}^{kl}\right)+f^{abe} f^{cde} \left(\bar{g}^{il} \bar{g}^{jk}-\bar{g}^{ik} \bar{g}^{jl}\right)\right)$$
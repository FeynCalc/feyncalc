## FCChargeConjugateTransposed

`FCChargeConjugateTransposed[exp]` represents the application of the charge conjugation operator to the transposed of `exp`, i.e. $C^{-1} \;\text{exp}^T C$. Here `exp` is understood to be a single Dirac matrix or a chain thereof. The option setting `Explicit` determines whether the explicit result is returned or whether it is left in the unevaluated form.The unevaluated form will be also maintained if the function does not know how to obtain $C^{-1} \;\text{exp}^T C$ from the given exp.

The shortcut for `FCChargeConjugateTransposed` is `FCCCT`.

### See also

[Overview](Extra/FeynCalc.md), [SpinorChainTranspose](SpinorChainTranspose.md), [DiracGamma](DiracGamma.md), [Spinor](Spinor.md).

### Examples

```mathematica
GA[\[Mu], \[Nu], \[Rho]]
FCChargeConjugateTransposed[%]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }$$

$$C\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }\right)^TC^{-1}$$

```mathematica
FCChargeConjugateTransposed[GA[\[Mu], \[Nu], \[Rho]], Explicit -> True]
```

$$-\bar{\gamma }^{\rho }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\mu }$$

```mathematica
GA[5]
FCCCT[%]
% // Explicit
```

$$\bar{\gamma }^5$$

$$C\left(\bar{\gamma }^5\right)^TC^{-1}$$

$$\bar{\gamma }^5$$

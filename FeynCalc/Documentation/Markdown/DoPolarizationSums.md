## DoPolarizationSums

`DoPolarizationSums[exp, k, ...]` acts on an expression `exp` that must contain a polarization vector $\varepsilon(k)$  and its complex conjugate (e.g. `exp` can be a matrix element squared).

Depending on the arguments of the function, it will perform a sum over the polarization of $\varepsilon(k)$ and its c.c.

- `DoPolarizationSums[exp, k]` sums over the three physical polarizations of an external massive vector boson with the $4$-momentum `k` and the mass $k^2$.
- `DoPolarizationSums[exp, k, 0]` replaces the polarization sum of an external massless vector boson with the momentum `k` by $-g^{\mu \nu}$.
This corresponds to the summation over all 4 polarizations, including the unphysical ones.
- `DoPolarizationSums[exp, k, n]` sums over physical (transverse) polarizations of an external massless vector boson with the momentum `k`, where `n` is an auxiliary 4-vector from the gauge-dependent polarization sum formula.

Cf. `PolarizationSum` for more examples and explanations on different polarizations.

`DoPolarizationSums` also work with $D$-dimensional amplitudes.

### See also

[Overview](Extra/FeynCalc.md), [Polarization](Polarization.md), [PolarizationSum](PolarizationSum.md), [NumberOfPolarizations](NumberOfPolarizations.md), [VirtualBoson](VirtualBoson.md), [Uncontract](Uncontract.md).

### Examples

The standard formula for massless vector bosons is valid for all types of the corresponding particles, including gluons.

```mathematica
FCClearScalarProducts[]
SP[p] = 0;
Pair[LorentzIndex[\[Mu]], Momentum[Polarization[p, -I]]] Pair[LorentzIndex[\[Nu]], 
   Momentum[Polarization[p, I]]]
```

$$\bar{\varepsilon }^{*\mu }(p) \bar{\varepsilon }^{\nu }(p)$$

```mathematica
DoPolarizationSums[%, p, n]
```

$$-\frac{\overline{n}^2 \overline{p}^{\mu } \overline{p}^{\nu }}{(\overline{n}\cdot \overline{p})^2}-\bar{g}^{\mu \nu }+\frac{\overline{n}^{\nu } \overline{p}^{\mu }}{\overline{n}\cdot \overline{p}}+\frac{\overline{n}^{\mu } \overline{p}^{\nu }}{\overline{n}\cdot \overline{p}}$$

In QED the gauge invariance ensures the cancellation of the unphysical polarizations so that for photons one can also employ the simpler replacement with the metric tensor.

```mathematica
FCClearScalarProducts[]
SP[p] = 0;
Pair[LorentzIndex[\[Mu]], Momentum[Polarization[p, -I]]] Pair[LorentzIndex[\[Nu]], 
   Momentum[Polarization[p, I]]]
```

$$\bar{\varepsilon }^{*\mu }(p) \bar{\varepsilon }^{\nu }(p)$$

```mathematica
DoPolarizationSums[%, p, 0]
```

$$-\bar{g}^{\mu \nu }$$

You can also use this trick in QCD, provided that the unphysical degrees of freedom are subtracted using ghosts at a later stage.

Notice that in this case you should not make the polarization vectors transverse using the `Transversality` option.

Furthermore, the averaging over the polarizations of the initial gluons must be done on the physical amplitude squared, i.e. after the ghost contributions have been subtracted.

Massive vector bosons (e.g. W or Z) have 3 degrees of freedom and require no auxiliary vector.

```mathematica
FCClearScalarProducts[]
SP[p] = m^2;
Pair[LorentzIndex[\[Mu]], Momentum[Polarization[p, -I]]] Pair[LorentzIndex[\[Nu]], 
   Momentum[Polarization[p, I]]]
```

$$\bar{\varepsilon }^{*\mu }(p) \bar{\varepsilon }^{\nu }(p)$$

```mathematica
DoPolarizationSums[%, p]
```

$$\frac{\overline{p}^{\mu } \overline{p}^{\nu }}{m^2}-\bar{g}^{\mu \nu }$$

A more realistic example of summing over the polarizations of the photons in $e^+e^ \to  \gamma \gamma$

```mathematica
ClearAll[s, t, u];
FCClearScalarProducts[];
SP[k1] = 0;
SP[k2] = 0;
amp = (-((Spinor[Momentum[p1], 0, 1] . GS[Polarization[k1, I, 
             Transversality -> True]] . GS[k2 - p2] . GS[Polarization[k2, I, 
             Transversality -> True]] . Spinor[-Momentum[p2], 0, 1]*SMP["e"]^2)/t) - 
     (Spinor[Momentum[p1], 0, 1] . GS[Polarization[k2, I, 
           Transversality -> True]] . GS[k1 - p2] . GS[Polarization[k1, I, 
           Transversality -> True]] . Spinor[-Momentum[p2], 0, 1]*SMP["e"]^2)/u)*
   (-((Spinor[-Momentum[p2], 0, 1] . GS[Polarization[k1, -I, 
             Transversality -> True]] . GS[k1 - p2] . GS[Polarization[k2, -I, 
             Transversality -> True]] . Spinor[Momentum[p1], 0, 1]*
          SMP["e"]^2)/u) - (Spinor[-Momentum[p2], 0, 1] . GS[Polarization[k2, -I, 
           Transversality -> True]] . GS[k2 - p2] . GS[Polarization[k1, -I, 
           Transversality -> True]] . Spinor[Momentum[p1], 0, 1]*SMP["e"]^2)/t)
```

$$\left(-\frac{\text{e}^2 \left(\varphi (\overline{\text{p1}})\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }(\text{k1})\right).\left(\bar{\gamma }\cdot \left(\overline{\text{k2}}-\overline{\text{p2}}\right)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }(\text{k2})\right).\left(\varphi (-\overline{\text{p2}})\right)}{t}-\frac{\text{e}^2 \left(\varphi (\overline{\text{p1}})\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }(\text{k2})\right).\left(\bar{\gamma }\cdot \left(\overline{\text{k1}}-\overline{\text{p2}}\right)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }(\text{k1})\right).\left(\varphi (-\overline{\text{p2}})\right)}{u}\right) \left(-\frac{\text{e}^2 \left(\varphi (-\overline{\text{p2}})\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*(\text{k2})\right).\left(\bar{\gamma }\cdot \left(\overline{\text{k2}}-\overline{\text{p2}}\right)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*(\text{k1})\right).\left(\varphi (\overline{\text{p1}})\right)}{t}-\frac{\text{e}^2 \left(\varphi (-\overline{\text{p2}})\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*(\text{k1})\right).\left(\bar{\gamma }\cdot \left(\overline{\text{k1}}-\overline{\text{p2}}\right)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*(\text{k2})\right).\left(\varphi (\overline{\text{p1}})\right)}{u}\right)$$

```mathematica
amp // DoPolarizationSums[#, k1, 0] & // DoPolarizationSums[#, k2, 0] &
```

$$\frac{\text{e}^4 \left(\varphi (-\overline{\text{p2}})\right).\bar{\gamma }^{\text{\$MU}(\text{\$26})}.\left(\bar{\gamma }\cdot \left(\overline{\text{k1}}-\overline{\text{p2}}\right)\right).\bar{\gamma }^{\text{\$MU}(\text{\$28})}.\left(\varphi (\overline{\text{p1}})\right) \left(\varphi (\overline{\text{p1}})\right).\bar{\gamma }^{\text{\$MU}(\text{\$26})}.\left(\bar{\gamma }\cdot \left(\overline{\text{k2}}-\overline{\text{p2}}\right)\right).\bar{\gamma }^{\text{\$MU}(\text{\$28})}.\left(\varphi (-\overline{\text{p2}})\right)}{t u}+\frac{\text{e}^4 \left(\varphi (\overline{\text{p1}})\right).\bar{\gamma }^{\text{\$MU}(\text{\$28})}.\left(\bar{\gamma }\cdot \left(\overline{\text{k1}}-\overline{\text{p2}}\right)\right).\bar{\gamma }^{\text{\$MU}(\text{\$26})}.\left(\varphi (-\overline{\text{p2}})\right) \left(\varphi (-\overline{\text{p2}})\right).\bar{\gamma }^{\text{\$MU}(\text{\$28})}.\left(\bar{\gamma }\cdot \left(\overline{\text{k2}}-\overline{\text{p2}}\right)\right).\bar{\gamma }^{\text{\$MU}(\text{\$26})}.\left(\varphi (\overline{\text{p1}})\right)}{t u}+\frac{\text{e}^4 \left(\varphi (\overline{\text{p1}})\right).\bar{\gamma }^{\text{\$MU}(\text{\$28})}.\left(\bar{\gamma }\cdot \left(\overline{\text{k1}}-\overline{\text{p2}}\right)\right).\bar{\gamma }^{\text{\$MU}(\text{\$26})}.\left(\varphi (-\overline{\text{p2}})\right) \left(\varphi (-\overline{\text{p2}})\right).\bar{\gamma }^{\text{\$MU}(\text{\$26})}.\left(\bar{\gamma }\cdot \left(\overline{\text{k1}}-\overline{\text{p2}}\right)\right).\bar{\gamma }^{\text{\$MU}(\text{\$28})}.\left(\varphi (\overline{\text{p1}})\right)}{u^2}+\frac{\text{e}^4 \left(\varphi (\overline{\text{p1}})\right).\bar{\gamma }^{\text{\$MU}(\text{\$26})}.\left(\bar{\gamma }\cdot \left(\overline{\text{k2}}-\overline{\text{p2}}\right)\right).\bar{\gamma }^{\text{\$MU}(\text{\$28})}.\left(\varphi (-\overline{\text{p2}})\right) \left(\varphi (-\overline{\text{p2}})\right).\bar{\gamma }^{\text{\$MU}(\text{\$28})}.\left(\bar{\gamma }\cdot \left(\overline{\text{k2}}-\overline{\text{p2}}\right)\right).\bar{\gamma }^{\text{\$MU}(\text{\$26})}.\left(\varphi (\overline{\text{p1}})\right)}{t^2}$$

This is a small piece of the matrix element squared for $g g to  Q \bar{Q}$.

The proper summation over the polarizations of the gluons requires a choice of two auxiliary vectors (unless we subtract the unphysical contributions using ghosts).

It is customary to take the 4-momentum of another gluon as the auxiliary vector in the summation formula.

The option `ExtraFactor` is used to average over the polarizations of the initial gluons.

```mathematica
ClearAll[s, t, u];
FCClearScalarProducts[];
SP[p1] = 0;
SP[p2] = 0;
```

```mathematica
amp = 1/(s^2 SUNN (1 - SUNN^2) u^2) 2 SMP["g_s"]^4 SP[k1, 
    Polarization[p2, -I, Transversality -> True]] SP[k1, Polarization[p2, 
     I, Transversality -> True]] (2 s^2 SP[k1, Polarization[p1, I, 
        Transversality -> True]] SP[k2, Polarization[p1, -I, 
        Transversality -> True]] + 2 s SUNN^2 t SP[k1, Polarization[p1, I, 
        Transversality -> True]] SP[k2, Polarization[p1, -I, 
        Transversality -> True]] + s SUNN^2 u SP[k1, Polarization[p1, I, 
        Transversality -> True]] SP[k2, Polarization[p1, -I, 
        Transversality -> True]] + 2 s^2 SP[k1, Polarization[p1, -I, 
        Transversality -> True]] SP[k2, Polarization[p1, I, 
        Transversality -> True]] + 2 s SUNN^2 t SP[k1, Polarization[p1, -I, 
        Transversality -> True]] SP[k2, Polarization[p1, I, 
        Transversality -> True]] + s SUNN^2 u SP[k1, Polarization[p1, -I, 
        Transversality -> True]] SP[k2, Polarization[p1, I, 
        Transversality -> True]] + 2 SUNN^2 u^2 SP[k2, 
       Polarization[p1, -I, Transversality -> True]] SP[k2, 
       Polarization[p1, I, Transversality -> True]])
```

$$\frac{1}{N \left(1-N^2\right) s^2 u^2}2 g_s^4 \left(\overline{\text{k1}}\cdot \bar{\varepsilon }^*(\text{p2})\right) \left(\overline{\text{k1}}\cdot \bar{\varepsilon }(\text{p2})\right) \left(2 N^2 s t \left(\overline{\text{k1}}\cdot \bar{\varepsilon }(\text{p1})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }^*(\text{p1})\right)+2 N^2 s t \left(\overline{\text{k1}}\cdot \bar{\varepsilon }^*(\text{p1})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }(\text{p1})\right)+N^2 s u \left(\overline{\text{k1}}\cdot \bar{\varepsilon }(\text{p1})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }^*(\text{p1})\right)+N^2 s u \left(\overline{\text{k1}}\cdot \bar{\varepsilon }^*(\text{p1})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }(\text{p1})\right)+2 s^2 \left(\overline{\text{k1}}\cdot \bar{\varepsilon }(\text{p1})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }^*(\text{p1})\right)+2 s^2 \left(\overline{\text{k1}}\cdot \bar{\varepsilon }^*(\text{p1})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }(\text{p1})\right)+2 N^2 u^2 \left(\overline{\text{k2}}\cdot \bar{\varepsilon }^*(\text{p1})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }(\text{p1})\right)\right)$$

```mathematica
amp // DoPolarizationSums[#, p1, p2, ExtraFactor -> 1/2] & // 
   DoPolarizationSums[#, p2, p1, ExtraFactor -> 1/2] & // Simplify
```

$$-\frac{1}{N \left(N^2-1\right) s^2 u^2 (\overline{\text{p1}}\cdot \overline{\text{p2}})^2}g_s^4 \left(2 \left(\overline{\text{k1}}\cdot \overline{\text{p1}}\right) \left(\overline{\text{k1}}\cdot \overline{\text{p2}}\right)-\overline{\text{k1}}^2 \left(\overline{\text{p1}}\cdot \overline{\text{p2}}\right)\right) \left(s \left(\overline{\text{k1}}\cdot \overline{\text{p2}}\right) \left(\overline{\text{k2}}\cdot \overline{\text{p1}}\right) \left(N^2 (2 t+u)+2 s\right)+s \left(\overline{\text{k1}}\cdot \overline{\text{p1}}\right) \left(\overline{\text{k2}}\cdot \overline{\text{p2}}\right) \left(N^2 (2 t+u)+2 s\right)-2 N^2 s t \left(\overline{\text{k1}}\cdot \overline{\text{k2}}\right) \left(\overline{\text{p1}}\cdot \overline{\text{p2}}\right)-N^2 s u \left(\overline{\text{k1}}\cdot \overline{\text{k2}}\right) \left(\overline{\text{p1}}\cdot \overline{\text{p2}}\right)-2 s^2 \left(\overline{\text{k1}}\cdot \overline{\text{k2}}\right) \left(\overline{\text{p1}}\cdot \overline{\text{p2}}\right)+2 N^2 u^2 \left(\overline{\text{k2}}\cdot \overline{\text{p1}}\right) \left(\overline{\text{k2}}\cdot \overline{\text{p2}}\right)-N^2 u^2 \overline{\text{k2}}^2 \left(\overline{\text{p1}}\cdot \overline{\text{p2}}\right)\right)$$

We can also do the same calculation in $D$-dimensions

```mathematica
ClearAll[s, t, u];
FCClearScalarProducts[];
SPD[p1] = 0;
SPD[p2] = 0;
ChangeDimension[amp, D] // DoPolarizationSums[#, p1, p2, ExtraFactor -> 1/2] & // 
   DoPolarizationSums[#, p2, p1, ExtraFactor -> 1/2] & // Simplify
```

$$-\frac{1}{N \left(N^2-1\right) s^2 u^2 (\text{p1}\cdot \;\text{p2})^2}g_s^4 \left(2 (\text{k1}\cdot \;\text{p1}) (\text{k1}\cdot \;\text{p2})-\text{k1}^2 (\text{p1}\cdot \;\text{p2})\right) \left(s (\text{k1}\cdot \;\text{p2}) (\text{k2}\cdot \;\text{p1}) \left(N^2 (2 t+u)+2 s\right)+s (\text{k1}\cdot \;\text{p1}) (\text{k2}\cdot \;\text{p2}) \left(N^2 (2 t+u)+2 s\right)-2 N^2 s t (\text{k1}\cdot \;\text{k2}) (\text{p1}\cdot \;\text{p2})-N^2 s u (\text{k1}\cdot \;\text{k2}) (\text{p1}\cdot \;\text{p2})-2 s^2 (\text{k1}\cdot \;\text{k2}) (\text{p1}\cdot \;\text{p2})+2 N^2 u^2 (\text{k2}\cdot \;\text{p1}) (\text{k2}\cdot \;\text{p2})-\text{k2}^2 N^2 u^2 (\text{p1}\cdot \;\text{p2})\right)$$

`DoPolarizationSums` will complain if you try to sum over the polarizations of a massless vector boson that is not on-shell

```mathematica
PolarizationVector[p, mu] ComplexConjugate[PolarizationVector[p, mu]]
DoPolarizationSums[%, p, 0]
```

$$\bar{\varepsilon }^{*\text{mu}}(p) \bar{\varepsilon }^{\text{mu}}(p)$$

![1l0desl1odmbt](img/1l0desl1odmbt.svg)

$$-4$$

The obvious solution to remove this warning is to put the boson on-shell

```mathematica
FCClearScalarProducts[]
ScalarProduct[p, p] = 0
PolarizationVector[p, mu] ComplexConjugate[PolarizationVector[p, mu]]
DoPolarizationSums[%, p, 0]
```

$$0$$

$$\bar{\varepsilon }^{*\text{mu}}(p) \bar{\varepsilon }^{\text{mu}}(p)$$

$$-4$$

However, if you have a massless virtual boson in the final state that by definition cannot be on-shell, (e.g. in the process $q \bar{q} \to g \gamma^\ast$), you can tell this to the function by setting the option `VirtualBoson` to `True`.

```mathematica
FCClearScalarProducts[]
PolarizationVector[p, mu] ComplexConjugate[PolarizationVector[p, mu]]
DoPolarizationSums[%, p, 0, VirtualBoson -> True]
```

$$\bar{\varepsilon }^{*\text{mu}}(p) \bar{\varepsilon }^{\text{mu}}(p)$$

$$-4$$

It may happen that your expression is not directly proportional to a pair of polarization vectors. In this case terms that are free of polarization vectors will be multiplied by the suitable number of polarizations. This behavior is controlled by the option `NumberOfPolarizations`. The default value `Automatic` means that the function will automatically figure out the correct number of polarizations.

Here we have 2 physical polarizations (massless vector boson)

```mathematica
FCClearScalarProducts[];
ScalarProduct[p, p] = 0;
PolarizationVector[p, mu] ComplexConjugate[PolarizationVector[p, mu]] + xyz
DoPolarizationSums[%, p, n]
```

$$\bar{\varepsilon }^{*\text{mu}}(p) \bar{\varepsilon }^{\text{mu}}(p)+\text{xyz}$$

$$\text{DoPolarizationSums: The input expression contains terms free of polarization vectors. Those will be multiplied with the number of polarizations given by }2.$$

$$2 \;\text{xyz}-2$$

In $D$ dimensions the number of polarizations becomes $D-2$

```mathematica
FCClearScalarProducts[];
ScalarProduct[p, p] = 0;
ChangeDimension[PolarizationVector[p, mu]*
    ComplexConjugate[PolarizationVector[p, mu]] + xyz, D]
DoPolarizationSums[%, p, n]
```

$$\varepsilon ^{*\text{mu}}(p) \varepsilon ^{\text{mu}}(p)+\text{xyz}$$

$$\text{DoPolarizationSums: The input expression contains terms free of polarization vectors. Those will be multiplied with the number of polarizations given by }D-2.$$

$$(D-2) \;\text{xyz}-D+2$$

A massive vector boson has 3 physical polarizations in 4 dimensions

```mathematica
FCClearScalarProducts[];
ScalarProduct[p, p] = M^2;
PolarizationVector[p, mu] ComplexConjugate[PolarizationVector[p, mu]] + xyz
DoPolarizationSums[%, p]
```

$$\bar{\varepsilon }^{*\text{mu}}(p) \bar{\varepsilon }^{\text{mu}}(p)+\text{xyz}$$

$$\text{DoPolarizationSums: The input expression contains terms free of polarization vectors. Those will be multiplied with the number of polarizations given by }3.$$

$$3 \;\text{xyz}-3$$

or $D-1$ physical polarizations in $D$ dimensions

```mathematica
FCClearScalarProducts[];
ScalarProduct[p, p] = M^2;
ChangeDimension[PolarizationVector[p, mu]*
    ComplexConjugate[PolarizationVector[p, mu]] + xyz, D]
DoPolarizationSums[%, p]
```

$$\varepsilon ^{*\text{mu}}(p) \varepsilon ^{\text{mu}}(p)+\text{xyz}$$

$$\text{DoPolarizationSums: The input expression contains terms free of polarization vectors. Those will be multiplied with the number of polarizations given by }D-1.$$

$$(D-1) \;\text{xyz}-D+1$$

In the case of a standalone expression that contains no polarization vectors whatsoever, the function
has no way to determine the correct number of polarizations.

```mathematica
DoPolarizationSums[xyz, p, n]
```

![18de6vblvu214](img/18de6vblvu214.svg)

$$\text{\$Aborted}$$

Here additional user input is needed

```mathematica
DoPolarizationSums[xyz, p, NumberOfPolarizations -> 2] 
  
 

```

$$\text{DoPolarizationSums: The input expression contains terms free of polarization vectors. Those will be multiplied with the number of polarizations given by }2.$$

$$2 \;\text{xyz}$$
`PolarizationVector[p, mu]` gives a polarization vector.

### See also

[FV](FV), [Pair](Pair), [Polarization](Polarization).

### Examples

A polarization vector $\varepsilon_{\mu }(k)$is a special $4$-vector.

```mathematica
PolarizationVector[k, \[Mu]]
% // StandardForm
```

$$\bar{\varepsilon }^{\mu }(k)$$

```
(*Pair[LorentzIndex[\[Mu]], Momentum[Polarization[k, I]]]*)
```

```mathematica
Conjugate[PolarizationVector[k, \[Mu]]]
% // StandardForm
```

$$\bar{\varepsilon }^*^{\mu }(k)$$

```
(*Pair[LorentzIndex[\[Mu]], Momentum[Polarization[k, -I]]]*)
```

The transversality property is not automatic and must be explicitly activated using the option `Transversality`

```mathematica
 PolarizationVector[k, \[Mu]] FV[k, \[Mu]] 
  Contract[%]
```

$$\overline{k}^{\mu } \bar{\varepsilon }^{\mu }(k)$$

$$\overline{k}\cdot \bar{\varepsilon }(k)$$

```mathematica
 PolarizationVector[k, \[Mu], Transversality -> True] FV[k, \[Mu]] 
  Contract[%]
```

$$\overline{k}^{\mu } \bar{\varepsilon }^{\mu }(k)$$

$$0$$

Suppose that you are using unphysical polarization vectors for massless gauge bosons and intend to remove the unphysical degrees of freedom at a later stage using ghosts. In this case you must not use  `Transversality->True`, since your polarization vectors are not transverse. Otherwise the result will be inconsistent.

Here everything is correct, we can use the gauge trick with unphysical polarization vectors.

```mathematica
FCClearScalarProducts[];
SP[k1] = 0;
SP[k2] = 0;
```

```mathematica
ех1 = SP[k1, Polarization[k1, I]] SP[k2, Polarization[k1, -I]] SP[k1, Polarization[k2, I]] SP[k2, Polarization[k2, -I]]
```

$$\left(\overline{\text{k1}}\cdot \bar{\varepsilon }(\text{k1})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }^*(\text{k2})\right) \left(\overline{\text{k1}}\cdot \bar{\varepsilon }(\text{k2})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }^*(\text{k1})\right)$$

```mathematica
ех1 // DoPolarizationSums[#, k1, 0] & // DoPolarizationSums[#, k2, 0] &
```

$$(\overline{\text{k1}}\cdot \overline{\text{k2}})^2$$

Here we erroneously set `Transversality->True`  and consequently obtain a wrong result. In pure QED the full result (physical amplitude squared) would still come out right owing to the Ward identities, but e.g. in QCD this would not be the case.

```mathematica
ех2 = SP[k1, Polarization[k1, I, Transversality -> True]] SP[k2, Polarization[k1, -I, Transversality -> True]] SP[k1, Polarization[k2, I, Transversality -> True]] SP[k2, Polarization[k2, -I, Transversality -> True]] // FCI
```

$$0$$

```mathematica
ех2 // DoPolarizationSums[#, k1, 0] & // DoPolarizationSums[#, k2, 0] &
```

$$0$$
## HypergeometricAC

`HypergeometricAC[n][exp]` analytically continues `Hypergeometric2F1` functions in `exp`. The second argument `n` refers to the equation number ($n$) in chapter 2.10 of "Higher Transcendental Functions" by Erdelyi, Magnus, Oberhettinger, Tricomi. In case of eq. (6) (p.109) the last line is returned for `HypergeometricAC[6][exp]`, while the first equality is given by `HypergeometricAC[61][exp]`.

(2.10.1) is identical to eq. (9.5.7) of "Special Functions & their Applications" by N.N.Lebedev.

### See also

[Overview](Extra/FeynCalc.md), [HypExplicit](HypExplicit.md), [HypergeometricIR](HypergeometricIR.md), [HypergeometricSE](HypergeometricSE.md), [ToHypergeometric](ToHypergeometric.md).

### Examples

These are all transformation rules currently built in.

```mathematica
HypergeometricAC[1][Hypergeometric2F1[\[Alpha], \[Beta], \[Gamma], z]]
```

$$\frac{\Gamma (\gamma ) \Gamma (\alpha +\beta -\gamma ) (1-z)^{-\alpha -\beta +\gamma } \, _2F_1(\gamma -\alpha ,\gamma -\beta ;-\alpha -\beta +\gamma +1;1-z)}{\Gamma (\alpha ) \Gamma (\beta )}+\frac{\Gamma (\gamma ) \Gamma (-\alpha -\beta +\gamma ) \, _2F_1(\alpha ,\beta ;\alpha +\beta -\gamma +1;1-z)}{\Gamma (\gamma -\alpha ) \Gamma (\gamma -\beta )}$$

```mathematica
HypergeometricAC[2][Hypergeometric2F1[\[Alpha], \[Beta], \[Gamma], z]]
```

$$\frac{\Gamma (\gamma ) (-z)^{-\alpha } \Gamma (\beta -\alpha ) \, _2F_1\left(\alpha ,\alpha -\gamma +1;\alpha -\beta +1;\frac{1}{z}\right)}{\Gamma (\beta ) \Gamma (\gamma -\alpha )}+\frac{\Gamma (\gamma ) (-z)^{-\beta } \Gamma (\alpha -\beta ) \, _2F_1\left(\beta ,\beta -\gamma +1;-\alpha +\beta +1;\frac{1}{z}\right)}{\Gamma (\alpha ) \Gamma (\gamma -\beta )}$$

```mathematica
HypergeometricAC[3][Hypergeometric2F1[\[Alpha], \[Beta], \[Gamma], z]]
```

$$\frac{\Gamma (\gamma ) (1-z)^{-\alpha } \Gamma (\beta -\alpha ) \, _2F_1\left(\alpha ,\gamma -\beta ;\alpha -\beta +1;\frac{1}{1-z}\right)}{\Gamma (\beta ) \Gamma (\gamma -\alpha )}+\frac{\Gamma (\gamma ) (1-z)^{-\beta } \Gamma (\alpha -\beta ) \, _2F_1\left(\beta ,\gamma -\alpha ;-\alpha +\beta +1;\frac{1}{1-z}\right)}{\Gamma (\alpha ) \Gamma (\gamma -\beta )}$$

```mathematica
HypergeometricAC[4][Hypergeometric2F1[\[Alpha], \[Beta], \[Gamma], z]]
```

$$\frac{\Gamma (\gamma ) z^{-\alpha } \Gamma (-\alpha -\beta +\gamma ) \, _2F_1\left(\alpha ,\alpha -\gamma +1;\alpha +\beta -\gamma +1;-\frac{1-z}{z}\right)}{\Gamma (\gamma -\alpha ) \Gamma (\gamma -\beta )}+\frac{\Gamma (\gamma ) z^{\alpha -\gamma } \Gamma (\alpha +\beta -\gamma ) (1-z)^{-\alpha -\beta +\gamma } \, _2F_1\left(1-\alpha ,\gamma -\alpha ;-\alpha -\beta +\gamma +1;-\frac{1-z}{z}\right)}{\Gamma (\alpha ) \Gamma (\beta )}$$

```mathematica
HypergeometricAC[6][Hypergeometric2F1[\[Alpha], \[Beta], \[Gamma], z]]
```

$$(1-z)^{-\beta } \, _2F_1\left(\beta ,\gamma -\alpha ;\gamma ;-\frac{z}{1-z}\right)$$

```mathematica
HypergeometricAC[61][Hypergeometric2F1[\[Alpha], \[Beta], \[Gamma], z]]
```

$$(1-z)^{-\alpha } \, _2F_1\left(\alpha ,\gamma -\beta ;\gamma ;-\frac{z}{1-z}\right)$$

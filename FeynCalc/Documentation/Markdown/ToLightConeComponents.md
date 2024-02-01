## ToLightConeComponents

`ToLightConeComponents[expr, n, nb]` rewrites all Dirac matrices, scalar products, 4-vectors and metric tensors in terms of their component along the lightcone directions `n` and `nb`

Using the option `NotMomentum` one can specify that quantities containing the listed 4-momenta should be left untouched.

### See also

[Overview](Extra/FeynCalc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [LightConePerpendicularComponent](LightConePerpendicularComponent.md).

### Examples

```mathematica
ToLightConeComponents[SP[a, b], n, nb]
```

$$\frac{1}{2} \left(\overline{a}\cdot \overline{\text{nb}}\right) \left(\overline{b}\cdot \overline{n}\right)+\frac{1}{2} \left(\overline{a}\cdot \overline{n}\right) \left(\overline{b}\cdot \overline{\text{nb}}\right)+\overline{a}\cdot \overline{b}_{\perp }$$

```mathematica
ToLightConeComponents[FV[p, \[Mu]], n, nb]
```

$$\frac{1}{2} \overline{\text{nb}}^{\mu } \left(\overline{n}\cdot \overline{p}\right)+\frac{1}{2} \overline{n}^{\mu } \left(\overline{\text{nb}}\cdot \overline{p}\right)+\overline{p}^{\mu }{}_{\perp }$$

```mathematica
ToLightConeComponents[GA[\[Mu]], n, nb]
```

$$\bar{\gamma }^{\mu }{}_{\perp }+\frac{1}{2} \overline{n}^{\mu } \bar{\gamma }\cdot \overline{\text{nb}}+\frac{1}{2} \overline{\text{nb}}^{\mu } \bar{\gamma }\cdot \overline{n}$$

```mathematica
ToLightConeComponents[GS[p], n, nb]
```

$$\frac{1}{2} \left(\overline{n}\cdot \overline{p}\right) \bar{\gamma }\cdot \overline{\text{nb}}+\frac{1}{2} \bar{\gamma }\cdot \overline{n} \left(\overline{\text{nb}}\cdot \overline{p}\right)+\bar{\gamma }\cdot \overline{p}_{\perp }$$
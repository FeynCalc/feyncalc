```mathematica
 
```

## GSLN

`GSLN[p,n,nb]` denotes the negative component in the lightcone decomposition of the slashed Dirac matrix $(\gamma \cdot p)$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} (n \cdot p) (\gamma \cdot \bar{n})$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALP](GALP.md), [GALN](GALN.md), [GALR](GALR.md), [GSLP](GSLP.md), [GSLR](GSLR.md).

### Examples

```mathematica
GSLN[p, n, nb]
```

$$\frac{1}{2} \left(\overline{n}\cdot \overline{p}\right) \bar{\gamma }\cdot \overline{\text{nb}}$$

```mathematica
StandardForm[GSLN[p, n, nb] // FCI]
```

$$\frac{1}{2} \;\text{DiracGamma}[\text{Momentum}[\text{nb}]] \;\text{Pair}[\text{Momentum}[n],\text{Momentum}[p]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GSLN[p, n, nb] . GSLN[q, n, nb] // DiracSimplify
```

$$\frac{1}{4} \overline{\text{nb}}^2 \left(\overline{n}\cdot \overline{p}\right) \left(\overline{n}\cdot \overline{q}\right)$$

```mathematica
FCClearScalarProducts[]
SP[n] = 0;
SP[nb] = 0;
SP[n, nb] = 2;
```

```mathematica
GSLN[p, n, nb] . GSLN[q, n, nb] // DiracSimplify
```

$$0$$

```mathematica
FCClearScalarProducts[]
```
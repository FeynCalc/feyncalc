## GALN

`GALN[mu,n,nb]` denotes the negative component in the lightcone decomposition of the Dirac matrix $\gamma^{\mu }$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} n^{\mu} (\gamma \cdot \bar{n})$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALP](GALP.md), [GALR](GALR.md), [GSLP](GSLP.md), [GSLN](GSLN.md), [GSLR](GSLR.md).

### Examples

```mathematica
GALN[\[Mu], n, nb]
```

$$\frac{1}{2} \overline{n}^{\mu } \bar{\gamma }\cdot \overline{\text{nb}}$$

```mathematica
StandardForm[GALN[\[Mu], n, nb] // FCI]
```

$$\frac{1}{2} \;\text{DiracGamma}[\text{Momentum}[\text{nb}]] \;\text{Pair}[\text{LorentzIndex}[\mu ],\text{Momentum}[n]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GALN[\[Mu], n, nb] . GALN[\[Nu], n, nb] // DiracSimplify
```

$$\frac{1}{4} \overline{\text{nb}}^2 \overline{n}^{\mu } \overline{n}^{\nu }$$

```mathematica
FCClearScalarProducts[]
SP[n] = 0;
SP[nb] = 0;
SP[n, nb] = 2;
```

```mathematica
GALN[\[Mu], n, nb] . GALN[\[Nu], n, nb] // DiracSimplify
```

$$0$$

```mathematica
FCClearScalarProducts[]
```
```mathematica
 
```

## GALP

`GALP[mu,n,nb]` denotes the positive component in the lightcone decomposition of the Dirac matrix $\gamma^{\mu }$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} \bar{n}^{\mu} (\gamma \cdot n)$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALN](GALN.md), [GALR](GALR.md), [GSLP](GSLP.md), [GSLN](GSLN.md), [GSLR](GSLR.md).

### Examples

```mathematica
GALP[\[Mu], n, nb]
```

$$\frac{1}{2} \overline{\text{nb}}^{\mu } \bar{\gamma }\cdot \overline{n}$$

```mathematica
StandardForm[GALP[\[Mu], n, nb] // FCI]
```

$$\frac{1}{2} \;\text{DiracGamma}[\text{Momentum}[n]] \;\text{Pair}[\text{LorentzIndex}[\mu ],\text{Momentum}[\text{nb}]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GALP[\[Mu], n, nb] . GALP[\[Nu], n, nb] // DiracSimplify
```

$$\frac{1}{4} \overline{n}^2 \overline{\text{nb}}^{\mu } \overline{\text{nb}}^{\nu }$$

```mathematica
FCClearScalarProducts[]
SP[n] = 0;
SP[nb] = 0;
SP[n, nb] = 2;
```

```mathematica
GALP[\[Mu], n, nb] . GALP[\[Nu], n, nb] // DiracSimplify
```

$$0$$

```mathematica
FCClearScalarProducts[]
```
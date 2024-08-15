## MTLP

`MTLP[mu,nu,n,nb]` denotes the positive component in the lightcone decomposition of the metric tensor $g^{\mu \nu}$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} \bar{n}^{\mu}  n^\nu$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLP](FVLP.md), [FVLN](FVLN.md), [FVLR](FVLR.md), [SPLP](SPLP.md), [SPLN](SPLN.md), [SPLR](SPLR.md), [MTLN](MTLN.md), [MTLR](MTLR.md).

### Examples

```mathematica
MTLP[\[Mu], \[Nu], n, nb]
```

$$\frac{1}{2} \overline{n}^{\nu } \overline{\text{nb}}^{\mu }$$

```mathematica
StandardForm[MTLP[\[Mu], \[Nu], n, nb] // FCI]
```

$$\frac{1}{2} \;\text{Pair}[\text{LorentzIndex}[\mu ],\text{Momentum}[\text{nb}]] \;\text{Pair}[\text{LorentzIndex}[\nu ],\text{Momentum}[n]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
MTLP[\[Mu], \[Nu], n, nb] FV[p, \[Mu]] // Contract
```

$$\frac{1}{2} \overline{n}^{\nu } \left(\overline{\text{nb}}\cdot \overline{p}\right)$$

```mathematica
MTLP[\[Mu], \[Nu], n, nb] FV[p, \[Nu]] // Contract
```

$$\frac{1}{2} \overline{\text{nb}}^{\mu } \left(\overline{n}\cdot \overline{p}\right)$$

```mathematica
MTLP[\[Mu], \[Nu], n, nb] FV[n, \[Nu]] // Contract
```

$$\frac{1}{2} \overline{n}^2 \overline{\text{nb}}^{\mu }$$

```mathematica
FCClearScalarProducts[]
SP[n] = 0;
SP[nb] = 0;
SP[n, nb] = 2;
```

```mathematica
MTLP[\[Mu], \[Nu], n, nb] FV[n, \[Nu]] // Contract
```

$$0$$

```mathematica
FCClearScalarProducts[]
```
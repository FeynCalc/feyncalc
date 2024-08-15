## GALND

`GALND[mu,n,nb]` denotes the negative component in the lightcone decomposition of the Dirac matrix $\gamma^{\mu }$  along the vectors `n` and `nb`  in $D$-dimensions. It corresponds to $\frac{1}{2} n^{\mu} (\gamma \cdot \bar{n})$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALPD](GALPD.md), [GALRD](GALRD.md), [GSLPD](GSLPD.md), [GSLND](GSLND.md), [GSLRD](GSLRD.md).

### Examples

```mathematica
GALND[\[Mu], n, nb]
```

$$\frac{1}{2} n^{\mu } \gamma \cdot \;\text{nb}$$

```mathematica
StandardForm[GALND[\[Mu], n, nb] // FCI]
```

$$\frac{1}{2} \;\text{DiracGamma}[\text{Momentum}[\text{nb},D],D] \;\text{Pair}[\text{LorentzIndex}[\mu ,D],\text{Momentum}[n,D]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GALND[\[Mu], n, nb] . GALND[\[Nu], n, nb] // DiracSimplify
```

$$\frac{1}{4} \;\text{nb}^2 n^{\mu } n^{\nu }$$

```mathematica
FCClearScalarProducts[]
SPD[n] = 0;
SPD[nb] = 0;
SPD[n, nb] = 2;
```

```mathematica
GALND[\[Mu], n, nb] . GALND[\[Nu], n, nb] // DiracSimplify
```

$$0$$

```mathematica
FCClearScalarProducts[]
```
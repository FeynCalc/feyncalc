```mathematica
 
```

## GALPD

`GALPD[mu,n,nb]` denotes the positive component in the lightcone decomposition of the Dirac matrix $\gamma^{\mu }$  along the vectors `n` and `nb`  in $D$-dimensions. It corresponds to $\frac{1}{2} \bar{n}^{\mu} (\gamma \cdot n)$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALND](GALND.md), [GALRD](GALRD.md), [GSLPD](GSLPD.md), [GSLND](GSLND.md), [GSLRD](GSLRD.md).

### Examples

```mathematica
GALPD[\[Mu], n, nb]
```

$$\frac{1}{2} \;\text{nb}^{\mu } \gamma \cdot n$$

```mathematica
StandardForm[GALPD[\[Mu], n, nb] // FCI]
```

$$\frac{1}{2} \;\text{DiracGamma}[\text{Momentum}[n,D],D] \;\text{Pair}[\text{LorentzIndex}[\mu ,D],\text{Momentum}[\text{nb},D]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GALPD[\[Mu], n, nb] . GALPD[\[Nu], n, nb] // DiracSimplify
```

$$\frac{1}{4} n^2 \;\text{nb}^{\mu } \;\text{nb}^{\nu }$$

```mathematica
FCClearScalarProducts[]
SPD[n] = 0;
SPD[nb] = 0;
SPD[n, nb] = 2;
```

```mathematica
GALPD[\[Mu], n, nb] . GALPD[\[Nu], n, nb] // DiracSimplify
```

$$0$$

```mathematica
FCClearScalarProducts[]
```
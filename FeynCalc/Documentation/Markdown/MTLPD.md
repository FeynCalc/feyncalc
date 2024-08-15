## MTLPD

`MTLPD[mu,nu,n,nb]` denotes the positive component in the lightcone decomposition of the metric tensor $g^{\mu \nu}$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} \bar{n}^{\mu}  n^\nu$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLPD](FVLPD.md), [FVLND](FVLND.md), [FVLRD](FVLRD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).

### Examples

```mathematica
MTLPD[\[Mu], \[Nu], n, nb]
```

$$\frac{n^{\nu } \;\text{nb}^{\mu }}{2}$$

```mathematica
StandardForm[MTLPD[\[Mu], \[Nu], n, nb] // FCI]
```

$$\frac{1}{2} \;\text{Pair}[\text{LorentzIndex}[\mu ,D],\text{Momentum}[\text{nb},D]] \;\text{Pair}[\text{LorentzIndex}[\nu ,D],\text{Momentum}[n,D]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
MTLPD[\[Mu], \[Nu], n, nb] FVD[p, \[Mu]] // Contract
```

$$\frac{1}{2} n^{\nu } (\text{nb}\cdot p)$$

```mathematica
MTLPD[\[Mu], \[Nu], n, nb] FVD[p, \[Nu]] // Contract
```

$$\frac{1}{2} \;\text{nb}^{\mu } (n\cdot p)$$

```mathematica
MTLPD[\[Mu], \[Nu], n, nb] FVD[n, \[Nu]] // Contract
```

$$\frac{n^2 \;\text{nb}^{\mu }}{2}$$

```mathematica
FCClearScalarProducts[]
SPD[n] = 0;
SPD[nb] = 0;
SPD[n, nb] = 2;
```

```mathematica
MTLPD[\[Mu], \[Nu], n, nb] FVD[n, \[Nu]] // Contract
```

$$0$$

```mathematica
FCClearScalarProducts[]
```
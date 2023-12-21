```mathematica
 
```

## MTLND

`MTLND[mu,nu,n,nb]` denotes the positive component in the lightcone decomposition of the metric tensor $g^{\mu \nu}$  along the vectors `n` and `nb`in $D$ dimensions. It corresponds to $\frac{1}{2} n^{\mu}  \bar{n}^\nu$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLPD](FVLPD.md), [FVLND](FVLND.md), [FVLRD](FVLRD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLRD](MTLRD.md).

### Examples

```mathematica
MTLND[\[Mu], \[Nu], n, nb]
```

$$\frac{n^{\mu } \;\text{nb}^{\nu }}{2}$$

```mathematica
StandardForm[MTLND[\[Mu], \[Nu], n, nb] // FCI]
```

$$\frac{1}{2} \;\text{Pair}[\text{LorentzIndex}[\mu ,D],\text{Momentum}[n,D]] \;\text{Pair}[\text{LorentzIndex}[\nu ,D],\text{Momentum}[\text{nb},D]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
MTLND[\[Mu], \[Nu], n, nb] FVD[p, \[Mu]] // Contract
```

$$\frac{1}{2} \;\text{nb}^{\nu } (n\cdot p)$$

```mathematica
MTLND[\[Mu], \[Nu], n, nb] FVD[p, \[Nu]] // Contract
```

$$\frac{1}{2} n^{\mu } (\text{nb}\cdot p)$$

```mathematica
MTLND[\[Mu], \[Nu], n, nb] FVD[n, \[Nu]] // Contract
```

$$\frac{1}{2} n^{\mu } (n\cdot \;\text{nb})$$

```mathematica
FCClearScalarProducts[]
SPD[n] = 0;
SPD[nb] = 0;
SPD[n, nb] = 2;
```

```mathematica
MTLND[\[Mu], \[Nu], n, nb] FVD[n, \[Nu]] // Contract
```

$$n^{\mu }$$

```mathematica
FCClearScalarProducts[]
```
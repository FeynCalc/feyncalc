## FVLPD

`FVLPD[p,mu,n,nb]` denotes the positive component in the lightcone decomposition of the Lorentz vector $p^{\mu }$  along the vectors `n` and `nb` in $D$ dimensions. It corresponds to $\frac{1}{2} \bar{n}^{\mu} (p \cdot n)$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLRD](FVLRD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).

### Examples

```mathematica
FVLPD[p, \[Mu], n, nb]
```

$$\frac{1}{2} \;\text{nb}^{\mu } (n\cdot p)$$

```mathematica
StandardForm[FVLPD[p, \[Mu], n, nb] // FCI]
```

$$\frac{1}{2} \;\text{Pair}[\text{LorentzIndex}[\mu ,D],\text{Momentum}[\text{nb},D]] \;\text{Pair}[\text{Momentum}[n,D],\text{Momentum}[p,D]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
FVLPD[p, \[Mu], n, nb] FVLND[q, \[Mu], n, nb] // Contract
```

$$\frac{1}{4} (n\cdot \;\text{nb}) (n\cdot p) (\text{nb}\cdot q)$$

```mathematica
FVLPD[p, \[Mu], n, nb] FVLPD[q, \[Mu], n, nb] // Contract
```

$$\frac{1}{4} \;\text{nb}^2 (n\cdot p) (n\cdot q)$$

```mathematica
FCClearScalarProducts[]
SPD[n] = 0;
SPD[nb] = 0;
SPD[n, nb] = 2;
```

```mathematica
FVLPD[p, \[Mu], n, nb] FVLND[q, \[Mu], n, nb] // Contract
```

$$\frac{1}{2} (n\cdot p) (\text{nb}\cdot q)$$

```mathematica
FVLPD[p, \[Mu], n, nb] FVLPD[q, \[Mu], n, nb] // Contract
```

$$0$$

```mathematica
FCClearScalarProducts[]
```
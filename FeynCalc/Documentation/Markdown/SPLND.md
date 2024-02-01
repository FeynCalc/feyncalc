## SPLND

`SPLND[p,q,n,nb]` denotes the negative component in the lightcone decomposition of the scalar product $p \cdot q$  along the vectors `n` and `nb` in $D$-dimensions. It corresponds to $\frac{1}{2} (p \cdot \bar{n}) (q \cdot n)$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLPD](FVLPD.md), [FVLRD](FVLRD.md), [SPLPD](SPLPD.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).

### Examples

```mathematica
SPLND[p, q, n, nb]
```

$$\frac{1}{2} (n\cdot q) (\text{nb}\cdot p)$$

```mathematica
StandardForm[SPLND[p, q, n, nb] // FCI]
```

$$\frac{1}{2} \;\text{Pair}[\text{Momentum}[n,D],\text{Momentum}[q,D]] \;\text{Pair}[\text{Momentum}[\text{nb},D],\text{Momentum}[p,D]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
SPLND[p1 + p2 + n, q, n, nb] // ExpandScalarProduct
```

$$\frac{1}{2} (n\cdot q) (n\cdot \;\text{nb}+\text{nb}\cdot \;\text{p1}+\text{nb}\cdot \;\text{p2})$$

```mathematica
FCClearScalarProducts[]
SPD[n] = 0;
SPD[nb] = 0;
SPD[n, nb] = 2;
```

```mathematica
SPLND[p1 + p2 + n, q, n, nb] // ExpandScalarProduct
```

$$\frac{1}{2} (n\cdot q) (\text{nb}\cdot \;\text{p1}+\text{nb}\cdot \;\text{p2}+2)$$

```mathematica
FCClearScalarProducts[]
```
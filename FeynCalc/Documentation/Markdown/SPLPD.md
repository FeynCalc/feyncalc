```mathematica
 
```

## SPLPD

`SPLPD[p,q,n,nb]` denotes the positive component in the lightcone decomposition of the scalar product $p \cdot q$  along the vectors `n` and `nb` in $D$-dimensions. It corresponds to $\frac{1}{2} (p \cdot n) (q \cdot \bar{n})$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLPD](FVLPD.md), [FVLRD](FVLRD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).

### Examples

```mathematica
SPLPD[p, q, n, nb]
```

$$\frac{1}{2} (n\cdot p) (\text{nb}\cdot q)$$

```mathematica
StandardForm[SPLPD[p, q, n, nb] // FCI]
```

$$\frac{1}{2} \;\text{Pair}[\text{Momentum}[n,D],\text{Momentum}[p,D]] \;\text{Pair}[\text{Momentum}[\text{nb},D],\text{Momentum}[q,D]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
SPLPD[p1 + p2 + n, q, n, nb] // ExpandScalarProduct
```

$$\frac{1}{2} (\text{nb}\cdot q) \left(n\cdot \;\text{p1}+n\cdot \;\text{p2}+n^2\right)$$

```mathematica
FCClearScalarProducts[]
SPD[n] = 0;
SPD[nb] = 0;
SPD[n, nb] = 2;
```

```mathematica
SPLPD[p1 + p2 + n, q, n, nb] // ExpandScalarProduct
```

$$\frac{1}{2} (\text{nb}\cdot q) (n\cdot \;\text{p1}+n\cdot \;\text{p2})$$

```mathematica
FCClearScalarProducts[]
```
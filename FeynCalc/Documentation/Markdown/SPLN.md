## SPLN

`SPLN[p,q,n,nb]` denotes the negative component in the lightcone decomposition of the scalar product $p \cdot q$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} (p \cdot \bar{n}) (q \cdot n)$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLN](FVLN.md), [FVLP](FVLP.md), [FVLR](FVLR.md), [SPLP](SPLP.md), [SPLR](SPLR.md), [MTLP](MTLP.md), [MTLN](MTLN.md), [MTLR](MTLR.md).

### Examples

```mathematica
SPLN[p, q, n, nb]
```

$$\frac{1}{2} \left(\overline{n}\cdot \overline{q}\right) \left(\overline{\text{nb}}\cdot \overline{p}\right)$$

```mathematica
StandardForm[SPLN[p, q, n, nb] // FCI]
```

$$\frac{1}{2} \;\text{Pair}[\text{Momentum}[n],\text{Momentum}[q]] \;\text{Pair}[\text{Momentum}[\text{nb}],\text{Momentum}[p]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
SPLN[p1 + p2 + n, q, n, nb] // ExpandScalarProduct
```

$$\frac{1}{2} \left(\overline{n}\cdot \overline{q}\right) \left(\overline{n}\cdot \overline{\text{nb}}+\overline{\text{nb}}\cdot \overline{\text{p1}}+\overline{\text{nb}}\cdot \overline{\text{p2}}\right)$$

```mathematica
FCClearScalarProducts[]
SP[n] = 0;
SP[nb] = 0;
SP[n, nb] = 2;
```

```mathematica
SPLN[p1 + p2 + n, q, n, nb] // ExpandScalarProduct
```

$$\frac{1}{2} \left(\overline{n}\cdot \overline{q}\right) \left(\overline{\text{nb}}\cdot \overline{\text{p1}}+\overline{\text{nb}}\cdot \overline{\text{p2}}+2\right)$$

```mathematica
FCClearScalarProducts[]
```
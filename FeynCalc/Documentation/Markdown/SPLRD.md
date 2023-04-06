```mathematica
 
```

## SPLRD

`SPLRD[p,q,n,nb]` denotes the perpendicular component in the lightcone decomposition of the scalar product $p \cdot q$  along the vectors `n` and `nb`. It corresponds to $(p \cdot q)_{\perp}$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLPD](FVLPD.md), [FVLRD](FVLRD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).

### Examples

```mathematica
SPLRD[p, q, n, nb]
```

$$p\cdot q_{\perp }$$

```mathematica
StandardForm[SPLRD[p, q, n, nb] // FCI]

(*Pair[LightConePerpendicularComponent[Momentum[p, D], Momentum[n, D], Momentum[nb, D]], LightConePerpendicularComponent[Momentum[q, D], Momentum[n, D], Momentum[nb, D]]]*)
```

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
SPLRD[p1 + p2, q1 + q2, n, nb] // FCI // ExpandScalarProduct
```

$$\text{p1}\cdot \;\text{q1}_{\perp }+\text{p1}\cdot \;\text{q2}_{\perp }+\text{p2}\cdot \;\text{q1}_{\perp }+\text{p2}\cdot \;\text{q2}_{\perp }$$

```mathematica
SPLRD[p1 + p2 + n, q, n, nb] // FCI // ExpandScalarProduct
```

$$\text{p1}\cdot q_{\perp }+\text{p2}\cdot q_{\perp }$$
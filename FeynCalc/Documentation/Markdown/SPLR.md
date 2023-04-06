```mathematica
 
```

## SPLR

`SPLR[p,q,n,nb]` denotes the perpendicular component in the lightcone decomposition of the scalar product $p \cdot q$  along the vectors `n` and `nb`. It corresponds to $(p \cdot q)_{\perp}$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLN](FVLN.md), [FVLP](FVLP.md), [FVLR](FVLR.md), [SPLP](SPLP.md), [SPLN](SPLN.md), [MTLP](MTLP.md), [MTLN](MTLN.md), [MTLR](MTLR.md).

### Examples

```mathematica
SPLR[p, q, n, nb]
```

$$\overline{p}\cdot \overline{q}_{\unicode{27c2}}$$

```mathematica
StandardForm[SPLR[p, q, n, nb] // FCI]

(*Pair[LightConePerpendicularComponent[Momentum[p], Momentum[n], Momentum[nb]], LightConePerpendicularComponent[Momentum[q], Momentum[n], Momentum[nb]]]*)
```

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
SPLR[p1 + p2, q1 + q2, n, nb] // FCI // ExpandScalarProduct
```

$$\overline{\text{p1}}\cdot \overline{\text{q1}}_{\unicode{27c2}}+\overline{\text{p1}}\cdot \overline{\text{q2}}_{\unicode{27c2}}+\overline{\text{p2}}\cdot \overline{\text{q1}}_{\unicode{27c2}}+\overline{\text{p2}}\cdot \overline{\text{q2}}_{\unicode{27c2}}$$

```mathematica
SPLR[p1 + p2 + n, q, n, nb] // FCI // ExpandScalarProduct
```

$$\overline{\text{p1}}\cdot \overline{q}_{\unicode{27c2}}+\overline{\text{p2}}\cdot \overline{q}_{\unicode{27c2}}$$
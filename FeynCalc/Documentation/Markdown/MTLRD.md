## MTLRD

`MTLRD[p,mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the metric tensor $g^{\mu \nu}$  along the vectors `n` and `nb` in $D$ dimensions. It corresponds to $g^{\mu \nu}_{\perp}$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLPD](FVLPD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md).

### Examples

```mathematica
MTLRD[\[Mu], \[Nu], n, nb]
```

$$g^{\mu \nu }{}_{\perp }$$

```mathematica
MTLRD[\[Mu], \[Nu], n, nb] // FCI // StandardForm

(*Pair[LightConePerpendicularComponent[LorentzIndex[\[Mu], D], Momentum[n, D], Momentum[nb, D]], LightConePerpendicularComponent[LorentzIndex[\[Nu], D], Momentum[n, D], Momentum[nb, D]]]*)
```

```mathematica
MTLRD[\[Mu], \[Nu], n, nb] FVD[p, \[Mu]] // Contract
```

$$p^{\nu }{}_{\perp }$$

```mathematica
MTLRD[\[Mu], \[Nu], n, nb] FVLPD[q, \[Mu], n, nb] // Contract
```

$$0$$
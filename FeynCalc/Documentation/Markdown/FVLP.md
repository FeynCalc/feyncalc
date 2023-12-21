```mathematica
 
```

## FVLP

`FVLP[p,mu,n,nb]` denotes the positive component in the lightcone decomposition of the Lorentz vector $p^{\mu }$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} \bar{n}^{\mu} (p \cdot n)$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLN](FVLN.md), [FVLR](FVLR.md), [SPLP](SPLP.md), [SPLN](SPLN.md), [SPLR](SPLR.md), [MTLP](MTLP.md), [MTLN](MTLN.md), [MTLR](MTLR.md).

### Examples

```mathematica
FVLP[p, \[Mu], n, nb]
```

$$\frac{1}{2} \overline{\text{nb}}^{\mu } \left(\overline{n}\cdot \overline{p}\right)$$

```mathematica
StandardForm[FVLP[p, \[Mu], n, nb] // FCI]
```

$$\frac{1}{2} \;\text{Pair}[\text{LorentzIndex}[\mu ],\text{Momentum}[\text{nb}]] \;\text{Pair}[\text{Momentum}[n],\text{Momentum}[p]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
FVLP[p, \[Mu], n, nb] FVLN[q, \[Mu], n, nb] // Contract
```

$$\frac{1}{4} \left(\overline{n}\cdot \overline{\text{nb}}\right) \left(\overline{n}\cdot \overline{p}\right) \left(\overline{\text{nb}}\cdot \overline{q}\right)$$

```mathematica
FVLP[p, \[Mu], n, nb] FVLP[q, \[Mu], n, nb] // Contract
```

$$\frac{1}{4} \overline{\text{nb}}^2 \left(\overline{n}\cdot \overline{p}\right) \left(\overline{n}\cdot \overline{q}\right)$$

```mathematica
FCClearScalarProducts[]
SP[n] = 0;
SP[nb] = 0;
SP[n, nb] = 2;
```

```mathematica
FVLP[p, \[Mu], n, nb] FVLN[q, \[Mu], n, nb] // Contract
```

$$\frac{1}{2} \left(\overline{n}\cdot \overline{p}\right) \left(\overline{\text{nb}}\cdot \overline{q}\right)$$

```mathematica
FVLP[p, \[Mu], n, nb] FVLP[q, \[Mu], n, nb] // Contract
```

$$0$$

```mathematica
FCClearScalarProducts[]
```
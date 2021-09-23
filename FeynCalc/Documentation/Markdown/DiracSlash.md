## DiracSlash

`DiracSlash[p]` is the contraction $p^{\mu } \gamma _{\mu }$ (`FV[p, mu] GA[mu]`).

Products of those can be entered in the form GS[p1, p2, ...].

The shortcut DiracSlash is deprecated, please use GS instead!

### See also

[Overview](Extra/FeynCalc.md), [GS](GS.md), [FCI](FCI.md).

### Examples

This is $q$-slash, i.e., $\gamma^{\mu} q_{\mu }$

```mathematica
DiracSlash[q]
```

$$\bar{\gamma }\cdot \overline{q}$$

```mathematica
DiracSlash[p] . DiracSlash[q]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right)$$

```mathematica
DiracSlash[p, q]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right)$$

DiracSlash is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use GS.

```mathematica
GS[p]
```

$$\bar{\gamma }\cdot \overline{p}$$

```mathematica
GSD[p]
```

$$\gamma \cdot p$$

```mathematica
FCI[GS[p]] === DiracSlash[p]
```

$$\text{True}$$

```mathematica
FCI[GSD[p]] === DiracSlash[p, Dimension -> D]
```

$$\text{True}$$
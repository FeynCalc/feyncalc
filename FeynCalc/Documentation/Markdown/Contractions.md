```mathematica
 
```

## Contractions

### See also

[Overview](Extra/FeynCalc.md).

### Simplifications

Now that we have some basic understanding of FeynCalc objects, let us do something with them. Contractions of Lorentz indices are one of the most essential operations in symbolic QFT calculations. In FeynCalc the corresponding function is called `Contract`

```mathematica
FV[p, \[Mu]] MT[\[Mu], \[Nu]]
Contract[%]
```

$$\overline{p}^{\mu } \bar{g}^{\mu \nu }$$

$$\overline{p}^{\nu }$$

```mathematica
FV[p, \[Alpha]] FV[q, \[Alpha]]
Contract[%]
```

$$\overline{p}^{\alpha } \overline{q}^{\alpha }$$

$$\overline{p}\cdot \overline{q}$$

Notice that when we enter noncommutative objects, such as Dirac matrices, we use `Dot` (`.`) and not `Times` (`*`) 

```mathematica
FV[p, \[Alpha]] MT[\[Beta], \[Gamma]] GA[\[Alpha]] . GA[\[Beta]] . GA[\[Gamma]]
Contract[%]
```

$$\overline{p}^{\alpha } \bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }.\bar{\gamma }^{\gamma } \bar{g}^{\beta \gamma }$$

$$\left(\bar{\gamma }\cdot \overline{p}\right).\bar{\gamma }^{\gamma }.\bar{\gamma }^{\gamma }$$

This is because `Times` is commutative, so writing something like

```mathematica
GA[\[Delta]] GA[\[Beta]] GA[\[Alpha]]
```

$$\bar{\gamma }^{\alpha } \bar{\gamma }^{\beta } \bar{\gamma }^{\delta }$$

will give you completely wrong results. It is also a very common beginner's mistake!
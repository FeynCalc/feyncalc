## SUNF

`SUNF[a, b, c]` are the structure constants of $SU(N)$. The arguments `a, b, c` should be of symbolic type.

### See also

[Overview](Extra/FeynCalc.md), [SUND](SUND.md), [SUNDelta](SUNDelta.md), [SUNIndex](SUNIndex.md), [SUNSimplify](SUNSimplify.md), [SUNT](SUNT.md), [Trick](Trick.md).

### Examples

```mathematica
SUNF[a, b, c] x + SUNF[b, a, c]
Calc[%]
SUNSimplify[%%]
```

$$x f^{abc}+f^{bac}$$

$$x f^{abc}-f^{abc}$$

$$(x-1) f^{abc}$$

```mathematica
SUNF[a, a, b]
% // Calc
```

$$f^{aab}$$

$$0$$

This is a consequence of the usual choice for the normalization of the $T_a$ generators.

```mathematica
SUNF[a, b, c, Explicit -> True]
```

$$2 i \left(\text{tr}(T^a.T^c.T^b)-\text{tr}(T^a.T^b.T^c)\right)$$

```mathematica
SUNSimplify[SUNF[a, b, c] SUNF[a, b, d]]
```

$$C_A \delta ^{cd}$$

```mathematica
SUNSimplify[SUNF[a, b, c], Explicit -> True]
```

$$-2 i \left(\text{tr}(T^a.T^b.T^c)-\text{tr}(T^b.T^a.T^c)\right)$$

```mathematica
SUNF[a, b, c] // StandardForm

(*SUNF[a, b, c]*)
```

```mathematica
SUNF[a, b, c] // FCI // StandardForm

(*SUNF[SUNIndex[a], SUNIndex[b], SUNIndex[c]]*)
```

```mathematica
SUNF[a, b, c] // FCI // FCE // StandardForm

(*SUNF[a, b, c]*)
```

```mathematica
SUNF[b, a, c]
% // FCI
```

$$f^{bac}$$

$$-f^{abc}$$

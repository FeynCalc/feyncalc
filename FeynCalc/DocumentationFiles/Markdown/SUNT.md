## SUNT

`SUNT[a]` is the $SU(N)$ $T^a$ generator in the fundamental representation. The fundamental indices are implicit.

### See also

[Overview](Extra/FeynCalc.md), [CA](CA.md), [CF](CF.md), [SUND](SUND.md), [SUNDelta](SUNDelta.md), [SUNF](SUNF.md), [SUNSimplify](SUNSimplify.md).

### Examples

```mathematica
SUNT[a]
```

$$T^a$$

Since $T^a$ is a noncommutative object, products have to separated by a `Dot` (`.`).

```mathematica
SUNT[a] . SUNT[b] . SUNT[c]
```

$$T^a.T^b.T^c$$

```mathematica
SUNT[a, b, c, d]
```

$$T^a.T^b.T^c.T^d$$

```mathematica
SUNSimplify[SUNT[a, b, a], SUNNToCACF -> False]
```

$$-\frac{T^b}{2 N}$$

```mathematica
SUNSimplify[SUNT[a, b, b, a]]
```

$$C_F^2$$

```mathematica
SUNSimplify[SUNT[a, b, a]]
```

$$-\frac{1}{2} T^b \left(C_A-2 C_F\right)$$

```mathematica
SUNSimplify[SUNT[a, b, a], SUNNToCACF -> False]
```

$$-\frac{T^b}{2 N}$$

The normalization of the generators is chosen in the standard way, therefore $\textrm{Tr}(T^aT^b) = \frac{1}{2} \delta _{ab}$

```mathematica
SUNTrace[SUNT[a, b]]
```

$$\frac{\delta ^{ab}}{2}$$

In case you want $T_f$, you need to include a factor `2*Tf`inside the trace.

```mathematica
SUNTrace[2 Tf SUNT[a, b]]
```

$$T_f \delta ^{ab}$$

```mathematica
SUNTrace[SUNT[a, b]] // StandardForm
```

$$\frac{1}{2} \;\text{SUNDelta}[\text{SUNIndex}[a],\text{SUNIndex}[b]]$$

```mathematica
SUNT[a] // FCI // StandardForm

(*SUNT[SUNIndex[a]]*)
```

```mathematica
SUNT[a] // FCI // FCE // StandardForm

(*SUNT[a]*)
```

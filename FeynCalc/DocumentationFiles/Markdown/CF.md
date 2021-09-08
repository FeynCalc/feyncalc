## CF

`CF` is one of the Casimir operator eigenvalues of $SU(N)$ (CF = $\frac{N^2-1}{2N}$).

### See also

[Overview](Extra/FeynCalc.md), [CA](CA.md), [SUNSimplify](SUNSimplify.md).

### Examples

```mathematica
CF
```

$$C_F$$

```mathematica
SUNSimplify[CF, SUNNToCACF -> False]
```

$$\frac{N^2-1}{2 N}$$

```mathematica
SUNN
```

$$N$$

```mathematica
SUNSimplify[SUNN^2 - 1, SUNNToCACF -> True]
```

$$2 C_A C_F$$

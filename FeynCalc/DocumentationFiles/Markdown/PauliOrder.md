## PauliOrder

`PauliOrder[exp]` orders the Pauli matrices in `expr` alphabetically.

`PauliOrder[exp, orderlist]` orders the Pauli matrices in expr according to `orderlist`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
CSI[k, j, i]
PauliOrder[%]
```

$$\overline{\sigma }^k.\overline{\sigma }^j.\overline{\sigma }^i$$

$$2 \overline{\sigma }^i \bar{\delta }^{jk}-2 \overline{\sigma }^j \bar{\delta }^{ik}+2 \overline{\sigma }^k \bar{\delta }^{ij}-\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k$$

```mathematica
CSID[i, j, k]
PauliOrder[%]
```

$$\sigma ^i.\sigma ^j.\sigma ^k$$

$$\sigma ^i.\sigma ^j.\sigma ^k$$

```mathematica
PauliOrder[%%, {j, i, k}]
```

$$2 \sigma ^k \delta ^{ij}-\sigma ^j.\sigma ^i.\sigma ^k$$

## PauliSimplify

`PauliSimplify[exp]` simplifies products of Pauli matrices and expands non-commutative products. Double indices and vectors are contracted. The order of the Pauli matrices is not changed.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [PauliTrick](PauliTrick.md).

### Examples

```mathematica
CSIS[p1] . CSI[i] . CSIS[p2]
PauliSimplify[%]
```

$$\left(\overline{\sigma }\cdot \overline{\text{p1}}\right).\overline{\sigma }^i.\left(\overline{\sigma }\cdot \overline{\text{p2}}\right)$$

$$\left(\overline{\sigma }\cdot \overline{\text{p1}}\right).\overline{\sigma }^i.\left(\overline{\sigma }\cdot \overline{\text{p2}}\right)$$

```mathematica
CSIS[p] . CSI[i, j, k] . CSIS[p]
PauliSimplify[%]
```

$$\left(\overline{\sigma }\cdot \overline{p}\right).\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k.\left(\overline{\sigma }\cdot \overline{p}\right)$$

$$-\overline{p}^2 \overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k+2 \overline{p}^k \overline{\sigma }^i.\overline{\sigma }^j.\left(\overline{\sigma }\cdot \overline{p}\right)-2 \overline{p}^j \overline{\sigma }^i.\overline{\sigma }^k.\left(\overline{\sigma }\cdot \overline{p}\right)+2 \overline{p}^i \overline{\sigma }^j.\overline{\sigma }^k.\left(\overline{\sigma }\cdot \overline{p}\right)$$

```mathematica
PauliSimplify[CSIS[p] . CSI[i, j, k] . CSIS[p], PauliReduce -> False]
```

$$-\overline{p}^2 \overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k+2 \overline{p}^k \overline{\sigma }^i.\overline{\sigma }^j.\left(\overline{\sigma }\cdot \overline{p}\right)-2 \overline{p}^j \overline{\sigma }^i.\overline{\sigma }^k.\left(\overline{\sigma }\cdot \overline{p}\right)+2 \overline{p}^i \overline{\sigma }^j.\overline{\sigma }^k.\left(\overline{\sigma }\cdot \overline{p}\right)$$

```mathematica
CSID[i, j, i]
PauliSimplify[%]
```

$$\sigma ^i.\sigma ^j.\sigma ^i$$

$$3 \sigma ^j-D \sigma ^j$$

```mathematica
CSID[i, j, k, l, m, i]
PauliSimplify[%]
```

$$\sigma ^i.\sigma ^j.\sigma ^k.\sigma ^l.\sigma ^m.\sigma ^i$$

$$D \sigma ^j.\sigma ^k.\sigma ^l.\sigma ^m-3 \sigma ^j.\sigma ^k.\sigma ^l.\sigma ^m+2 \sigma ^j.\sigma ^k.\sigma ^m.\sigma ^l-2 \sigma ^j.\sigma ^l.\sigma ^m.\sigma ^k+2 \sigma ^k.\sigma ^l.\sigma ^m.\sigma ^j$$

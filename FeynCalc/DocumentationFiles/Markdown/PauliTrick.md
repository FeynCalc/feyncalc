## PauliTrick

`PauliTrick[exp]` contracts $\sigma$ matrices with each other and performs several simplifications (no expansion, use `PauliSimplify` for this).

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [PauliSimplify](PauliSimplify.md).

### Examples

```mathematica
CSIS[p1] . CSI[i] . CSIS[p2]
PauliTrick[%] // Contract
```

$$\left(\overline{\sigma }\cdot \overline{\text{p1}}\right).\overline{\sigma }^i.\left(\overline{\sigma }\cdot \overline{\text{p2}}\right)$$

$$\left(\overline{\sigma }\cdot \overline{\text{p1}}\right).\overline{\sigma }^i.\left(\overline{\sigma }\cdot \overline{\text{p2}}\right)$$

```mathematica
CSID[i, j, i]
PauliTrick[%] // Contract
```

$$\sigma ^i.\sigma ^j.\sigma ^i$$

$$-\left((D-3) \sigma ^j\right)$$

```mathematica
CSIS[p] . CSI[j] . CSIS[p] . CSIS[i]
PauliTrick[%] // Contract // EpsEvaluate // FCCanonicalizeDummyIndices
PauliTrick[%%, PauliReduce -> False]
```

$$\left(\overline{\sigma }\cdot \overline{p}\right).\overline{\sigma }^j.\left(\overline{\sigma }\cdot \overline{p}\right).\left(\overline{\sigma }\cdot \overline{i}\right)$$

$$2 \overline{p}^j \left(\overline{\sigma }\cdot \overline{p}\right).\left(\overline{\sigma }\cdot \overline{i}\right)-\overline{p}^2 \overline{\sigma }^j.\left(\overline{\sigma }\cdot \overline{i}\right)$$

$$2 \overline{p}^j \left(\overline{\sigma }\cdot \overline{p}\right).\left(\overline{\sigma }\cdot \overline{i}\right)-\overline{p}^2 \overline{\sigma }^j.\left(\overline{\sigma }\cdot \overline{i}\right)$$
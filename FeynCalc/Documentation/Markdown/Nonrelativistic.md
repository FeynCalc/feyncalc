```mathematica
 
```

## Nonrelativistic calculations

Since version 9.3 FeynCalc can also deal with manifestly noncovariant expressions, such as 3-vectors,
Kronecker deltas and Pauli matrices

```mathematica
CV[p, i]
```

$$\overline{p}^i$$

```mathematica
CV[p, i] CV[q, i]
% // Contract
```

$$\overline{p}^i \overline{q}^i$$

$$\overline{p}\cdot \overline{q}$$

```mathematica
CLC[i, j, k] CLC[i, j, l]
% // Contract
```

$$\bar{\epsilon }^{ijk} \bar{\epsilon }^{ijl}$$

$$2 \bar{\delta }^{kl}$$

```mathematica
CSI[i, j, i]
% // PauliSimplify
```

$$\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^i$$

$$-\overline{\sigma }^j$$

```mathematica
PauliTrace[CSI[i, j, i, j]]
% // PauliSimplify
```

$$\text{tr}\left(\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^i.\overline{\sigma }^j\right)$$

$$-6$$

The function `LorentzToCartesian` is used to break the manifest Lorentz covariance when doing nonrelativistic expansions

```mathematica
SP[p, q]
% // LorentzToCartesian

```

$$\overline{p}\cdot \overline{q}$$

$$p^0 q^0-\overline{p}\cdot \overline{q}$$
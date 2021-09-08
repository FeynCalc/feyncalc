## PauliSigma

`PauliSigma[x, dim]` is the internal representation of a Pauli matrix with a Lorentz or Cartesian index or a contraction of a Pauli matrix and a Lorentz or Cartesian vector.

`PauliSigma[x,3]` simplifies to `PauliSigma[x]`.

### See also

[Overview](Extra/FeynCalc.md), [SI](SI.md), [CSI](CSI.md).

### Examples

```mathematica
PauliSigma[LorentzIndex[\[Alpha]]]
```

$$\bar{\sigma }^{\alpha }$$

```mathematica
PauliSigma[CartesianIndex[i]]
```

$$\overline{\sigma }^i$$

A Pauli matrix contracted with a Lorentz or Cartesian vector is displayed as $\sigma \cdot p$

```mathematica
PauliSigma[Momentum[p]]
```

$$\bar{\sigma }\cdot \overline{p}$$

```mathematica
PauliSigma[CartesianMomentum[p]]
```

$$\overline{\sigma }\cdot \overline{p}$$

```mathematica
PauliSigma[Momentum[q]] . PauliSigma[Momentum[p - q]]
% // PauliSigmaExpand
```

$$\left(\bar{\sigma }\cdot \overline{q}\right).\left(\bar{\sigma }\cdot \left(\overline{p}-\overline{q}\right)\right)$$

$$\left(\bar{\sigma }\cdot \overline{q}\right).\left(\bar{\sigma }\cdot \overline{p}-\bar{\sigma }\cdot \overline{q}\right)$$

```mathematica
PauliSigma[CartesianMomentum[q]] . PauliSigma[CartesianMomentum[p - q]]
% // PauliSigmaExpand 
  
 

```

$$\left(\overline{\sigma }\cdot \overline{q}\right).\left(\overline{\sigma }\cdot \left(\overline{p}-\overline{q}\right)\right)$$

$$\left(\overline{\sigma }\cdot \overline{q}\right).\left(\overline{\sigma }\cdot \overline{p}-\overline{\sigma }\cdot \overline{q}\right)$$

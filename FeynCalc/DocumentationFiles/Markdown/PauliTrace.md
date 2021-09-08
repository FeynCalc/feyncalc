## PauliTrace

`PauliTrace[exp]` is the head of Pauli traces. By default the trace is not evaluated. The evaluation occurs only when the option `PauliTraceEvaluate` is set to `True`. It is recommended to use `PauliSimplify`, which will automatically evaluate all Pauli traces in the input expression.

### See also

[Overview](Extra/FeynCalc.md), [PauliSimplify](PauliSimplify.md).

### Examples

```mathematica
PauliTrace[CSI[i, j, k, l]]
```

$$\text{tr}\left(\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k.\overline{\sigma }^l\right)$$

```mathematica
PauliTrace[CSI[i, j, k, l], PauliTraceEvaluate -> True]
```

$$2 \left(\bar{\delta }^{il} \bar{\delta }^{jk}-\bar{\delta }^{ik} \bar{\delta }^{jl}+\bar{\delta }^{ij} \bar{\delta }^{kl}\right)$$

```mathematica
PauliTrace[CSI[i, j, k, l]]
% // PauliSimplify 
  
 

```

$$\text{tr}\left(\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k.\overline{\sigma }^l\right)$$

$$2 \bar{\delta }^{il} \bar{\delta }^{jk}-2 \bar{\delta }^{ik} \bar{\delta }^{jl}+2 \bar{\delta }^{ij} \bar{\delta }^{kl}$$

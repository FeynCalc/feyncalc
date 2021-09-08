## PauliReduce

`PauliReduce` is an option for `PauliTrick` and other functions. It specifies whether a chain of Pauli matrices should be reduced to at most one matrix by rewriting every pair of matrices in terms of commutator and anticommutator.

### See also

[Overview](Extra/FeynCalc.md), [PauliTrick](PauliTrick.md), [PauliSimplify](PauliSimplify.md).

### Examples

```mathematica
CSI[i, j, k]
PauliSimplify[%]
```

$$\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k$$

$$\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k$$

```mathematica
CSI[i, j, k]
PauliSimplify[%, PauliReduce -> True] 
  
 

```

$$\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k$$

$$\overline{\sigma }^i \bar{\delta }^{jk}-\overline{\sigma }^j \bar{\delta }^{ik}+\overline{\sigma }^k \bar{\delta }^{ij}+i \bar{\epsilon }^{ijk}$$

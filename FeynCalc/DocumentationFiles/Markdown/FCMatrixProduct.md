## FCMatrixProduct

`FCMatrixProduct[mat1, mat2, ...]` can be used to obtain products of matrices
with entries containing noncommutative symbols. Using the usual `Dot` on such matrices would otherwise destroy the original ordering.

The resulting expression can be then further simplified using `DotSimplify`.

### See also

[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [DeclareNonCommutative](DeclareNonCommutative.md), [UnDeclareNonCommutative](UnDeclareNonCommutative.md).

### Examples

#### Generic matrices

Consider two generic `2 \times 2`-matrices containing noncommutative heads

```mathematica
DeclareNonCommutative[opA, opB, opC, opD]
```

```mathematica
mat[1] = {{opA[1], opB[1]}, {opC[1], opD[1]}}
mat[2] = {{opA[2], opB[2]}, {opC[2], opD[2]}}
```

$$\left(
\begin{array}{cc}
 \;\text{opA}(1) & \;\text{opB}(1) \\
 \;\text{opC}(1) & \;\text{opD}(1) \\
\end{array}
\right)$$

$$\left(
\begin{array}{cc}
 \;\text{opA}(2) & \;\text{opB}(2) \\
 \;\text{opC}(2) & \;\text{opD}(2) \\
\end{array}
\right)$$

Using the usual `Dot` product the elements of the resulting matrix are now multiplied with each other commutatively. Hence, the result is incorrect.

```mathematica
mat[1] . mat[2]
```

$$\left(
\begin{array}{cc}
 \;\text{opA}(1) \;\text{opA}(2)+\text{opB}(1) \;\text{opC}(2) & \;\text{opA}(1) \;\text{opB}(2)+\text{opB}(1) \;\text{opD}(2) \\
 \;\text{opA}(2) \;\text{opC}(1)+\text{opC}(2) \;\text{opD}(1) & \;\text{opB}(2) \;\text{opC}(1)+\text{opD}(1) \;\text{opD}(2) \\
\end{array}
\right)$$

With `FCMatrixProduct` the proper ordering is preserved

```mathematica
FCMatrixProduct[mat[1], mat[2]]
```

$$\left(
\begin{array}{cc}
 \;\text{opA}(1).\text{opA}(2)+\text{opB}(1).\text{opC}(2) & \;\text{opA}(1).\text{opB}(2)+\text{opB}(1).\text{opD}(2) \\
 \;\text{opC}(1).\text{opA}(2)+\text{opD}(1).\text{opC}(2) & \;\text{opC}(1).\text{opB}(2)+\text{opD}(1).\text{opD}(2) \\
\end{array}
\right)$$

We can also multiply more than two matrices at once

```mathematica
mat[3] = {{opA[3], opB[3]}, {opC[3], opD[3]}}
```

$$\left(
\begin{array}{cc}
 \;\text{opA}(3) & \;\text{opB}(3) \\
 \;\text{opC}(3) & \;\text{opD}(3) \\
\end{array}
\right)$$

```mathematica
out = FCMatrixProduct[mat[1], mat[2], mat[3]]
```

$$\left(
\begin{array}{cc}
 \;\text{opB}(1).(\text{opC}(2).\text{opA}(3)+\text{opD}(2).\text{opC}(3))+\text{opA}(1).(\text{opA}(2).\text{opA}(3)+\text{opB}(2).\text{opC}(3)) & \;\text{opA}(1).(\text{opA}(2).\text{opB}(3)+\text{opB}(2).\text{opD}(3))+\text{opB}(1).(\text{opC}(2).\text{opB}(3)+\text{opD}(2).\text{opD}(3)) \\
 \;\text{opC}(1).(\text{opA}(2).\text{opA}(3)+\text{opB}(2).\text{opC}(3))+\text{opD}(1).(\text{opC}(2).\text{opA}(3)+\text{opD}(2).\text{opC}(3)) & \;\text{opC}(1).(\text{opA}(2).\text{opB}(3)+\text{opB}(2).\text{opD}(3))+\text{opD}(1).(\text{opC}(2).\text{opB}(3)+\text{opD}(2).\text{opD}(3)) \\
\end{array}
\right)$$

Now use `DotSimplify` to expand noncommutative products

```mathematica
DotSimplify[out]
```

$$\left(
\begin{array}{cc}
 \;\text{opA}(1).\text{opB}(2).\text{opC}(3)+\text{opB}(1).\text{opC}(2).\text{opA}(3)+\text{opA}(1).\text{opA}(2).\text{opA}(3)+\text{opB}(1).\text{opD}(2).\text{opC}(3) & \;\text{opA}(1).\text{opB}(2).\text{opD}(3)+\text{opA}(1).\text{opA}(2).\text{opB}(3)+\text{opB}(1).\text{opC}(2).\text{opB}(3)+\text{opB}(1).\text{opD}(2).\text{opD}(3) \\
 \;\text{opD}(1).\text{opC}(2).\text{opA}(3)+\text{opC}(1).\text{opA}(2).\text{opA}(3)+\text{opC}(1).\text{opB}(2).\text{opC}(3)+\text{opD}(1).\text{opD}(2).\text{opC}(3) & \;\text{opC}(1).\text{opA}(2).\text{opB}(3)+\text{opC}(1).\text{opB}(2).\text{opD}(3)+\text{opD}(1).\text{opC}(2).\text{opB}(3)+\text{opD}(1).\text{opD}(2).\text{opD}(3) \\
\end{array}
\right)$$

#### Dirac matrices in terms of Pauli matrices

Let us define Dirac matrices in the Dirac basis in terms of Pauli matrices

```mathematica
gamma[0] = {{1, 0}, {0, -1}};
gamma[i_] := {{0, CSI[i]}, {-CSI[i], 0}};
```

and express $\gamma^i \gamma^j \gamma^i$ as a $2 \times 2$-matrix

```mathematica
FCMatrixProduct[gamma[i], gamma[j], gamma[i]]
DotSimplify[%] 
  
 

```

$$\left(
\begin{array}{cc}
 0.\left(\overline{\sigma }^j.\left(-\overline{\sigma }^i\right)+0.0\right)+\overline{\sigma }^i.\left(0.\left(-\overline{\sigma }^i\right)+\left(-\overline{\sigma }^j\right).0\right) & 0.\left(0.\overline{\sigma }^i+\overline{\sigma }^j.0\right)+\overline{\sigma }^i.\left(\left(-\overline{\sigma }^j\right).\overline{\sigma }^i+0.0\right) \\
 0.\left(0.\left(-\overline{\sigma }^i\right)+\left(-\overline{\sigma }^j\right).0\right)+\left(-\overline{\sigma }^i\right).\left(\overline{\sigma }^j.\left(-\overline{\sigma }^i\right)+0.0\right) & 0.\left(\left(-\overline{\sigma }^j\right).\overline{\sigma }^i+0.0\right)+\left(-\overline{\sigma }^i\right).\left(0.\overline{\sigma }^i+\overline{\sigma }^j.0\right) \\
\end{array}
\right)$$

$$\left(
\begin{array}{cc}
 0 & -\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^i \\
 \overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^i & 0 \\
\end{array}
\right)$$

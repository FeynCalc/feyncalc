## FCTripleProduct

`FCTripleProduct[a,b,c]` returns the triple product $a \cdot (b \times c)$. By default `a`,`b` and `c` are assumed to be Cartesian vectors. Wrapping the arguments with `CartesianIndex` will create an expression with open indices.

If any of the arguments is noncommutative, `DOT` will be used instead of `Times` and the function will introduce dummy indices. To give those indices some specific names, use the option `CartesianIndexNames`.

If the arguments already contain free CartesianIndices, the first such index will be used for the contraction.

To obtain an explicit expression you need to set the option `Explicit` to `True` or apply the function `Explicit`

### See also

[Overview](Extra/FeynCalc.md), [Eps](Eps.md).

### Examples

```mathematica
FCTP[a, b, c] 
 
% // StandardForm
```

$$\overline{a}\cdot \left(\overline{b}\times \overline{c}\right)$$

```mathematica
(*FCTripleProduct[a, b, c]*)
```

```mathematica
FCTP[a, b, c, Explicit -> True] 
 
% // StandardForm
```

$$\bar{\epsilon }^{\overline{a}\overline{b}\overline{c}}$$

```mathematica
(*Eps[CartesianMomentum[a], CartesianMomentum[b], CartesianMomentum[c]]*)
```

```mathematica
FCTP[QuantumField[A, CartesianIndex[i]], QuantumField[B, CartesianIndex[j]], 
  QuantumField[C, CartesianIndex[k]], Explicit -> True]
```

$$\bar{\epsilon }^{ijk} A^i.B^j.C^k$$

```mathematica
FCTP[a, b, c, Explicit -> True, NonCommutative -> True, CartesianIndexNames -> {i, j, k}]
```

$$\bar{\epsilon }^{ijk} \overline{a}^i.\overline{b}^j.\overline{c}^k$$
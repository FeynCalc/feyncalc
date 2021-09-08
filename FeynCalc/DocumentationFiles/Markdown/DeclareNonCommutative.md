## DeclareNonCommutative

`DeclareNonCommutative[a, b, ...]` declares `a,b, ...` to be non-commutative, i.e., `DataType[a,b, ..., NonCommutative]` is set to `True`.

### See also

[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [UnDeclareNonCommutative](UnDeclareNonCommutative.md).

### Examples

As a side effect of `DeclareNonCommutative`, `x` is declared to be of data type `NonCommutative`.

```mathematica
DeclareNonCommutative[x]
```

```mathematica
DataType[x, NonCommutative]
```

$$\text{True}$$

```mathematica
DeclareNonCommutative[y, z]
DataType[a, x, y, z, NonCommutative]
```

$$\{\text{False},\text{True},\text{True},\text{True}\}$$

```mathematica
UnDeclareNonCommutative[x, y, z]
DataType[a, x, y, z, NonCommutative]
```

$$\{\text{False},\text{False},\text{False},\text{False}\}$$

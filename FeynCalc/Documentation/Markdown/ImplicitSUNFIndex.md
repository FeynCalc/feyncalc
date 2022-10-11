## ImplicitSUNFIndex

`ImplicitSUNFIndex` is a data type. It mainly applies to names of quantum fields specifying that the corresponding field carries an implicit $SU(N)$ index in the fundamental representation.

This information can be supplied e.g. via `DataType[QuarkField, ImplicitSUNFIndex] = True`, where `QuarkField` is a possible name of the relevant field.

The `ImplicitSUNFIndex` property becomes relevant when simplifying  noncommutative products involving `QuantumField`s via `ExpandPartialD`, `DotSimplify`.

### See also

[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [ImplicitDiracIndex](ImplicitDiracIndex.md), [ImplicitPauliIndex](ImplicitPauliIndex.md)

### Examples

Default (possibly unwanted) behavior

```mathematica
ex = QuantumField[AntiQuarkField] . SUNT[a] . QuantumField[QuarkField]
```

$$\bar{\psi }.T^a.\psi$$

```mathematica
DotSimplify[ex]
```

$$\bar{\psi }.\psi  T^a$$

```mathematica
ExpandPartialD[ex]
```

$$\bar{\psi }.\psi  T^a$$

Now we let FeynCalc know that `AntiQuarkField` and `QuarkField` carry an implicit color index that connects them to the color matrix.

```mathematica
DataType[QuarkField, ImplicitSUNFIndex] = True;
DataType[AntiQuarkField, ImplicitSUNFIndex] = True;
```

```mathematica
DotSimplify[ex]
```

$$\bar{\psi }.T^a.\psi$$

```mathematica
ExpandPartialD[ex]
```

$$\bar{\psi }.T^a.\psi$$
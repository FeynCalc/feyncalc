## ImplicitPauliIndex

`ImplicitPauliIndex` is a data type. It mainly applies to names of quantum fields specifying that the corresponding field carries an implicit Pauli index.

This information can be supplied e.g. via `DataType[QuarkFieldChi, ImplicitPauliIndex] = True`, where `QuarkFieldChi` is a possible name of the relevant field.

The `ImplicitDiracIndex` property becomes relevant when simplifying  noncommutative products involving `QuantumField`s via `ExpandPartialD`, `DotSimplify`.

### See also

[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [ImplicitSUNFIndex](ImplicitSUNFIndex.md), [ImplicitDiracIndex](ImplicitDiracIndex.md)

### Examples

Default (possibly unwanted) behavior

```mathematica
ex = QuantumField[QuarkFieldChiDagger] . CSI[i] . QuantumField[QuarkFieldChi]
```

$$\chi ^{\dagger }.\overline{\sigma }^i.\chi$$

```mathematica
ExpandPartialD[ex]
```

$$\overline{\sigma }^i.\chi ^{\dagger }.\chi$$

Now we let FeynCalc know that `QuarkFieldChiDagger` and `QuarkFieldChi` carry an implicit Pauli index that connects them to the Pauli matrix.

```mathematica
DataType[QuarkFieldChi, ImplicitPauliIndex] = True;
DataType[QuarkFieldChiDagger, ImplicitPauliIndex] = True;
```

```mathematica
ExpandPartialD[ex]
```

$$\chi ^{\dagger }.\overline{\sigma }^i.\chi$$
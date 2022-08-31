## ImplicitDiracIndex

`ImplicitDiracIndex` is a data type. It mainly applies to names of quantum fields specifying that the corresponding field carries an implicit Dirac index.

This information can be supplied e.g. via `DataType[QuarkField, ImplicitDiracIndex] = True`, where `QuarkField` is a possible name of the relevant field.

The `ImplicitDiracIndex` property becomes relevant when simplifying  noncommutative products involving `QuantumField`s via `ExpandPartialD`, `DotSimplify`.

### See also

[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [ImplicitSUNFIndex](ImplicitSUNFIndex.md), [ImplicitPauliIndex](ImplicitPauliIndex.md)

### Examples

Default (possibly unwanted) behavior

```mathematica
ex = QuantumField[AntiQuarkField] . GA[\[Mu]] . QuantumField[QuarkField]
```

$$\bar{\psi }.\bar{\gamma }^{\mu }.\psi$$

```mathematica
ExpandPartialD[ex]
```

$$\bar{\gamma }^{\mu }.\bar{\psi }.\psi$$

Now we let FeynCalc know that `AntiQuarkField` and `QuarkField` carry an implicit Dirac index that connects them to the Dirac matrix.

```mathematica
DataType[QuarkField, ImplicitDiracIndex] = True;
DataType[AntiQuarkField, ImplicitDiracIndex] = True;
```

```mathematica
ExpandPartialD[ex]
```

$$\bar{\psi }.\bar{\gamma }^{\mu }.\psi$$
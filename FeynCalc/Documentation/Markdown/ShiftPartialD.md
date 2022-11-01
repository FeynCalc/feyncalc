## ShiftPartialD

`ShiftPartialD[exp, {derivs}, field]` uses integration-by-parts identities to shift the derivatives of `QuantumField`s such, that a term containing `derivs` acting on `field` is eliminated from the final expression.

The function always assumes that the surface term vanishes

### See also

[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [ExpandPartialD](ExpandPartialD.md), [QuantumField](QuantumField.md)

### Examples

```mathematica
exp = QuantumField[QuarkFieldPsiDagger, PauliIndex[di1]] . RightPartialD[CartesianIndex[i 
    ]] . QuantumField[\[Phi]] . RightPartialD[CartesianIndex[j]] . QuantumField[QuarkFieldPsi, PauliIndex[di2]]
```

$$\psi ^{\dagger \;\text{di1}}.\vec{\partial }_i.\phi .\vec{\partial }_j.\psi ^{\text{di2}}$$

```mathematica
exp // ExpandPartialD
```

$$\psi ^{\dagger \;\text{di1}}.\phi .\left(\partial _i\partial _j\psi ^{\text{di2}}\right)+\psi ^{\dagger \;\text{di1}}.\left(\partial _i\phi \right).\left(\partial _j\psi ^{\text{di2}}\right)$$

```mathematica
ShiftPartialD[exp, {FCPartialD[CartesianIndex[i]]}, QuarkFieldPsi, FCVerbose -> -1]
```

$$-\left(\partial _i\psi ^{\dagger \;\text{di1}}\right).\phi .\left(\partial _j\psi ^{\text{di2}}\right)$$
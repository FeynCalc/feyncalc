## UnDeclareAntiCommutator

`UnDeclareAntiCommutator[a, b]` undeclares the value assigned to the anticommutator of `a` and `b`.

### See also

[Overview](Extra/FeynCalc.md), [Commutator](Commutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).

### Examples

```mathematica
AntiCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]], A], QuantumField[A]] = 0;
```

```mathematica
QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]]
ExpandPartialD[%]
```

$$A.A.\overleftarrow{\partial }_{\nu }$$

$$0$$

```mathematica
UnDeclareAntiCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]], A], QuantumField[A]];
```

```mathematica
ExpandPartialD[QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]]]
```

$$A.\left(\left.(\partial _{\nu }A\right)\right)+\left(\left.(\partial _{\nu }A\right)\right).A$$

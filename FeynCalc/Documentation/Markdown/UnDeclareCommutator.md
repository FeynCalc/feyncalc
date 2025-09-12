## UnDeclareCommutator

`UnDeclareCommutator[a, b]` undeclares the value assigned to the commutator of `a` and `b`.

### See also

[Overview](Extra/FeynCalc.md), [FCCommutator](FCCommutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).

### Examples

```mathematica
FCCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]], A], QuantumField[A]] = 0;
```

```mathematica
QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]] . QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]] 
 
ExpandPartialD[%]
```

$$A.A.\overleftarrow{\partial }_{\nu }.A.A.\overleftarrow{\partial }_{\nu }$$

$$6 A.A.\left(\partial _{\nu }A\right).\left(\partial _{\nu }A\right)+A.\left(\partial _{\nu }\partial _{\nu }A\right).A.A+\left(\partial _{\nu }\partial _{\nu }A\right).A.A.A$$

```mathematica
UnDeclareCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]], A], QuantumField[A]];
```

```mathematica
QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]] . QuantumField[A] . QuantumField[A] . LeftPartialD[\[Nu]] 
 
ExpandPartialD[%]
```

$$A.A.\overleftarrow{\partial }_{\nu }.A.A.\overleftarrow{\partial }_{\nu }$$

$$A.\left(\partial _{\nu }A\right).A.\left(\partial _{\nu }A\right)+A.\left(\partial _{\nu }A\right).\left(\partial _{\nu }A\right).A+A.\left(\partial _{\nu }\partial _{\nu }A\right).A.A+\left(\partial _{\nu }A\right).A.A.\left(\partial _{\nu }A\right)+\left(\partial _{\nu }A\right).A.\left(\partial _{\nu }A\right).A+2 \left(\partial _{\nu }A\right).\left(\partial _{\nu }A\right).A.A+\left(\partial _{\nu }\partial _{\nu }A\right).A.A.A$$
## FCPartialD

`FCPartialD[ind]` denotes a partial derivative of a field. It is an internal object that may appear only inside a `QuantumField`.

`FCPartialD[LorentzIndex[μ]]` denotes  $\partial _{\mu }$.

`FCPartialD[LorentzIndex[μ ,D]]` denotes the $D$-dimensional $\partial _{\mu }$.

`FCPartialD[CartesianIndex[i]]` denotes  $\partial^{i} = - \nabla^i$.

If you need to specify a derivative with respect to a particular variable it also possible to use `FCPartialD[{LorentzIndex[μ],y}]` or `FCPartialD[{CartesianIndex[i],x}]`although this notation is still somewhat experimental

### See also

[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [RightPartialD](RightPartialD.md).

### Examples

```mathematica
QuantumField[A, {\[Mu]}] . LeftPartialD[\[Nu]]
ExpandPartialD[%]
StandardForm[%]
```

$$A_{\mu }.\overleftarrow{\partial }_{\nu }$$

$$\left.(\partial _{\nu }A_{\mu }\right)$$

```
(*QuantumField[FCPartialD[LorentzIndex[\[Nu]]], A, LorentzIndex[\[Mu]]]*)
```

```mathematica
RightPartialD[{CartesianIndex[i], x}] . QuantumField[S, x]
ExpandPartialD[%]
StandardForm[%] 
  
 

```

$$\vec{\partial }_{\{i,x\}}.S^x$$

$$\left.(\partial _{\{i,x\}}S^x\right)$$

```
(*QuantumField[FCPartialD[{CartesianIndex[i], x}], S, x]*)
```
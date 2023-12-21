## FCPartialD

`FCPartialD[ind]` denotes a partial derivative of a field. It is an internal object that may appear only inside a `QuantumField`.

`FCPartialD[LorentzIndex[mu]]` denotes  $\partial_{\mu }$.

`FCPartialD[LorentzIndex[mu ,D]]` denotes the $D$-dimensional $\partial_{\mu }$.

`FCPartialD[CartesianIndex[i]]` denotes  $\partial^{i} = - \nabla^i$.

If you need to specify a derivative with respect to a particular variable it also possible to use `FCPartialD[{LorentzIndex[mu],y}]` or `FCPartialD[{CartesianIndex[i],x}]`although this notation is still somewhat experimental

### See also

[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [RightPartialD](RightPartialD.md).

### Examples

```mathematica
QuantumField[A, {\[Mu]}] . LeftPartialD[\[Nu]] 
 
ex = ExpandPartialD[%]
```

$$A_{\mu }.\overleftarrow{\partial }_{\nu }$$

$$\left(\partial _{\nu }A_{\mu }\right)$$

```mathematica
ex // StandardForm

(*QuantumField[FCPartialD[LorentzIndex[\[Nu]]], A, LorentzIndex[\[Mu]]]*)
```

```mathematica
RightPartialD[{CartesianIndex[i], x}] . QuantumField[S, x] 
 
ex = ExpandPartialD[%]
```

$$\vec{\partial }_{\{i,x\}}.S^x$$

$$\left(\partial _{\{i,x\}}S^x\right)$$

```mathematica
ex // StandardForm

(*QuantumField[FCPartialD[{CartesianIndex[i], x}], S, x]*)
```

`FCPartialD` also accepts `FCGV` symbols as arguments, which can be sometimes useful to make the final expression look nicer.

```mathematica
QuantumField[FCPartialD[FCGV["\[Del]"]], S, x]
```

$$\left(\nabla S^x\right)$$
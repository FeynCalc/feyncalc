##  Momentum 

Momentum[p] is the head of a four momentum (p). The internal representation of a four-dimensional p is Momentum[p]. For other than four dimensions: Momentum[p, dim]. Momentum[p, 4] simplifies to Momentum[p]..

###  See also 

DiracGamma, Eps, LorentzIndex, MomentumExpand.

###  Examples 

This is a 4-dimensional momentum.

```mathematica
Momentum[p]
```

$$\overline{p}$$

As an optional second argument the dimension must be specified if it is different from 4.

```mathematica
Momentum[p, D]
```

$$p$$

The dimension index is supressed in the output.

```mathematica
Momentum[p, d] 
 
Momentum[-q] 
 
% // StandardForm 
 
Momentum[p - q] + Momentum[2 q] 
 
% // StandardForm 
 
%% // MomentumExpand // StandardForm 
 
%%% // MomentumCombine // StandardForm 
 
ChangeDimension[Momentum[p], d] // StandardForm
```

$$p$$

$$-\overline{q}$$

```
(*-Momentum[q]*)
```

$$\left(\overline{p}-\overline{q}\right)+2 \overline{q}$$

```
(*Momentum[p - q] + 2 Momentum[q]*)

(*Momentum[p] + Momentum[q]*)

(*Momentum[p + q]*)

(*Momentum[p, d]*)
```
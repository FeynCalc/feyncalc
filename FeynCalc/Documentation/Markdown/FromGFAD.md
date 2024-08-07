## FromGFAD

`FromGFAD[exp]` converts all suitable generic propagator denominators into standard and Cartesian propagator denominators.

The options `InitialSubstitutions` and `IntermediateSubstitutions` can be used to help the function handle nontrivial propagators. In particular,  `InitialSubstitutions` can define rules for completing the square in the loop momenta of the propagator, while `IntermediateSubstitutions` contains relations for scalar products appearing in those rules.

Another useful option is `LoopMomenta` which is particularly helpful when converting mixed quadratic-eikonal propagators to quadratic ones.

For propagators containing symbolic variables it might be necessary to tell the function that those are larger than zero (if applicable), so that expressions such as $\sqrt{\lambda^2}$ can be simplified accordingly. To that aim one should use the option `PowerExpand`.

### See also

[Overview](Extra/FeynCalc.md), [GFAD](GFAD.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [FeynAmpDenominatorExplicit](FeynAmpDenominatorExplicit.md).

### Examples

```mathematica
GFAD[SPD[p1]] 
 
ex = FromGFAD[%]
```

$$\text{GFAD}(\text{SPD}(\text{p1}))$$

$$\text{FromGFAD}(\text{GFAD}(\text{SPD}(\text{p1})))$$

```mathematica
ex // StandardForm

(*FromGFAD[GFAD[SPD[p1]]]*)
```

```mathematica
ex = GFAD[SPD[p1] + 2 SPD[p1, p2]]
```

$$\text{GFAD}(2 \;\text{SPD}(\text{p1},\text{p2})+\text{SPD}(\text{p1}))$$

```mathematica
FromGFAD[ex]
```

$$\text{FromGFAD}(\text{GFAD}(2 \;\text{SPD}(\text{p1},\text{p2})+\text{SPD}(\text{p1})))$$

We can get a proper conversion into a quadratic propagator using the option `LoopMomenta`. Notice that here `p2.p2` is being put into the mass slot

```mathematica
FromGFAD[ex, LoopMomenta -> {p1}]
```

$$\text{FromGFAD}(\text{GFAD}(2 \;\text{SPD}(\text{p1},\text{p2})+\text{SPD}(\text{p1})),\text{LoopMomenta}\to \{\text{p1}\})$$

```mathematica
ex // StandardForm

(*GFAD[SPD[p1] + 2 SPD[p1, p2]]*)
```

```mathematica
GFAD[{{CSPD[p1] + 2 CSPD[p1, p2] + m^2, -1}, 2}] 
 
ex = FromGFAD[%]
```

$$\text{GFAD}\left(\left\{\left\{2 \;\text{CSPD}(\text{p1},\text{p2})+\text{CSPD}(\text{p1})+m^2,-1\right\},2\right\}\right)$$

$$\text{FromGFAD}\left(\text{GFAD}\left(\left\{\left\{2 \;\text{CSPD}(\text{p1},\text{p2})+\text{CSPD}(\text{p1})+m^2,-1\right\},2\right\}\right)\right)$$

```mathematica
ex // StandardForm

(*FromGFAD[GFAD[{{m^2 + CSPD[p1] + 2 CSPD[p1, p2], -1}, 2}]]*)
```

```mathematica
DataType[la, FCVariable] = True;
prop = FeynAmpDenominator[GenericPropagatorDenominator[-la Pair[Momentum[p1, D], 
       Momentum[p1, D]] + 2 Pair[Momentum[p1, D], Momentum[q, D]], {1,1}]]
```

$$\text{FeynAmpDenominator}(\text{GenericPropagatorDenominator}(2 \;\text{Pair}(\text{Momentum}(\text{p1},D),\text{Momentum}(q,D))-\text{la} \;\text{Pair}(\text{Momentum}(\text{p1},D),\text{Momentum}(\text{p1},D)),\{1,1\}))$$

```mathematica
ex = FromGFAD[prop]
```

$$\text{FromGFAD}(\text{FeynAmpDenominator}(\text{GenericPropagatorDenominator}(2 \;\text{Pair}(\text{Momentum}(\text{p1},D),\text{Momentum}(q,D))-\text{la} \;\text{Pair}(\text{Momentum}(\text{p1},D),\text{Momentum}(\text{p1},D)),\{1,1\})))$$

```mathematica
ex = FromGFAD[prop, LoopMomenta -> {p1}]
```

$$\text{FromGFAD}(\text{FeynAmpDenominator}(\text{GenericPropagatorDenominator}(2 \;\text{Pair}(\text{Momentum}(\text{p1},D),\text{Momentum}(q,D))-\text{la} \;\text{Pair}(\text{Momentum}(\text{p1},D),\text{Momentum}(\text{p1},D)),\{1,1\})),\text{LoopMomenta}\to \{\text{p1}\})$$

```mathematica
ex = GFAD[{{-SPD[p1, p1], 1}, 1}]*GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 1}, 1}]*
    GFAD[{{-SPD[p3, p3], 1}, 1}]*SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*
    SFAD[{{I*(p3 + q), 0}, {-mb^2, 1}, 1}] +  (-2*mg^2*GFAD[{{-SPD[p1, p1], 1}, 2}]*
       GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 1}, 1}]*GFAD[{{-SPD[p3, p3], 1}, 1}]*
       SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*SFAD[{{I*(p3 + q), 0}, {-mb^2, 1}, 1}] - 
        2*mg^2*GFAD[{{-SPD[p1, p1], 1}, 1}]*GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 
            1}, 2}]*GFAD[{{-SPD[p3, p3], 1}, 1}]*SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*
         SFAD[{{I*(p3 + q), 0}, {-mb^2, 1}, 1}] -    2*mg^2*GFAD[{{-SPD[p1, p1], 1}, 
           1}]*GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 1}, 1}]*GFAD[{{-SPD[p3, p3], 
            1}, 2}]*SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*SFAD[{{I*(p3 + q), 0}, 
           {-mb^2, 1}, 1}])/2
```

$$\frac{1}{2} \left(-2 \;\text{mg}^2 \;\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},2\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)-2 \;\text{mg}^2 \;\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},1\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},2\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)-2 \;\text{mg}^2 \;\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},1\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},2\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)\right)+\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},1\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)$$

Notice that `FromGFAD` does not expand scalar products in the propagators before trying to convert
them to `SFAD`s or `CFAD`s. If this is needed, the user should better apply `ExpandScalarProduct` to the expression by hand.

```mathematica
FromGFAD[ex]
```

$$\text{FromGFAD}\left(\frac{1}{2} \left(-2 \;\text{mg}^2 \;\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},2\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)-2 \;\text{mg}^2 \;\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},1\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},2\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)-2 \;\text{mg}^2 \;\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},1\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},2\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)\right)+\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},1\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)\right)$$

```mathematica
FromGFAD[ExpandScalarProduct[ex]]
```

$$\text{FromGFAD}\left(\text{ExpandScalarProduct}\left(\frac{1}{2} \left(-2 \;\text{mg}^2 \;\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},2\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)-2 \;\text{mg}^2 \;\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},1\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},2\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)-2 \;\text{mg}^2 \;\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},1\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},2\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)\right)+\text{GFAD}(\{\{-\text{SPD}(\text{p1},\text{p1}),1\},1\}) \;\text{GFAD}(\{\{-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{GFAD}(\{\{\text{SPD}(\text{p1},2 \;\text{p3}-\text{p1})-\text{SPD}(\text{p3},\text{p3}),1\},1\}) \;\text{SFAD}\left(\left\{\{i (\text{p1}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right) \;\text{SFAD}\left(\left\{\{i (\text{p3}+q),0\},\left\{-\text{mb}^2,1\right\},1\right\}\right)\right)\right)$$

Using the option `InitialSubstitutions` one can perform certain replacement that might not be found automatically. The values of scalar products can be set using `IntermediateSubstitutions`

```mathematica
ex = GFAD[{{SPD[k1, k1] - 2*gkin*meta*u0b*SPD[k1, n], 1}, 1}];
```

Notice that we need to declare the appearing variables as `FCVariable`s

```mathematica
(DataType[#, FCVariable] = True) & /@ {gkin, meta, u0b};
```

Without these options we get a mixed quadratic-eikonal propagator that will cause us troubles when doing topology minimizations.

```mathematica
FromGFAD[ex, FCE -> True]
% // InputForm
```

$$\text{FromGFAD}(\text{GFAD}(\{\{\text{SPD}(\text{k1},\text{k1})-2 \;\text{gkin} \;\text{meta} \;\text{u0b} \;\text{SPD}(\text{k1},n),1\},1\}),\text{FCE}\to \;\text{True})$$

```mathematica
FromGFAD[GFAD[{{SPD[k1, k1] - 2*gkin*meta*u0b*SPD[k1, n], 1}, 1}], 
 FCE -> True]
```

But when doing everything right we end up with a purely quadratic propagator

```mathematica
FromGFAD[ex, InitialSubstitutions -> {ExpandScalarProduct[SPD[k1 - gkin meta u0b n]] -> SPD[k1 - gkin meta u0b n]}, 
  IntermediateSubstitutions -> {SPD[n] -> 0, SPD[nb] -> 0, SPD[n, nb] -> 2}]
```

$$\text{FromGFAD}(\text{GFAD}(\{\{\text{SPD}(\text{k1},\text{k1})-2 \;\text{gkin} \;\text{meta} \;\text{u0b} \;\text{SPD}(\text{k1},n),1\},1\}),\text{InitialSubstitutions}\to \{\text{ExpandScalarProduct}(\text{SPD}(\text{k1}-\text{gkin} \;\text{meta} n \;\text{u0b}))\to \;\text{SPD}(\text{k1}-\text{gkin} \;\text{meta} n \;\text{u0b})\},\text{IntermediateSubstitutions}\to \{\text{SPD}(n)\to 0,\text{SPD}(\text{nb})\to 0,\text{SPD}(n,\text{nb})\to 2\})$$

However, in this case the function can also figure out the necessary square completion on its own if we tell it that `k1` is a momentum w.r.t which the square should be completed. In this case the option `IntermediateSubstitutions`  is not really needed

```mathematica
FromGFAD[ex, LoopMomenta -> {k1}]
```

$$\text{FromGFAD}(\text{GFAD}(\{\{\text{SPD}(\text{k1},\text{k1})-2 \;\text{gkin} \;\text{meta} \;\text{u0b} \;\text{SPD}(\text{k1},n),1\},1\}),\text{LoopMomenta}\to \{\text{k1}\})$$

It is still helpful, though

```mathematica
FromGFAD[ex, LoopMomenta -> {k1}, IntermediateSubstitutions -> {SPD[n] -> 0, SPD[nb] -> 0, SPD[n, nb] -> 2}]
```

$$\text{FromGFAD}(\text{GFAD}(\{\{\text{SPD}(\text{k1},\text{k1})-2 \;\text{gkin} \;\text{meta} \;\text{u0b} \;\text{SPD}(\text{k1},n),1\},1\}),\text{LoopMomenta}\to \{\text{k1}\},\text{IntermediateSubstitutions}\to \{\text{SPD}(n)\to 0,\text{SPD}(\text{nb})\to 0,\text{SPD}(n,\text{nb})\to 2\})$$

If we have multiple loop momenta, we need to first complete the square with respect to them before handling the full expression

```mathematica
ex = GFAD[{{SPD[k1, k1] + 2 SPD[k1, k2] + SPD[k2, k2] + 2 gkin meta (SPD[k1, n] + SPD[k2, n]), 1}, 1}]
```

$$\text{GFAD}(\{\{2 \;\text{gkin} \;\text{meta} (\text{SPD}(\text{k1},n)+\text{SPD}(\text{k2},n))+2 \;\text{SPD}(\text{k1},\text{k2})+\text{SPD}(\text{k1},\text{k1})+\text{SPD}(\text{k2},\text{k2}),1\},1\})$$

```mathematica
FromGFAD[ex, LoopMomenta -> {k1, k2}]
```

$$\text{FromGFAD}(\text{GFAD}(\{\{2 \;\text{gkin} \;\text{meta} (\text{SPD}(\text{k1},n)+\text{SPD}(\text{k2},n))+2 \;\text{SPD}(\text{k1},\text{k2})+\text{SPD}(\text{k1},\text{k1})+\text{SPD}(\text{k2},\text{k2}),1\},1\}),\text{LoopMomenta}\to \{\text{k1},\text{k2}\})$$

```mathematica
FromGFAD[ex, LoopMomenta -> {k1, k2}, 
  InitialSubstitutions -> {ExpandScalarProduct[SPD[k1 + k2]] -> SPD[k1 + k2]}, 
  IntermediateSubstitutions -> {SPD[n] -> 0}]
```

$$\text{FromGFAD}(\text{GFAD}(\{\{2 \;\text{gkin} \;\text{meta} (\text{SPD}(\text{k1},n)+\text{SPD}(\text{k2},n))+2 \;\text{SPD}(\text{k1},\text{k2})+\text{SPD}(\text{k1},\text{k1})+\text{SPD}(\text{k2},\text{k2}),1\},1\}),\text{LoopMomenta}\to \{\text{k1},\text{k2}\},\text{InitialSubstitutions}\to \{\text{ExpandScalarProduct}(\text{SPD}(\text{k1}+\text{k2}))\to \;\text{SPD}(\text{k1}+\text{k2})\},\text{IntermediateSubstitutions}\to \{\text{SPD}(n)\to 0\})$$
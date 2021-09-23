## CommutatorOrder

`CommutatorOrder[exp]` orders any `Commutator` and `AntiCommutator` lexicographically.

### See also

[Overview](Extra/FeynCalc.md), [Commutator](Commutator.md), [AntiCommutator](AntiCommutator.md).

### Examples

```mathematica
Commutator[a, b] + Commutator[b, a]
CommutatorOrder[%] 
  
 

```

$$[a,b]+[b,a]$$

$$0$$

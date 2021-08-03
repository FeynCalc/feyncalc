## CommutatorOrder

`CommutatorOrder[exp]` orders any `Commutator` and `AntiCommutator` lexicographically.

### See also

[Commutator](Commutator), [AntiCommutator](AntiCommutator).

### Examples

```mathematica
Commutator[a, b] + Commutator[b, a]
CommutatorOrder[%] 
  
 

```

$$[a,b]+[b,a]$$

$$0$$
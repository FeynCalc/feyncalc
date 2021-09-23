## CartesianPairContract

`CartesianPairContract` is like `CartesianPair`, but with (local) contraction properties.

### See also

[Overview](Extra/FeynCalc.md), [CartesianPair](CartesianPair.md), [Contract](Contract.md).

### Examples

```mathematica
CartesianPair[CartesianIndex[i], CartesianMomentum[p]] CartesianPair[CartesianIndex[i], CartesianMomentum[q]]
% /. CartesianPair -> CartesianPairContract
% /. CartesianPairContract -> CartesianPair 
  
 

```

$$\overline{p}^i \overline{q}^i$$

$$\text{CartesianPairContract}\left(\overline{p},\overline{q}\right)$$

$$\overline{p}\cdot \overline{q}$$

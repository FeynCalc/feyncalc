## CartesianPairContract

`CartesianPairContract` is like `CartesianPair`, but with (local) contraction properties.  The function fully supports the BMHV algebra and will not expand momenta inside scalar products.

`CartesianPairContract` is an auxiliary function used in higher level FeynCalc functions that require fast contractions between multiple expressions, where `Contract` would be too slow.

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

```mathematica
CartesianPair[CartesianIndex[i], CartesianMomentum[p]] CartesianPair[CartesianIndex[j], CartesianMomentum[q]] CartesianPair[CartesianIndex[i], CartesianIndex[j]] 
 
% /. CartesianPair -> CartesianPairContract 
 
% /. CartesianPairContract -> CartesianPair
```

$$\overline{p}^i \overline{q}^j \bar{\delta }^{ij}$$

$$\text{CartesianPairContract}\left(\overline{p},\overline{q}\right)$$

$$\overline{p}\cdot \overline{q}$$

```mathematica
CartesianPair[CartesianIndex[i], CartesianMomentum[p + q]] CartesianPair[CartesianIndex[i], CartesianMomentum[r + s]] 
 
% /. CartesianPair -> CartesianPairContract 
 
% /. CartesianPairContract -> CartesianPair
```

$$\left(\overline{p}+\overline{q}\right)^i \left(\overline{r}+\overline{s}\right)^i$$

$$\text{CartesianPairContract}\left(\overline{p}+\overline{q},\overline{r}+\overline{s}\right)$$

$$(\overline{p}+\overline{q})\cdot (\overline{r}+\overline{s})$$
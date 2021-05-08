##  CartesianIndex 

CartesianIndex is the head of Cartesian indices. The internal representation of a three-dimensional i is CartesianIndex[i]. For other than three dimensions: CartesianIndex[i, Dimension]. CartesianIndex[i, 3] simplifies to CartesianIndex[i]. The first argument cannot be an integer..

###  See also 

LorentzIndex, ExplicitLorentzIndex.

###  Examples 

This denotes a 3-dimensional Cartesian index.

```mathematica
CartesianIndex[i]
```

$$i$$

An optional second argument can be given for a dimension different from 3.

```mathematica
CartesianIndex[i, D - 1] 
 
CartesianIndex[i, D - 4]
```

$$i$$

$$i$$
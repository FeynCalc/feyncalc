##  CartesianMomentum 

CartesianMomentum[p] is the head of a three momentum (p). The internal representation of a three-dimensional p is CartesianMomentum[p]. For other than three dimensions: CartesianMomentum[p, Dimension]. CartesianMomentum[p, 3] simplifies to CartesianMomentum[p]..

###  See also 

Momentum, TemporalMomentum.

###  Examples 

This is a 3-dimensional momentum.

```mathematica
CartesianMomentum[p]
```

$$\overline{p}$$

As an optional second argument the dimension must be specified if it is different from 3.

```mathematica
CartesianMomentum[p, D - 1]
```

$$p$$

The dimension index is supressed in the output.

```mathematica
CartesianMomentum[p, d - 1] 
 
a1 = CartesianMomentum[-q] 
 
a1 // StandardForm 
 
a2 = CartesianMomentum[p - q] + CartesianMomentum[2 q] 
 
a2 // StandardForm 
 
a2 // MomentumExpand // StandardForm 
 
a2 // MomentumCombine // StandardForm
```

$$p$$

$$-\overline{q}$$

```
(*-CartesianMomentum[q]*)
```

$$\left(\overline{p}-\overline{q}\right)+2 \overline{q}$$

```
(*CartesianMomentum[p - q] + 2 CartesianMomentum[q]*)

(*CartesianMomentum[p] + CartesianMomentum[q]*)

(*CartesianMomentum[p + q]*)
```

Notice that when changing the dimension, one must specify its value as if the the 3-vector were the spatial component of the corresponding 4-vector

```mathematica
ChangeDimension[CartesianMomentum[p], d - 1] // StandardForm
ChangeDimension::failmsg : Error! ChangeDimension has encountered a fatal problem and must abort the computation . The problem reads : Unsupported choice of dimension! >> 
   
   ChangeDimension[CartesianMomentum[p], d] // StandardForm 
 
Clear[a1, a2]

(*CartesianMomentum[p, -2 + d]*)
```

$$![0sl1bgq3309nt](img/0sl1bgq3309nt.png)$$

$$![0ixl3lb44pxiz](img/0ixl3lb44pxiz.png)$$

$$\text{$\$$Failed}$$
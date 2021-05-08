##  FCLoopMixedToCartesianAndTemporal 

FCLoopMixedToCartesianAndTemporal[int, {q1, q2, ...}] attempts to convert loop integrals that contain both Lorentz and Cartesian or temporal indices/momenta to pure temporal and Cartesian indices..

###  Examples 

```mathematica
FCI@SFAD[q] 
 
FCLoopMixedToCartesianAndTemporal[%, {q}, FCE -> True] 
 
FCI@SFAD[{q1 + q2 + p, m^2}] 
 
FCLoopMixedToCartesianAndTemporal[%, {q1, q2}] 
 
FCI[TC[k] FVD[k, mu] FAD[k, k + p]] 
 
FCLoopMixedToCartesianAndTemporal[%, {k}]
```

$$![0cj9w44r1gyto](img/0cj9w44r1gyto.png)$$

$$![0pdp46k5lt9dp](img/0pdp46k5lt9dp.png)$$

$$![1tdm07yihhgyz](img/1tdm07yihhgyz.png)$$

$$![0sp07mox1owio](img/0sp07mox1owio.png)$$

$$\frac{k^0 k^{\text{mu}}}{k^2.(k+p)^2}$$

$$![1e18tqnq89w0x](img/1e18tqnq89w0x.png)$$
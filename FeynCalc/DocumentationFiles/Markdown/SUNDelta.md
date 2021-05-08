##  SUNDelta 

SUNDelta[a, b]  is the Kronecker-delta for SU(N) with color indices a and b in the adjoint representation..

###  See also 

ExplicitSUNIndex, SD, SUNF, SUNIndex, SUNSimplify, Trick.

###  Examples 

```mathematica
SUNDelta[SUNIndex[a], SUNIndex[b]] 
 
SUNDelta[SUNIndex[a], SUNIndex[b]] SUNDelta[SUNIndex[b], SUNIndex[c]] 
 
SUNSimplify[%] 
 
SUNDelta[SUNIndex[a], SUNIndex[b]] 
 
SUNDelta[SUNIndex[a], SUNIndex[b]] // StandardForm 
 
SUNDelta[SUNIndex[a], SUNIndex[b]] // FCI // FCE // StandardForm 
 
SD[a, b] // FCI // StandardForm
```

$$\delta ^{ab}$$

$$\delta ^{ab} \delta ^{bc}$$

$$\delta ^{ac}$$

$$\delta ^{ab}$$

```
(*SUNDelta[SUNIndex[a], SUNIndex[b]]*)

(*SD[a, b]*)

(*SUNDelta[SUNIndex[a], SUNIndex[b]]*)
```

The arguments of SUNDelta may also represent explicit integer indices via the head ExplictiSUNIndex. The difference is that SUNSimplify will only sum over symbolic indices.

```mathematica
SUNDelta[SUNIndex[a], ExplicitSUNIndex[2]] SUNDelta[SUNIndex[a], SUNIndex[b]] SUNDelta[SUNIndex[c], ExplicitSUNIndex[2]] // SUNSimplify 
 
% // StandardForm 
 
SD[1, 2] // FCI // StandardForm
```

$$\delta ^{2b} \delta ^{2c}$$

```
(*SUNDelta[ExplicitSUNIndex[2], SUNIndex[b]] SUNDelta[ExplicitSUNIndex[2], SUNIndex[c]]*)

(*SUNDelta[ExplicitSUNIndex[1], ExplicitSUNIndex[2]]*)
```
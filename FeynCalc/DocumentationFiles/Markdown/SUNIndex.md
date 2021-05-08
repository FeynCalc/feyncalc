##  SUNIndex 

SUNIndex[a] is an SU(N) index in the adjoint representation. If the argument is an integer, SUNIndex[a] turns into ExplicitSUNIndex[a]..

###  See also 

ExplicitSUNIndex, SUNDelta, SUNF.

###  Examples 

```mathematica
SUNIndex[i] 
 
% // StandardForm 
 
SUNIndex[2] 
 
% // StandardForm 
 
SUNDelta[i, j] // FCI // StandardForm
```

$$i$$

```
(*SUNIndex[i]*)
```

$$2$$

```
(*ExplicitSUNIndex[2]*)

(*SUNDelta[SUNIndex[i], SUNIndex[j]]*)
```
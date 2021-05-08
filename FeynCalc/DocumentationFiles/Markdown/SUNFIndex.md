##  SUNFIndex 

SUNFIndex[a]  is an SU(N) index in the fundamental representation. If the argument is an integer, SUNFIndex[a] turns into ExplicitSUNFIndex[a]..

###  Examples 

```mathematica
SUNFIndex[i] 
 
% // StandardForm 
 
SUNFIndex[2] 
 
% // StandardForm 
 
SUNFDelta[i, j] // FCI // StandardForm
```

$$i$$

```
(*SUNFIndex[i]*)
```

$$2$$

```
(*ExplicitSUNFIndex[2]*)

(*SUNFDelta[SUNFIndex[i], SUNFIndex[j]]*)
```
##  CSI 

CSI[i] can be used as input for 3-dimensional $\sigma ^i$ with 3-dimensional Cartesian index i and is transformed into PauliSigma[CartesianIndex[i]] by FeynCalcInternal..

###  See also 

PauliSigma.

###  Examples 

```mathematica
CSI[i] 
 
CSI[i, j] - CSI[j, i] 
 
StandardForm[FCI[CSI[i]]] 
 
CSI[i, j, k, l] 
 
StandardForm[CSI[i, j, k, l]]
```

$$\overline{\sigma }^i$$

$$\overline{\sigma }^i.\overline{\sigma }^j-\overline{\sigma }^j.\overline{\sigma }^i$$

```
(*PauliSigma[CartesianIndex[i]]*)
```

$$\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k.\overline{\sigma }^l$$

```
(*CSI[i] . CSI[j] . CSI[k] . CSI[l]*)
```
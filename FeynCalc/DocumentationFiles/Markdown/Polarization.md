##  Polarization 

Polarization[k] is the head of a polarization momentum with (incoming) momentum k. A slashed polarization vector (e1(k) slash) has to be entered as GS[Polarization[k]]. The internal representation for a polarization vector e1 corresponding to a boson with four momentum k is: Momentum[ Polarization[ k, I ] ]. With this notation transversality of polarization vectors is provided, i.e. , Pair[ Momentum[k], Momentum[ Polarization[k, I] ] ] yields 0. Polarization[k,-I] denotes the complex conjugate polarization. Polarization is also an option of various functions related to the operator product expansion. The setting 0 denotes the unpolarized and 1 the polarized case..

###  See also 

PolarizationVector, PolarizationSum, DoPolarizationSums.

###  Examples 

```mathematica
Polarization[k] 
 
Polarization[k] // StandardForm 
 
Polarization[k] // ComplexConjugate 
 
Polarization[k] // ComplexConjugate // StandardForm 
 
GS[Polarization[k]] 
 
GS[Polarization[k]] // StandardForm 
 
Pair[ Momentum[k], Momentum[ Polarization[k, I] ] ]
```

$$\text{Polarization}(k,i)$$

```
(*Polarization[k, I]*)
```

$$\text{Polarization}(k,-i)$$

```
(*Polarization[k, -I]*)
```

$$\bar{\gamma }\cdot \bar{\varepsilon }(k)$$

```
(*GS[Polarization[k, I]]*)
```

$$\overline{k}\cdot \bar{\varepsilon }(k)$$
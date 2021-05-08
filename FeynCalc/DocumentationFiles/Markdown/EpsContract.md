##  EpsContract 

EpsContract[exp] handles contractions of two Levi-Civita tensors. It is also an option of Contract and other functions that specifies whether such contractions should be done or not..

###  See also 

Eps, Contract.

###  Examples 

```mathematica
LCD[\[Mu], \[Nu], \[Rho], \[Sigma]] 
 
EpsContract[% %] // Factor2 
 
Contract[% %] // Factor2 
 
Contract[LCD[\[Mu], \[Nu], \[Rho], \[Sigma]]^2, EpsContract -> False]
```

$$\overset{\text{}}{\epsilon }^{\mu \nu \rho \sigma }$$

$$(1-D) (2-D) (3-D) D$$

$$(1-D)^2 (2-D)^2 (3-D)^2 D^2$$

$$\left(\overset{\text{}}{\epsilon }^{\mu \nu \rho \sigma }\right)^2$$
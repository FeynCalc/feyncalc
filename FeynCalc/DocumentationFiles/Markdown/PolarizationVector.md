##  PolarizationVector 

PolarizationVector[p, mu] gives a polarization vector..

###  See also 

FV, Pair, Polarization.

##  PolarizationVector 

[k,μ]Conjugate[PolarizationVector[k,μ]]A polarization vector $varepsilon _{mu }(k)$is a special four-vector.PolarizationVector[k,mu]//StandardFormThe transverality property is not automatic. PolarizationVector[k,μ] FV[k,μ]Contract[%] PolarizationVector[k,μ,Transversality->True] FV[k,μ]Contract[%]Suppose that you are using unphysical polarization vectors for massless gauge bosons and intend to remove the unphysical degrees of freedom at a later stage using ghosts. In this case you must not use TransversalityTrue, since your polarization vectors are not transverse. Otherwise the result will be inconsistent..

###  Examples 

Here everything is correct, we can use the gauge trick with unphysical polarization vectors.

```mathematica
FCClearScalarProducts[]; SP[k1] = 0; SP[k2] = 0;
ех1 = SP[k1, Polarization[k1, I]] SP[k2, Polarization[k1, -I]] SP[k1, Polarization[k2, I]] SP[k2, Polarization[k2, -I]] 
 
ех1 // DoPolarizationSums[#, k1, 0] & // DoPolarizationSums[#, k2, 0] &
```

$$\left(\overline{\text{k1}}\cdot \bar{\varepsilon }(\text{k1})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }^*(\text{k2})\right) \left(\overline{\text{k1}}\cdot \bar{\varepsilon }(\text{k2})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }^*(\text{k1})\right)$$

$$(\overline{\text{k1}}\cdot \overline{\text{k2}})^2$$

Here we erroneously set  TransversalityTrue  and consequently obtain a wrong result. In pure QED the full result (physical amplitude squared) would still come out correct owing to Ward identities, but e.g. in QCD this would not be the case.

```mathematica
ех2 = SP[k1, Polarization[k1, I, Transversality -> True]] SP[k2, Polarization[k1, -I, Transversality -> True]] SP[k1, Polarization[k2, I, Transversality -> True]] SP[k2, Polarization[k2, -I, Transversality -> True]] // FCI 
 
ех2 // DoPolarizationSums[#, k1, 0] & // DoPolarizationSums[#, k2, 0] &
```

$$0$$

$$0$$
##  CovariantD 

CovariantD[μ] is a generic covariant derivative with Lorentz index μ.   CovariantD[x, μ] is a generic covariant derivative with respect to $x^{\mu }$.   CovariantD[μ, a, b] is a covariant derivative for a bosonic field that acts on $\text{QuantumField}[f,\{\},\{a,b\}]$, where $\text{f}$ is some field name and $\text{a}$ and $\text{b}$ are two $\text{SU}(N)$ indices in the adjoint representation.   CovariantD[OPEDelta, a, b] is a short form for CovariantD[μ, a, b] FV[OPEDelta, μ].   CovariantD[{OPEDelta, a, b}, {n}] yields the product of $\text{n}$ operators, where $\text{n}$ is an integer.   CovariantD[OPEDelta, a, b, {m, n}] gives the expanded form of CovariantD[OPEDelta, a, b]^m up to order $g{}^{\wedge}n$ for the gluon, where $\text{n}$ is an integer and $\text{g}$ the coupling constant indicated by the setting of the option CouplingConstant.   CovariantD[OPEDelta, {m, n}] gives the expanded form of CovariantD[OPEDelta]^m up to order $g{}^{\wedge}n$ of the fermionic field.To obtain the explicit expression for a particular covariant derivatine, the option $text{Explicit}$ must be set to $text{True}$..

###  Examples 

```mathematica
CovariantD[\[Mu]] 
 
CovariantD[\[Mu], a, b] 
 
CovariantD[\[Mu], Explicit -> True]
```

$$D_{\mu }$$

$$D_{\mu }^{ab}$$

$$\vec{\partial }_{\mu }-i g_s T^{\text{c24}}.A_{\mu }^{\text{c24}}$$

The first argument of CovariantD is intepreted as type LorentzIndex, except for OPEDelta, which is type Momentum.

```mathematica
CovariantD[OPEDelta] 
 
CovariantD[OPEDelta, a, b] 
 
CovariantD[OPEDelta, a, b, Explicit -> True] 
 
CovariantD[OPEDelta, Explicit -> True] 
 
CovariantD[OPEDelta, a, b, {2}]
```

$$D_{\Delta }$$

$$D_{\Delta }^{ab}$$

$$\delta ^{ab} \vec{\partial }_{\Delta }-g_s A_{\Delta }^{\text{c25}} f^{ab\text{c25}}$$

$$\vec{\partial }_{\Delta }-i g_s T^{\text{c26}}.A_{\Delta }^{\text{c26}}$$

$$\left(\delta ^{a\text{c27}} \vec{\partial }_{\Delta }-g_s A_{\Delta }^{\text{e28}} f^{a\text{c27}\text{e28}}\right).\left(\delta ^{b\text{c27}} \vec{\partial }_{\Delta }-g_s A_{\Delta }^{\text{e29}} f^{\text{c27}b\text{e29}}\right)$$

This gives$m * vec{partial }_{Delta },$the partial derivative $vec{partial }_{mu  }$contracted with $Delta ^{mu }.$

```mathematica
CovariantD[OPEDelta, a, b, {OPEm, 0}]
```

$$\delta ^{ab} \left(\vec{\partial }_{\Delta }\right){}^m$$

The expansion up to first order in the coupling constant $g_s$ (the sum is the FeynCalcOPESum)

```mathematica
CovariantD[OPEDelta, a, b, {OPEm, 1}]
```

$$\delta ^{ab} \left(\vec{\partial }_{\Delta }\right){}^m-g_s \left(\sum _{i=0}^{-1+m} \left(\vec{\partial }_{\Delta }\right){}^i.A_{\Delta }^{\text{c39}_1}.\left(\vec{\partial }_{\Delta }\right){}^{-1-i+m} f^{ab\text{c39}_1}\right)$$

The expansion up to second order in the $g_s.$

```mathematica
CovariantD[OPEDelta, a, b, {OPEm, 2}] 
 
CovariantD[OPEDelta, a, b]^OPEm 
 
CovariantD[OPEDelta, {OPEm, 2}] 
 
CovariantD[OPEDelta, Explicit -> True] // StandardForm 
 
CovariantD[\[Mu], a, b, Explicit -> True] // StandardForm
```

$$-g_s \left(\sum _{i=0}^{-1+m} \left(\vec{\partial }_{\Delta }\right){}^i.A_{\Delta }^{\text{c47}_1}.\left(\vec{\partial }_{\Delta }\right){}^{-1-i+m} f^{ab\text{c47}_1}\right)-g_s^2 \left(\sum _{j=0}^{-2+m} \left(\sum _{i=0}^j \left(\vec{\partial }_{\Delta }\right){}^i.A_{\Delta }^{\text{c51}_1}.\left(\vec{\partial }_{\Delta }\right){}^{-i+j}.A_{\Delta }^{\text{c51}_2}.\left(\vec{\partial }_{\Delta }\right){}^{-2-j+m} f^{a\text{c51}_1\text{e50}_1} f^{b\text{c51}_2\text{e50}_1}\right)\right)+\delta ^{ab} \left(\vec{\partial }_{\Delta }\right){}^m$$

$$\left(D_{\Delta }^{ab}\right){}^m$$

$$-i g_s \left(\sum _{i=0}^{-1+m} \left(\vec{\partial }_{\Delta }\right){}^i.A_{\Delta }^{\text{c60}_1}.\left(\vec{\partial }_{\Delta }\right){}^{-1-i+m} T^{\text{c60}_1}\right)-g_s^2 \left(\sum _{j=0}^{-2+m} \left(\sum _{i=0}^j T^{\text{c64}_1}.T^{\text{c64}_2} \left(\vec{\partial }_{\Delta }\right){}^i.A_{\Delta }^{\text{c64}_1}.\left(\vec{\partial }_{\Delta }\right){}^{-i+j}.A_{\Delta }^{\text{c64}_2}.\left(\vec{\partial }_{\Delta }\right){}^{-2-j+m}\right)\right)+\left(\vec{\partial }_{\Delta }\right){}^m$$

```
(*RightPartialD[Momentum[OPEDelta]] - I SUNT[SUNIndex[c67]] . QuantumField[GaugeField, Momentum[OPEDelta], SUNIndex[c67]] SMP["g_s"]*)

(*RightPartialD[LorentzIndex[\[Mu]]] SUNDelta[a, b] - QuantumField[GaugeField, LorentzIndex[\[Mu]], SUNIndex[c68]] SMP["g_s"] SUNF[a, b, c68]*)
```
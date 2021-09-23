## CouplingConstant

`CouplingConstant` is an option for several Feynman rule functions and for `CovariantD` and `FieldStrength`. In the convention of the add-on PHI, `CouplingConstant` is also the head of coupling constants.  `CouplingConstant` takes three extra optional arguments, with heads `RenormalizationState`, `RenormalizationScheme` and `ExpansionState` respectively.  E.g. `CouplingConstant[QED[1]]` is the unit charge, `CouplingConstant[ChPT2[4],1]` is the first of the coupling constants of the Lagrangian `ChPT2[4]`.  

`CouplingConstant[a_,b_,c___][i_] := CouplingConstant[a,b,RenormalizationState[i],c]`.

### See also

[Overview](Extra/FeynCalc.md), [CovariantD](CovariantD.md), [FieldStrength](FieldStrength.md).

### Examples

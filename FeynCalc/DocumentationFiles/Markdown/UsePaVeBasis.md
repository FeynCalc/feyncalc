## UsePaVeBasis

`UsePaVeBasis` is an option of `TID`. When set to True, tensor reduction is always performed in terms of the Passarino-Veltman coefficient functions (e.g. $B_1$, $B_{11}$, $C_{001}$ etc.) even if those can be reduced to the scalar functions $A_0$, $B_0$, $C_0$, $D_0$. By default this is done automatically only for tensor integrals with vanishing Gram determinants.

This option may be useful, if you are doing computations where the kinematics may later lead to vanishing Gram determinants or if you plan to evaluate all the Passarino-Veltman coefficient functions numerically (e.g. with LoopTools or Collier)

### See also

[Overview](Extra/FeynCalc.md), [TID](TID.md).

### Examples

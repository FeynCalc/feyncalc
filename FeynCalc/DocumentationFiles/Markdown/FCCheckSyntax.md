##  FCCheckSyntax 

FCCheckSyntax[exp] attempts to detect mistakes and inconsistencies in the user input. The function returns the original expression but will abort the evaluation if it thinks that the input is incorrect.Notice that false positives are possible and it is not guaranteed that the input which passes FCCheckSyntax is indeed fully correct. FCCheckSyntax is also an option for several FeynCalc routines If set to True, those functions will try to check the syntax of the input expressions to detect possible inconsistencies. However, on large expressions such checks may cost a lot of performance, which is why this option is set to False by default..

###  See also 

Typical mistake, using $text{Times}$ instead of $text{Dot}$ in noncommutative products

###  Examples 

```mathematica
FCCheckSyntax[GA[mu]*GA[nu]]
FCCheckSyntax::failmsg : Error! FCCheckSyntax has found an inconsistency in your input expression and must abort the evaluation . The problem reads : Commutative products of DiracGamma in  Overscript[\[Gamma], _]^mu Overscript[\[Gamma], _]^nu >> 
  
 
```

$$![11xpt5lqikapr](img/11xpt5lqikapr.png)$$

$$\text{$\$$Aborted}$$

Another common mistake, Einstein summation convention is violated

```mathematica
FCCheckSyntax[FV[p, \[Mu]] FV[q, \[Mu]] FV[r, \[Mu]]]
FCCheckSyntax::failmsg : Error! FCCheckSyntax has found an inconsistency in your input expression and must abort the evaluation . The problem reads : More than two repeating indices in  Pair[LorentzIndex[\[Mu]], Momentum[p]]*Pair[LorentzIndex[\[Mu]], Momentum[q]]*Pair[LorentzIndex[\[Mu]], Momentum[r]] >>
```

$$![046y2d6549xga](img/046y2d6549xga.png)$$

$$\text{$\$$Aborted}$$
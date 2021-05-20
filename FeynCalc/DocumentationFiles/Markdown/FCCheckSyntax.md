##  FCCheckSyntax 

`FCCheckSyntax[exp]` attempts to detect mistakes and inconsistencies in the user input. The function returns the original expression but will abort the evaluation if it thinks that the input is incorrect. Notice that false positives are possible and it is not guaranteed that the input which passes `FCCheckSyntax` is indeed fully correct.

`FCCheckSyntax` is also an option for several FeynCalc routines. If set to `True`, those functions will try to check the syntax of the input expressions to detect possible inconsistencies. However, on large expressions such checks may cost a lot of performance, which is why this option is set to `False` by default..

###  See also 

###  Examples 

Typical mistake, using `Times` instead of `Dot` in noncommutative products

```mathematica
FCCheckSyntax[GA[mu]*GA[nu]]
```

$$![1sq4e53u4o4w2](img/1sq4e53u4o4w2.png)$$

$$\text{$\$$Aborted}$$

Another common mistake, Einstein summation convention is violated

```mathematica
FCCheckSyntax[FV[p, \[Mu]] FV[q, \[Mu]] FV[r, \[Mu]]]
```

$$![1qjo3b996z6wx](img/1qjo3b996z6wx.png)$$

$$\text{$\$$Aborted}$$
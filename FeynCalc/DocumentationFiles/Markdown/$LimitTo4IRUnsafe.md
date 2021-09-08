## $LimitTo4IRUnsafe

`$LimitTo4IRUnsafe` is a global variable that determines whether the simplifications described in `$LimitTo4` are applied also to $C$ and $D$ Passarino-Veltman functions. In this case it is assumed that such  functions are either IR finite, or the IR divergences are regulated  without using dimensional regularization (i.e. by introducing  fictitious masses). Otherwise the results will be inconsistent. If this option is activated, it is the task of the user to ensure that IR divergences are properly regulated, such that no mixing of $\varepsilon$ from IR and UV can occur. The default value is `False`.

### See also

[Overview](Extra/FeynCalc.md), [PaVe](PaVe.md), [PaVeReduce](PaVeReduce.md), [OneLoop](OneLoop.md), [$LimitTo4]($LimitTo4.md), [PaVeLimitTo4](PaVeLimitTo4.md).

### Examples

```mathematica
$LimitTo4IRUnsafe
```

$$\text{False}$$

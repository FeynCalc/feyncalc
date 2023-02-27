```mathematica
 
```

## FCIteratedIntegralSimplify

`FCIteratedIntegralSimplify[ex]` uses linearity to simplify nested products and linear combinations of `FCIteratedIntegral`s.

### See also

[Overview](Extra/FeynCalc.md), [FCIteratedIntegral](FCIteratedIntegral.md), [FCIteratedIntegral](FCIteratedIntegral.md), [FCGPL](FCGPL.md).

### Examples

```mathematica
int = C[1, 0] + Epsilon*(C[1, 1] + FCIteratedIntegral[C[1, 0]*FCPartialFractionForm[0, {{{x, -1}, -2}}, x], x]) + 
   Epsilon^2*(C[1, 2] + FCIteratedIntegral[(C[1, 1] + FCIteratedIntegral[C[1, 0]*FCPartialFractionForm[0, 
             {{{x, -1}, -2}}, x], x])*FCPartialFractionForm[0, {{{x, -1}, -2}}, x], x]) + 
   Epsilon^3*(C[1, 3] + FCIteratedIntegral[(C[1, 2] + FCIteratedIntegral[(C[1, 1] + 
              FCIteratedIntegral[C[1, 0]*FCPartialFractionForm[0, {{{x, -1}, -2}}, x], x])*FCPartialFractionForm[0, 
             {{{x, -1}, -2}}, x], x])*FCPartialFractionForm[0, {{{x, -1}, -2}}, x], x])
```

$$\varepsilon ^3 \left(\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right) \left(\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right) \left(\text{FCIteratedIntegral}\left(C[1,0] \;\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right),x\right)+C[1,1]\right),x\right)+C[1,2]\right),x\right)+C[1,3]\right)+\varepsilon ^2 \left(\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right) \left(\text{FCIteratedIntegral}\left(C[1,0] \;\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right),x\right)+C[1,1]\right),x\right)+C[1,2]\right)+\varepsilon  \left(\text{FCIteratedIntegral}\left(C[1,0] \;\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right),x\right)+C[1,1]\right)+C[1,0]$$

```mathematica
FCIteratedIntegralSimplify[int]
```

$$\varepsilon ^3 \left(C[1,2] \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right),x\right)+C[1,1] \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right) \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right),x\right),x\right)+C[1,0] \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right) \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right) \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right),x\right),x\right),x\right)+C[1,3]\right)+\varepsilon ^2 \left(C[1,1] \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right),x\right)+C[1,0] \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right) \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right),x\right),x\right)+C[1,2]\right)+\varepsilon  \left(C[1,0] \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right),x\right)+C[1,1]\right)+C[1,0]$$
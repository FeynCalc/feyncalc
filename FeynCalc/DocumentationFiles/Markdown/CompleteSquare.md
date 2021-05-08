##  CompleteSquare 

CompleteSquare completes the square of a second order polynomial in the momentum x. CompleteSquare[a $p^2$+b p+c, p] -> -$b^2$/(4 a)+c+a (b/(2 a)+x)^2. CompleteSquare[a $p^2$+b p+c, p, q] -> {-$b^2$/(4 a)+c+a $q^2$, q->b/(2 a)+p}..

###  Examples 

```mathematica
5 SP[2 p + 3 r, p + r] 
 
CompleteSquare[%, p] 
 
% - %% // ScalarProductExpand // Expand 
 
CompleteSquare[5 SP[2 p + 3 r, p + r], p, q]
```

$$5 \left((\overline{p}+\overline{r})\cdot (2 \overline{p}+3 \overline{r})\right)$$

$$10 (\overline{p}+\frac{5 \overline{r}}{4})^2-\frac{5 \overline{r}^2}{8}$$

$$0$$

$$\left\{10 \overline{q}^2-\frac{5 \overline{r}^2}{8},\overline{q}\to \overline{p}+\frac{5 \overline{r}}{4}\right\}$$
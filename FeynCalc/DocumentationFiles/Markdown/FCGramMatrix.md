##  FCGramMatrix 

FCGramMatrix[{p1, p2, ...}] creates Gram matrix from the given list of momenta..

###  See also 

FCGramDeterminant.

###  Examples 

```mathematica
FCGramMatrix[{p1, p2}] 
 
FCGramMatrix[{p1, p2, p3}] 
 
FCGramMatrix[{p1, p2, p3}, Head -> {CartesianPair, CartesianMomentum},Dimension -> D - 1] 
 
Det[%] 
 
FCGramDeterminant[{p1, p2, p3}, Head -> {CartesianPair, CartesianMomentum}, Dimension -> D - 1]
```

$$\left(
\begin{array}{cc}
 2 \text{p1}^2 & 2 (\text{p1}\cdot \text{p2}) \\
 2 (\text{p1}\cdot \text{p2}) & 2 \text{p2}^2 \\
\end{array}
\right)$$

$$\left(
\begin{array}{ccc}
 2 \text{p1}^2 & 2 (\text{p1}\cdot \text{p2}) & 2 (\text{p1}\cdot \text{p3}) \\
 2 (\text{p1}\cdot \text{p2}) & 2 \text{p2}^2 & 2 (\text{p2}\cdot \text{p3}) \\
 2 (\text{p1}\cdot \text{p3}) & 2 (\text{p2}\cdot \text{p3}) & 2 \text{p3}^2 \\
\end{array}
\right)$$

$$\left(
\begin{array}{ccc}
 2 \text{p1}^2 & 2 (\text{p1}\cdot \text{p2}) & 2 (\text{p1}\cdot \text{p3}) \\
 2 (\text{p1}\cdot \text{p2}) & 2 \text{p2}^2 & 2 (\text{p2}\cdot \text{p3}) \\
 2 (\text{p1}\cdot \text{p3}) & 2 (\text{p2}\cdot \text{p3}) & 2 \text{p3}^2 \\
\end{array}
\right)$$

$$-8 \text{p3}^2 (\text{p1}\cdot \text{p2})^2-8 \text{p1}^2 (\text{p2}\cdot \text{p3})^2-8 \text{p2}^2 (\text{p1}\cdot \text{p3})^2+8 \text{p1}^2 \text{p2}^2 \text{p3}^2+16 (\text{p1}\cdot \text{p2}) (\text{p1}\cdot \text{p3}) (\text{p2}\cdot \text{p3})$$

$$-8 \text{p3}^2 (\text{p1}\cdot \text{p2})^2-8 \text{p1}^2 (\text{p2}\cdot \text{p3})^2-8 \text{p2}^2 (\text{p1}\cdot \text{p3})^2+8 \text{p1}^2 \text{p2}^2 \text{p3}^2+16 (\text{p1}\cdot \text{p2}) (\text{p1}\cdot \text{p3}) (\text{p2}\cdot \text{p3})$$
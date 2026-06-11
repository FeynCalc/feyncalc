## FCLoopIntegralToGraph

`FCLoopIntegralToGraph[int, {q1, q2, ...}]` constructs a graph representation of the loop integral `int` that depends on the loop momenta `q1, q2, ...`. The function returns a list of the form `{edges,labels,props,pref}`, where `edges` is a list of edge rules representing the loop integral `int`, `labels` is a list of lists containing the line momentum, multiplicity and the mass term of each propagator, `props` is a list with the original propagators and `pref` is the piece of the integral that was ignored when constructing the graph representation (e.g. scalar products or vectors in the numerator) .

Use `FCLoopGraphPlot` to visualize the output of `FCLoopIntegralToGraph`.

A quick and simple way to plot the graph is to evaluate `GraphPlot[List @@@ Transpose[output[[1 ;; 2]]]]` or `GraphPlot[Labeled @@@ Transpose[output[[1 ;; 2]]]]`. The visual quality will not be that great, though. To obtain a nicer plot one might use `GraphPlot` with a custom `EdgeTaggedGraph` or export the output to a file and visualize it with an external tool such as dot/neato from [graphviz](https://graphviz.org/).

It is also possible to invoke the function as `FCLoopIntegralToGraph[GLI[...], FCTopology[...]]` or `FCLoopIntegralToGraph[FCTopology[...]]`.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopFactorizingSplit](FCLoopFactorizingSplit.md), [FCLoopGraphPlot](FCLoopGraphPlot.md).

### Examples

```mathematica
out = FCLoopIntegralToGraph[FAD[{q - k1}, k1, q - k2, k2, {k2 - k3, mb}, {k1 - k3, mb}], {k1, k2, k3}]
```

$$\left\{\{-3\to 2,-1\to 1,1\to 3,1\to 4,2\to 3,2\to 4,3\to 4,3\to 4\},\left\{-q,q,\{\text{k2},1,0\},\{q-\text{k2},1,0\},\{\text{k1},1,0\},\{q-\text{k1},1,0\},\left\{\text{k2}-\text{k3},1,-\text{mb}^2\right\},\left\{\text{k1}-\text{k3},1,-\text{mb}^2\right\}\right\},\left\{0,0,\frac{1}{(\text{k2}^2+i \eta )},\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((q-\text{k2})^2+i \eta )},\frac{1}{((q-\text{k1})^2+i \eta )},\frac{1}{((\text{k2}-\text{k3})^2-\text{mb}^2+i \eta )},\frac{1}{((\text{k1}-\text{k3})^2-\text{mb}^2+i \eta )}\right\},1\right\}$$

```mathematica
FCLoopGraphPlot[out]
```

![0gffghxrhsdaq](img/0gffghxrhsdaq.svg)

```mathematica
Labeled @@@ Transpose[out[[1 ;; 2]]]
```

![1sdh86qtjcuid](img/1sdh86qtjcuid.svg)

```mathematica
GraphPlot[List @@@ Transpose[out[[1 ;; 2]]]]
```

![0f7wd5u42u06b](img/0f7wd5u42u06b.svg)

```mathematica
FCLoopIntegralToGraph[FAD[{q - k1}, k1, q - k2, k2, {k2 - k3, mb}, {k1 - k3, mb}], {k1, k2,k3}]
```

$$\left\{\{-3\to 2,-1\to 1,1\to 3,1\to 4,2\to 3,2\to 4,3\to 4,3\to 4\},\left\{-q,q,\{\text{k2},1,0\},\{q-\text{k2},1,0\},\{\text{k1},1,0\},\{q-\text{k1},1,0\},\left\{\text{k2}-\text{k3},1,-\text{mb}^2\right\},\left\{\text{k1}-\text{k3},1,-\text{mb}^2\right\}\right\},\left\{0,0,\frac{1}{(\text{k2}^2+i \eta )},\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((q-\text{k2})^2+i \eta )},\frac{1}{((q-\text{k1})^2+i \eta )},\frac{1}{((\text{k2}-\text{k3})^2-\text{mb}^2+i \eta )},\frac{1}{((\text{k1}-\text{k3})^2-\text{mb}^2+i \eta )}\right\},1\right\}$$

```mathematica
FAD[q - k1, k1, q - k2, k2, {k2 - k3, mb}, {k1 - k3, mb}]
```

$$\frac{1}{(q-\text{k1})^2.\text{k1}^2.(q-\text{k2})^2.\text{k2}^2.\left((\text{k2}-\text{k3})^2-\text{mb}^2\right).\left((\text{k1}-\text{k3})^2-\text{mb}^2\right)}$$

If the input is given as a list of propagators, their ordering will be preserved when constructing the graph

```mathematica
FCLoopIntegralToGraph[FCTopology[topo1, {FAD[q - k1], FAD[k1], FAD[q - k2], FAD[k2], 
    FAD[{k2 - k3, mb}], FAD[{k1 - k3, mb}]}, {k1, k2, k3}, {q}, {}, {}]]
```

$$\left\{\{-3\to 2,-1\to 1,1\to 3,1\to 4,2\to 3,2\to 4,3\to 4,3\to 4\},\left\{-q,q,\{q-\text{k1},1,0\},\{\text{k1},1,0\},\{q-\text{k2},1,0\},\{\text{k2},1,0\},\left\{\text{k2}-\text{k3},1,-\text{mb}^2\right\},\left\{\text{k1}-\text{k3},1,-\text{mb}^2\right\}\right\},\left\{0,0,\frac{1}{((q-\text{k1})^2+i \eta )},\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((q-\text{k2})^2+i \eta )},\frac{1}{(\text{k2}^2+i \eta )},\frac{1}{((\text{k2}-\text{k3})^2-\text{mb}^2+i \eta )},\frac{1}{((\text{k1}-\text{k3})^2-\text{mb}^2+i \eta )}\right\},1\right\}$$

```mathematica
FCLoopIntegralToGraph[GLI[topo1, {1, 1, 1, 1, 1, 1}], 
  FCTopology[topo1, {FAD[q - k1], FAD[k1], FAD[q - k2], FAD[k2], 
    FAD[{k2 - k3, mb}], FAD[{k1 - k3, mb}]}, {k1, k2, k3}, {q}, {}, {}]]
```

$$\left\{\{-3\to 2,-1\to 1,1\to 3,1\to 4,2\to 3,2\to 4,3\to 4,3\to 4\},\left\{-q,q,\{\text{k2},1,0\},\{q-\text{k2},1,0\},\{\text{k1},1,0\},\{q-\text{k1},1,0\},\left\{\text{k2}-\text{k3},1,-\text{mb}^2\right\},\left\{\text{k1}-\text{k3},1,-\text{mb}^2\right\}\right\},\left\{0,0,\frac{1}{(\text{k2}^2+i \eta )},\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((q-\text{k2})^2+i \eta )},\frac{1}{((q-\text{k1})^2+i \eta )},\frac{1}{((\text{k2}-\text{k3})^2-\text{mb}^2+i \eta )},\frac{1}{((\text{k1}-\text{k3})^2-\text{mb}^2+i \eta )}\right\},1\right\}$$

If the second argument contains multiple topologies, the function will automatically select the relevant ones.

```mathematica
FCLoopIntegralToGraph[GLI[topo1, {1, 1, 1, 1, 1, 1}], 
  {FCTopology[topo1, {FAD[q - k1], FAD[k1], FAD[q - k2], FAD[k2], 
     FAD[{k2 - k3, mb}], FAD[{k1 - k3, mb}]}, {k1, k2, k3}, {q}, {}, {}], 
   FCTopology[topo2, {FAD[q - k1], FAD[k1], FAD[q - k2], FAD[k2], 
     FAD[{k2 - k3, mg}], FAD[{k1 - k3, mg}]}, {k1, k2, k3}, {q}, {}, {}] 
  }]
```

$$\left\{\{-3\to 2,-1\to 1,1\to 3,1\to 4,2\to 3,2\to 4,3\to 4,3\to 4\},\left\{-q,q,\{\text{k2},1,0\},\{q-\text{k2},1,0\},\{\text{k1},1,0\},\{q-\text{k1},1,0\},\left\{\text{k2}-\text{k3},1,-\text{mb}^2\right\},\left\{\text{k1}-\text{k3},1,-\text{mb}^2\right\}\right\},\left\{0,0,\frac{1}{(\text{k2}^2+i \eta )},\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((q-\text{k2})^2+i \eta )},\frac{1}{((q-\text{k1})^2+i \eta )},\frac{1}{((\text{k2}-\text{k3})^2-\text{mb}^2+i \eta )},\frac{1}{((\text{k1}-\text{k3})^2-\text{mb}^2+i \eta )}\right\},1\right\}$$

Factorizing integrals cannot be reliably converted to graphs, so once such integrals are detected, the evaluation automatically halts. In this case the user should employ `FCLoopFactorizingSplit` or `FCLoopCreateFactorizingRules` to split the integrals into simpler ones and graph those.

```mathematica
int = SFAD[p1, Q - p1, p2, Q - p2]
```

$$\frac{1}{(\text{p1}^2+i \eta ).((Q-\text{p1})^2+i \eta ).(\text{p2}^2+i \eta ).((Q-\text{p2})^2+i \eta )}$$

```mathematica
FCLoopIntegralToGraph[int, {p1, p2}]
```

![0oqv7cyq1xvy8](img/0oqv7cyq1xvy8.svg)

$$\text{\$Aborted}$$

```mathematica
aux = FCLoopFactorizingSplit[int, {p1, p2}]
```

$$\left(
\begin{array}{ccc}
 \frac{1}{(\text{p1}^2+i \eta ) ((Q-\text{p1})^2+i \eta )} & \{\text{p1}\} & \{\} \\
 \frac{1}{(\text{p2}^2+i \eta ) ((Q-\text{p2})^2+i \eta )} & \{\text{p2}\} & \{\} \\
\end{array}
\right)$$

```mathematica
FCLoopIntegralToGraph[#[[1]], #[[2]]] & /@ aux
```

$$\left(
\begin{array}{cccc}
 \{-3\to 2,-1\to 1,1\to 2,1\to 2\} & \{-Q,Q,\{\text{p1},1,0\},\{Q-\text{p1},1,0\}\} & \left\{0,0,\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((Q-\text{p1})^2+i \eta )}\right\} & 1 \\
 \{-3\to 2,-1\to 1,1\to 2,1\to 2\} & \{-Q,Q,\{\text{p2},1,0\},\{Q-\text{p2},1,0\}\} & \left\{0,0,\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{((Q-\text{p2})^2+i \eta )}\right\} & 1 \\
\end{array}
\right)$$

```mathematica
aux = FCLoopFactorizingSplit[SFAD[p1, Q - p1, p3, Q - p3], {p1, p3}]
```

$$\left(
\begin{array}{ccc}
 \frac{1}{(\text{p1}^2+i \eta ) ((Q-\text{p1})^2+i \eta )} & \{\text{p1}\} & \{\} \\
 \frac{1}{(\text{p3}^2+i \eta ) ((Q-\text{p3})^2+i \eta )} & \{\text{p3}\} & \{\} \\
\end{array}
\right)$$

```mathematica
FCLoopIntegralToGraph[#[[1]], #[[2]]] & /@ aux
```

$$\left(
\begin{array}{cccc}
 \{-3\to 2,-1\to 1,1\to 2,1\to 2\} & \{-Q,Q,\{\text{p1},1,0\},\{Q-\text{p1},1,0\}\} & \left\{0,0,\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((Q-\text{p1})^2+i \eta )}\right\} & 1 \\
 \{-3\to 2,-1\to 1,1\to 2,1\to 2\} & \{-Q,Q,\{\text{p3},1,0\},\{Q-\text{p3},1,0\}\} & \left\{0,0,\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{((Q-\text{p3})^2+i \eta )}\right\} & 1 \\
\end{array}
\right)$$

```mathematica
aux = FCLoopFactorizingSplit[FAD[{q - k1}, k1, {k2, mg}, {k3, mb}, {k2 - k3, mb}], {k1, k2, k3}]
```

$$\left(
\begin{array}{ccc}
 \frac{1}{\left(\text{k2}^2-\text{mg}^2\right) \left(\text{k3}^2-\text{mb}^2\right) \left((\text{k2}-\text{k3})^2-\text{mb}^2\right)} & \{\text{k2},\text{k3}\} & \{\} \\
 \frac{1}{\text{k1}^2 (q-\text{k1})^2} & \{\text{k1}\} & \{\} \\
\end{array}
\right)$$

```mathematica
FCLoopIntegralToGraph[#[[1]], #[[2]]] & /@ aux
```

$$\left(
\begin{array}{cccc}
 \{1\to 2,1\to 2,1\to 2\} & \left(
\begin{array}{ccc}
 \;\text{k3} & 1 & -\text{mb}^2 \\
 \;\text{k2} & 1 & -\text{mg}^2 \\
 \;\text{k2}-\text{k3} & 1 & -\text{mb}^2 \\
\end{array}
\right) & \left\{\frac{1}{(\text{k3}^2-\text{mb}^2+i \eta )},\frac{1}{(\text{k2}^2-\text{mg}^2+i \eta )},\frac{1}{((\text{k2}-\text{k3})^2-\text{mb}^2+i \eta )}\right\} & 1 \\
 \{-3\to 2,-1\to 1,1\to 2,1\to 2\} & \{-q,q,\{\text{k1},1,0\},\{q-\text{k1},1,0\}\} & \left\{0,0,\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((q-\text{k1})^2+i \eta )}\right\} & 1 \\
\end{array}
\right)$$
##  FCLoopIntegralToGraph 

`FCLoopIntegralToGraph[int, {q1, q2, ...}]` constructs a graph representation of the loop integral `int` that depends on the loop momenta `q1, q2, ...`. The function returns a list of the form `{edges,labels,props,pref}`, where `edges` is a list of edge rules representing the loop integral `int`, `labels` is a list of lists containing the line momentum, multiplicity and the mass term of each propagator, `props` is a list with the original propagators and `pref` is the piece of the integral that was ignored when constructing the graph representation (e.g. scalar products or vectors in the numerator) 

Use `FCLoopGraphPlot` to visualize the output of `FCLoopIntegralToGraph`

A quick and simple way to plot the graph is to evaluate `GraphPlot[List @@@ Transpose[output[[1 ;; 2]]]]` or `GraphPlot[Labeled @@@ Transpose[output[[1 ;; 2]]]]`. The visual quality will not be that great, though. To obtain a nicer plot one might use `GraphPlot` with a custom `EdgeTaggedGraph` or export the output to a file and visualize it with an external tool such as dot/neato from graphviz.

###  See also 

FCLoopGraphPlot.

###  Examples 

```mathematica
out = FCLoopIntegralToGraph[ FAD[{q - k1}, k1, q - k2, k2, {k2 - k3, mb}, {k1 - k3, mb}], {k1, k2, k3}]
```

$$![07wzdouq1l71c](img/07wzdouq1l71c.png)$$

```mathematica
FCLoopGraphPlot[out]
```

$$![1l90setro1gdh](img/1l90setro1gdh.png)$$

```mathematica
Labeled @@@ Transpose[out[[1 ;; 2]]]
```

$$![0w6o8tzfkmc09](img/0w6o8tzfkmc09.png)$$

```mathematica
GraphPlot[List @@@ Transpose[out[[1 ;; 2]]]]
```

$$![0u8xrfncrww4f](img/0u8xrfncrww4f.png)$$
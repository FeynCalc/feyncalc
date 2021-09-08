## FCLoopAddEdgeTags

`FCLoopAddEdgeTags[edges_List, labels_List]` adds user-defined styles and labels to the given edges using the provided list of labels. Styles and labels are attached using the replacement rules provided via the `Style` and `Labeled` options.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopGraphPlot](FCLoopGraphPlot.md).

### Examples

If you use `FCLoopIntegralToGraph` for visualizing loop integrals, the first two entries of its output can be used as the input for FCLoopAddEdgeTags, e.g.  

```mathematica
FCLoopAddEdgeTags[FCLoopIntegralToGraph[FAD[p, p - k], {p}][[1 ;; 2]]]
GraphPlot[%]
```

$$\{-3\leftrightarrow 2,-1\leftrightarrow 1,1\leftrightarrow 2,1\leftrightarrow 2\}$$

![0z92umyme84rx](img/0z92umyme84rx.svg)

If you just want to plot the obtained graph, it is easier to process the output of `FCLoopIntegralToGraph` directly with `FCLoopGraphPlot`, which internally uses `FCLoopAddEdgeTags`.

```mathematica
FCLoopIntegralToGraph[FAD[p, p - k], {p}]
FCLoopGraphPlot[%] 
  
 

```

$$\left\{\{-3\to 2,-1\to 1,1\to 2,1\to 2\},\{k,k,\{p,1,0\},\{p-k,1,0\}\},\left\{0,0,\frac{1}{(p^2+i \eta )},\frac{1}{((p-k)^2+i \eta )}\right\},1\right\}$$

![18zlvfvc5dy6q](img/18zlvfvc5dy6q.svg)
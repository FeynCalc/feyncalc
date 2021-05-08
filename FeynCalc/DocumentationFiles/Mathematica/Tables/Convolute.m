 
(* ::Section:: *)
(* Convolute *)
(* ::Text:: *)
(*Convolute[f, g, x] convolutes f(x) and g(x), i.e., $\int _0^1dx_1 \int _0^1dx_2 \delta \left(x - x_1 x_2\right) f\left(x_1\right) g\left(x_2\right) .$ Convolute[f, g] is equivalent to Convolute[f, g, x]. Convolute[exp, {x1, x2}] assumes that exp is polynomial in x1 and x2. Convolute uses table-look-up and does not do any integral calculations, only linear algebra..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*PlusDistribution, ConvoluteTable.*)



(* ::Subsection:: *)
(* Examples *)



Convolute[1,1]/.FCGV[z_]:>ToExpression[z]

Convolute[x,x]/.FCGV[z_]:>ToExpression[z]

Convolute[1,x]/.FCGV[z_]:>ToExpression[z]

Convolute[1,1/(1-x)]/.FCGV[z_]:>ToExpression[z]

Convolute[1,PlusDistribution[1/(1-x)]]/.FCGV[z_]:>ToExpression[z]

Convolute[1/(1-x),x]/.FCGV[z_]:>ToExpression[z]

Convolute[1/(1-x),1/(1-x)]/.FCGV[z_]:>ToExpression[z]

Convolute[1,Log[1-x]]/.FCGV[z_]:>ToExpression[z]

Convolute[1,x Log[1-x]]/.FCGV[z_]:>ToExpression[z]

Convolute[1/(1-x),Log[1-x]]/.FCGV[z_]:>ToExpression[z]

Convolute[1/(1-x),x Log[1-x]]/.FCGV[z_]:>ToExpression[z]

Convolute[Log[1-x]/(1-x),x]/.FCGV[z_]:>ToExpression[z]

Convolute[1,x Log[x]]/.FCGV[z_]:>ToExpression[z]

Convolute[Log[1-x],x]/.FCGV[z_]:>ToExpression[z]

Convolute[1/(1-x),Log[x]/(1-x)]/.FCGV[z_]:>ToExpression[z]

Convolute[1,Log[x]]/.FCGV[z_]:>ToExpression[z]

Convolute[x, x Log[x]]/.FCGV[z_]:>ToExpression[z]

Convolute[1/(1-x),Log[x]]/.FCGV[z_]:>ToExpression[z]

Convolute[1,Log[x]/(1-x)]/.FCGV[z_]:>ToExpression[z]

Convolute[1/(1-x),x Log[x]]/.FCGV[z_]:>ToExpression[z]

Convolute[Log[x]/(1-x),x]/.FCGV[z_]:>ToExpression[z]

Convolute[1,x Log[x]]/.FCGV[z_]:>ToExpression[z]

Convolute[Log[x],x]/.FCGV[z_]:>ToExpression[z]

Convolute[1/(1-x),Log[1-x]/(1-x)]/.FCGV[z_]:>ToExpression[z]

Convolute[Log[1-x]/(1-x),Log[1-x]]/.FCGV[z_]:>ToExpression[z]

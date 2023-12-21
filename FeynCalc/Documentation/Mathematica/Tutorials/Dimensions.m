(* ::Package:: *)

 


(* ::Section:: *)
(*Dimensions*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Notation*)


(* ::Text:: *)
(*You might have wondered why 4-vectors, scalar products and Dirac matrices all have a bar, like*)
(*$\bar{p}^\mu$ or $\bar{p} \cdot \bar{q}$. The bar is there to specify that they are 4-dimensional objects. Objects that live in $D$ dimensions do not have a bar, cf.*)


FVD[p,\[Mu]]
%//FCI//StandardForm


MTD[\[Mu],\[Nu]]
%//FCI//StandardForm


(* ::Text:: *)
(*This origin of this notation is a [publication](https://inspirehep.net/record/124212) by Breitenlohner and Maison on the treatment of $\gamma^5$ in $D$ dimensions in the t'Hooft-Veltman scheme. The main idea was that we can decompose indexed objects into $4$- and $D-4$-dimensional pieces, e.g. $p^\mu = \bar{p}^\mu + \hat{p}^\mu$. Consequently, in FeynCalc we can also enter $D-4$-dimensional objects*)


FVE[p,\[Mu]]
%//FCI//StandardForm


MTE[p,q]
%//FCI//StandardForm


(* ::Text:: *)
(*When we contract Lorentz tensors from different dimensions, the contractions are resolved according to the rules from the paper of Breitenlohner and Maison, e. g.*)


FVD[p,\[Mu]]FV[q,\[Mu]]
Contract[%]


FV[p,\[Mu]]FVE[q,\[Mu]]
Contract[%]


(* ::Text:: *)
(*Sometimes we need to switch from one dimension to another, e.g. to convert a 4-dimensional object to a $D$-dimensional one or vice versa. This is done via*)


FVD[p,\[Mu]]
ChangeDimension[%,4]


(* ::Text:: *)
(*The second argument of `ChangeDimension` is the new dimension . The most common choices are $4$, $D$ or $D-4$*)


FVD[p,\[Mu]]
ChangeDimension[%,D-4]

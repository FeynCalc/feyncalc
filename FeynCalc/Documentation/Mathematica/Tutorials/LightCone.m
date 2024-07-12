(* ::Package:: *)

 


(* ::Section:: *)
(*Light-cone formalism*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Notation for light-cone components*)


(* ::Text:: *)
(*FeynCalc is equipped with special symbols that facilitate calculations involving light-cone vectors. The default $n$ and $\bar{n}$ vectors are defined via the global variables `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`. By default those are set to `FCGV["n"]` and `FCGV["nb"]` to avoid possible conflicts with user-defined variables*)


{$FCDefaultLightconeVectorN,$FCDefaultLightconeVectorNB}


(* ::Text:: *)
(*These names can be of course changed. A particularly convenient choice is to use `n` and `nb`. Notice that these commands must be evaluated at the beginning of every FeynCalc session*)


$FCDefaultLightconeVectorN=n;
$FCDefaultLightconeVectorNB=nb;


(* ::Text:: *)
(*Apart from this you must also explicitly define the values of the scalar products $n^2$, $\bar{n}^2$ and $n \cdot \bar{n}$*)


FCClearScalarProducts[]
ScalarProduct[n]=0;
ScalarProduct[nb]=0;
ScalarProduct[n,nb]=2;


(* ::Text:: *)
(*The Plus, Minus and peRpendicular components of 4-vectors are called `FVLP`, `FVLN` and `FVLR` respectively. The plus and minus components are immediately rewritten into forms involving $n$ and $\bar{n}$. The perpendicular component is a separate entity that cannot be simplified further.*)


{FVLP[p,\[Mu]],FVLN[p,\[Mu]],FVLR[p,\[Mu]]}


{FVLPD[p,\[Mu]],FVLND[p,\[Mu]],FVLRD[p,\[Mu]]}


(* ::Text:: *)
(*It is also possible to specify your own symbols for the light-cone vectors thus overriding what is set via the global variables*)


FVLR[p,mu,myN,myNB]
%//FCI//StandardForm


(* ::Text:: *)
(*Internally, the perpendicular component is implemented as an extra head wrapped around such internal symbols as `LorentzIndex` or `Momentum`. This head is called `LightConePerpendicularComponent` and has 3 arguments. The last two arguments specify the light-cone vectors.*)


?LightConePerpendicularComponent


(* ::Text:: *)
(*The pattern introduced for 4-vectors can be also found when working scalar products, metric tensors or Dirac matrices*)


{SPLP[p,q],SPLN[p,q],SPLR[p,q]}


{SPLPD[p,q],SPLND[p,q],SPLRD[p,q]}


{MTLP[\[Mu],\[Nu]],MTLN[\[Mu],\[Nu]],MTLR[\[Mu],\[Nu]]}


{GALP[\[Mu]],GALN[\[Mu]],GALR[\[Mu]]}


{GSLP[\[Mu]],GSLN[\[Mu]],GSLR[\[Mu]]}


(* ::Text:: *)
(*Contracting the full metric tensor with the perpendicular component returns the latter*)


MT[\[Mu],\[Nu]]MTLR[\[Mu],\[Rho]]
%//Contract


(* ::Text:: *)
(*The dimensionality of the perpendicular component is $2$ in $4$-dimensions and $D-2$ in $D$-dimensions*)


MT[\[Mu],\[Nu]]MTLR[\[Mu],\[Nu]]
%//Contract


MTD[\[Mu],\[Nu]]MTLRD[\[Mu],\[Nu]]
%//Contract


(* ::Subsection:: *)
(*Dirac matrices with light-cone components*)


(* ::Text:: *)
(*Dirac algebra involving matrices contracted to light-cone momenta or having particular light-cone components is fully supported. The general strategy followed by `DiracSimplify` is to move all perpendicular components to the very right of the chain.*)


ex1=GALR[p] . GA[\[Mu],\[Nu]]


ex1//DiracSimplify


ex2=GALR[p] . GA[\[Mu],\[Nu]] . GALR[p]


ex2//DiracSimplify


(* ::Text:: *)
(*Notice that when entering particular light-cone components of Dirac matrices, the standard trick for entering multiple indices does not work. This is because the 2nd and 3rd arguments are reserved for user-specified light-cone vectors*)


GALR[mu1,myN,myNB]
%//FCI//StandardForm


(* ::Text:: *)
(*Instead, you should put your list of indices into curly brackets*)


GALR[{\[Mu],\[Nu],\[Rho]}]


ex3=GALR[p] . GALR[{\[Mu],\[Nu]}] . GALR[p]


ex3//DiracSimplify


ex4=DiracTrace[GA[\[Rho],\[Sigma]] . GALR[{\[Mu],\[Nu]}]]


ex4//DiracSimplify


ex5=DiracTrace[GA[\[Rho],\[Sigma]] . GA[5] . GALR[{\[Mu],\[Nu]}]]


ex5//DiracSimplify


(* ::Subsection:: *)
(*Introducing light-cone components by hand*)


(* ::Text:: *)
(*In some calculations one might end up with a mixture of explicit light-cone components and generic Lorentz tensors. If those tensors admit a particularly simple representation in terms of light-cone components, it can be enforced using the function `ToLightConeComponents`*)


(* ::Text:: *)
(*For example, the following expression cannot be simplified any further*)


ex6=GS[nb,vp]


(* ::Text:: *)
(*Now let us suppose that $(v')^{\mu}$ can be actually written as $\alpha n^\mu + \bar{n}^{\mu}/(4 \alpha)$. We can implement this as  follows*)


SP[vp,nb]= 2*alpha;
SP[vp,n]= 2*1/(4alpha);
LightConePerpendicularComponent[Momentum[vp],Momentum[n],Momentum[nb]]=0;


FV[vp,mu]
%//ToLightConeComponents


(* ::Text:: *)
(*However, this will not make FeynCalc automatically simplify the Dirac chain*)


ex6//DiracSimplify


(* ::Text:: *)
(*Using `ToLightConeComponents` we can explicitly rewrite `vp` in the chain in terms of the light-cone components and hence enforce the desired simplification. In fact, the function will also automatically simplify some common expressions such $\gamma \cdot \bar{n} \gamma \cdot \bar{n} = \gamma \cdot n \gamma \cdot n = 0$*)


ex6//ToLightConeComponents
%//DiracSimplify


(* ::Text:: *)
(*Such simplifications inside `ToLightConeComponents` can be disabled using the option `DotSimplify`*)


ex6//ToLightConeComponents[#,DotSimplify->False]&
%//DiracSimplify


(* ::Subsection:: *)
(*Reductions of loop integrals with numerators involving light-cone components*)


int=FVLRD[p,\[Mu]]SFAD[p,p-q]


TID[int,p]


(* ::Subsection:: *)
(*Differentiations*)


(* ::Text:: *)
(*`FourDivergence` cannot yet differentiate w.r.t light-cone components directly. However, the same effect can be easily achieved by first differentiating w.r.t the usual 4-momentum and then contracting the free index with the corresponding metric tensor*)


ex=FV[p1,\[Mu]]/SP[p1]


(* ::Text:: *)
(*Differentiating w.r.t $p_{1,+}$, $p_{1,-}$ or $p_{1,\perp}$*)


MTLN[\[Nu],\[Rho]]FourDivergence[ex,FV[p1,\[Rho]]]//Contract


MTLP[\[Nu],\[Rho]]FourDivergence[ex,FV[p1,\[Rho]]]//Contract


MTLR[\[Nu],\[Rho]]FourDivergence[ex,FV[p1,\[Rho]]]//Contract

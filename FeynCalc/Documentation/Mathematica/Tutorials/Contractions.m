(* ::Package:: *)

 


(* ::Section:: *)
(*Contractions*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Simplifications of tensorial expressions*)


(* ::Text:: *)
(*Now that we have some basic understanding of FeynCalc objects, let us do something with them. Contractions of Lorentz indices are one of the most essential operations in symbolic QFT calculations. In FeynCalc the corresponding function is called `Contract`*)


FV[p,\[Mu]]MT[\[Mu],\[Nu]]
Contract[%]


FV[p,\[Alpha]]FV[q,\[Alpha]]
Contract[%]


(* ::Text:: *)
(*Notice that when we enter noncommutative objects, such as Dirac matrices, we use `Dot` (`.`) and not `Times` (`*`) *)


FV[p,\[Alpha]]MT[\[Beta],\[Gamma]]GA[\[Alpha]] . GA[\[Beta]] . GA[\[Gamma]]
Contract[%]


(* ::Text:: *)
(*This is because `Times` is commutative, so writing something like*)


GA[\[Delta]] GA[\[Beta]]GA[\[Alpha]]


(* ::Text:: *)
(*will give you completely wrong results. It is also a very common beginner's mistake!*)


(* ::Text:: *)
(*It might be surprising that FeynCalc does not seem to distinguish between upper and lower Lorentz indices. *)


(* ::Text:: *)
(*In fact, FeynCalc tacitly assumes that all your expressions with Lorentz indices are manifestly Lorentz covariant and respect Einstein's summation. In particular, this implies that*)


(* ::Text:: *)
(*In an equality, if a free Lorentz index appears upstairs on the right hand side, it must also appear upstairs*)
(*on the left hand side. Something like $p^{\mu} = c q_{\mu}$ would violate manifest Lorentz covariance. Hence,*)


FV[p,\[Mu]]==c FV[q,\[Mu]]


(* ::Text:: *)
(*could equally stand for $p^{\mu} = c q^{\mu}$ or $p_{\mu} = c q_{\mu}$. *)


(* ::Text:: *)
(*For the sake of definiteness, we impose that a free Lorentz should be always understood to be an upper index. This becomes important when dealing with nonrelativistic expressions involving Cartesian indices, where there's no manifest Lorentz covariance.*)


(* ::Text:: *)
(*Since FeynCalc assumes that the expressions you enter are mathematically sensible, it will not check your input or complain, even if the expression you provided is obviously incorrect*)


MT[\[Mu],\[Nu]]FV[p,\[Mu]]FV[q,\[Mu]]
Contract[%]


(* ::Text:: *)
(*When it comes to products of Levi-Civita tensors (`Eps`), `Contract` will by default apply the product formula with the determinant of metric tensors*)


LC[\[Mu],\[Nu]][p,q]LC[\[Rho],\[Sigma]][r,s]FV[x,\[Mu]]
Contract[%]


(* ::Text:: *)
(*This is, however, not always what we want and can be inhibited via the option `EpsContract`*)


LC[\[Mu],\[Nu]][p,q]LC[\[Rho],\[Sigma]][r,s]FV[x,\[Mu]]
Contract[%,EpsContract->False]

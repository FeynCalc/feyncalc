(* ::Package:: *)

 


(* ::Section:: *)
(*PaVeLimitTo4*)


(* ::Text:: *)
(*`PaVeLimitTo4[expr]`  simplifies products of Passarino-Veltman functions and $D$-dependent prefactors by evaluating the prefactors at $D=4$ and adding an extra term from the product of $(D-4)$ and the UV pole of the Passarino-Veltman function.*)


(* ::Text:: *)
(*This is possible because the UV poles of arbitrary Passarino-Veltman functions can be determined via `PaVeUVPart`. The result is valid up to 0th order in Epsilon, i.e. it is sufficient for 1-loop calculations.*)


(* ::Text:: *)
(*Warning! This simplification always ignores possible IR poles of Passarino-Veltman functions. Therefore, it can be used only if all IR poles are regulated without using dimensional regularization (e.g. by assigning gluons or photons a fake mass) or if it is known in advance that the given expression is free of IR singularities.*)


(* ::Text:: *)
(*The application of `PaVeLimitTo4` is equivalent to using the old `OneLoop` routine with the flags `$LimitTo4` and `$LimitTo4IRUnsafe` set to `True`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [$LimitTo4]($LimitTo4.md).*)


(* ::Subsection:: *)
(*Examples*)


ex=(D-2)/(D-3)A0[m^2]
PaVeLimitTo4[ex]


(* ::Text:: *)
(*Simplify the 1-loop amplitude for $H \to g g$*)


ex=(-(1/((-2+D) mH^2 mW sinW))2 I (-4+D) e gs^2 mt^2 \[Pi]^2 B0[mH^2,mt^2,mt^2] 
SD[Glu2,Glu3] (-2 SPD[k1,Polarization[k2,-I,Transversality->True]] 
SPD[k2,Polarization[k1,-I,Transversality->True]]+
mH^2 SPD[Polarization[k1,-I,Transversality->True],
Polarization[k2,-I,Transversality->True]])-1/((-2+
D) mH^2 mW sinW) I e gs^2 mt^2 (-2 mH^2+D mH^2-
8 mt^2) \[Pi]^2 C0[0,0,mH^2,mt^2,mt^2,mt^2] SD[Glu2,Glu3] (-2 SPD[k1,
Polarization[k2,-I,Transversality->True]] SPD[k2,Polarization[k1,
-I,Transversality->True]]+mH^2 SPD[Polarization[k1,-I,
Transversality->True],Polarization[k2,-I,Transversality->True]]))


PaVeLimitTo4[ex]

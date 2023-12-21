(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GammaEpsilon*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

GammaEpsilon::usage=
"GammaEpsilon[exp] gives a series expansion of Gamma[exp] in Epsilon up to
order 6 (where EulerGamma is neglected).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`GammaEpsilon`Private`"]

BLA := Unique[C];

GammaEpsilon[1 + a_. Epsilon] := GammaEpsilon[1 + a Epsilon] =
BLA Epsilon^6 +
1  + (a^2*Epsilon^2*Pi^2)/12 +
	(a^4*Epsilon^4*Pi^4)/160 - (a^3*Epsilon^3*Zeta[3])/3 +
	Epsilon^5*(-(a^5*Pi^2*Zeta[3])/36 - (a^5*Zeta[5])/5)
(*
					Normal[
						SeriesData[Epsilon, 0, {1, 0, (a^2*Pi^2)/12, -(a^3*Zeta[3])/3,
											(a^4*Pi^4)/160, -(a^5*Pi^2*Zeta[3])/36 -
											(a^5*Zeta[5])/5, BLA}, 0, 7, 1
											]
								];
*)

GammaEpsilon[exp_] :=
					If[!FreeQ[exp,Epsilon],
							Normal[Series[Gamma[exp], {Epsilon, 0, 6}]/.{
											EulerGamma :>0,
											PolyGamma[4, 1] -> -24*Zeta[5]}
										] + BLA Epsilon^6 (* + O[Epsilon]^7 *)
							,
							Gamma[exp]
						];

FCPrint[1,"GammaEpsilon.m loaded."];
End[]

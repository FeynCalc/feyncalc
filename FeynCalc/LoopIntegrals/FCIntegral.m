(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCIntegral*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 7 February '99 at 17:56 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

FCIntegral::usage =
"FCIntegral is the head of integrals in a setting of the option IntegralTable
of FeynAmpDenominatorSimplify. Currently implemented only for 2-loop
integrals.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCIntegral`Private`"]


FCIntegral[1,___] = 1;
FCIntegral[0,___] = 0;
FCIntegral[a_,q2_,q1_,p_] :=
	FCIntegral[a,q1,q2,p]/; !OrderedQ[{q2, q1}];

FCPrint[1,"FCIntegral.m loaded."];
End[]



(* :Title: Chisholm *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 15 October '97 at 14:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: applies the Chisholm identity *)

(* ------------------------------------------------------------------------ *)

Chisholm::usage =
"Chisholm[x] substitutes products of three Dirac matrices or \
slashes by the Chisholm identity.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Chisholm`Private`"]

Chisholm[x_] :=
	Contract[DiracSimplify[FCI[x] //. chish1 //. chish2]];

chish1 = (f_. DOT[a_DiracGamma, b_DiracGamma, c_DiracGamma,
		d_DiracGamma, e_DiracGamma, f_DiracGamma,
		g___]
	) :> Chisholm[Contract[DiracSimplify[
		DOT[Chisholm[f DOT[a,b,c]] ,Chisholm[d,e,f,g]]],Rename->False]];

chish2 = DOT[a___, DiracGamma[lv1_[pe1_]],DiracGamma[lv2_[pe2_]],
			DiracGamma[lv3_[pe3_]],b___
		] :>
			Block[ {index},
				index = Unique[$MU];
				Contract[DiracSimplify[
				(
				Pair[lv1[pe1], lv2[pe2]] DOT[a, DiracGamma[lv3[pe3]], b] -
				Pair[lv1[pe1], lv3[pe3]] DOT[a, DiracGamma[lv2[pe2]], b] +
				Pair[lv2[pe2], lv3[pe3]] DOT[a, DiracGamma[lv1[pe1]], b] +
				I Eps[ lv1[pe1],lv2[pe2],lv3[pe3],LorentzIndex[index] ]*
				DOT[a, DiracGamma[LorentzIndex[index]].
					DiracGamma[5], b]
						)
							], EpsContract->True,
				Rename->False]
			];
FCPrint[1,"Chisholm.m loaded"];
End[]

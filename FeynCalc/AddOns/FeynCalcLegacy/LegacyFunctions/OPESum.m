(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPESum*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 9 July '98 at 15:16 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the sum *)

(* ------------------------------------------------------------------------ *)

OPESum::usage =
"OPESum[exp, {i, 0, m}] denotes a symbolic sum. The syntax is the same as for
Sum.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`OPESum`Private`"]


(*change 03/98 *)
OPESum[a_,{j_,0,i_},{i_,0,em_}] :=
	OPESum[a, {i,0,em},{j,0,i}];

OPESum[0,_] :=
	0;


OPESum[__, {_,0,_Integer?Negative},___List] :=
	0;

OPESum[a_, {i_, j_, n_Integer/; n>=0}, any___] :=
	Sum[a,{i,j,n},any];

	OPESum /:
	MakeBoxes[OPESum[a_,b_], TraditionalForm] :=
			TBox[HoldForm[Sum[a,b]]];

	OPESum /:
	MakeBoxes[OPESum[a_,b_,c_], TraditionalForm] :=
			TBox[HoldForm[Sum["",b]], Sum["",c], a];

(*
************************************************

psitos[exp_] := exp /. {
		PolyGamma[0,i_] :> SumS[1,i-1] - EulerGamma ,
		PolyGamma[1,i_] :> -SumS[2,i-1] + Zeta2};

stopsi[exp_] := exp /. {
		SumS[1,i] :> PolyGamma[0,i+1] + EulerGamma ,
		PolyGamma[1,i_] :> -SumS[2,i-1] + Zeta2};


Unprotect[Sum];

Sum[SumS[1,m-i+1],{i,m-1}] :=
(*
Sum[SumS[1,i+1],{i,m-1}];
*)
m SumS[1,m]-m +SumS[1,m] -1;

Sum[SumS[1,m_ -i_+1]/i_, {i_,1,m_ -1}] :=
SumS[1,m]^2 - SumS[2,m] + 1/(m+1) (2 SumS[1,m-1]+1/m-1);

Sum[SumS[1,i_],{i,1,m_}] := (m+1) SumS[1,m]-m;

Sum[PolyGamma[0,i_],{i_,1,m_}] :=
(m (PolyGamma[m]-1)+1);

Sum[-PolyGamma[0,i_],{i_,1,m_}] :=
-(m (PolyGamma[m]-1)+1);

Sum[PolyGamma[0,i_]/(i_+1),{i_,1,m_}] :=
SumS[1,1,m+1]- SumS[2,m+1]+1/(m+1)-1-EulerGamma SumS[1,m+1]+
	EulerGamma;

Sum[(y_^i_*PolyGamma[1, i_])/i_, {i_, 1, Infinity}] :=
Log[1 - y]*PolyLog[2, 1 - y] - 2*PolyLog[3, 1 - y] + 2*Zeta[3];

Sum[(-1)^n PolyGamma[1, 2+n]/(1+n), {n, 0, Infinity}]
==
-Integrate[Log[t]*Log[1 + t]/(1 - t),{t, 0, 1}]
==
(Pi^2*Log[2])/4 - Zeta[3]

mysum[u_^nu_*PolyGamma[1, nu_], {nu_, 1, Infinity}]  :=
-Pi^2/6 + (Log[1 - u]*Log[u])/(1 - u) + PolyLog[2, 1 - u]/(1 - u) +
	PolyLog[2, u]

**************************************************

Sum[(-1)^n PolyGamma[0,2+n]^2/(1+n), {n,0,Infinity}]
==
-(EulerGamma*Pi^2)/6 + EulerGamma^2*Log[2] - (Pi^2*Log[2])/12 +
	EulerGamma*Log[2]^2 + Log[2]^3/3 + (3*Zeta[3])/4

*)

FCPrint[1,"OPESum.m loaded"];
End[]

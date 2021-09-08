(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Nielsen *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 23 August '97 at 13:45 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

Nielsen::usage=
"Nielsen[i, j, x] denotes Nielsen's polylogarithm.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Nielsen`Private`"]

Options[Nielsen] = {
	PolyLog -> False
};

Nielsen[3,1,y_, OptionsPattern[]] :=
	PolyLog[4, y] /; OptionValue[PolyLog];

Nielsen[1,2,0, OptionsPattern[]] =
	0;

Nielsen[1,2,-1, OptionsPattern[]] =
	Zeta[3]/8;

Nielsen[1,2,1/2, OptionsPattern[]] =
	Zeta[3]/8;

Nielsen[1,2,1, OptionsPattern[]] =
	Zeta[3];

Nielsen /:
	N[Nielsen[n_,p_,x_, OptionsPattern[]]] :=
		(-1)^(n+p-1)/(n-1)!/p! NIntegrate[Log[1-x t]^p Log[t]^(n-1)/t,{t,0,1}];

niel[n_,p_,x_] :=
	(-1)^(n+p-1)/(n-1)!/p! Integrate[Log[1-x t]^p Log[t]^(n-1)/t,{t,0,1}];

Nielsen[1,2,x_, OptionsPattern[]] :=
	Expand[
		((Log[1 - x]^2*Log[x])/2 + Log[1 - x]*PolyLog[2, 1 - x] - PolyLog[3, 1 - x] + Zeta[3]
		)/.	PolyLog[2,y_Symbol] :> Zeta2 - Log[1 - y] Log[y] - PolyLog[2, 1 - y]
	]/; OptionValue[PolyLog];

Nielsen[1,3,x_, OptionsPattern[]] :=
	Expand[
		(Pi^4/15 - Log[1 - x]^3*Log[x] -
		3*Log[1 - x]^2*PolyLog[2, 1 - x] +
		6*Log[1 - x]*PolyLog[3, 1 - x] - 6*PolyLog[4, 1 - x])/6
	]/; OptionValue[PolyLog];

Nielsen[2,2,s_, OptionsPattern[]] :=
	Expand[
		Pi^4/90 + (Zeta2*Log[1 - s]^2)/2 + Log[1 - s]^4/24 -
		(Log[1 - s]^3*Log[s])/6 - Log[1 - s]*PolyLog[3, s] - PolyLog[4, 1 - s] +
		PolyLog[4, s] + PolyLog[4, s/(-1 + s)] + Log[1 - s]*Zeta[3]
	]/; OptionValue[PolyLog];

(*
Nielsen[3,1,y] := -1/2 Integrate[Log[t]^2 Log[1-y t]/t, {t,0,1}]
*)

Nielsen /:
	MakeBoxes[Nielsen[i_,j_,exp_, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubscriptBox["S",TBox[i,j]],"(", TBox[exp],")"}];

FCPrint[1,"Nielsen.m loaded."];
End[]

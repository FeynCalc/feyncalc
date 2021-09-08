(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SumS *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 23 October '98 at 18:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

SumP::usage=
"SumP[k, m] is $2^{k-1}\\sum _{i=1}^{2m}\\left(1+(-1)^i\\right)/i^k$.";

SumS::usage=
"SumS[1, m] is the harmonic number $S_ 1(m) = \\sum _ {i=1}^m i^{-1}$.

SumS[1,1,m] is $\\sum_{i=1}^m S_ 1 (i)/i$.

SumS[k,l,m] is $\\sum _ {i=1}^m S_l (i)/i^k$.

SumS[r, n] represents Sum[Sign[r]^i/i^Abs[r], {i, 1, n}].

SumS[r,s, n] is Sum[Sign[r]^k/k^Abs[r] Sign[s]^j/j^Abs[s], {k, 1, n}, {j, 1,
k}] etc.";

SumT::usage=
"SumT[1, m] is the alternative harmonic number $\\sum _{i=1}^m
(-1){}^{\\wedge}i/i$ 

SumT[r, n] represents Sum[(-1)^i/i^r, {i,1,n}]

SumT[r,s, n] is Sum[1/k^r (-1)^j/j^s, {k, 1, n}, {j, 1, k}].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];

End[]

Begin["`Sum`Private`"];

Options[SumS] = {Reduce -> False};
Options[SumT] = {Reduce -> False};

SumP[k_Integer, m_ /; (Head[m] === Integer || Head[m] === Rational)] :=
	2^(k-1) Sum[(1+(-1)^j)/j^k, {j,1,2 m}];

SumP /: Explicit[SumP[k_Integer, m_]] :=
	1/2 (1+(-1)^(2 m)) SumS[k,m] + 1/2 (1-(-1)^(2 m)) SumS[k,Factor[m-1/2]];

SumP /:
	MakeBoxes[ SumP[i_Integer, m_], TraditionalForm] :=
		RowBox[{SubsuperscriptBox["S", i,"'"], "\[NoBreak]", "(", "\[NoBreak]",
		ToBoxes[m,TraditionalForm], "\[NoBreak]",")"}];

SumS[n__Integer, em_Symbol -1, Reduce -> True] :=
	SumS[n,em-1];

SumS[r_Integer, n_Integer, OptionsPattern[]] :=
	Sum[Sign[r]^i/i^Abs[r],{i,1,n}];

SumS[r_Integer, s__Integer, n_Integer, OptionsPattern[]] :=
	Sum[Sign[r]^k/k^Abs[r] SumS[s,k], {k, 1, n}];

SumS[r_Integer, n_Symbol + a_., OptionsPattern[]] :=
	(SumS[r, n+a-1] + Sign[r]^(n+a)/(n+a)^Abs[r]) /; a >= 0 && OptionValue[Reduce];

SumS[r_Integer, n_Symbol + a_Integer,  OptionsPattern[]] :=
	(SumS[r, n+a+1] - Sign[r]^(n+a+1)/(n+a+1)^Abs[r] ) /; a < -1 && OptionValue[Reduce];

SumS[1, 2, n_Symbol + a_., OptionsPattern[]] :=
	(1/(n+a)^3 + 1/(n+a) SumS[2,n+a-1] + SumS[1,2,n+a-1]) /; a>=0 && OptionValue[Reduce];

SumS[2, 1, n_Symbol + a_., OptionsPattern[]] :=
	(1/(n+a)^3 + 1/(n+a)^2  SumS[1,n+a-1] + SumS[2,1,n+a-1]) /; a>=0 && OptionValue[Reduce];

SumS[1,1, n_Symbol +a_., OptionsPattern[]] :=
	SumS[1,1, n+a-1] + 1/(n+a) SumS[1, n+a]  /; a>=0 && OptionValue[Reduce];

MakeBoxes[SumS[i_Integer, m_]^n_Integer, TraditionalForm] :=
	RowBox[{SubsuperscriptBox["S", i, n], "(", TBox[m] , ")"}];

SumS /:
	MakeBoxes[ SumS[i_Integer, m_], TraditionalForm] :=
		RowBox[{SubscriptBox["S", i], "(", TBox[m] , ")"}];

SumS /:
	MakeBoxes[ SumS[i_Integer,j__Integer, m_], TraditionalForm] :=
		RowBox[{SubscriptBox["S", StringJoin@@Map[ToString,
		{i,j}]],"(", TBox[m], ")"}];

SumT[n__Integer, em_Symbol -1, Reduce -> True] :=
	SumT[n, em-1];


SumT[n_Integer, OptionsPattern[]] :=
	Sum[(-1)^j/j^2 SumS[1, j],{j, 1, n}];

SumT[i_Integer, n_Symbol + a_., OptionsPattern[]] :=
	PowerSimplify[(SumT[i, n+a-1] + (-1)^(n+a)/(n+a)^i)] /; (a >= 0) &&	OptionValue[Reduce];

SumT[i_Integer, n_Symbol + a_Integer, OptionsPattern[]] :=
	PowerSimplify[(SumT[i, n+a+1] - (-1)^(n+a+1) /(n+a+1)^i) ] /;(a<-1) && OptionValue[Reduce];

SumT[r_Integer, n_Integer] :=
	Sum[(-1)^i/i^r, {i,1,n}];

SumT[r_Integer, s_Integer, n_Integer] :=
	Sum[1/k^r (-1)^j/j^s, {k, 1, n}, {j, 1, k}];

SumT[1,2,n_Symbol + a_., OptionsPattern[]] :=
	((-1)^(n+a)/(n+a)^3 + 1/(n+a) SumT[2,n+a-1] + SumT[1,2, n+a-1]) /;
	(a >= 0) && OptionValue[Reduce];

MakeBoxes[SumT[i_Integer, m_, OptionsPattern[]]^n_Integer, TraditionalForm] :=
	RowBox[{Subsuperscript[OverscriptBox["S","~"], TBox[i], n], "(", m , ")"}];

SumT /:
	MakeBoxes[SumT[m_, OptionsPattern[]], TraditionalForm] :=
		RowBox[{OverscriptBox["S","~"],"(", TBox[m] , ")"}];

SumT /:
	MakeBoxes[SumT[i_Integer, m_, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubscriptBox[OverscriptBox["S","~"], i], "(", TBox[m] , ")"}];

SumT /:
	MakeBoxes[ SumT[i_Integer,j__Integer, m_, OptionsPattern[]], TraditionalForm] :=
		RowBox@@{{SubscriptBox[OverscriptBox["S","~"], StringJoin@@Map[ToString,{i,j}]], "(",
		TBox[m] , ")"}};

FCPrint[1,"Sum loaded"];
End[]

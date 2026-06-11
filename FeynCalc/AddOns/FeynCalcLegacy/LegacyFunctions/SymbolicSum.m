(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SymbolicSum *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 March '98 at 23:32 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

SymbolicSum2::usage =
"SymbolicSum2 is similar to SymbolicSum (SymbolicSum was a Mathematica function
to do symbolic summation. It was obsolete from version 3 - all functionality
is now autoloaded by Sum), but extended to several double sums.";

SymbolicSum3::usage =
"SymbolicSum3 is similar to SymbolicSum (SymbolicSum was a Mathematica function
to do symbolic summation. It was obsolete from version 3 - all functionality
is now autoloaded by Sum), but extended to several double sums.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SymbolicSum`Private`"]

SymbolicSum2[a__] :=
	summ2[a] /. summ2->symbolics2/.
	symbolics2->Sum;


summ2[a_^OPEi b_^(OPEm-3-OPEi),{OPEi,0,OPEm-3}] :=
	PowerSimplify[Factor2[PowerSimplify[a^(OPEm-2)/(a-b)]]]+
	PowerSimplify[Factor2[PowerSimplify[b^(OPEm-2)/(b-a)]]];


(*
symbolics2[a_Times, {OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}] :=
Select[a, (FreeQ[#,OPEi] && FreeQ[#,OPEj])&] *
	Sum3[Select[a, ( (!FreeQ[#, OPEi]) || (!FreeQ[#, OPEj]) )&],
			{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}] /;
	MatchQ[Select[a, ( (!FreeQ[#, OPEi]) || (!FreeQ[#, OPEj]) )&],
				_^OPEj _^(OPEi - OPEj) _^(OPEm-4-OPEi)
				] ||
	MatchQ[Select[a, ( (!FreeQ[#, OPEi]) || (!FreeQ[#, OPEj]) )&],
				(-1)^OPEi _^OPEj _^(OPEi - OPEj) _^(OPEm-4-OPEi)
				];
*)

Sum3[a_^OPEj b_^(OPEi - OPEj) / (c_^OPEi),
		{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
	] :=
	c^(4-OPEm) a^(OPEm-2)/((a-b) (a-c)) +
		c^(4-OPEm) b^(OPEm-2)/((b-a) (b-c)) +
		c^2 / ((c-a) (c-b));

(* R.Hamberg, (3A.18) *)
Sum3[a_^OPEj b_^(OPEi - OPEj) c_^(OPEm-4-OPEi),
		{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
	] :=
	a^(OPEm-2)/((a-b) (a-c)) + b^(OPEm-2)/((b-a) (b-c)) +
		c^(OPEm-2)/((c-a) (c-b));

Sum3[a_^OPEj b_^(OPEi - OPEj) c_^(OPEm-3-OPEi),
		{OPEj, 0, OPEi}, {OPEi, 0, -3 + OPEm}
	] :=
	a^(OPEm-1)/((a-b) (a-c)) + b^(OPEm-1)/((b-a) (b-c)) +
		c^(OPEm-1)/((c-a) (c-b));

symbolics2[a_ /; ((NumericalFactor[a] =!= 1) &&
						IntegerQ[a]), b__] :=
	NumericalFactor[a] symbolics2[a/NumericalFactor[a], b];

symbolics2[a_ b_, {indd_,0,up_}] :=
	(a symbolics2[b,{indd,0,up}] ) /; FreeQ[a, indd];

symbolics2[Binomial[up_,indd_], {indd_,0,up_}] :=
	2^up;

symbolics2[Binomial[up_,indd_] *
						a_^(evtl_. + indd_) (b_^(any_ - indd_)),
						{indd_, 0, up_}
						] :=
	(Power[(a + b),up] b^(any - up) a^evtl);

symbolics2[Binomial[up_,indd_] * (-1)^indd_ *
						a_^(evtl_. + indd_) (b_^(any_ - indd_)),
						{indd_, 0, up_}
						] :=
	(Power[(-a + b),up] b^(any - up) a^evtl);

symbolics2[(-1)^(-OPEi) a_, b__] :=
	symbolics2[(-1)^OPEi a, b];
symbolics2[(-1)^(-OPEj) a_, b__] :=
	symbolics2[(-1)^OPEj a, b];

symbolics2[(-1)^(OPEi+OPEj) a_, b__] :=
	symbolics2[(-1)^(OPEi-OPEj) a, b];
symbolics2[(-1)^(OPEi-OPEj+OPEm) a_, b__] :=
	(-1)^OPEm symbolics2[(-1)^(OPEi-OPEj) a, b];

symbolics2[(-1)^OPEi a_^OPEj b_^(OPEi - OPEj) c_^xx_,
		{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
	] :=
	symbolics2[(-a)^OPEj (-b)^(OPEi-OPEj) c^xx,
				{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
			];

symbolics2[(-1)^OPEj a_^OPEj b_^(OPEi - OPEj) c_^xx_,
		{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
	] :=
	symbolics2[(-a)^OPEj b^(OPEi-OPEj) c^xx,
				{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
			];

symbolics2[(-1)^(OPEi-OPEj) a_^OPEj b_^(OPEi - OPEj) c_^xx_,
		{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
	] :=
	symbolics2[a^OPEj (-b)^(OPEi-OPEj) c^xx,
				{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
			];

symbolics2[(-1)^(OPEj-OPEi) a_^OPEj b_^(OPEi - OPEj) c_^xx_,
		{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
	] :=
	symbolics2[a^OPEj (-b)^(OPEi-OPEj) c^xx,
				{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
			];

SymbolicSum3[a_^OPEj b_^(OPEi - OPEj) / (c_^OPEi),
		{OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
	] :=
	c^(4-OPEm) a^(OPEm-2)/((a-b) (a-c)) +
		c^(4-OPEm) b^(OPEm-2)/((b-a) (b-c)) +
		c^2 / ((c-a) (c-b));

(* R.Hamberg, (3A.18) *)
SymbolicSum3[a_^OPEj b_^(any_. + OPEi - OPEj) c_^(opem4_ -OPEi),
		{OPEi, 0, opem4_}, {OPEj, 0, OPEi}
	] :=
	b^(any)*(
			(a^(opem4+2)/((a-b) (a-c)) + b^(opem4+2)/((b-a) (b-c)) +
			c^(opem4+2)/((c-a) (c-b)) )
						) /; FreeQ2[{any,opem4},{OPEi,OPEj}];

SymbolicSum3[a_^OPEj b_^(- OPEi + OPEj) c_^(opem4_ -OPEi),
		{OPEi, 0, opem4_}, {OPEj, 0, OPEi}
	] :=
	(
			(a^(opem4+2)/((a-(1/b)) (a-c)) + b^(-opem4-2)/(
			((1/b)-a) ((1/b)-c)) +
			c^(opem4+2)/((c-a) (c-(1/b))) )
						) /; FreeQ2[{(*any,*)opem4},{OPEi,OPEj}];

SymbolicSum3[a_^OPEj b_^(any_. + OPEi - OPEj) c_^(opem4_ -OPEi),
			{OPEi, 0, opem4_}, {OPEj, 0, OPEi}
	] :=
	b^(any)*(
			(a^(opem4+2)/((a-b) (a-c)) + b^(opem4+2)/((b-a) (b-c)) +
			c^(opem4+2)/((c-a) (c-b)) )
						) /; FreeQ2[{any,opem4},{OPEi,OPEj}];

SymbolicSum3[a_^OPEi b_^(any_. + OPEj - OPEi) c_^(opem4_ -OPEj),
		{OPEi, 0, OPEj}, {OPEj, 0, opem4_}
	] :=
	b^(any)*(
		(a^(opem4+2)/((a-b) (a-c)) + b^(opem4+2)/((b-a) (b-c)) +
		c^(opem4+2)/((c-a) (c-b)) )
						) /; FreeQ2[{opem4,any},{OPEi,OPEj}];

FCPrint[1,"SymbolicSum.m loaded"];
End[]

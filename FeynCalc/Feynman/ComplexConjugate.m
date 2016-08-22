(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ComplexConjugate *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 February '99 at 2:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: construct the complex conjugate amplitude,
			introducing complex conjugated indices automatically
*)

(* :Comments: ComplexConjugate does NOT work if complex
				quantities are in denominators!!!!!!!!!!!!!!!
*)

(* ------------------------------------------------------------------------ *)

ComplexConjugate::usage =
"ComplexConjugate[expr] complex conjugates expr. It operates on \
Fermion-lines, i.e., products of Spinor[..] .DiracMatrix[..] . Spinor[..]. \
For taking the spin sum (i.e. constructing the traces) use FermionSpinSum. \n \n
WARNING: In expr should be NO explicit I in denominators!";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ComplexConjugate`Private`"]

dotlin[x_] :=
	DotSimplify[x, Expanding -> False];
(*
HoldPattern[ rev[yz__ /; FreeQ2[{yz}, {SUNT}] ] ]:=

*)
(*
HoldPattern[ rev[yz__] ]:=
*)
rev[yz__] :=
(*
Isolate[
*)
	(DOT @@ (Reverse[FRH[{ yz }]]/.
	(*Changed 28/2-2001 by F.Orellana, because of bug report by T.Rashba*)(*
						DiracGamma[5]->(-DiracGamma[5])/.
						{DiracGamma[6] :> DiracGamma[7],
						DiracGamma[7]:>DiracGamma[6]}/.*)
						{ChargeConjugationMatrix :> (-ChargeConjugationMatrix),  ChargeConjugationMatrixInv -> (-ChargeConjugationMatrixInv)}
			)(*, IsolateNames->c$CC, IsolateSplit->Infinity
			]*)
	) /; Length[Position[{yz}, Spinor]] < 3;

c$CCfrh /: HoldForm[c$CCfrh[ii_]] := c$CC[ii];

(*cLIndex[x_, dime___] := LorentzIndex[ComplexIndex[x], dime];
cSIndex[x_]          := SUNIndex[ComplexIndex[x]];*)

conpa[x__] :=
	conpa[x] = Pair[x]

(* ComplexConjugatedef *)
(*sunfcomp[a___] := SUNF @@ ({a}/.ComplexIndex -> Identity);
sundeltacomp[a___] := SUNDelta @@ ({a}/.ComplexIndex -> Identity);*)
(*Added SumOver stuff. F.Orellana. 20/8-2002*)
(*Dropped again 19/1-2003. Let's try and keep some hierarchy:
	FeynCalc -> PHI -> FeynArts. Dropped this whole business and ComplexIndex also;
	it is not maintainable (say you want another real function - you don't want to
	have to to type it in here in this file...)*)
(*sumovercomp[a___] := FeynArts`SumOver @@ ({a}/.ComplexIndex -> Identity);
ugencomp[a__] := Phi`Objects`UGenerator @@ ({a}/.ComplexIndex -> Identity);*)

(*
nenenen
sundcomp[a___] := SUND @@ ({a}/.ComplexIndex -> Identity);
*)

(* for large expressions it is better to not use DotSimplify *)
Options[ComplexConjugate] = {DotSimplify -> True};

ComplexConjugate[b_HoldForm,___?OptionQ] :=
	b /; FreeQ2[FeynCalcInternal[FRH[b]],
	{DOT,LorentzIndex,SUNIndex,SUNTF,Complex}];

ComplexConjugate[x_ /; (Head[x] =!= HoldForm), opts___?OptionQ] :=
	compcon[FeynCalcInternal[x]/.SUNTrace->suntrac, opts
			] /. (*SUNF -> sunfcomp /.*)
		(*FeynArts`SumOver -> sumovercomp /.
		Phi`Objects`UGenerator -> ugencomp /.*)
		SUNDelta -> SUNDeltaContract /.
			(*SUNDeltaContract -> sundeltacomp/.*)
				compcon -> compcon2 /. compcon2 ->
				ComplexConjugate /. suntrac->
					SUNTrace;

compcon2[x_/;!FreeQ[x, HoldForm], opts___?OptionQ] :=
	compcon[FRH[x], opts];
compcon[x_^n_?(Element[#,Reals]===True)&, opts___?OptionQ] :=
	compcon[x,opts]^n;
compcon[x_Plus, opts___?OptionQ] :=
	compcon[#,opts]& /@ x;
compcon[x_Times, opts___?OptionQ] :=
	compcon[#,opts]& /@ x;


compcon[b_HoldForm, ___?OptionQ] :=
	b /;
	FreeQ2[FRH[b], {DOT,LorentzIndex,SUNIndex,SUNTF,Complex}];
compcon[x_ /; (Head[x] =!= Plus) && (Head[x] =!= Times), opts___?OptionQ] :=
	If[ FreeQ[x, DOT | Complex | DiracGamma | SUNTF],
		x,
		Block[ {nx = x,oone, suntrac, dotsim},
			dotsim = DotSimplify /. {opts} /. Options[ComplexConjugate];
			If[ !FreeQ[nx, SUNF],
				nx = Expand[nx, SUNF]
			];
			If[ !FreeQ[nx,SUNT],
				If[ dotsim,
					nx = dotlin[nx]
				]
			];
(* this is wrong if nx had Head List ... (change 02/99)
					nx = (DOT[oone, nx] /. DOT -> rev /. rev -> DOT);


*)
			If[ !FreeQ[nx, SUNTF],
				nx = nx /.{SUNTF[{a__},b_,c_]:> SUNTF[Reverse[{a}], c, b] }
			];

			If[ !FreeQ[nx, Eps],
				nx = nx /. Eps[a__] :> Conjugate[$LeviCivitaSign]/$LeviCivitaSign Eps[a]
			];

			nx = nx /. DiracGamma[a__]:>
			DOT[oone, DiracGamma[a]](*Added 28/2-2001 by F.Orellana - see above*)/;
			FreeQ[{5,6,7},Evaluate[{a}[[1]]]]/.
			DiracGamma[5]->(-DiracGamma[5])/.
			{DiracGamma[6] :> DiracGamma[7],
			DiracGamma[7]:>DiracGamma[6]}(**)/.
				DOT -> rev /. rev -> DOT /. oone -> 1;
(*
I think this Isolate-optimization is outdated and causes too much overhead, RM, 14.10.2003

					nx = nx //. c$CC -> c$CCfrh /. oone -> 1;
*)
			nx = nx /.
					(*LorentzIndex -> cLIndex /.
					SUNIndex  -> cSIndex /.*)
					Complex[a_, b_] -> Complex[a, -b];
			If[ dotsim,
				nx = dotlin[nx]
			];
			nx
		]
	] /; FreeQ[x, HoldForm];

(* CAREFUL: Complex[a_, b_] -> Complex[a, -b] is only true if no complex
	variables are in denominators!!!!, (which is the case in HEP, unless you
	have width in the propagators ...)
*)
FCPrint[1,"ComplexConjugate.m loaded."];
End[]

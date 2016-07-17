(* ::Package:: *)



(* :Title: Collect															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Expansion of the Mathematica Collect						*)

(* ------------------------------------------------------------------------ *)

Collect2::usage=
"Collect2[expr, x] collects together terms which are not free of any  \
occurrence of x. \
Collect2[expr, {x1, x2, ...}]  (or also Collect2[expr, x1, x2,  ...]) \
collects together terms which are not free of any occurrence of \
x1, x2, ... . \
The coefficients are put over a common denominator. \
If expr is expanded before collecting depends on the option  Factoring, \
which may be set to Factor, Factor2, or any other function, \
which is applied to the coefficients. \
If expr is already expanded with respect to x, the \
option Expanding can be set to False.";

Collect3::usage=
"Collect3[expr, head] collects all monomials of the form head[..]*head[..]*.. \
in expr. \n
Collect3[expr, {x, y, ...}] collects terms involving the same powers \
of monomials x[...]^n1*y[...]^n2 ... The option Factoring can be set to False, True or Factor2; \
the latter two of these cause the coefficients to be factored. The option Head (default Plus) \
specified the function applied to the list of monomials multiplied by their coefficients.";

Collect2::failmsg =
"Error! Collect2 has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Collect`Private`"]

cl2Verbose::usage="";


Options[Collect2] = {
	Denominator -> False,
	Dot -> False,
	FCVerbose -> False,
	Expanding -> True,
	Factoring -> Factor,
	IsolateFast -> False,
	IsolateNames -> False,
	Head->Identity
};

Options[Collect3] = {
	Factoring -> False,
	Head -> Plus
};

SetAttributes[holdForm,HoldAll];

Collect2[a_ == b_, y__] :=
	Collect2[a,y] == Collect2[b,y];

Collect2[x_List, y__] :=
	Collect2[#, y]& /@ x;

Collect2[x_, y_, opts:OptionsPattern[]] :=
	Collect2[x, {y}, opts] /; (Head[y]=!=List && !OptionQ[y]);

Collect2[x_, z__, y_, opts:OptionsPattern[]] :=
	Collect2[x, {z,y}, opts] /; (Head[y]=!=List && !OptionQ[y]);

Collect2[ expr_, vv_List/; (!OptionQ[vv] || vv==={}), opts:OptionsPattern[]] :=
	Block[{monomList,ru,nx,lk,factoring,optIsolateNames,tog,fr0,frx,lin,tv={},mp,mp2,cd,co,dde,
		new = 0, unity,re,compCON,ccflag = False, factor,expanding, times,time,
		null1,null2,coeffArray,tvm,coeffHead,optIsolateFast,tempIso},

		If [OptionValue[FCVerbose]===False,
			cl2Verbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				cl2Verbose=OptionValue[FCVerbose]
			];
		];

		{factoring, optIsolateNames, expanding, dde, optIsolateFast} =
			{OptionValue[Factoring], OptionValue[IsolateNames],
			OptionValue[Expanding], OptionValue[Denominator],
			OptionValue[IsolateFast]  };


		Which[
			OptionValue[Dot] === True,
			times = Dot,
			OptionValue[Dot] === False,
			times = Times,
			True,
			times = OptionValue[Dot]
		];

		If[factoring === True || factoring === Factor2,
			factor = Factor2,
			If[factoring =!= False,
				factor = factoring; factoring = True,
				factor = Identity
			];
		];

		FCPrint[1,"Collect2: Entering Collect2.", FCDoControl->cl2Verbose];
		FCPrint[2,"Collect2: Entering with: ", expr, FCDoControl->cl2Verbose];

		monomList = Union[Select[ vv, ((Head[#] =!= Plus) && (Head[#] =!= Times) && (!NumberQ[#]))& ]];
		monomList = Select[ monomList, !FreeQ[expr, #]&];

		FCPrint[1,"Collect2: Monomials w.r.t which we will collect: ", monomList, FCDoControl->cl2Verbose];

		If[Length[monomList] === 0,
			(* The expression is free of terms in momomList*)
			unity = 1;
			Return[expr]
		];

		nx = expr;
		(* Hm, that's a problem, maybe *)
		If[!FreeQ[nx, ComplexConjugate],
			ccflag = True;
			nx = nx /. ComplexConjugate -> compCON;
			monomList = monomList /. ComplexConjugate -> compCON;
		];

		nx = nx/. holdForm[k_[ii_]] -> lk[k][ii];

		If[ factoring === False,
			FCPrint[1,"Collect2: No factoring.", FCDoControl->cl2Verbose];
			(* 	This can speed things up, if the expression contains very large sums free of
				monomials *)
			nx = nx /. Plus -> holdPlus /. holdPlus[x__] /; FreeQ2[{x}, monomList] :>
				Isolate[(Plus[x]/.holdPlus -> Plus), IsolateFast -> True, IsolateNames -> tempIso] /. holdPlus -> Plus;

			tog[x_] := FRH[x/.holdForm->Identity, IsolateNames->optIsolateNames],
			FCPrint[1,"Collect2: Factoring with", factor, FCDoControl->cl2Verbose];
			fr0[x__] :=
				Plus[x] /; !FreeQ2[{x}, monomList];
			tog[x_]  :=
				factor[FRH[x/.holdForm->Identity, IsolateNames->optIsolateNames]];
			frx[x__] :=
				holdForm[Plus[x]];
			nx = nx /. Plus -> fr0 /. fr0 -> frx
		];

		If[ expanding =!= False,
			time=AbsoluteTime[];
			FCPrint[1,"Collect2: Expanding", FCDoControl->cl2Verbose];
			nx  = Expand2[nx,monomList];
			FCPrint[1,"Collect2: Expanding done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cl2Verbose]
		];

		time=AbsoluteTime[];
		FCPrint[1,"Collect2: Separating the part free of the monomials (linear part)", FCDoControl->cl2Verbose];
		(* lin denotes the part free of monomList *)
		{lin,nx} = FCSplit[nx,monomList,Expanding->False];
		FCPrint[1,"Collect2: Separation done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cl2Verbose];
		FCPrint[2,"Collect2: Part that contains the monomials: ", nx, FCDoControl->cl2Verbose];

		If[factoring =!= False && lin=!=0,
			time=AbsoluteTime[];
			FCPrint[1,"Collect2: Factoring the linear part", FCDoControl->cl2Verbose];
			lin = tog[lin];
			FCPrint[1,"Collect2: Factoring done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cl2Verbose];

		];

		time=AbsoluteTime[];
		FCPrint[1,"Collect2: Wrapping the momomials with special heads.", FCDoControl->cl2Verbose];

		nx = (Map[(SelectFree[#, monomList] mp2[SelectNotFree[#, monomList]]) &,
				nx + null1 + null2] /. {null1 | null2 -> 0}) ;
		tv = Cases2[nx,mp2];
		FCPrint[1,"Collect2: Wrapping done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cl2Verbose];

		FCPrint[3,"Collect2: nx: ", nx , FCDoControl->cl2Verbose];
		FCPrint[3,"Collect2: tv: ", tv , FCDoControl->cl2Verbose];

		If[dde === True,
			FCPrint[1,"Collect2: Also denominators containing variables will be collected", FCDoControl->cl2Verbose];
			cd[x_] := ((Numerator[#]/(factor[Denominator[#]] /.
			Plus-> (Collect2[Plus[##], monomList, opts]&)))& @ x ) /;
			(!FreeQ[Denominator[x], Plus]) && (!FreeQ2[Denominator[x], monomList])
		];

		FCPrint[1,"Collect2: There are ", Length[tv] , " momomials to collect", FCDoControl->cl2Verbose];

		time=AbsoluteTime[];
		FCPrint[1,"Collect2: Computing CoefficientArrays.", FCDoControl->cl2Verbose];
		coeffArray = CoefficientArrays[nx,tv];
		FCPrint[1,"Collect2: CoefficientArrays ready, timing:", N[AbsoluteTime[] - time, 4], FCDoControl->cl2Verbose];

		time=AbsoluteTime[];
		FCPrint[1,"Collect2: Collecting the monomials.", FCDoControl->cl2Verbose];

		tvm = (FRH[# /.holdForm->Identity, IsolateNames->optIsolateNames] /. mp2 -> cd /. cd -> OptionValue[Head])&/@tv;

		FCPrint[3,"Collect2: tvm: ", tvm, FCDoControl->cl2Verbose];

		If[	coeffArray[[1]]=!=0,
			Message[Collect2::failmsg,"There is another linear part!"];
			Abort[]
		];

		new =  Sum[Dot[coeffHead[coeffArray[[i]]] , Sequence @@ Table[tvm, {i - 1}]], {i, 2, Length[coeffArray]}];

		coeffHead[li_SparseArray]:=
			(tog[unity*#])&/@li /; optIsolateNames===False;

		coeffHead[li_SparseArray]:=
			Isolate[tog[unity*#]/. {unity:>1, lk[ka_][j_] :> holdForm[ka[j]]},
				IsolateNames -> optIsolateNames, IsolateFast-> optIsolateFast]&/@li /;optIsolateNames=!=False;

		FCPrint[1,"Collect2: Done collecting the monomials, timing:", N[AbsoluteTime[] - time, 4], FCDoControl->cl2Verbose];

		If[	!FreeQ2[lin,monomList],
			Message[Collect2::failmsg,"Linear part contains monomials!"];
			Abort[]
		];

		If[ optIsolateNames =!= False,
			lin = Isolate[ FRH[lin/.holdForm->Identity, IsolateNames->optIsolateNames], IsolateNames->optIsolateNames, IsolateFast->optIsolateFast],
			lin = FRH[lin/.holdForm->Identity, IsolateNames->optIsolateNames]
		];

		re = ((new + lin) /. lk[ka_][j_] -> holdForm[ka[j]] /.	frx->Plus);

		time=AbsoluteTime[];

		If[ factoring === False,
			re = FRH[re,IsolateNames->tempIso]
		];
		FCPrint[1,"Collect2: Done releasing tempIso, timing:", N[AbsoluteTime[] - time, 4], FCDoControl->cl2Verbose];

		If[ccflag,
			re = re /. compCON -> ComplexConjugate
		];

		unity=1;

		FCPrint[1,"Collect2: Leaving.", FCDoControl->cl2Verbose];
		FCPrint[3,"Collect2: Leaving with", re, FCDoControl->cl2Verbose];

		re
	];

Collect3[a_,b_,c_Symbol, opts___?OptionQ]:=
	Collect3[a, b, Factoring -> c, opts];

Collect3[expr_, vars_/;Head[vars]=!=List, opts___Rule] :=
	Collect3[expr, {vars}, opts];

Collect3[expr_, vars_List, opts___?OptionQ] :=
	Block[{fac, hva, mli},
		fac = Factoring/.{opts}/.Options[Collect3];
		If[	fac === True,
			fac = Factor
		];
		hva = (Hold[HoldPattern][#[___]]& /@ ( Hold/@vars ) ) /. Hold[a_] :> a;
		hva = Alternatives @@ hva;
		mli = MonomialList[expr, Union@Cases[expr,hva,-1]];
		If[fac =!= False,
			mli = Map[fac, mli]
		];
		Apply[Head/.{opts}/.Options[Collect3], mli]
	];

FCPrint[1,"Collect loaded"];
End[]

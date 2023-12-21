(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: IFPDOff *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: IFPDOff *)

(* ------------------------------------------------------------------------ *)

IFPDOff::usage =
"IFPDOff[exp_, q1, q2, ...] changes from IFPD representation to
FeynAmpDenominator[...]. The q1, q2, ... are the integration momenta.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`IFPDOff`Private`"];

SetAttributes[IFPDOff, HoldAll];
SetAttributes[unset, HoldAll];

(* this is a side effect on Pair ... *)
(* it is undone by IFPDOff *)

unasign[quu__][pp_Plus, m_] :=
	If[ Head[SelectNotFree[pp,quu]] === Plus,
		unasign[quu][First[SelectNotFree[pp,quu]], Rest[SelectNotFree[pp,quu]] +
								SelectFree[pp,quu], m],
		unasign[quu][SelectNotFree[pp,quu], SelectFree[pp,quu], m]
	];

unassign0[quu__][Momentum[q_, di___],_] :=
	If[ !FreeQ[{quu},q],
		If[ Head[Pair[Momentum[q,di],Momentum[q,di]]]=!=Pair,
			uns[pair[Momentum[q,di], Momentum[q,di]]]/.
			uns->unset/.pair->Pair/.unset->Unset;
		]
	];

pair[-Momentum[a__], Momentum[b__]] :=
	pair[Momentum[a],Momentum[b]];
pair[-Momentum[a__],-Momentum[b__]] :=
	pair[Momentum[a],Momentum[b]];
pair[ Momentum[a__],-Momentum[b__]] :=
	pair[Momentum[a],Momentum[b]];

unassign1[quu__][Momentum[q_, di___], pes_, _] :=
	If[ !FreeQ[{quu},q],
		If[ (Head[Pair[Momentum[q,di],pes]]=!=Pair) &&
				(Head[-Pair[Momentum[q,di],pes]]=!=Pair),
			uns[pair@@Sort[{Momentum[q,di], pes}]]/.
			uns->unset/.pair->Pair/.unset->Unset;
		];
	];

unassign2[quu__][-Momentum[q_, di___], pes_, _] :=
	If[ !FreeQ[{quu},q],
		If[ Head[Pair[-Momentum[q,di],pes]]=!=Pair,
			uns[pair@@Sort[{-Momentum[q,di], pes}]]/.
			uns->unset/.pair->Pair/.unset->Unset;
		]
	];

ifex[a_,b_] :=
	ifex[a,b] = Pair[a,a] - b^2;

FP[y__] :=
	FeynAmpDenominator[PropagatorDenominator[y]];

IFPDOff[exp_,qu__] :=
	If[ FreeQ[exp, IFPD],
		exp,
		Block[ {int,qq,sub,t1,t2,t3,t4},

			If[	!FreeQ2[{exp}, FeynCalc`Package`NRStuff],
				Message[FeynCalc::nrfail];
				Abort[]
			];


			int = Apply[Hold, {exp}];
			qq = {qu} /. Momentum[a_,___] :> a;
			t2 = Cases2[int, IFPD];
			t3 = Map[(#  -> (1/#/.IFPD->FEP))&,t2];
			(* unassign *)
			Cases2[DownValues@@{Pair},IFPD]/.
			IFPD -> unasign[qq] /. unasign -> unassign0 /.
							unassign0 -> unassign1 /. unassign1 -> unassign2;
			sub = Dispatch[t3];
			int = int /. sub;
			int = Operate[# /. Hold -> Identity&, int];
			int = int /. FEP[a_, b_]^n_Integer?Negative :>
								(ExpandScalarProduct[Pair[a, a]] - b^2)^(-n);
			int = int /. {FEP :> FP, IFPD :> ifex};
			int
		]
	];

Off[Unset::norep];

FCPrint[1,"IFPDOff.m loaded."];
End[]

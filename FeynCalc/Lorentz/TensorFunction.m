(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TensorFunction *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)


TensorFunction::usage =
"TensorFunction[t, mu, nu, ...] transform into
t[LorentzIndex[mu], LorentzIndex[nu], ...], i.e., it can be used
as an unspecified tensorial function t.
A symmetric tensor can be obtained by
TensorFunction[{t, \"S\"}, mu, nu, ...],
and an antisymmteric one by
TensorFunction[{t, \"A\"}, mu, nu, ...].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`TensorFunction`Private`"]

Options[TensorFunction] = {Dimension -> 4};

TensorFunction[ef_, munu___,last_/;Head[last]=!=Rule, OptionsPattern[]] :=
	Block[ {f, dim, at},
		dim = OptionValue[Dimension];
		If[ Head[ef] === List && Length[ef] === 2,
			f  = First[ef];
			at = Last[ef],
			f = ef
		];
		With[ {tf = f},
			Which[
				at === "S",
				SetAttributes@@{tf, Orderless},
				at === "A",
				Evaluate[tf[a__LorentzIndex]] :=
					Condition[Signature[{a}] Apply[tf, Sort[{a}]],{a} =!= Sort[{a}]]
			];
			tf@@(Map[LorentzIndex[#, dim]&, {munu,last}])
		]
	];
FCPrint[1,"TensorFunction.m loaded."];
End[]

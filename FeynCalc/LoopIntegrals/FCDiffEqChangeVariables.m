(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCDiffEqChangeVariables											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Variable transformations for differential equations			*)

(* ------------------------------------------------------------------------ *)

FCDiffEqChangeVariables::usage =
"FCDiffEqChangeVariables[mat, x, y, rule, yOfX] applies a variable
transformation from x to ydescribed by rule, where yOfX denotes $y(x)$. Here
mat is a matrix in the context of differential equations, i.e. it can be
either the matrix $\\mathcal{A}$ or $\\mathcal{B}$ from the pre-canonical $F' =
\\mathcal{A} F$ or canonical $G' = \\varepsilon \\mathcal{B} G$  form, or the
transformation matrix $\\mathcal{T}$ with $F = \\mathcal{T} G$ .

By default, the transformation also includes the prefactor $1/f'(y)$. This is
correct for $\\mathcal{A}$ or $\\mathcal{B}$ but not for $\\mathcal{T}$ matrices.
The inclusion of the prefactor can be disabled by setting the option Prefactor
to False.";

FCDiffEqChangeVariables::failmsg =
"Error! FCDiffEqChangeVariables has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCDiffEqChangeVariables`Private`"]

fcdecv::usage="";

Options[FCDiffEqChangeVariables] = {
	FCVerbose	->	False,
	PowerExpand	->	False,
	Prefactor	->	True,
	Reverse		->	False,
	Simplify	->	True,
	Assumptions -> {}
};

FCDiffEqChangeVariables[mat_, origVar_, newVar_, repRule_Rule, yOfX_, opts:OptionsPattern[]] :=
	FCDiffEqChangeVariables[mat, newVar, origVar, newVar->yOfX, repRule[[2]], FilterRules[{opts}, Except[Reverse]]]/;
	OptionValue[FCDiffEqChangeVariables,{opts},Reverse];

FCDiffEqChangeVariables[mat_, origVar_, newVar_, repRule_Rule, yOfX_, opts:OptionsPattern[]] :=
	Block[{res},

		If [OptionValue[FCVerbose]===False,
			fcdecv =$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer | 0],
				fcdecv=OptionValue[FCVerbose]
			];
		];

		If[	FreeQ[mat,origVar],
			Message[FCDiffEqChangeVariables::failmsg,"The matrix does not depend on the variable "<>ToString[origVar,InputForm]];
			Abort[]
		];

		If[	!FreeQ[mat,newVar],
			Message[FCDiffEqChangeVariables::failmsg,"The matrix already contains the variable "<>ToString[newVar,InputForm]];
			Abort[]
		];

		If[	repRule[[1]]=!=origVar,
			Message[FCDiffEqChangeVariables::failmsg,"The transformation rules does not contain the original variable"];
			Abort[]
		];

		If[	FreeQ[repRule[[2]],newVar],
			Message[FCDiffEqChangeVariables::failmsg,"The transformation rules does not contain the new variable"];
			Abort[]
		];

		If[	OptionValue[Prefactor],
			res = (1/D[yOfX,origVar] mat) /. repRule,
			res = mat /. repRule
		];

		If[	OptionValue[Simplify],
			res = Simplify[res, Assumptions->OptionValue[Assumptions]]
		];

		If[	OptionValue[PowerExpand],
			res = PowerExpand[res];

			If[	OptionValue[Simplify],
				res = Simplify[res, Assumptions->OptionValue[Assumptions]]
			];
		];

		res

	]/; !OptionValue[FCDiffEqChangeVariables,{opts},Reverse];



FCPrint[1,"FCDiffEqChangeVariables.m loaded."];
End[]

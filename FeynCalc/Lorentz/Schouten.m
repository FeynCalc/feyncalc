(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Schouten															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: Applies Schouten identity 										*)

(* ------------------------------------------------------------------------ *)

Schouten::usage =
"Schouten[exp] attempts to automatically remove spurious terms in exp by
applying the Schouten's identity.

Schouten applies the identity for $4$-vectors on at most $42$ terms in a sum.
If it should operate on a larger expression you can give a second argument,
e.g. Schouten[expr, 4711] which will work on sums with less than $4711$ terms.

Schouten is also an option of Contract and DiracTrace. It may be set to an
integer indicating the maximum number of terms onto which the function
Schouten will be applied.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Schouten`Private`"]

Options[Schouten] = {
	FCI -> False,
	FCE -> False,
	MaxIterations -> 10
};

Schouten[y_, 0, OptionsPattern[]] :=
	y;

Schouten[expr_, oparg_:42, OptionsPattern[]] :=
	Block[{res,ex},

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		res = FixedPoint[schouten[#, oparg]&, ex, OptionValue[MaxIterations]];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

liget[_. Eps[x1_[y1_], x2_[y2_], x3_[y3_], x4_[y4_]] Pair[x5_[y5_], x6_[y6_]]] :=
	{x1[y1],x2[y2],x3[y3],x4[y4],x5[y5],x6[y6]};

lisch[{a1_,a2_,a3_,a4_,a5_,a6_}] :=
	Pair[a5,a6] Eps[a1,a2,a3,a4];


epc[a_, b_] :=
	If[ Length[Position[FCSplit[a, {Eps}][[2]], LorentzIndex]] <Length[Position[FCSplit[b, {Eps}][[2]], LorentzIndex]],
		True,
		False,
		False
	];

epsnterms[0] =
	0;

epsnterms[a_] :=
	Block[ {tem},
		tem = Collect2[a, Eps, Factoring ->False];
		If[ Head[tem]===Plus,
			Length[tem],
			1
		]
	];

schouten[x_, optarg_] :=
	Block[{	i=0, nx, temp0, temp, lind, ltemp, ntemp,
			schou, sor, all, result, numberofli, temp1, nxf = 1},

		nx = EpsEvaluate[ExpandScalarProduct[x,FCI->True],FCI->True];
		(* Split the sum into two parts *)
		result = nx;

		(* eps x1 + eps x2 + ... + rest, then split the eps part from the rest! *)
		all = FCSplit[nx,{Eps}];

		If[ !(Head[all[[2]]]===Plus),
			Return[result nxf]
		];

		temp0 = FCSplit[all[[2]], {Pair}];
		temp = temp0[[2]];

		If[ !((Head[temp]===Plus) && Length[temp] > 1 && (Length[temp] < optarg)),
			Return[result nxf]
		];

		ltemp = Length[temp];

		(*pick up the first term of temp and count the number of Lorentz indices in that term*)
		temp1 = temp[[1]];
		numberofli = Length[Position[temp1, LorentzIndex]];
		(* it is interesting that the number of indices in the very first term influences the whole algorithm!*)

		temp =
			Catch[
					(*so we look at each term in the expression temp *)
					While[i < Length[temp], i++;

						(*if the term contains no Eps or Pair, do nothing. *)
						If[ !FreeQ2[temp[[i]],{Eps,Pair}],

							(*
								extracts indices or momenta of the current term eps[i1,i2,i3,i4] pair[i5,i6]
								Notice that if there are several eps' and pairs', the choice becomes sort of random

							*)
							lind = liget[temp[[i]]];

							(* If lind returns something different than 6, then the structure Eps Pair is not there.*)
							If[ Length[lind]===6,
								(* create a list of 5 possible arrangements of terms of Schouten ident. *)

								schou = lisch /@ Map[Append[#, Last[lind]]&,NestList[RotateLeft,Take[lind,5],4]];

								(*special sorting, w.r.t the position of the Lorentz indices in eps *)
								sor = Sort[ schou, epc ]//Reverse;
								(*rotate one position to the left if temp does not contain the first element of sor *)
								sor = Nest[If[ FreeQ[temp, #[[1]]],
										RotateLeft[#], #]&, sor, 6];

								If[ !FreeQ[temp, sor[[1]]],
									(*for the case that temp does contain an element of sort, apply the Schouten identity*)
									ntemp = Expand[EpsEvaluate[temp/.sor[[1]]-> (-Plus@@Rest[sor]),FCI->True]];

									(* if the number of terms containing eps is smaller than the original length, or the number
									of the Lorentz indices remains unchanged, take this identity(?).*)
									If[ (epsnterms[ntemp] < ltemp) ||
										(Union[Length[Position[#, LorentzIndex]]& /@ Select[Variables[ntemp], Head[#]===Eps&]]  === {numberofli}),
										Throw[temp = ntemp]
									]
								]
							]
						]
					];
					temp
			];

		result = all[[1]]  + temp0[[1]] + temp;
		result nxf
	];

FCPrint[1,"Schouten.m loaded."];
End[]

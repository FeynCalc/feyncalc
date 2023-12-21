

(* :Title: Schouten *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option and Function *)

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

Schouten[y_, 0] :=
	y;
Schouten[y_, oparg_:42] :=
	(
	If[	!FreeQ2[{y}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
	];

	FixedPoint[schouten[#, oparg]&, FeynCalcInternal[y], 14]
	);

liget[_. Eps[x1_[y1_], x2_[y2_], x3_[y3_], x4_[y4_]] Pair[x5_[y5_], x6_[y6_]]] :=
	{x1[y1],x2[y2],x3[y3],x4[y4],x5[y5],x6[y6]};

lisch[{a1_,a2_,a3_,a4_,a5_,a6_}] :=
	Pair[a5,a6] Eps[a1,a2,a3,a4];


epc[a_, b_] :=
	If[ Length[Position[PartitHead[a, Eps][[2]], LorentzIndex]] <Length[Position[PartitHead[b, Eps][[2]], LorentzIndex]],
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

schouten[x_,opar_:42] :=
	Block[ {i=0,nx,temp0,temp,lind,ltemp,ntemp,
			schou,sor, all,result,numberofli, optarg = opar,
			temp1, nxf = 1},

		nx = EpsEvaluate[ExpandScalarProduct[x]//Expand]//Expand;
		(* Split the sum into two parts *)
		result = nx;

		(* eps x1 + eps x2 + ... + rest, then split the eps part from the rest! *)
		all  = PartitHead[nx, Eps];

		If[ !(Head[all[[2]]]===Plus),
			Return[result nxf]
		];

		temp0 = PartitHead[all[[2]], Pair];

		temp = temp0[[2]];

		(*let us assume that optarg is always an Integer! *)
		(* then dummIlabel is always false! *)

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

							(* Strange, can lind return something different than 6??*)
							If[ Length[lind]===6,
								(* create a list of 5 possible arrangements of terms of Schouten ident. *)

								schou = lisch /@ Map[Append[#, Last[lind]]&,NestList[RotateLeft,Take[lind,5],4]];

								(*special sorting, w.r.t the position of the Lorentz indices in eps *)
								sor = Sort[ schou, epc ]//Reverse;
								(*rotate one position to the left if temp does not contain the first element of sor *)
								Do[
									If[ FreeQ[temp, sor[[1]]],
										sor = RotateLeft[sor]
									],
									{6}
								];

								If[ !FreeQ[temp, sor[[1]]],
									(*for the case that temp does contain an element of sort, apply the identity*)
									ntemp = Expand[EpsEvaluate[temp/.sor[[1]]-> (-Plus@@Rest[sor])]];
									(* or all LorentzIndices are inside all Eps's *)
									(* if the number of terms containing eps is smaller than the original length, or the number o
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

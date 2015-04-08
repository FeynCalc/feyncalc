

(* :Title: Schouten *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option and Function *)

(* ------------------------------------------------------------------------ *)

Schouten::usage =
"Schouten[expr] applies the Schouten identity for four-vectors on at most
42 terms in a sum. If Schouten should operate on larger
expression you can give a second argument, e.g.:
Schouten[expr, 4711] which will work
on sums with less than 4711 terms.\n\n

Schouten is also an option of Contract and
DiracTrace. It may be set to an integer
indicating the maximum number of terms onto which the
function Schouten will be applied .";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Schouten`Private`"]

Schouten[y_, 0] :=
	y;
Schouten[y_, oparg_:42] :=
	FixedPoint[schouten[#, oparg]&, FeynCalcInternal[y], 14];

liget[_. Eps[x1_[y1_], x2_[y2_], x3_[y3_], x4_[y4_]] Pair[x5_[y5_],
x6_[y6_]]] :=
	{x1[y1],x2[y2],x3[y3],x4[y4],x5[y5],x6[y6]};

lisch[{a1_,a2_,a3_,a4_,a5_,a6_}] :=
	Pair[a5,a6] Eps[a1,a2,a3,a4];

schouten[x_,opar_:42] :=
	MemSet[schouten[x, opar],
		Block[ {i,nx,temp0,temp,lind,ltemp,ntemp,dummIlabel = False,DUMMI,
		schou,sor, all,result,epc, epsnterms,numberofli, optarg = opar,
		temp1, nxf = 1},
			epc[a_, b_] :=
				If[ Length[Position[PartitHead[a, Eps][[2]],
				LorentzIndex]] <Length[Position[PartitHead[b, Eps][[2]],
				LorentzIndex]],
					True,
					False,
					False
				];
			epsnterms[0] = 0;
			epsnterms[a_] :=
				Block[ {tem},
					tem = Collect2[a, Eps, Factoring ->False];
					If[ Head[tem]===Plus,
						Length[tem],
						1
					]
				];
			nx = EpsEvaluate[ExpandScalarProduct[x]//Expand]//Expand;
			(* Split the sum into two parts *)
			result = nx;
			If[ (optarg === I) && (Head[nx] === Times),
				nxf = Select[nx, !FreeQ[#, _^(a_/;Head[a] =!= Integer)]&];
				nx = nx/nxf + DUMMI;
				dummIlabel = True;
				optarg = 6
			];
			all  = PartitHead[nx, Eps];
			If[ Head[all[[2]]]===Plus || dummIlabel === True,
				If[ dummIlabel === True,
					temp0 = {0,all[[2]]},
					temp0 = PartitHead[all[[2]], Pair];
				];
				temp = temp0[[2]];
				If[ ((Head[temp]===Plus) && Length[temp] > 1 &&
				If[ IntegerQ[optarg],
					Length[temp] < optarg,
					False
				]) || dummIlabel === True,
					ltemp = Length[temp];
					If[ dummIlabel =!= True,
						temp1 = temp[[1]],
						temp1 = temp;
						ltemp = 7
					];
					numberofli = Length[Position[temp1, LorentzIndex]];
					i = 0;
					temp =
					If[ dummIlabel === True,
						Catch[
							lind = liget[ temp ];
							If[ Length[lind]===6,
								schou = lisch /@
								Map[Append[#, Last[lind]]&,
								NestList[RotateLeft,Take[lind,5],4]];
								sor = schou;
								Do[
									If[ FreeQ[temp, sor[[1]]],
										sor = RotateLeft[sor]
									],
									{6}
								];
								If[ !FreeQ[temp, sor[[1]]],
									ntemp = Expand[EpsEvaluate[temp/.sor[[1]]->
									(-Apply[Plus, Drop[sor,1]])]];
									Throw[ntemp]
								]
							];
						],
						Catch[
							While[i < Length[temp], i++;
													If[ !FreeQ2[temp[[i]],{Eps,Pair}],
														lind = liget[ temp[[i]] ];
														If[ Length[lind]===6,
					(* create a list of 5 possible arrangements of terms of Schouten ident. *)
															schou = lisch /@ Map[Append[#, Last[lind]]&,
															NestList[RotateLeft,Take[lind,5],4]];
															sor = Sort[ schou, epc ]//Reverse;
															Do[
																If[ FreeQ[temp, sor[[1]]],
																	sor = RotateLeft[sor]
																],
																{6}
															];
															FCPrint[2,"sor = ", sor];
															If[ !FreeQ[temp, sor[[1]]],
																ntemp = Expand[EpsEvaluate[temp/.sor[[1]]->
																(-Apply[Plus, Drop[sor,1]])]];
																(* or all LorentzIndices are inside all Eps's *)
																If[ (epsnterms[ntemp] < ltemp) || (Union[Length[ Position[#, LorentzIndex] ]& /@
																Select[ Variables[ntemp], Head[#]===Eps& ]]  === {numberofli}),
																	Throw[temp = ntemp]
																]
															]
														]
													]
							];
							temp
						]
					];
					result = (all[[1]]/.DUMMI->0)  + temp0[[1]] + temp
				]
			];
			result nxf
		]
	];

FCPrint[1,"Schouten.m loaded."];
End[]

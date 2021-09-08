(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalc2FORM                                                   	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Converts FeynCalc expressions to FORM							*)

(* ------------------------------------------------------------------------ *)

EpsDiscard::usage=
"EpsDiscard is an option for FeynCalc2FORM. If set to True all Levi-Civita
tensors are replaced by 0 after contraction.";

FeynCalc2FORM::usage =
"FeynCalc2FORM[exp] displays exp in FORM syntax.

FeynCalc2FORM[file, x] writes x in FORM syntax to a file.

FeynCalc2FORM[file, x == y] writes $x=y$ to a file in FORM syntax.

The capabilities of this function are very limited, so you should not expect
it to easily handle large and complicated expressions.";

FORMEpilog::usage =
"FORMEpilog is an option for FeynCalc2FORM. It may be set to a string which is
put at the end of the FORM-file.";

FORMProlog::usage =
"FORMProlog is an option for FeynCalc2FORM. It may be set to a string which is
put after the type declarations of the FORM-file.";

FORMIdStatements::usage =
"FORMIdStatements is an option for FeynCalc2FORM. It may be set to a string
which is put after the local expression of the FORM-file. When set to True,
FeynCalc will try to generate the statements automatically from the known
values of scalar products.";

TraceDimension::usage =
"TraceDimension is an option for FeynCalc2FORM. If set to 4 then trace is used,
if set to n then tracen is employed.";

FeynCalc2FORM::failmsg = "Error! FeynCalc2FORM has encountered a fatal problem and must \
abort the evaluation. The problem reads: `1`";

FORMAbbreviations::usage =
"FORMAbbreviations is an option for FeynCalc2FORM. It specifies how special
symbols will be abbreviated in the resulting FORM file.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynCalc2FORM`Private`"]

fc2fVerbose::usage="";

(* for taking traces *)
Options[FeynCalc2FORM] = {
	EpsDiscard -> False,
	FCVerbose -> False,
	FinalSubstitutions -> {},
	FORMAbbreviations -> {"syFC","vFC"},
	FORMEpilog -> {"print;",".end"},
	FORMIdStatements -> True,
	FORMProlog -> "write statistics;",
	Replace -> {
		"\\[Alpha]"-> "al", "\\[Beta]"->"be",
		"\\[Gamma]" -> "ga", "\\[Delta]" -> "de",
		"\\[Mu]" -> "mu", "\\[Nu]" -> "nu", "\\[Rho]" -> "ro",
		"\\[Sigma]" -> "si"},
	TraceDimension -> 4
};

FeynCalc2FORM[ file_:"tFc2F", xy_, OptionsPattern[]] :=
	Block[ {holdy, lors, lors4, lorsn, lordim, other, noatomic, newx ,x,y,
			srules, srule2, temp, downp, index4list, indexnlist, momentumlist, addmom,
			newsymlist, idlist, lm2form, dpi, polvecs, form2fc, newxstr, ij, new, nidlist,
			formpro, n2form, nosyml, form2l, jjj, formepi,optFinalSubstitutions,
			optFORMIdStatements},

		If[	!FreeQ2[{xy}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		If [OptionValue[FCVerbose]===False,
			fc2fVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fc2fVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "FeynCalc2FORM: Entering.", FCDoControl->fc2fVerbose];
		FCPrint[3, "FeynCalc2FORM: Entering with: ", xy, FCDoControl->fc2fVerbose];

		If[ Head[xy] === Equal,
			x = xy[[1]];
			y = xy[[2]],
			x = False;
			y = xy
		];

		srules = OptionValue[Replace];
		optFinalSubstitutions = OptionValue[FinalSubstitutions];
		optFORMIdStatements = OptionValue[FORMIdStatements];

		srule2 = Table[ StringJoin@@Rest[Drop[Characters[ToString[InputForm[srules[[i,1]] ]]],-1]] -> srules[[i,2]], {i,Length[srules]}];

		srules = Join[srules,srule2];

		holdy = Hold@@
			{(FeynCalcInternal[y]//DiracGammaExpand//MomentumExpand) /.
				z_Pair -> ExpandScalarProduct[z] /.
				{Pair[a_,b_]^2 :> (Pair[a,b] . Pair[a,b]) /;
				!FreeQ[{a,b}, LorentzIndex]
				}
			};

		If[ OptionValue[TraceDimension],

			If[ !FreeQ[holdy, LorentzIndex[_,_]],
				holdy = holdy /. LorentzIndex[a,_] :> LorentzIndex[a]
			];
			If[ !FreeQ[holdy, Momentum[_,_]],
				holdy = holdy /. Momentum[a_,_] :> Momentum[a]
			];
			If[ !FreeQ[holdy, DiracGamma[_,_]],
				holdy = holdy /. DiracGamma[a,_] :> DiracGamma[a]
			];
		];



		(* get the list of LorentzIndex *)
		lors = Cases2[holdy, LorentzIndex];
		lors = Union[lors] /. LorentzIndex[a_] :> a;
		lors4 = SelectFree[lors, LorentzIndex];
		lorsn = SelectNotFree[lors, LorentzIndex];
		lordim = Union[lorsn /. LorentzIndex[_, di_] :> di];

		If[ Length[lordim] === 1,
			lordim = lordim[[1]],
			If[ Length[lordim] >1,
				Print["too many different dimensions!!"],
				lordim = 4
			]
		];



		lors  = lors /. LorentzIndex[a_, _] :> a;
		lorsn = lorsn /. LorentzIndex[a_, _] :> a;

		(* get the list of Momentum*)
		momentumlist = Union[Flatten[Cases2[{holdy,$ScalarProducts}, Momentum] /. Momentum[a_,___] :> Variables[a]]];



		noatomic =
			Union[Flatten[
				Map[Variables, Cases[holdy/.DOT->Times, h_ /;(!MemberQ[{LorentzIndex,Momentum,DiracGamma,Eps, DiracTrace,Pair, Symbol}, Head[y]]),Infinity]]]
			];

		FCPrint[2, "FeynCalc2FORM: preliminary noatomic: ", noatomic, FCDoControl->fc2fVerbose];

		noatomic = Select[noatomic, (!MemberQ[{LorentzIndex,Momentum,DiracGamma,Eps, DiracTrace,Symbol,Pair, Polarization, String}, Head[#]])&];

		FCPrint[2, "FeynCalc2FORM: final noatomic: ", noatomic, FCDoControl->fc2fVerbose];

		(* replace the non-Symbol arguments of LorentzIndex and Momentum by Symbols *)
		nosyml = Select[Join[momentumlist, lors], Head[#] =!= Symbol &];
		lm2form = Table[ nosyml[[i]] -> ToExpression[ StringJoin[ OptionValue[FORMAbbreviations][[2]], ToString[i] ] ], {i, Length[nosyml]}];
		FCPrint[2, "FeynCalc2FORM: Replacement table for the non-Symbol arguments of LorentzIndex and Momentum: ", lm2form, FCDoControl->fc2fVerbose];

		(* get all other atomic variables *)         (* see p. 725  *)

		other = SelectFree[Union[Cases[holdy/.lm2form, _Symbol, -1]], Join[lors, momentumlist,(lm2form /. Rule[_, b_] :> b)]];
		other = Union[other,SelectFree[Union[Cases[FCI[((SP @@ #) & /@ $ScalarProducts)]/.lm2form, _Symbol, -1]], Join[lors, momentumlist,(lm2form /. Rule[_, b_] :> b)]]];


		FCPrint[2, "FeynCalc2FORM: other: ", other, FCDoControl->fc2fVerbose];

		(* for the reverse substitutions *)
		form2l = Map[Reverse, lm2form];
		FCPrint[2, "FeynCalc2FORM: form2l : ", form2l, FCDoControl->fc2fVerbose];

		index4list = lors4 /. lm2form;
		indexnlist = lorsn /. lm2form;
		momentumlist = momentumlist /. lm2form;
		eps2f[a__] :=
			-I Global`eE[a] /. Momentum[aa_,___] :> aa /. LorentzIndex[bb_,___]:>bb;

		pair2f[LorentzIndex[a_,___], LorentzIndex[b_,___]] :=
			Global`dD[a/.lm2form, b/.lm2form];

		$tracecount = 0;

		diracg[5] :=
			Global`gA5[$tracecount];
		diracg[6] :=
			Global`gA6[$tracecount]/2;
		diracg[7] :=
			Global`gA7[$tracecount]/2;
		diracg[_[ls_]] :=
			Global`gA[$tracecount, ls];


		(* assume that momenta are Symbols *)
		pair2f[LorentzIndex[a_Symbol,___], Momentum[b_, ___]] :=
			b[a/.lm2form];
		pair2f[Momentum[a_Symbol,___], Momentum[b_, ___]] :=
			b.a;

		(* construct the list of substitutions for all noatomics *)
		n2form  = Table[ noatomic[[i]] -> ToExpression[ StringJoin[ OptionValue[FORMAbbreviations][[1]], ToString[i] ] ], {i, Length[noatomic]}];
		FCPrint[2, "FeynCalc2FORM: n2form : ", n2form, FCDoControl->fc2fVerbose];


		form2fc = Join[form2l, Map[Reverse, n2form]];

		FCPrint[2, "FeynCalc2FORM: form2fc : ", form2fc, FCDoControl->fc2fVerbose];

		newsymlist = noatomic /. n2form;
		FCPrint[2, "FeynCalc2FORM: preliminary newsymlist: ", newsymlist, FCDoControl->fc2fVerbose];

		newsymlist = Join[other, newsymlist];

		(* ???
		If[ !FreeQ[holdy/.lm2form, Complex],
			AppendTo[newsymlist, I]
		];*)

		FCPrint[2, "FeynCalc2FORM: final newsymlist: ", newsymlist, FCDoControl->fc2fVerbose];



		dirtr[a_] :=
			(
			$tracecount++;
			a /. diracgamma -> diracg
			);

		new = ( (holdy /. lm2form /. Pair -> pair2f /. Eps -> eps2f /.
					DiracGamma -> diracgamma /. DOT->NonCommutativeMultiply /.
					DiracTrace -> dirtr /. diracgamma -> diracg
					)[[1]] ) //.n2form;

		temp = OpenWrite[$TemporaryPrefix<>"teEmpf", FormatType -> InputForm];
		Write[temp, new];
		newx = ReadList[$TemporaryPrefix <> "teEmpf", String];
		Close[temp];
		DeleteFile[$TemporaryPrefix <> "teEmpf" ];


		(*Mac fix 18/9-2000, F.Orellana. Ditto for FileType below*)
		If[ FileType[file] === File,
			DeleteFile[file]
		];

		newx =
			StringReplace[StringReplace[newx,srules],	{
				"\""->"",
				"dD"->"d_",
				"["->"(", "\\"->"",
				"]" -> ")",
				" " -> "",
				"I" -> "i_",
				"gA5" -> "g5_",
				"gA6" -> "g6_",
				"gA7" -> "g7_",
				"gA"->"g_",
				"eE"->"e_",
				" . "->".",
				"$"->"_",
				"**" -> "*",
				"Sqrt" -> "sqrt_"
			}
		];

		(* construct the id  -  statements *)
		downp = Select[DownValues[Pair]/.Momentum[a_,___]:>Momentum[a], FreeQ2[#, {Blank, Pattern}]&];

		If[	FreeQ[holdy,OPEDelta],
			downp = SelectFree[downp,OPEDelta];
			idlist = Map[Join[#, {SP @@ #}] &, SelectFree[$ScalarProducts, Plus, OPEDelta, ExplicitLorentzIndex[0], TemporalMomentum, CartesianIndex, CartesianMomentum] /. Momentum[z_, ___] :> z],

			idlist = Map[Join[#, {SP @@ #}] &, SelectFree[$ScalarProducts, Plus, ExplicitLorentzIndex[0], TemporalMomentum, CartesianIndex, CartesianMomentum] /. Momentum[z_, ___] :> z]
		];





		FCPrint[2, "FeynCalc2FORM: Preliminary list of id statements: ", idlist, FCDoControl->fc2fVerbose];

		idlist = idlist /. lm2form /. n2form;

		FCPrint[2, "FeynCalc2FORM: List of id statements after applying lm2form: ", idlist, FCDoControl->fc2fVerbose];

		If[ !FreeQ[momentumlist/.form2fc, Polarization],
			polvecs = SelectNotFree[momentumlist/.form2fc, Polarization];
			nidlist = Table[{polvecs[[j]], polvecs[[j,1]],0},{j,Length[polvecs]}];
			nidlist = nidlist /. lm2form;
			idlist = Join[idlist, nidlist];
			FCPrint[3, "FeynCalc2FORM: List of id statements  after adding polarization vectors: ", idlist, FCDoControl->fc2fVerbose]
		];



		(* there might be additional momenta *)
		addmom = Cases[SelectFree[Cases2[$ScalarProducts, Momentum], Polarization], _Symbol, -1];
		momentumlist = Union[momentumlist, addmom];

		(*	Final symbols *)

		OpenWrite[file, FormatType -> InputForm];
		If[ Length[newsymlist] > 0,
			WriteString[file, "Symbols "];
			For[ij = 1, ij < Length[newsymlist], ij++,
					WriteString[file, newsymlist[[ij]]];
					If[ ij < Length[newsymlist],
						WriteString[file, ","]
					];
					];
			WriteString[file, Last[newsymlist], ";\n"];
		];

		(*	Final indices *)

		index4list = Map[StringReplace[ToString[#],srules]&,index4list];
		indexnlist = Map[StringReplace[ToString[#],srules]&,indexnlist];



		If[ Length[index4list] > 0 && x =!= False,
			WriteString[file, "Indices "];

			For[ij = 1, ij < Length[index4list], ij++,
				WriteString[file, index4list[[ij]],","];
			];

			WriteString[file, Last[index4list], ";\n"];
		];

		If[ Length[indexnlist] > 0 && x =!= False,
			WriteString[file, "Indices "];

			For[ij = 1, ij < Length[indexnlist], ij++,
					WriteString[file, indexnlist[[ij]],"=",lordim,","];
			];

			WriteString[file, Last[indexnlist], "=",lordim," ;\n"];
		];

		(*	Final vectors *)

		If[ Length[momentumlist] > 0 && x =!= False,
			WriteString[file, "Vectors "];

			For[ij = 1, ij < Length[momentumlist], ij++,
				WriteString[file, momentumlist[[ij]]];
				If[ ij < Length[momentumlist],
						WriteString[file, ","]
				];
			];

			WriteString[file, Last[momentumlist], ";\n"];
		];

		(*	Final prolog *)

		formpro = OptionValue[FORMProlog];
		If[ formpro =!= "" && x =!= False,
			If[ Head[formpro] =!= List,
				formpro = Flatten[{formpro}]
			];
			Write[file];
			For[ij = 1, ij <= Length[formpro], ij++,
					WriteString[file, formpro[[ij]],"\n"];
			];
			Write[file];
		];

		(*	Final local expresisons *)

		If[ x=!= False,
			WriteString[file, "Local ",x , " = ( \n"];
		];
		newxstr = "";
		For[jjj = 1, jjj <= Length[newx],jjj++,
				WriteString[file, newx[[jjj]]];
				newxstr = newxstr <> newx[[jjj]];
				If[ jjj < Length[newx],
					If[ StringLength[newxstr<>newx[[jjj+1]]] > 79,
						newxstr = "";
						WriteString[file, "\n"];
					]
				];

				If[ x===False && file === "tFc2F",
					WriteString["stdout", newx[[jjj]],"\n"]
				]
		];

		If[ x=!= False,
			WriteString[file, " ); \n   \n"];
		];

		(*	Final trace statements *)

		(* in case there are traces *)
		If[ $tracecount > 0 && x =!= False,
			For[i = 1, i <= $tracecount, i++,
					If[ OptionValue[TraceDimension] === 4,
						WriteString[file, "trace4,"<>ToString[i]<>";\n"],
						WriteString[file, "tracen,"<>ToString[i]<>";\n"]
					];
			];
			WriteString[file, "contract 0;\n\n"]
		];

		If[ OptionValue[EpsDiscard] && x =!= False,
			WriteString[file,
				"if ( count(e_,1) > 0 );\n",
				"     discard;\n",
				"endif;\n\n"
			]
		];

		(*	Final id statements *)

		Which[
			optFORMIdStatements===True,
				If[ Length[idlist] > 0 && x =!= False,
					Write[file];
					For[ij = 1, ij <= Length[idlist], ij++,
						WriteString[file, "id  ",idlist[[ij, 1]],".",idlist[[ij, 2]], " = ", idlist[[ij, 3]]//InputForm, "; \n"];
					];
					Write[file];
				],
			optFORMIdStatements===False,
				Null,

			MatchQ[optFORMIdStatements,_String|_List],
				If[ optFORMIdStatements =!= "" && x=!= False,
					If[ Head[optFORMIdStatements] =!= List,
						optFORMIdStatements = Flatten[{optFORMIdStatements}]
					];

					Write[file];
					For[ij = 1, ij <= Length[optFORMIdStatements], ij++,
						WriteString[file, optFORMIdStatements[[ij]],"\n"];
					];

					Write[file];
				],
			True,
			Message[FeynCalc2FORM::failmsg,"Unsupported value of FORMIdStatements"];
			Abort[]
		];


		(*	Final epilog *)

		formepi = OptionValue[FORMEpilog];

		If[ formepi =!= "" && x=!= False,
			If[ Head[formepi] =!= List,
				formepi = Flatten[{formepi}]
			];

			Write[file];
			For[ij = 1, ij <= Length[formepi], ij++,
					WriteString[file, formepi[[ij]],"\n"];
			];

			Write[file];
		];

(* This goes into FORMEpilog!
		If[ x=!= False,
			WriteString[file, "Print; \n"];
			WriteString[file, ".end"]
		];
*)

		Close[file];
		If[ file === "tFc2F",
			If[ FileType["tFc2F"]===File,
				DeleteFile["tFc2F"]
			]
		];



		form2fc
	];

FCPrint[1,"FeynCalc2FORM.m loaded."];
End[]



(* :Title: FORM2FeynCalc *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 17 December '98 at 11:25 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  read in FORM-result files and translate the syntax *)

(* ------------------------------------------------------------------------ *)

FORM2FeynCalc::usage =
"FORM2FeynCalc[exp] translates the FORM expression exp into FeynCalc notation.

FORM2FeynCalc[file]  translates the FORM expressions in file into FeynCalc
notation.   

FORM2FeynCalc[file, x1, x2, ...] reads in a file in FORM-format and translates
the assignments for the variables $a, b, \\ldots$ into FeynCalc syntax.

If the option Set is True, the variables x1, x2 are assigned to the right hand
sides defined in the FORM-file.The capabilities of this function are very
limited, so that you should not expect it to easily handle large and
complicated expressions.";

Vectors::usage =
"Vectors is an option for FORM2FeynCalc. Its default setting is Automatic. It
may be set to a list, if the FORM-file does not contain a V(ectors) statement.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FORM2FeynCalc`Private`"]

(* can be changed *)
$ReplaceAlwaysFirst =
	{"  "->" ","i_" -> "I","d_" -> "MT","( "->"(",
		"(  "->"("," )"->")","  )"->")",
	" -"->"-","- "->"-"," +"->"+","+ "->"+"," + "->"+"};

$ReplaceAlwaysLast  = {"_" -> "$", " [" -> " Hold[",
						"-["->"-Hold[","+["->"+Hold[",
						"*["->"*Hold["};

Options[FORM2FeynCalc] =
{Dimension -> 4, FinalSubstitutions -> {},
Dot -> Dot,
HoldForm -> True,
LorentzIndex -> {FCGV["mu"], FCGV["nu"], FCGV["al"], FCGV["be"]},
Set -> False,
Replace -> {   (*     "d_" -> "SUNDelta",*)
(*
							"d_(a1,a2)" -> "SUNDelta[a1,a2]",
							"[d0^m]"    -> "SO[p]^OPEm",
							"Li2" -> "(PolyLog[2,#]&)",
							"li2" -> "(PolyLog[2,#]&)",
							"Li3" -> "(PolyLog[3,#]&)",
							"Li4" -> "(PolyLog[4,#]&)",
							"li3" -> "(PolyLog[3,#]&)",
							"S12(1 - y)" -> "Nielsen[1,2,1-y]",
							"[d_(1-x)]"  -> "DeltaFunction[1-x]",
							"[(-)^m]"    -> "(-1)^OPEm",
							"[1-x]^-1"  -> "1/(1-x)",
							"[1-x]"  -> "(1-x)",
							"[1+x]"  -> "(1+x)",
							"zeta2"    -> "Zeta2",
							"zeta3"    -> "Zeta[3]",
							"[s12(1-x)]" -> "Nielsen[1,2, 1-x]",
							"s12(1-x)" -> "Nielsen[1,2, 1-x]",
							"[li2(1-x)]" -> "PolyLog[2,1-x]",
							"[li3(1-x)]" -> "PolyLog[3,1-x]",
							"[li2(-x)]"  -> "PolyLog[2,-x]",
							"[log(x)]"   -> "Log[x]",
							"[log(1+x)]" -> "Log[1+x]",
							"[log(1-x)]" -> "Log[1-x]"
*)
					},
Vectors -> Automatic
};

toexp[x_String] :=
	Block[ {temp},
		Begin["Global`"];
		temp = ToExpression[x, TraditionalForm];
		End[];
		temp
	];

SetAttributes[FORM2FeynCalc, HoldAll];


FORM2FeynCalc[fi_,ru___Rule] :=
	FORM2FeynCalc[fi, False, ru];


FORM2FeynCalc[fi_, exprs___, lastexpr_ /; Head[lastexpr] =!= Rule, ru___Rule] :=
	Block[{	file, dim, ff, ffj, hh, dott, parsit, rr, vectors, indices,
			stringrules, vv, rv, nof, holdform , dotrule, holdplus,
			myDot,myfile, set, vecs, vecsubs, rli},


		If[	!FreeQ2[{fi,exprs}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		Map[Unset, Hold[exprs, lastexpr]];
		dim = Dimension /. {ru}/. Options[FORM2FeynCalc];
		holdform = HoldForm /. {ru}/. Options[FORM2FeynCalc];
		dotrule = Dot -> (Dot /. {ru}/. Options[FORM2FeynCalc]);
		set = Set /. {ru} /. Options[FORM2FeynCalc];
		stringrules = Join[$ReplaceAlwaysFirst,
					Replace /. {ru} /. Options[FORM2FeynCalc]] /.
					Rule[a_, b_] :> Rule[ToString[a], ToString[b]];
		indices = Map[ToExpression, Map[ToString, LorentzIndex /. {ru} /.
										Options[FORM2FeynCalc]
									] /. stringrules
					];
		file = ToString[fi, PageWidth->Infinity];
		(*Mac fix 18/9-2000, F.Orellana*)
		myfile = FileType[file];
		Which[myfile === File,
			ff = ReadList[file, String];,
	myfile === None,
	ff = file,
	True,
	Print["There was a problem:\n",file," is inaccessible"];
	Return[]
	];

(*If[FileNames[file] === {},
	ff = file,
	ff = ReadList[file, String];
	];*)
		If[ (Vectors /. {ru}) =!= Automatic,
			vectors = Vectors /. {ru},
			vectors = Catch[
			For[j = 1, j <= Length[ff], j++,
				If[ StringMatchQ[ff[[j]], "V*;*",IgnoreCase -> True],
					vecs = StringReplace[ff[[j]],"  "->" "];
					While[StringMatchQ[vecs,"*  *"],
						vecs = StringReplace[vecs, "  " -> " "];
				];
					vecs = StringReplace[vecs, {"V* "->"{", ";"->"};"},
										IgnoreCase -> True];
					FCPrint[1,"vecs = ",vecs];
					Throw[vecs]
				]
				]           ];
		];

		(* create uniquely 3 characters long vectors *)
		count[a_] :=
			If[ a<10,
				"0"<>#,
				#
			]&[ToString[a]];
		vecsubs = Table[vecs[[r]]->toexp["C"<>count[r]],{r,Length[vecs]}];


		(* XXX *)
		ffj = StringJoin@@ff;
		If[ StringMatchQ[ffj, "*=*"],
			hh = getformexpr[ff],
			hh = ffj
		];
		dott[a_,b_] :=
			Pair[Momentum[a,dim], Momentum[b,dim]];
		Clear[holdplus];
		SetAttributes[holdplus, HoldAll];
		SetAttributes[holdplus2, HoldAll];
		holdplus[z__] :=
			((holdplus2@@{z}) /. holdplus->Plus /.
			holdplus2->holdplus) /; !FreeQ[{z},holdplus];
		parsit[y_String] :=
			(toexp[ StringReplace[
			FixedPoint[StringReplace[#,stringrules]&,y,100] ,
					$ReplaceAlwaysLast]
				] /. Dot -> dott /. Times -> myDot /.
					myDot -> Dot /. Plus->
					holdplus
			) /. dotrule;
		myDot[n_Integer, m__] :=
			n myDot[m];
		Unprotect[Times, Plus];
		ClearAttributes[Times, Orderless];
		ClearAttributes[Plus, Orderless];
		rr = parsit[ StringReplace[hh, {"=" -> "->"}] ];
		SetAttributes[Times, Orderless];
		SetAttributes[Plus, Orderless];
		Protect[Times, Plus];


		(*???????
		(* fix a problem ... *)
		nof = SelectNotFree[Cases[rr, w_Symbol[_], Infinity],
						{Dot,Times,Plus,holdplus}];

		nfv = Thread[nof -> (Map[(#/.(a_[b_]) :> (a . b))&, nof])];
		rr = rr /. nfv;
		*)
		(* check for four-vectors *)
		vv = SelectNotFree[Cases[rr, _[_], Infinity], indices];
		If[ Length[vv]===1,
			vv = Join[vv,vv]
		];
		rv = Thread[vv -> (vv /. ff_[in_] :> FV[ff,in])];
		rr = rr /. rv;
		rr = rr /. myDot -> Dot /. dotrule;
		If[ holdform === True,
			rr = rr /. holdplus[ww__] :> HoldForm[pluh[ww]]  /.
				pluh -> Plus,
			rr = rr /. holdplus->Plus
		];
		If[ dim =!= 4,
			rr = ChangeDimension[rr, dim]
		];
		rr = FeynCalcExternal[rr];
		If[ !FreeQ[rr, Rule],
			If[ Head[Union[First/@rr]//First] === Rule,
				rli = Map[First, rr],
				rli = {}
			]
		];
		If[ set === True,
			rli = rr /. Rule -> Set,
			rli = rr
		];
		If[ Length[rli] === 1,
			rli[[1]],
			rli
		]
	];

(* ******************** *)


(* create one String of     l = ... *)
(* or make a list *)
Clear[getformexpr];
getformexpr[si_List] :=
	Block[ {new = "{", flag = False, s, res},
		s = Append[si," "];
		For[i = 1, i <= Length[s], i++,
			(* this way also other garbage can be around *)
			If[ flag === True,
				If[ StringMatchQ[s[[i]], "*;*"], (*Print[i];*)
					new = new <> (StringReplace[s[[i]], ";" -> ","]),
					new = new <> s[[i]]
				],
				If[ StringMatchQ[s[[i]], "*=*"],
					new = new <> s[[i]];
					flag = True
				];
			] ];
		flag = True;
		While[flag && (StringLength[new]>0),
				If[ StringTake[new,-1] =!= ",",
					new = StringDrop[new,-1],
					flag = False;
					new = StringDrop[new,-1]<>"}"
				]
			];
		new = StringReplace[new,";"->","];
(*
*)
		new
	];

FCPrint[1,"FORM2FeynCalc.m loaded."];
End[]

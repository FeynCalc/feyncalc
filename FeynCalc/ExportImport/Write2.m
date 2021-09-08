(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Write2															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Writes expressions to files 									*)

(* ------------------------------------------------------------------------ *)

FortranFormatDoublePrecision::usage =
"FortranFormatDoublePrecision is an option for Write2.";

FUNCTION::usage =
"FUNCTION[exp, string] is a head of an expression to be declared a function (of
type String), if used in Write2.";

PostFortranFile::usage =
"PostFortranFile is an option for Write2 which may be set to a file name (or a
list of file names) or a string,  which will be put at the end of the
generated Fortran file.";

PreFortranFile::usage =
"PreFortranFile is an option for Write2 which may be set to a file name (or a
list of file names) or a string, which will be put at the beginning of the
generated Fortran file.";

Write2::usage =
"Write2[file, val1 = expr1, val2 = expr2, ...] writes the settings val1 =
expr1, val2 = expr2 in sequence followed by a newline, to the specified output
file. Setting the option FormatType of Write2 to FortranForm results in
Fortran syntax output.";

$FortranContinuationCharacter::usage =
"$FortranContinuationCharacter is the continuation character used in Write2.";

D0Convention::usage =
"D0Convention is an option for Write2. If set to 1, the convention for the
arguments of D0 is changed when writing a Fortran file with Write2: The fifth
and sixth argument of D0 are interchanged and the square root is taken of the
last four arguments.";

Write2::failmsg =
"Error! Write2 has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Write2`Private`"]

optFFDP::usage="";
w2Verbose::usage="";
finsubst::usage="";
pagewidth::usage="";
optFormatType::usage="";

Options[Write2] = {
	D0Convention					-> 0,
	FCVerbose						-> False,
	FinalSubstitutions				-> {},
	FormatType						-> InputForm,
	FortranFormatDoublePrecision	-> True,
	PageWidth						-> 62,
	PostFortranFile					-> {""},
	PreFortranFile					-> {""},
	Precision						-> Floor[$MachinePrecision],
	StringReplace					-> {}
};

SetAttributes[Write2, HoldRest];

$FortranContinuationCharacter = "&";

Write2[f_String, x__ , opts:OptionsPattern[]] :=
	Write2[f, Hold[x], opts]/; !MatchQ[Hold[x], Hold[_Hold ..]] && FreeQ[Hold[x],Rule];

Write2[file_String, expr:Except[_?OptionQ].., OptionsPattern[]] :=
	Block[ {j,vv,eq,k2str,tmp,
	ide, aa0, be00, be11,be0,be1, db0, ce0, de0, ansg,d0convention,
	prefortran, postfortran, prerec,tostring,flag,strep,prec, eqj1, eqj2,
	mantissa, exponent, jj, iir, iv, ii, rfile, rf1, rf2, ir, joinlabel, optFormatType},

		If[	!FreeQ2[{expr}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		If[	OptionValue[FCVerbose]===False,
			w2Verbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				w2Verbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "Write2. Entering.", FCDoControl->w2Verbose];
		FCPrint[3, "Write2: Entering with ", expr, FCDoControl->w2Verbose];

		finsubst 	= OptionValue[FinalSubstitutions];
		pagewidth	= OptionValue[PageWidth];
		prefortran	= OptionValue[PreFortranFile];
		postfortran	= OptionValue[PostFortranFile];
		strep		= OptionValue[StringReplace];
		optFFDP		= OptionValue[FortranFormatDoublePrecision];
		prec 		= OptionValue[Precision];
		optFormatType = OptionValue[FormatType];
		d0convention = OptionValue[D0Convention];

		{aa0,be0,be1,be00,be11,db0,ce0,de0} = {A0,B0,B1,B00,B11,DB0,C0,D0}/.finsubst;



		ide = {##}&;

		eq = Flatten[{Hold[{expr}]} /. Set -> Equal /. Hold -> ide];

		FCPrint[3, "Write2: Rewritten input expression ", eq, FCDoControl->w2Verbose];


		If[ optFormatType === FortranForm,
		(* N@ added by RM on Sept. 13th 2003, because of http://www.feyncalc.org/forum/0153.html*)
			(*	When we apply N to the full expression, this also affects terms from Isolate, e.g.
				HoldForm[KK[10]] will become HoldForm[KK[10.]], after which KK[10.] will not return
				the original expressio anymore. The following prevents this from happending *)
			FCPrint[1, "Write2: FormatType is FortranForm.", FCDoControl->w2Verbose];
			eq = eq /. HoldForm[x_[y_]] :> With[{z = ToString[y]}, HoldForm[x[z]]];
			eq = N[eq];
			eq = eq /. HoldForm[x_[y_String]] :> With[{z = ToExpression[y]}, HoldForm[x[z]]];

			FCPrint[3, "Write2: Prepared expression: ", eq, FCDoControl->w2Verbose];

			If[	optFFDP,
				eq = eq  /. xxx_Real :> fhead[xxx] /. fhead[xxx_?Negative] :> - fhead[-xxx];
					(* Define a custom formatter that will generate Fortran's DOUBLE PRECISION format*)
					fhead /: Format[fhead[r_Real], FortranForm] := (
						{mantissa, exponent} = MantissaExponent[r];
						If[	r === 0.,
							exponent = 1
						];
						If[	Abs[r] < 10 && Chop[FractionalPart[r]] === 0,
							SequenceForm[ToExpression[ToString[NumberForm[r // Floor, prec]]], D, exponent - 1],
							SequenceForm[ToExpression[ToString[NumberForm[10. mantissa, prec]]], D,	exponent - 1]
						])
			];
		];


		tostring =
			If[ Head[#] === String,
				#,
				ToString[#]
			]&;

		If[ !FreeQ[ eq, HoldForm ],
			vv = Union[Flatten[Table[vhf[eq[[jj,2]]], {jj, Length[eq]}]]],
			vv = {}
		];

		FCPrint[3, "Write2: vv: ", vv, FCDoControl->w2Verbose];


		streamOpen[file];


		FCPrint[1, "Write2: Starting the j-loop.", FCDoControl->w2Verbose];

		For[ j = 1, j<=Length[eq], j++,

				FCPrint[4, "Write2: Inside the j-loop: j: ", j, FCDoControl->w2Verbose];
				eqj1 = eq[[j,1]];
				FCPrint[4, "Write2: Inside the j-loop: eqj1: ", eqj1, FCDoControl->w2Verbose];

				If[ optFormatType === FortranForm,
					FCPrint[4, "Write2: Inside the j-loop: FormatType is FortranForm.", FCDoControl->w2Verbose];
					eqj2 = eq[[j,2]] /. Power->pww;
					FCPrint[4, "Write2: Inside the j-loop: eqj2: ", eqj2, FCDoControl->w2Verbose];
					If[ Head[eqj1] === FUNCTION,
						FCPrint[4, "Write2: Head is FUNCTION.", FCDoControl->w2Verbose];
						WriteString[file, "      FUNCTION ", eqj1[[1]]//tostring, "()"];
						Write[file];
						eqj1 = eqj1[[1]]
					];
					If[ prefortran =!= {""},
						FCPrint[4, "Write2: PreFortranFile is not empty.", FCDoControl->w2Verbose];
						(*Mac fix 18/9-2000, F.Orellana. Ditto for FileType below*)
						If[ FileType[prefortran[[1]]] =!= None,
						(*If[FileNames[prefortran[[1]]] =!= {},*)
							prerec = Flatten[ReadList[#, Record]& /@ prefortran],
							prefortran = prefortran[[1]];
							prerec = ReadList[StringToStream[prefortran], String]
						];
						flag = False;
						For[iir = 1, iir <= Length[prerec], iir++,
							If[ flag =!= True,
								If[ !StringMatchQ[StringJoin@@Drop[prerec,iir-1], "*IMPLICIT*"],
									flag = True
								];
							];
							WriteString[file, prerec[[iir]]];
							Write[file];
						]
					],
					FCPrint[4, "Write2: Inside the j-loop: FormatType is not FortranForm.", FCDoControl->w2Verbose];
					eqj2 = eq[[j,2]]
				];
				FCPrint[4, "Write2: Inside the j-loop: Entering Which.", FCDoControl->w2Verbose];
				Which[
						optFormatType === InputForm,
							FCPrint[4, "Write2: Inside the j-loop: Inside Which: FormatType is InputForm.", FCDoControl->w2Verbose];
							mal[x_] :=
								(True/;Length[Characters[ToString[InputForm[x]]]]<73) /; Length[x]<22;
							If[ (!FreeQ[ eqj2, HoldForm ]) && j===1,
								For[iv = 1, iv<=Length[vv], iv++,
									If[ mal[vv[[iv]]//ReleaseHold]=!=True,
										WriteString[file,
											ToString[vv[[iv]]], " = ( "],
										WriteString[file,
											ToString[vv[[iv]]], " = ("]
									];
									Write[file, ReleaseHold[ vv[[iv]] ] ];
									If[ mal[vv[[iv]]//ReleaseHold]=!=True,
										WriteString[file, "       );\n"],
										WriteString[file, "       );\n"]
									]
								]
							];(* Write[file];*)
							If[ mal[eqj2]=!=True,
								WriteString[file, eqj1//InputForm, " = ( " ],
								WriteString[file, eqj1//InputForm, " = "]
							];
							Write[ file, eqj2 ];
							If[ mal[eqj2]=!=True,
								WriteString[file, "       );\n"],
								Null
							],

						optFormatType === FortranForm,
							FCPrint[4, "Write2: Inside the j-loop: Inside Which: FormatType is FortranForm.", FCDoControl->w2Verbose];

							If[ d0convention === 0,
								ansg[x_] :=
									x/. 0 -> Null/.  finsubst
							];
							If[ d0convention === 1,
								ansg[v_. x_] :=
									(v x)/; FreeQ2[(v x)/.finsubst,
													{de0, ce0, be0, aa0, db0}];
								ansg[v_. x_] :=
									Block[ {args,t4,t5,t6,ll},
										args = Apply[List, x];
										t4 = Take[args,4];
										t5 = args[[5]];
										t6 = args[[6]];
										ll = PowerExpand[ Sqrt[Take[args,-4]] ] /. finsubst;
										(v (Apply[de0, Join[t4, {t6,t5}, ll]]) /. 0 -> Null)
									] /;
									( (Head[x/.finsubst]===(de0)) && (Head[v/.finsubst] =!= (de0)) );
								ansg[v_. x_] :=
									Block[ {args, mm},
										args = List @@ x;
										mm = PowerExpand[ Sqrt[Take[args,-3]] ] /. finsubst;
										(v (ce0@@Join[Take[args,3], mm])/. 0 -> Null)
									] /;
									( (Head[x/.finsubst]===(ce0)) && (Head[v/.finsubst] =!= (ce0)) );
								ansg[x_] :=
									Block[ {args, mm},
										args = List@@x;
										mm = PowerExpand[ Sqrt[Take[args,-2]] ] /. finsubst;
										((be0@@Join[{args[[1]]}, mm])/. 0 ->Null)
									]/;
											Head[x/.finsubst]===(be0);
								ansg[v_. x_] :=
									Block[ {args, mm},
										args = List@@x;
										mm = PowerExpand[ Sqrt[Take[args,-2]] ] /. finsubst;
										(v (db0@@Join[{args[[1]]}, mm])/. 0 -> Null)
									] /;
									( (Head[x/.finsubst]===(db0)) && (Head[v/.finsubst] =!= (db0)) );
								ansg[x_] :=
									Block[ {mm},
										mm = PowerExpand[ Sqrt[x] ] /. finsubst;
										aa0[mm]
									] /; Head[x/.finsubst]===(aa0);
							];
							If[ (!FreeQ[ eqj2, HoldForm ]) && (j===1),
								For[iv = 1, iv<=Length[vv], iv++,

									WriteString[file, "        ", (vv[[iv]])//FortranForm,"= "];
									If[ !FreeQ2[{ be0, be1, be00, be11, db0, ce0, de0 },
										Map[Head, Select[ Variables[ReleaseHold[ vv[[iv]]]]/.finsubst, !FreeQ2[{be0, be1, be00, be11, db0, ce0, de0}, Head[#] ]& ]]
										],
										Apply[WriteString,{file, StringReplace[ToString[ansg[(ReleaseHold[vv[[iv]]]/.finsubst) /.
											SmallVariable->Identity/. Power->pww ] /. ansg->Identity,
											FormatType->FortranForm, PageWidth->pagewidth],
											Flatten[{"Null" -> "0D0", strep}]]
										}];
										Write[file],
										Write[file, ReleaseHold[vv[[iv]]]/. SmallVariable->Identity/.Power->pww ];
									];
								]
							];
							WriteString[file, "        ",FortranForm[eqj1]," = "];
							(* 	Seems that the internal behavior of Write has changed in MMA 10, which is
								why we need this trick with tmp...	*)
							tmp = ansg[(eqj2/.SmallVariable-> Identity/. Power->pww )/.finsubst] /. ansg -> Identity;
							(*If [$VersionNumber >= 10,
								tmp = FortranForm[tmp]
							];*)
							Write[file, tmp]
					](* endWhich *)
			]; (* end j - loop *)

		FCPrint[1, "Write2: Finished the j-loop.", FCDoControl->w2Verbose];


		If[ optFormatType === FortranForm,

			If[ postfortran =!= {""},
				If[ FileType[postfortran[[1]]] =!= None,
					If[ Head[postfortran===String],
						prerec = ReadList[StringToStream[postfortran], String],
						prerec = Flatten[ReadList[#, Record]& /@ postfortran]
					];
					postfortran = postfortran[[1]];
					For[iir = 1, iir <= Length[prerec], iir++,
						WriteString[file, prerec[[iir]]];
						Write[file]
					];
					Write[file]
				]
			];

		];

		Close @@ {file};

		(* for Fortran: check if no line is larger than 72 columns*)
		If[ optFormatType === FortranForm,


			rfile = ReadList[file, Record];
			rfile = StringReplace[#, "=         "->"= "]&/@rfile;
			If[ MatchQ[strep , {__Rule}],
				rfile = Map[StringReplace[#,strep]&, rfile],
				strep = {}
			];
			streamOpen[file];
			If[ OddQ[ Length[rfile] ],
				rfile = Append[rfile, "                  "]
			];
			For[ir = 1, ir < Length[rfile], ir = ir + 2,
					joinlabel = False;
					rf1 = StringReplace[rfile[[ir]],   {"\\"->""}];
					If[ StringLength[rf1] > 8,
						rf1 = StringReplace[StringTake[rf1, 8],"-" ->
											$FortranContinuationCharacter
											] <>
								StringReplace[StringDrop[rf1, 8],strep]
					];
					rf2 = StringReplace[rfile[[ir+1]], {"\\"->""}];
					If[ StringLength[rf2] > 8,
						rf2 = StringReplace[StringTake[rf2, 8],"-" ->
											$FortranContinuationCharacter
											] <>
								StringReplace[StringDrop[rf2, 8],strep]
					];
					If[ (StringLength[rf1] > 72) || (StringLength[rf2]) > 72,
						Print["FORTRAN generation WARNING! Line encountered with more than 72 characters. Check line ", ir, " and ",ir+1];
					];

					If[ joinlabel === True,
						WriteString[file, rf1, "\n" ],
						WriteString[file, rf1, "\n", rf2, "\n" ]
					]
				];
			Close @@ {file};

			(* reestablish old FortranForm format behaviour *)
			If[ optFFDP,
				Unset[FormatValues[fhead]];
			]
		];

		file
	];


(* Open the stream	*)
streamOpen[file_]:=
	Which[
		optFormatType === FortranForm,
			OpenWrite[file, FormatType -> FortranForm, PageWidth-> pagewidth],
		optFormatType === InputForm,
			OpenWrite[file, FormatType -> InputForm, PageWidth-> pagewidth],
		True,
			Message[Write2::failmsg,"Unknown FormatType."];
			Abort[]
	];


(* vhf gives all "KK" which are really present *)
vhf[n_. y_HoldForm] :=
	Block[ {kk, qq},
		FCPrint[2,"Write2: vhf: Entering with ", n y];
		kk = y[[1, 0]];
		(Table[ HoldForm @@ {qq[i]}, {i, y[[1,1]]} ] /. qq -> kk)/.finsubst
	] /; NumberQ[n];

vhf[y_] :=
	Block[ {te = y, var = {}},
		FCPrint[2,"Write2: vhf: Entering with ", y];
		While[	!FreeQ[te, HoldForm],
				var = Union[ var, allvar[te] ];
				te = ReleaseHold[te]
		];
		var = Union[ var, allvar[te] ];
		var/.finsubst
	];

(* allvar gives all Variables in HoldForm,( KK[i] ) *)
allvar[y_] :=
	Block[ {arr = {},ia,new,alt = Drop[#, -1]& /@ Position[y,HoldForm]},
		For[ia = 1, ia <= Length[alt], ia++,
			new = Part @@ Prepend[alt[[ia]], y];
			If[ !MemberQ[arr, new],
				AppendTo[arr,new]
			] ];
		arr
	];


(* a modified Power function, for avoiding Fortran-Complications *)
pww[x_?NumberQ, 1/2] :=
	(Sqrt[N[x]]/. xxx_Real/; optFFDP :> fhead[xxx]);
pww[x_?NumberQ, rat_Rational] :=
	((Power[N[x], N[rat]])/. xxx_Real/; optFFDP -> fhead[xxx]);
pww[x_,1/2] :=
	(Sqrt[x]/. xxx_Real/; optFFDP :> fhead[xxx]);
pww[x_, rat_Rational] :=
	(Power[x,N[rat]]/. xxx_Real/; optFFDP :> fhead[xxx]);
pww[x_, he_] :=
	((x^he)/. z_Real/; optFFDP :> fhead[z]) /; Head[he]=!=Rational;

fhead[x_]:=
	x/; Head[x]=!=Real && FreeQ2[{x},{Pattern,Blank}];

FCPrint[1,"Write2.m loaded."];
End[]

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Write2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: RM: changed Sept. 13, 2003*)
(* ------------------------------------------------------------------------ *)

(* :Summary: Write2 *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`Write2`",
             "HighEnergyPhysics`FeynCalc`"];

FortranFormatDoublePrecision::"usage"=
"FortranFormatDoublePrecision is an option for Write2.";

FUNCTION::"usage"=
"FUNCTION[exp, string] is a head of an expression to be declared a
function (of type string), if used in Write2.";

PostFortranFile::"usage"=
"PostFortranFile is an option for Write2 which may be set to a file
name (or a list of file names) or a string, 
which will be put at the end of the generated
Fortran file.";

PreFortranFile::"usage"=
"PreFortranFile is an option for Write2 which may be set to a file
name (or a list of file names) or a string,
which will be put at the beginning of the generated
Fortran file.";

Write2::"usage" = 
"Write2[file, val1 = expr1, val2 = expr2, ...] writes the settings
val1 = expr1, val2 = expr2 in sequence followed by a newline, to the
specified output file. Setting the option FormatType of Write2 to 
FortranForm results in FORTRAN syntax output.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
 


D0Convention             = MakeContext["D0Convention"];
finalsubstitutions       = MakeContext["FinalSubstitutions"];
a0  :=  a0               = MakeContext["A0"];
b0  :=  b0               = MakeContext["B0"];
b1  :=  b1               = MakeContext["B1"];
b00 :=  b00              = MakeContext["B00"];
b11 :=  b11              = MakeContext["B11"];
dB0 :=  dB0              = MakeContext["DB0"];
c0  :=  c0               = MakeContext["C0"];
d0  :=  d0               = MakeContext["D0"];
freeq2                   = MakeContext["FreeQ2"];
small                    = MakeContext["SmallVariable"];

Options[Write2]={ 
                  D0Convention -> 0, 
                  finalsubstitutions -> {},
                  FormatType -> InputForm, 
 	          FortranFormatDoublePrecision -> True,                 
                  PageWidth    -> 62,
                  PostFortranFile -> "",
                  PreFortranFile -> "", 
                  StringReplace->{}
                };

SetAttributes[Write2, HoldRest];

Write2[f_String, x___, l_] := 
 Write2[f, Hold[x, l], dummyrule->False ]/; FreeQ[Hold[l], Rule];

Write2[file_String, eeq__, opts___Rule] := Block[{j,vhf,vv,eq,k2str,
ops,ide, aa0, be00, be11,be0, db0, ce0, de0, ansg,d0convention,
oldopenops,pww,prefortran, postfortran, pagewidth,prerec,tostring,flag,strep},

ops         = Join[{opts}, Options[Write2]];
{finsubst, pagewidth } = {finalsubstitutions, PageWidth} /. ops;
{prefortran, postfortran}  = Flatten /@ {{PreFortranFile}, 
                                         {PostFortranFile}} /. ops;
strep = StringReplace/.ops/.Options[Write2];
(* a modified Power function, for avoiding Fortran-Complications *)
pww[x_?NumberQ, 1/2]          := Sqrt[N[x]];
pww[x_?NumberQ, rat_Rational] := Power[N[x], N[rat]];
pww[x_,1/2]                   := Sqrt[x];
pww[x_, rat_Rational]         := Power[x,N[rat]];
pww[x_, he_]                  := (x^he) /; Head[he]=!=Rational;

{aa0,be0,be1,be00,be11,db0,ce0,de0} = {a0,b0,b1,b00,b11,dB0,c0,d0}/.finsubst;
 (* allvar gives all Variables in HoldForm,( KK[i] ) *)
allvar[y_] := Block[{arr={},ia,new,alt=Drop[#, -1]& /@ Position[y,HoldForm]},
                     For[ia = 1, ia <= Length[alt], ia++,
                         new = Part @@ Prepend[alt[[ia]], y];
                         If[!MemberQ[arr, new], AppendTo[arr,new] ] ];
               arr];
ide = {##}&;
eq = Flatten[{Hold[{eeq}]} /. Set -> Equal /. Hold -> ide];
mrel[x_] := MapAll[ReleaseHold, x];
(* vhf gives all "KK" which are really present *)
vhf[n_. y_HoldForm]:= Block[{kk, qq}, kk = y[[1, 0]];
  (Table[ HoldForm @@ {qq[ii]}, {ii, y[[1,1]]} ] /. qq -> kk)/.finsubst
                           ] /; NumberQ[n];
vhf[y_] := Block[{te=y, var={}}, 
   While[!FreeQ[te, HoldForm], var = Union[ var, allvar[te] ];
         te = ReleaseHold[te]
        ]; 
    var = Union[ var, allvar[te] ];
   var/.finsubst];

If[(FormatType/.ops/.Options[Write2]) === FortranForm,
(* N@ added by RM on Sept. 13th 2003, because of http://www.feyncalc.org/forum/0153.html*)
eq = N[eq];
               oldopenops = Options[OpenWrite];

togglerule  = False;
Unprotect[Real];
Real /: Format[r_Real, FortranForm] :=
          ({mantissa, exponent} = MantissaExponent[r];
            If[r === 0., exponent = 1];
           If[Abs[r] < 10^16 && Chop[FractionalPart[r]] === 0,
              togglerule=False;
              SequenceForm[r//Floor, D, exponent-1]
              ,
              SequenceForm[10. mantissa,D,exponent-1]
             ]
          ) /; (togglerule = !  togglerule
               ) && ((FortranFormatDoublePrecision/.{opts}/.Options[Write2])===True);

               SetOptions[OpenWrite, FormatType->FortranForm, 
                                     PageWidth-> pagewidth ];
(*
                  WriteString[file,
"C  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *"];
                  Write[file];
                  WriteString[file,"C      ", ToString[Date[][[3]]],".",
                   ToString[Date[][[2]]], ".",ToString[Date[][[1]]]
                             ];
                  Write[file];
                  WriteString[file,
"C  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *"];
                  Write[file];
*)
  ];
tostring = If[Head[#] === String, #, ToString[#]]&;
If[ !FreeQ[ eq, HoldForm ], 
    vv = Union[Flatten[Table[vhf[eq[[jj,2]]], {jj, Length[eq]}]]
              ], 
    vv = {} 
  ];
    For[ j=1, j<=Length[eq], j++,
          eqj1 = eq[[j,1]]; 
          If[(FormatType/.ops/.Options[Write2]) === FortranForm,
             eqj2 = eq[[j,2]] /. Power->pww;
             If[Head[eqj1] === FUNCTION,
                WriteString[file, "      FUNCTION ", eqj1[[1]]//tostring,
                                  "()"];
                Write[file];
                eqj1 = eqj1[[1]]
               ];

               If[prefortran =!= {""},
                  (*Mac fix 18/9-2000, F.Orellana. Ditto for FileType below*)
		  If[FileType[prefortran[[1]]] =!= None,
		  (*If[FileNames[prefortran[[1]]] =!= {},*)
                  prerec = Flatten[ReadList[#, Record]& /@ prefortran],
                     prefortran = prefortran[[1]];
                     prerec = ReadList[StringToStream[prefortran], String]
                    ];
                  flag = False;
                  For[iir = 1, iir <= Length[prerec], iir++,
                      If[flag =!= True, 
                         If[!StringMatchQ[StringJoin @@ 
                                          Drop[prerec,iir-1], "*IMPLICIT*"],
(*
                            If[Length[eqj1]===2, 
                               WriteString[file, eqj1[[2]]],
                               WriteString[file, "      REAL*8"]
                              ];
                            WriteString[file, "      ",eqj1, "\n"];
(*
                            WriteString[file, eqj1, "\n"];
*)
                            If[Length[vv] > 0,
                               vardec = StringJoin["      COMPLEX*16  ",
                                                   ToString[vv[[1,1,0]]],
                                                   "(",
                                                   ToString[Length[vv]+42],
                                                   ")\n"];
                               WriteString[file,vardec ];
                              ];
*)
                            flag = True
                           ];
                        ];
                      WriteString[file, prerec[[iir]]];
                      Write[file];
                     ]
                 ],
              eqj2 = eq[[j,2]]
            ];

         Which[ 
               (FormatType/.ops/.Options[Write2]) === InputForm,
                     If[FreeQ[Streams[], file],
                        OpenWrite[file, FormatType -> InputForm]
                       ];
                     mal[x_]:=(True/;Length[Characters[ToString[ 
                                             InputForm[x]]]]<73
                              ) /; Length[x]<22;
                     If[ (!FreeQ[ eqj2, HoldForm ]) && j===1,
                         For[iv=1, iv<=Length[vv], iv++,
                             If[mal[vv[[iv]]//ReleaseHold]=!=True,
                                WriteString[file, 
                                   ToString[vv[[iv]]], " = ( "],
                                WriteString[file, 
                                   ToString[vv[[iv]]], " = ("] 
                               ];
                             Write[file, ReleaseHold[ vv[[iv]] ] ];
                             If[mal[vv[[iv]]//ReleaseHold]=!=True,
                                WriteString[file, "       );\n"],
(* NEW*)
                                WriteString[file, "       );\n"]
                               ]
                            ] 
                       ];(* Write[file];*)
                     If[mal[eqj2]=!=True,
                        WriteString[file, eqj1//InputForm, " = ( " ],
                        WriteString[file, eqj1//InputForm, " = "]
                       ];
                     Write[ file, eqj2 ];
                     If[mal[eqj2]=!=True,
                        WriteString[file, "       );\n"],Null
(*
(* NEW*)
                                WriteString[file, "        ;\n"]
*)
                       ],

               (FormatType/.ops/.Options[Write2]) === FortranForm,
               oldopenops = Options[OpenWrite];
               SetOptions[OpenWrite, FormatType->FortranForm,
                                     PageWidth-> pagewidth
                         ];

               d0convention = D0Convention /. ops /. Options[Write2];
               If[ d0convention === 0, 
                   ansg[x_] := x/. 0 -> Null/.  finsubst
                 ];
If[ d0convention === 1, 
    ansg[v_. x_]:= (v x)/; freeq2[(v x)/.finsubst, 
                                  {de0, ce0, be0, aa0, db0}];
    ansg[v_. x_] := Block[{args,t4,t5,t6,ll}, args = Apply[List, x];
         t4 = Take[args,4]; t5 = args[[5]]; t6 = args[[6]]; 
         ll = PowerExpand[ Sqrt[Take[args,-4]] ] /. finsubst;
         (v (Apply[de0, Join[t4, {t6,t5}, ll]]) /. 0 -> Null) ] /;
         ( (Head[x/.finsubst]===(de0)) && (Head[v/.finsubst] =!= (de0)) );
    ansg[v_. x_]:=Block[{args, mm}, args = List @@ x;
         mm = PowerExpand[ Sqrt[Take[args,-3]] ] /. finsubst;
         (v (ce0@@Join[Take[args,3], mm])/. 0 -> Null)] /;
          ( (Head[x/.finsubst]===(ce0)) && (Head[v/.finsubst] =!= (ce0)) );
    ansg[x_]:=Block[ {args, mm}, args = List@@x;
         mm = PowerExpand[ Sqrt[Take[args,-2]] ] /. finsubst;
         ((be0@@Join[{args[[1]]}, mm])/. 0 ->Null)]/;
                      Head[x/.finsubst]===(be0);
    ansg[v_. x_]:=Block[{args, mm}, args = List@@x;
         mm = PowerExpand[ Sqrt[Take[args,-2]] ] /. finsubst;
         (v (db0@@Join[{args[[1]]}, mm])/. 0 -> Null)] /; 
         ( (Head[x/.finsubst]===(db0)) && (Head[v/.finsubst] =!= (db0)) );
    ansg[x_]:=Block[{mm}, mm = PowerExpand[ Sqrt[x] ] /. finsubst;
                    aa0[mm] ] /; Head[x/.finsubst]===(aa0);
  ];
If[ (!FreeQ[ eqj2, HoldForm ]) && (j===1),
    For[iv=1, iv<=Length[vv], iv++,
(*XXXX *)
        WriteString[file, "        ", (vv[[iv]])//FortranForm,"= "];
        If[!freeq2[{ be0, be1, be00, be11, db0, ce0, de0 }, 
                    Map[Head, Select[ Variables[ReleaseHold[ vv[[iv]] ] 
                                             ]/.finsubst,
                                   !freeq2[{be0, be1, be00, be11, 
                                            db0, ce0, de0
                                           }, Head[#] ]& ]
                       ]
                  ]
         ,
          Apply[WriteString,{file, StringReplace[ToString[ansg[
                            ( ReleaseHold[vv[[iv]]]/.finsubst ) /.
                             small->Identity/.
                               Power->pww ] /. ansg->Identity,
                            FormatType->FortranForm, PageWidth->pagewidth],
                                           Flatten[{"Null" -> "0D0", strep}]]
                          }];
          Write[file], Write[file, ReleaseHold[vv[[iv]]]/.      
                           small->Identity/.Power->pww ];
           ];
       ]
  ];
  WriteString[file, "        ",FortranForm[eqj1]," = "];
(*
  WriteString[file, FortranForm[eqj1]," = "];
*)
  Write[file, ansg[(eqj2/.small-> Identity/. Power->pww )/.finsubst
                  ] /. ansg -> Identity];
  SetOptions @@ Prepend[oldopenops, OpenWrite]
          ](* endWhich *)
      ]; (* end j - loop *)

If[(FormatType/.ops/.Options[Write2]) === FortranForm,
   If[postfortran =!= {""},
   If[FileType[postfortran[[1]]] =!= None,
        If[Head[postfortran===String],
           prerec = ReadList[StringToStream[postfortran], String],
           prerec = Flatten[ReadList[#, Record]& /@ postfortran]
          ];
        postfortran = postfortran[[1]];
      For[iir = 1, iir <= Length[prerec], iir++,
          WriteString[file, prerec[[iir]]]; Write[file]
         ]; Write[file]
     ] ]
  ];
Close @@ {file};
 (* for Fortran: check if no line is larger than 72 columns*)
If[(FormatType/.ops/.Options[Write2]) === FortranForm,
   rfile = ReadList[file, Record];
   rfile = StringReplace[#, "=         "->"= "]&/@rfile;
   If[MatchQ[strep , {__Rule}],
      rfile = Map[StringReplace[#,strep]&, rfile],
      strep = {}
     ];
   OpenWrite @@ {file};
   If[ OddQ[ Length[rfile] ],
       rfile = Append[rfile, "                  "]
     ];
   For[ir = 1, ir < Length[rfile], ir = ir + 2,
          joinlabel = False;
          rf1 = StringReplace[rfile[[ir]],   {"\\"->""}];
          If[StringLength[rf1] > 8,
             rf1 = StringReplace[StringTake[rf1, 8],"-" ->
                                 $FortranContinuationCharacter
                                ] <> 
                   StringReplace[StringDrop[rf1, 8],strep]
            ];
          rf2 = StringReplace[rfile[[ir+1]], {"\\"->""}];
          If[StringLength[rf2] > 8,
             rf2 = StringReplace[StringTake[rf2, 8],"-" ->
                                 $FortranContinuationCharacter
                                ] <> 
                   StringReplace[StringDrop[rf2, 8],strep]
            ];

          If[(StringLength[rf1] > 72) || (StringLength[rf2]) > 72,
             Print["FORTRAN generation WARNING! 
                    Line encountered with more than 72 characters. " <>
                    "Check line ",ir,
                    " and ",ir+1];
            ];
(* well, ... 
          If[ (StringLength[rf1] > 1) && (StringLength[rf2] > 7),
              If[StringTake[rf2, {6, 8}] === "&  ",
                 rf3 = StringDrop[rf2, 8];
                 If[StringTake[rf1, -1] === " ", rf1 = StringDrop[rf1,-1]];
                 If[StringLength[rf1] + StringLength[rf3] < 72,
                    joinlabel = True;
                    rf1 = StringJoin[rf1, rf3];
            ]   ]  ];
*)
          If[joinlabel === True,
             WriteString[file, rf1, "\n" ],
             WriteString[file, rf1, "\n", rf2, "\n" ]
            ]
      ];
Close @@ {file};

(* reestablish old FortranForm format behaviour *)
If[(FormatType/.ops/.Options[Write2]) === FortranForm,
   Unset[FormatValues[Real]];
   Protect[Real];
  ]
 ];
If[ValueQ[oldopenops],SetOptions @@ Prepend[oldopenops, OpenWrite]];
file];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Write2 | \n "]];
Null

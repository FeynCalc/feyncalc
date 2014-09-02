(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Feynman amplitudes *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FeynAmp`",{"HighEnergyPhysics`FeynCalc`"}];

FeynAmp::"usage"=
"FeynAmp[q, amp] denotes a Feynman amplitude.
amp denotes the analytical expression for the amplitude,
where q is the integration variable.
FeynAmp[q1, q2, amp] denotes a two-loop amplitude.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

FeynAmp /:
  MakeBoxes[ FeynAmp[q_Symbol,amp_], TraditionalForm ] :=
RowBox[{"\[Integral]",
      RowBox[{
         StyleBox[ RowBox[{SuperscriptBox["\[DifferentialD]","D"], q}],
             ZeroWidthTimes -> True ]
            }
            ], "(",Tbox[amp],")"
       }
      ];

FeynAmp /:
  MakeBoxes[ FeynAmp[gr_[__],q_Symbol,amp_], TraditionalForm ] :=
RowBox[{"\[Integral]",
      RowBox[{
         StyleBox[ RowBox[{SuperscriptBox["\[DifferentialD]","D"], q}],
             ZeroWidthTimes -> True ]
            }
            ], "(",Tbox[amp],")"
       }
      ];

FeynAmp /:
  MakeBoxes[ FeynAmp[gr_[__],q_Symbol,amp_], TraditionalForm ] :=
RowBox[{"\[Integral]",
      RowBox[{
StyleBox[
             RowBox[{SuperscriptBox["\[DifferentialD]","D"], q}],
             ZeroWidthTimes -> True
                    ]
            }
            ], "(",Tbox[amp],")"
       }
      ];

FeynAmp /:
  MakeBoxes[ FeynAmp[ q1_, q2_, amp_],
             TraditionalForm
           ] :=
RowBox[
{"\[Integral]",
  RowBox[{
StyleBox[
 RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}],
 ZeroWidthTimes->True
        ] ,
"\[Integral]",
RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q2]}]
         }
        ], "(",Tbox[amp],")"
     }];

FeynAmp /:
  MakeBoxes[ FeynAmp[gr_[__], q1_Symbol, q2_Symbol, amp_],
             TraditionalForm
           ] :=
RowBox[
{"\[Integral]",
  RowBox[{
RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}] ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q2]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         }
        ], "(",Tbox[amp],")"
     }];

  MakeBoxes[ HighEnergyPhysics`FeynCalc`FeynAmp`FeynAmp[
             q1_, q2_, q3_, amp_],
             TraditionalForm
           ] :=
RowBox[
{"\[Integral]",
  RowBox[{
FractionBox[
RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}],
        SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q2]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q3]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         }
        ], "(",Tbox[amp],")"
     }];

  MakeBoxes[ HighEnergyPhysics`FeynCalc`FeynAmp`FeynAmp[
             gr_[__], q1_Symbol, q2_Symbol, q3_Symbol, amp_],
             TraditionalForm
           ] :=
RowBox[
{"\[Integral]",
  RowBox[{
FractionBox[
RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}],
        SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q2]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q3]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         }
        ], "(",Tbox[amp],")"
     }];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynAmp | \n "]];
Null

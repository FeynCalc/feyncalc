(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FCPrint *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FCPrint`",{"HighEnergyPhysics`FeynCalc`"}];

FCPrint::"usage" =
"FCPrint[level, x] outputs Print[x] if the value of $VeryVerbose
is larger than level.";

UseWriteString::"usage" =
"UseWriteString is an option for FCPrint. If set to True,
the expression is printed via WriteString instead of Print.";  

WriteStringOutput::"usage" =
"UseWriteStringOutput an option for FCPrint. It specifies, to which 
stream WriteString should output the expression"; 

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Options[FCPrint] = {    
    UseWriteString -> False,
    WriteStringOutput ->"stdout"
}

FCPrint[level_, x__,opts:OptionsPattern[]] :=
    If[ $VeryVerbose >= level,
        If[ OptionValue[UseWriteString],
            WriteString[OptionValue[WriteStringOutput],x],
            Print[x]
        ]
    ];
SetAttributes[FCPrint, HoldAll];

End[];
EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[ $VeryVerbose > 0,
    WriteString["stdout", "FCPrint | \n "]
];
Null
	
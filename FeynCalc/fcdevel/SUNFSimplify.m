(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNFSimplify     												*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Simplifies expression that contain SU(N) indices in the
              fundamental representation                                    *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcdevel`SUNFSimplify`",{"HighEnergyPhysics`FeynCalc`"}];

SUNFSimplify::"usage" = "
SUNFSimplify[expr] simplifies expression that contain SU(N) indices in the
fundamental representation";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

CA                  := CA = MakeContext["CoreObjects","CA"];
DotSimplify         := DotSimplify = MakeContext["DotSimplify"];
Expand2             := Expand2 = MakeContext["Expand2"];
Explicit            := Explicit = MakeContext["Explicit"];
ExplicitSUNFIndex   := ExplicitSUNFIndex = MakeContext["CoreObjects","ExplicitSUNFIndex"];
ExplicitSUNIndex    := ExplicitSUNIndex = MakeContext["CoreObjects","ExplicitSUNIndex"];
FCI                 := FCI = MakeContext["FeynCalcInternal"];
FreeQ2              := FreeQ2 = MakeContext["FreeQ2"];
SUNFDelta           := SUNFDelta = MakeContext["CoreObjects","SUNFDelta"];
SUNFDeltaContract   := SUNFDeltaContract = MakeContext["SUNFDeltaContract"];
SUNFIndex           := SUNFIndex = MakeContext["CoreObjects","SUNFIndex"];
SUNIndex            := SUNIndex = MakeContext["CoreObjects","SUNIndex"];
SUNN                := SUNN = MakeContext["CoreObjects","SUNN"];
SUNNToCACF          := SUNNToCACF = MakeContext["CoreOptions","SUNNToCACF"];
SUNSimplify         := SUNSimplify = MakeContext["SUNSimplify"];
SUNT                := SUNT = MakeContext["CoreObjects","SUNT"];
SUNTF               := SUNTF = MakeContext["CoreObjects","SUNTF"];
SUNTrace            := SUNTrace = MakeContext["SUNTrace"];

SetAttributes[SUNFSimplify, Listable];

fci[z_ /; FreeQ[z, Pattern]] := (fci[z] = FCI[z]);

Options[SUNFSimplify] = {SUNNToCACF->True ,Explicit->False};

SUNFSimplify[expr_, OptionsPattern[]] :=
    Block[ {temp = fci[expr],simplify},

        simplify[ex_] :=
            ex/. SUNFDelta -> SUNFDeltaContract /. SUNFDeltaContract -> SUNFDelta //.
            SUNTF[{x__}, i_, j_SUNFIndex] SUNTF[{y__}, j_SUNFIndex, k_] :>
            SUNTF[{x,y}, i, k] /. SUNTF[{x__}, i_SUNFIndex, i_SUNFIndex] :>
            SUNSimplify[SUNTrace[SUNT[x],Explicit->OptionValue[Explicit]],SUNNToCACF->OptionValue[SUNNToCACF]] /.
            SUNTF[{x_,y__},i_,j_]/; FreeQ2[SUNSimplify[SUNT[x,y],
                Explicit->OptionValue[Explicit],SUNNToCACF->OptionValue[SUNNToCACF]], {SUNT,SUNTF,SUNFDelta}] :>
                SUNSimplify[SUNT[x,y],Explicit->OptionValue[Explicit],SUNNToCACF->OptionValue[SUNNToCACF]]*
                SUNFDelta[SUNFIndex[i],SUNFIndex[j]];

        If[ !FreeQ[temp,SUNFIndex],
            temp = Expand2[DotSimplify[temp, Expanding -> False],SUNFIndex];
            temp = FixedPoint[simplify,temp,10];
            If[ OptionValue[SUNNToCACF],
                temp = temp /. SUNN -> CA
            ];
            temp,
            temp
        ]
    ]
End[];
EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[ $VeryVerbose > 0,
    WriteString["stdout", "SUNFSimplify | \n "]
];
Null

(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCPrepareFAAmp                                                   *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  FCPrepareFAAmp converts a FeynArts amplitude to FeynCalc      *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FCPrepareFAAmp`",{"HighEnergyPhysics`FeynCalc`"}];

FCPrepareFAAmp::"usage" =
"FCPrepareFAAmp[exp] converts a FeynArts amplitude to FeynCalc.";

UndoChiralSplittings::"usage" =
"UndoChiralSplittings is an option of FCPrepareFAAmp. When set to True, it attempts
to undo splittings of couplings into left and right handed pieces, e.g
(a*GA[6].GA[mu] + a*GA[7].GA[mu]) will be converted back to a*GA[mu]";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracGamma              := DiracGamma               = MakeContext["CoreObjects","DiracGamma"];
FeynAmp                 := FeynAmp                  = MakeContext["CoreObjects","FeynAmp"];
FeynAmpDenominator      := FeynAmpDenominator       = MakeContext["CoreObjects","FeynAmpDenominator"];
GaugeXi                 := GaugeXi                  = MakeContext["CoreObjects","GaugeXi"];
Gstrong                 := Gstrong                  = MakeContext["CoreObjects","Gstrong"];
LorentzIndex            := LorentzIndex             = MakeContext["CoreObjects","LorentzIndex"];
Momentum                := Momentum                 = MakeContext["CoreObjects","Momentum"];
NonCommutative          := NonCommutative           = MakeContext["CoreObjects","NonCommutative"];
Pair                    := Pair                     = MakeContext["CoreObjects","Pair"];
Polarization            := Polarization             = MakeContext["CoreObjects","Polarization"];
PropagatorDenominator   := PropagatorDenominator    = MakeContext["CoreObjects","PropagatorDenominator"];
SUNF                    := SUNF                     = MakeContext["CoreObjects","SUNF"];
SUNFIndex               := SUNFIndex                = MakeContext["CoreObjects","SUNFIndex"];
SUNIndex                := SUNIndex                 = MakeContext["CoreObjects","SUNIndex"];
SUNTF                   := SUNTF                    = MakeContext["CoreObjects","SUNTF"];
Spinor                  := Spinor                   = MakeContext["CoreObjects","Spinor"];

MakeContext[DiracTrace];

Options[FCPrepareFAAmp] = {UndoChiralSplittings -> False};

FCPrepareFAAmp[expr_, OptionsPattern[]] :=
    Block[ {replist1,replist2,tempvar,temp},
        replist1 = {HighEnergyPhysics`FeynArts`Index[Global`Lorentz, x_] :> LorentzIndex[ToExpression["Lor" <> ToString[x]]],
                    HighEnergyPhysics`FeynArts`Index[Global`Gluon, x_] :> SUNIndex[ToExpression["Glu" <> ToString[x]]],
                    HighEnergyPhysics`FeynArts`Index[Global`Colour, x_] :> SUNFIndex[ToExpression["Col" <> ToString[x]]],
                    HighEnergyPhysics`FeynArts`FourMomentum[HighEnergyPhysics`FeynArts`Incoming,x_] :> ToExpression["InMom" <> ToString[x]],
                    HighEnergyPhysics`FeynArts`FourMomentum[HighEnergyPhysics`FeynArts`Outgoing,x_] :> ToExpression["OutMom" <> ToString[x]],
                    HighEnergyPhysics`FeynArts`FourMomentum[HighEnergyPhysics`FeynArts`Internal,x_] :> ToExpression["LoopMom" <> ToString[x]]
                    };
        replist2 = Dispatch[{
                    Conjugate[Global`FAPolarizationVector][_, x_, y_] :> Pair[LorentzIndex[y],Momentum[Polarization[x,-I]]],
                    Global`FAChiralityProjector[-1] :> DiracGamma[7],
                    Global`FAChiralityProjector[1] :> DiracGamma[6],
                    Global`FADiracMatrix :> DiracGamma,
                    Global`FADiracSlash[x_] :> DiracGamma[Momentum[x]],
                    Global`FADiracSpinor :> Spinor,
                    Global`FADiracTrace :> DiracTrace,
                    Global`FAFourVector[x_,y_] :> Pair[Momentum[x],y],
                    Global`FAGS :> Gstrong,
                    Global`FAMetricTensor :> Pair,
                    Global`FAPolarizationVector[_, x_, y_] :> Pair[LorentzIndex[y],Momentum[Polarization[x,I]]],
                    Global`FASUNF[a_,b_,c_, d_] :> (Clear[tempvar];
                                                    tempvar = Unique[$AL];
                                                    SUNF[a,b,tempvar]SUNF[tempvar,c,d]),
                    Global`FASUNF[a_,b_,c_] :> SUNF[a,b,c],
                    Global`FASUNT :> SUNTF,
                    HighEnergyPhysics`FeynArts`FAFeynAmp :> FeynAmp,
                    HighEnergyPhysics`FeynArts`FAFeynAmpDenominator :> FeynAmpDenominator,
                    HighEnergyPhysics`FeynArts`FAGaugeXi :> GaugeXi,
                    HighEnergyPhysics`FeynArts`FANonCommutative :> DOT,
                    HighEnergyPhysics`FeynArts`FAPropagatorDenominator :> PropagatorDenominator,
                    HighEnergyPhysics`FeynArts`FermionChain :> DOT,
                    HighEnergyPhysics`FeynArts`MatrixTrace :> DiracTrace
                    }];
        temp = expr /. replist1 /. replist2;
        If[ OptionValue[UndoChiralSplittings],
            temp = temp/.{(a1__ DiracGamma[x_].DiracGamma[6] a2__ + a1__ DiracGamma[x_].DiracGamma[7] a2__) :> a1 DiracGamma[x] a2}
        ];
        temp
    ];

End[];
EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[ $VeryVerbose > 0,
    WriteString["stdout", "FCPrepareFAAmp | \n "]
];
Null

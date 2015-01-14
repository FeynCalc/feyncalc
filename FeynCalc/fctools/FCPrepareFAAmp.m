(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCPrepareFAAmp                                                   *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  FCPrepareFAAmp replaces FA* objects by the corresponding
                FeynCalc objects                                            *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FCPrepareFAAmp`",{"HighEnergyPhysics`FeynCalc`"}];

FCPrepareFAAmp::"usage" =
"FCPrepareFAAmp[exp] replaces objects like FASlash, FADiracMatrix etc., \n
by the corresponding FeynCalc objects.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

ChiralityProjector = MakeContext["CoreObjects","ChiralityProjector"];
DiracMatrix = MakeContext["CoreObjects","DiracMatrix"];
DiracSlash = MakeContext["CoreObjects","DiracSlash"];
DiracSpinor = MakeContext["CoreObjects","DiracSpinor"];
FeynAmp = MakeContext["CoreObjects","FeynAmp"];
FeynAmpDenominator = MakeContext["CoreObjects","FeynAmpDenominator"];
FourVector = MakeContext["CoreObjects","FourVector"];
Gstrong = MakeContext["CoreObjects","Gstrong"];
GaugeXi = MakeContext["CoreObjects","GaugeXi"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
MetricTensor = MakeContext["CoreObjects","MetricTensor"];
NonCommutative = MakeContext["CoreObjects","NonCommutative"];
PolarizationVector = MakeContext["CoreObjects","PolarizationVector"];
PropagatorDenominator = MakeContext["CoreObjects","PropagatorDenominator"];

MakeContext[DiracTrace];

FCPrepareFAAmp[expr_] :=
    ReplaceAll[expr,{
    Global`FAFourVector -> FourVector,
    Global`FAPolarizationVector -> PolarizationVector,
    Global`FADiracSpinor -> DiracSpinor,
    Global`FADiracSlash -> DiracSlash,
    Global`FADiracMatrix -> DiracMatrix,
    Global`FADiracTrace -> DiracTrace,
    Global`FAMetricTensor -> MetricTensor,
    Global`FAChiralityProjector ->ChiralityProjector,
    Global`FAGS -> Gstrong,
    HighEnergyPhysics`FeynArts`FAGaugeXi -> GaugeXi,
    HighEnergyPhysics`FeynArts`FANonCommutative -> NonCommutative,
    HighEnergyPhysics`FeynArts`FAFeynAmp -> FeynAmp,
    HighEnergyPhysics`FeynArts`FAFeynAmpDenominator -> FeynAmpDenominator,
    HighEnergyPhysics`FeynArts`FAPropagatorDenominator -> PropagatorDenominator
    }];

End[];
EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[ $VeryVerbose > 0,
    WriteString["stdout", "FCPrepareFAAmp | \n "]
];
Null

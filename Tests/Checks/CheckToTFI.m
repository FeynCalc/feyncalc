(* ::Package:: *)



(* :Title: CheckToTFI														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Checks that ToTFI is able to correctly convert all
				the integrals that appear in a typical the 2-loop self-energy
				topology.

				The calculation by itself is not correct, since we are neglecting
				the denominators of the loop integrals. But for the purpose of
				checking ToTFI it is fully ok.								*)


(* ------------------------------------------------------------------------ *)

Print["Checking ToTFI"];

$FeynCalcStartupMessages = False;
$LoadFeynArts=True;
$LoadTARCER=True;
<<FeynCalc`;

$FAVerbose = 0;

Paint[inserts = Rest@InsertFields[CreateTopologies[2, 1 -> 1,
			ExcludeTopologies -> {Tadpoles}], {U[5]} -> {U[5]},
			InsertionLevel -> {Classes}, GenericModel -> "Lorentz",
			Model -> "SMQCD"],
		SheetHeader -> False,   Numbering -> None];


amps = Map[ReplaceAll[#, FeynAmp[_, _, amp_, ___] :> amp] &,Apply[List,
	FCPrepareFAAmp[CreateFeynAmp[inserts, Truncated -> True,
				GaugeRules -> {}, PreFactor -> 1/((2^D)*(Pi)^(D/2))],
	UndoChiralSplittings -> True]]] /. {SumOver[__] :> 1,
			MQU[Index[Generation, 3]] -> MQ,
	GaugeXi[_] -> GaugeXi} /. {InMom1 -> p, OutMom1 -> p,
	LoopMom1 -> q1, LoopMom2 -> q2};

xx = ApartFF[Total[amps], {q1, q2}];
yy = Isolate[Collect2[xx, FeynAmpDenominator], FeynAmpDenominator] /.
FeynAmpDenominator[x__] :> FRH[FeynAmpDenominator[x]];
zz = ToTFI[#, q1, q2, p] & /@
	Cases[yy, FeynAmpDenominator[__], Infinity] // Union;
a = Cases[zz, FAD[__], Infinity] // Union;
b = Cases[zz, TFI[__], Infinity] // Union;




On[Assert];

Assert[a === {FAD[p]}]
Assert[b === {TFI[D,
	SPD[p, p], {{1, 0}, {1, 0}, {1, 0}, {1, 0}, {0, 0}}],
TFI[D, SPD[p, p], {{1, 0}, {1, 0}, {1, 0}, {1, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{1, 0}, {1, 0}, {1, 0}, {1, 0}, {2, 0}}],
TFI[D, SPD[p, p], {{1, 0}, {1, 0}, {2, 0}, {1, 0}, {0, 0}}],
TFI[D, SPD[p, p], {{1, 0}, {1, 0}, {2, 0}, {1, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{1, 0}, {1, 0}, {2, 0}, {1, 0}, {2, 0}}],
TFI[D, SPD[p, p], {{1, 0}, {1, 0}, {2, 0}, {2, 0}, {0, 0}}],
TFI[D, SPD[p, p], {{1, 0}, {1, 0}, {2, 0}, {2, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{1, 0}, {1, 0}, {2, 0}, {2, 0}, {2, 0}}],
TFI[D, SPD[p, p], {{2, 0}, {1, 0}, {1, 0}, {0, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{2, 0}, {1, 0}, {1, 0}, {0, 0}, {2, 0}}],
TFI[D, SPD[p, p], {{2, 0}, {1, 0}, {1, 0}, {1, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{2, 0}, {1, 0}, {1, 0}, {2, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{2, 0}, {1, 0}, {2, 0}, {0, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{2, 0}, {1, 0}, {2, 0}, {0, 0}, {2, 0}}],
TFI[D, SPD[p, p], {{2, 0}, {1, MQ}, {1, 0}, {0, 0}, {1, MQ}}],
TFI[D, SPD[p,
	p], {{2, 0}, {1, MQD[Index[Generation, 3]]}, {1, 0}, {0, 0}, {1,
	MQD[Index[Generation, 3]]}}],
TFI[D, SPD[p, p], {{2, 0}, {2, 0}, {1, 0}, {0, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{2, 0}, {2, 0}, {1, 0}, {0, 0}, {2, 0}}],
TFI[D, SPD[p, p], {{3, 0}, {1, 0}, {1, 0}, {0, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{3, 0}, {1, 0}, {1, 0}, {0, 0}, {2, 0}}],
TFI[D, SPD[p, p], {{3, 0}, {1, MQ}, {1, 0}, {0, 0}, {1, MQ}}],
TFI[D, SPD[p,
	p], {{3, 0}, {1, MQD[Index[Generation, 3]]}, {1, 0}, {0, 0}, {1,
	MQD[Index[Generation, 3]]}}],
TFI[D, SPD[p, p], {{3, 0}, {2, 0}, {1, 0}, {0, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{3, 0}, {2, 0}, {1, 0}, {0, 0}, {2, 0}}],
TFI[D, SPD[p, p], {{4, 0}, {1, 0}, {1, 0}, {0, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{4, 0}, {1, 0}, {1, 0}, {0, 0}, {2, 0}}],
TFI[D, SPD[p, p], {{4, 0}, {1, MQ}, {1, 0}, {0, 0}, {1, MQ}}],
TFI[D, SPD[p,
	p], {{4, 0}, {1, MQD[Index[Generation, 3]]}, {1, 0}, {0, 0}, {1,
	MQD[Index[Generation, 3]]}}],
TFI[D, SPD[p, p], {{4, 0}, {2, 0}, {1, 0}, {0, 0}, {1, 0}}],
TFI[D, SPD[p, p], {{4, 0}, {2, 0}, {1, 0}, {0, 0}, {2, 0}}]}];

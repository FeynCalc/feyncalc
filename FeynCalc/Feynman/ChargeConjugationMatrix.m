(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ChargeConjugationMatrix *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ChargeConjugationMatrix is an experimental
						implementation of C *)

(* ------------------------------------------------------------------------ *)
ChargeConjugationMatrix::usage =
"ChargeConjugationMatrix denotes the charge conjugation matrix C.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ChargeConjugationMatrix`Private`"]

If[ FreeQ[$NonComm, ChargeConjugationMatrix] && Head[$NonComm] === List,
	AppendTo[$NonComm, ChargeConjugationMatrix]
];

ChargeConjugationMatrix /: (ChargeConjugationMatrix^n_Integer?EvenQ /; n>0) :=
I^n;

ChargeConjugationMatrix /: (ChargeConjugationMatrix^n_Integer?OddQ /; n>2) :=
ChargeConjugationMatrix (I^(n-1));

ChargeConjugationMatrix /:
MakeBoxes[ChargeConjugationMatrix, TraditionalForm] := "C";

FCPrint[1,"ChargeConjugationMatrix.m loaded."];
End[]

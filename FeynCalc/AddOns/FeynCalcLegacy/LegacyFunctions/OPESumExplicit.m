(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPESumExplicit*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 March '98 at 23:33 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the sum *)

(* ------------------------------------------------------------------------ *)

OPESumExplicit::usage =
"OPESumExplicit[exp] calculates OPESums.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`OPESumExplicit`Private`"]

Options[OPESumExplicit]= {
	Assumptions -> True
};

OPESumExplicit[ex_, OptionsPattern[]] :=
	If[ FreeQ[ex,OPESum],
		ex,
		Block[ {symbolicsum, te},
			te = ex/. OPESum->SymbolicSum3 /. SymbolicSum3 -> OPESum /.
						OPESum -> SymbolicSum2 /. symbolicsum -> SymbolicSum3 /. SymbolicSum3 -> summ;
			If[ (!FreeQ[te, summ]) && !FreeQ[te, OPEi] && !FreeQ[te,OPEj],
				te = te /.(* {OPEi:>OPEj, OPEj :> OPEi} /.*)
						summ -> SymbolicSum2 /. SymbolicSum2 -> SymbolicSum3 /. SymbolicSum3 -> summ;
				If[ !FreeQ[te, summ]&& !FreeQ[te, OPEi] && !FreeQ[te,OPEj],
					te = te /.summ -> SymbolicSum2 /. (*{OPEi:>OPEj, OPEj :> OPEi} /.*)
							SymbolicSum2 -> SymbolicSum3 /.(* {OPEi:>OPEj, OPEj :> OPEi} /.*)
							SymbolicSum3 -> summ;
				];
			];
			te = te /. summ -> symbolicsum/. symbolicsum -> Sum /.
					Sum -> OPESum;
			te = te//PowerSimplify[#, Assumptions->OptionValue[Assumptions]]&;
			te = te /. Power[a_, b_/;Head[b] =!= Integer] :> Power2[a,b];
			te = Expand2[te, Power2] /. Power2 -> Power;
			te = te /. Power[a_, b_/;Head[b] =!= Integer] :> Power2[a,b];
			te
		]
	];

FCPrint[1,"OPESumExplicit.m loaded"];
End[]

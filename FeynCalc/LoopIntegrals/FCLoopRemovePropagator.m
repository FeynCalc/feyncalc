(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopRemovePropagator											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:	Removes propagators from topologies or GLIs					*)

(* ------------------------------------------------------------------------ *)

FCLoopRemovePropagator::usage =
"FCLoopRemovePropagator[input,{pos1,pos2,...}] returns a new FCTopology or GLI
obtained from input by removing propagators at positions listed in
{pos1,pos2,...}.";

FCLoopRemovePropagator::failmsg =
"Error! FCLoopRemovePropagator has encountered a fatal problem and must abort the computation. The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopRemovePropagator`Private`"];

rpVerbose::usage="";


Options[FCLoopRemovePropagator] = {
	FCE			-> False,
	FCI			-> False,
	Names		-> "PFR",
	FCVerbose	-> False
};



FCLoopRemovePropagator[input_, {}, OptionsPattern[]]:=
	input;

FCLoopRemovePropagator[inputsRaw:{__}, propPos:{__List}, opts:OptionsPattern[]]:=
	MapThread[FCLoopRemovePropagator[#1, #2,opts]&,{inputsRaw,propPos}]/; FreeQ[propPos,GLI];

FCLoopRemovePropagator[inputRaw_, propPos_List/; !OptionQ[propPos], OptionsPattern[]] :=
	Block[{	props, nProps, id,  res, propsNew, idNew, input},

		If [OptionValue[FCVerbose]===False,
			rpVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				rpVerbose=OptionValue[FCVerbose]
			];
		];



		If[ Head[input]===FCTopology && !OptionValue[FCI],
			input = FCI[inputRaw],
			input = inputRaw
		];


		FCPrint[1,"FCLoopRemovePropagator: Entering.", FCDoControl->rpVerbose];
		FCPrint[3,"FCLoopRemovePropagator: Entering with: ", input, FCDoControl->rpVerbose];


		If[!MatchQ[propPos,{__Integer?Positive}],
			Message[FCLoopRemovePropagator::failmsg,"The positions of propagators must be postive integerers."];
			Abort[]

		];


		If[ Head[input]===FCTopology,
			If[	!FCLoopValidTopologyQ[input],
				Message[FCLoopValidTopologyQ::failmsg, "The supplied topology is incorrect."];
				Abort[]
			];

		];



		id = input[[1]];
		props = input[[2]];


		propsNew = Delete[props, List/@propPos];
		idNew = ToString[id] <> OptionValue[Names] <> StringJoin[ToString /@ Sort[propPos]];

		If[ TrueQ[Head[input]===FCTopology],
			res = FCTopology[idNew,propsNew,Sequence@@input[[3;;]]],
			res = GLI[idNew,propsNew];
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3,"FCLoopRemovePropagator: Final set of rules: ", res, FCDoControl->rpVerbose];
		FCPrint[1,"FCLoopRemovePropagator: Leaving.", FCDoControl->rpVerbose];

		res

]/; MemberQ[{FCTopology,GLI},Head[inputRaw]];


End[]

(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopAddEdgeTags													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Visualize the output of FCLoopIntegralToGraph				*)

(* ------------------------------------------------------------------------ *)

FCLoopAddEdgeTags::usage =
"FCLoopAddEdgeTags[edges_List, labels_List] adds user-defined styles and labels
to the given edges using the provided list of labels. Styles and labels are
attached using the replacement rules provided via the Style and Labeled
options.";

FCLoopAddEdgeTags::failmsg =
"Error! FCLoopAddEdgeTags has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopAddEdgeTags`Private`"]

fclaet::usage="";
optStyle::usage = "";
optLabel::usage = "";
edgeType::usage = "";
intStyle::usage = "";
extStyle::usage = "";


Options[FCLoopAddEdgeTags] = {
	FCVerbose		-> False,
	Labeled			-> {
		{"InternalLine", _, 1, _}					:> {},
		{"InternalLine", _, pow_ /; pow =!= 1, _}	:> "X",
		{"ExternalLine", _} 						:> {}
	},
	Style 			-> {
		{"InternalLine", _, _, 0} 				:> {Dashed, Thick, Black},
		{"InternalLine", _, _, mm_ /; mm =!= 0} :> {Thick, Black},
		{"ExternalLine", _} 					:> {Thick, Black}
	},
	UndirectedEdge	-> True
};

standardStyles = OptionValue[FCLoopAddEdgeTags, Style];
standardLabels = OptionValue[FCLoopAddEdgeTags, Labeled];

FCLoopAddEdgeTags[{edges_List, labels_List}/; ! OptionQ[{edges, labels}], opts : OptionsPattern[]] :=
	FCLoopAddEdgeTags[edges, labels, opts];

FCLoopAddEdgeTags[edges_List, labels_List/; ! OptionQ[labels], OptionsPattern[]] :=
	Block[{aux, res},

		optStyle = OptionValue[Style];
		optLabel = OptionValue[Labeled];

		If[	TrueQ[OptionValue[UndirectedEdge]],
			edgeType = UndirectedEdge,
			edgeType = DirectedEdge
		];

		If [OptionValue[FCVerbose]===False,
			fclaet=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fclaet=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"FCLoopAddEdgeTags: Entering.", FCDoControl->fclaet];

		aux = Transpose[{edges, labels}];

		res = addStyle /@ aux;

		FCPrint[3,"FCLoopAddEdgeTags: Raw styles: " res, FCDoControl->fclaet];

		res = addLabel /@ res;

		FCPrint[3,"FCLoopAddEdgeTags: Raw labels: " res, FCDoControl->fclaet];

		res = res /. labeled[x_, {}] :> x /. labeled -> Labeled /. intStyle | extStyle -> Style;

		FCPrint[1,"FCLoopAddEdgeTags: Leaving.", FCDoControl->fclaet];
		FCPrint[1,"FCLoopAddEdgeTags: Leaving with: ", res, FCDoControl->fclaet];

		res
	];


addStyle[{edge_, extMom_ /; Head[extMom] =!= List}] :=
	{extStyle[edgeType @@ edge, {"ExternalLine", extMom} /. optStyle /.	standardStyles], extMom};

addStyle[{edge_, labels_List}] :=
	{intStyle[edgeType @@ edge, Join[{"InternalLine"}, labels] /. optStyle /. standardStyles], labels};

addLabel[{st_extStyle, extMom_}] :=
	labeled[st, {"ExternalLine", extMom} /. optLabel /. standardLabels];

addLabel[{st_intStyle, labels_}] :=
	labeled[st, Join[{"InternalLine"}, labels] /. optLabel /. standardLabels];





FCPrint[1,"FCLoopAddEdgeTags.m loaded."];
End[]

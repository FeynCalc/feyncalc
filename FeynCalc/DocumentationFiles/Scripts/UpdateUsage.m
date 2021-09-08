(* ::Package:: *)

<<FeynCalc`


ClearAll[docuToUsage];
docuToUsage[name_String]:=
Block[{path,text,tmp,res},

	path=FileNames[name<>".m",FileNameJoin[{$FeynCalcDirectory,"DocumentationFiles","Mathematica"}],Infinity];
	If[Length[path]=!=1,
		Print["Error, failed to find the documentation file for ", name];
		Abort[],
		path=First[path]
	];
	
	text=Import[path,"Text"];
	tmp=StringCases[text,
	"(* ::Section:: *)\n(*"<>name<>"*)"~~Shortest[x__]~~"(* ::Subsection:: *)":>x]//First;
	tmp=StringReplace[tmp,{"(* ::Text:: *)"->"","*)"|"(*"|"`"->"","\""->"\\\""}];

	tmp=FixedPoint[StringReplace[#,"\n\n\n"->"\n\n"]&,tmp];

	tmp=StringTrim[tmp];

	res=StringRiffle[InsertLinebreaks/@StringSplit[tmp,"\n"],"\n"];
	res=StringReplace[res,"\\"~~x:WordCharacter:>"\\\\"<>x];
	res
];


ClearAll[updateUsage,importAllSymbols, allFiles];
allFiles[]:=allFiles[]=(FileNames["*.m",FileNameJoin[{$FeynCalcDirectory,#}]&/@{"Dirac","ExportImport","Feynman",
"LoopIntegrals","Lorentz","NonCommAlgebra","Pauli","QCD","Shared","SUN","Tables"},Infinity]);
importAllSymbols[]:=importAllSymbols[]=Import[#,"Text"]&/@allFiles[];
updateUsage[name_String,newText_String,save_:False]:=
Block[{path,pos,text,tmp,res,outFile,allTexts},
	
	allTexts=importAllSymbols[];
	tmp=StringCases[allTexts,
	{(name<>"::usage =\n"~~Shortest[x__]~~"\";"):>x,(name<>"::usage=\n"~~Shortest[x__]~~"\";"):>x}];
	pos=SequencePosition[tmp,{{_String}}];
	If[!MatchQ[pos,{{_Integer,_Integer}}],
		Print["Error, failed to locate the file containing the symbol ", name];
		Abort[],
		pos=pos[[1]][[1]];
		path=allFiles[][[pos]];
		text=allTexts[[pos]]
	];
	
	tmp=tmp[[pos]]//First;
	(*Print[newText];*)
	(*Print[tmp];*)
	If[tmp==="\""<>newText<>"\n",
	Print["The usage information for ", name, " doesn't need to be updated"];
	Return[]
	];
	
	res=StringReplace[text,tmp->"\""<>newText<>"\n"];
	
	If[save,
		outFile=OpenWrite[path];
		WriteString[outFile,res<>"\n"];
		Close[outFile]
	]
];


aux=docuToUsage["PolarizationSum"];


updateUsage["PolarizationSum",aux,True]

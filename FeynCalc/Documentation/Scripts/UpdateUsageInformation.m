(* ::Package:: *)

$FeynCalcStartupMessages=False;
<<FeynCalc`


ClearAll[docuToUsage];
docuToUsage[name_String]:=
Block[{path,text,tmp,res},

	path=FileNames[name<>".m",FileNameJoin[{$FeynCalcDirectory,"Documentation","Mathematica"}],Infinity];
	If[Length[path]=!=1,
		Print["Error, failed to find the documentation file for ", name];
		Abort[],
		path=First[path]
	];
	
	text=Import[path,"Text"];

	tmp=StringCases[text,"(* ::Section:: *)\n(*"<>name<>"*)"~~Shortest[x__]~~"(* ::Subsection:: *)":>x];

	If[Length[tmp]=!=1,	
		Print["Error: The documentation file for " name, " is missing a proper section header"];
		Abort[],
		tmp=First[tmp]
	];
	
	tmp=StringReplace[tmp,{"(* ::Text:: *)"->"","*)"|"(*"|"`"->"","\""->"\\\""}];

	tmp=FixedPoint[StringReplace[#,"\n\n\n"->"\n\n"]&,tmp];

	tmp=StringTrim[tmp];

	res=StringRiffle[InsertLinebreaks/@StringSplit[tmp,"\n"],"\n"];
	res=StringReplace[res,"\\"~~x:WordCharacter:>"\\\\"<>x];
	res
];


ClearAll[updateUsage,importAllSymbols, allFiles,importAllSymbols];
allFiles[]:=Join[(FileNames["*.m",FileNameJoin[{$FeynCalcDirectory,#}]&/@{"Dirac","ExportImport","Feynman",
"LoopIntegrals","Lorentz","NonCommAlgebra","Pauli","QCD","Shared","SUN","Tables"},Infinity]),{FileNameJoin[{$FeynCalcDirectory,"FCMain.m"}],FileNameJoin[{$FeynCalcDirectory,"FeynCalc.m"}]}];
importAllSymbols[]:=Import[#,"Text"]&/@allFiles[];
rereadSymbols=True;
updateUsage[name_String,newText_String,save_:False]:=
Block[{path,pos,text,tmp,res,outFile},
	If[TrueQ[rereadSymbols],
		(*Print["rereading"];*)
		allTexts=importAllSymbols[];
		rereadSymbols=False
	];
	tmp=StringCases[allTexts,
	{("\n"<>name<>"::usage =\n"~~Shortest[x__]~~"\";"):>x,("\n"<>name<>"::usage=\n"~~Shortest[x__]~~"\";"):>x}];
	pos=SequencePosition[tmp,{{_String}}];	
	
	If[!MatchQ[pos,{{_Integer,_Integer}}],
		Print["Error, failed to locate the file containing the symbol ", name];
		Abort[],
		pos=pos[[1]][[1]];
		path=allFiles[][[pos]];
		text=allTexts[[pos]]
	];
	
	tmp=tmp[[pos]]//First;
	
	If[tmp==="\""<>newText(*<>"\n"*),
		Return[],
		Print["Updating usage information for ", name];
		rereadSymbols=True
	];
	res=StringReplace[text,tmp->"\""<>newText(*<>"\n"*)];
	If[save,
		outFile=OpenWrite[path];
		WriteString[outFile,res<>"\n"];
		Close[outFile]
	]
];


baseNames=FileBaseName/@FileNames["*.m",FileNameJoin[{$FeynCalcDirectory,"Documentation","Mathematica"}],Infinity];
Print["In total there are ", Length[baseNames], " FeynCalc symbols having usage information."]


input=baseNames;
(*Monitor[*)
MapIndexed[(
If[First[#2]===1||Mod[First[#2],80]===0,
	WriteString["stdout","\n"<>ToString[First[#2]]<>"/"<>ToString[Length[input]]<>"\n"];
];
WriteString["stdout","."];
aux=docuToUsage[#1];updateUsage[#1,aux,True]
)&,input
]
(*,ProgressIndicator[counter,{1,Length[input]}]]*)

(* ::Package:: *)

<<FeynCalc`


ClearAll[docuToUsage];
docuToUsage[path_]:=
Block[{name,text,tmp,res},
	name=FileBaseName[path];
	text=Import[path,"Text"];
	tmp=StringCases[text,
	"(* ::Section:: *)\n(*"<>name<>"*)"~~Shortest[x__]~~"(* ::Subsection:: *)":>x]//First;
	tmp=StringReplace[tmp,{"(* ::Text:: *)"->"","*)"|"(*"|"`"->"","\""->"\\\""}];

	tmp=FixedPoint[StringReplace[#,"\n\n\n"->"\n\n"]&,tmp];

	tmp=StringTrim[tmp];

	res=StringRiffle[InsertLinebreaks/@StringSplit[tmp,"\n"],"\n"];
	res
];


ClearAll[updateUsage];
updateUsage[path_,newText_String,save_:False]:=
Block[{name,text,tmp,res,outFile},
	name=FileBaseName[path];
	text=Import[path,"Text"];
	tmp=StringCases[text,
	(name<>"::usage =\n"~~Shortest[x__]~~"\";"):>x];
	Print[Length[tmp]];
	tmp=tmp//First;
	res=StringReplace[text,tmp->"\""<>newText<>"\n"];
	Print[tmp];
	If[save,
		outFile=OpenWrite[path];
		WriteString[outFile,res<>"\n"];
		Close[outFile]
	]
];


aux


aux=docuToUsage["/media/Data/Projects/VS/FeynCalc/FeynCalc/DocumentationFiles/Mathematica/LoopIntegrals/FCLoopScalelessQ.m"];


updateUsage["/media/Data/Projects/VS/FeynCalc/FeynCalc/LoopIntegrals/FCLoopScalelessQ.m",aux,True]




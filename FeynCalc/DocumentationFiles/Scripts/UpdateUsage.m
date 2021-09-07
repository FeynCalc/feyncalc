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
	res=StringReplace[res,"\\"~~x:WordCharacter:>"\\\\"<>x];
	res
];


ClearAll[updateUsage];
updateUsage[path_,nameRaw_,newText_String,save_:False]:=
Block[{name,text,tmp,res,outFile},	
	name=ToString[nameRaw];
	text=Import[path,"Text"];
	tmp=StringCases[text,
	{(name<>"::usage =\n"~~Shortest[x__]~~"\";"):>x,(name<>"::usage=\n"~~Shortest[x__]~~"\";"):>x}];
	(*Print[Length[tmp]];*)
	If[Length[tmp]=!=1,
		Print["Error, the file does not contain the symbol ", name];
		Abort[];
	];
	
	tmp=tmp//First;
	(*Print[newText];*)
	(*Print[tmp];*)
	If[tmp==="\""<>newText<>"\n",
	Print["The usage information for ", name, " doesn't need to be updated"];
	Return[]
	];
		
	res=StringReplace[text,tmp->"\""<>newText<>"\n"];
	Print[tmp];	
	If[save,
		outFile=OpenWrite[path];
		WriteString[outFile,res<>"\n"];
		Close[outFile]
	]
];


FileNames["*.m",FileNameJoin[{$FeynCalcDirectory,"DocumentationFiles","Mathematica","LoopIntegrals"}],Infinity];


aux=docuToUsage["/media/Data/Projects/VS/FeynCalc/FeynCalc/DocumentationFiles/Mathematica/Shared/Tools/SelectFree2.m"];


updateUsage["/media/Data/Projects/VS/FeynCalc/FeynCalc/Shared/SharedTools.m",SelectFree2,aux,True]




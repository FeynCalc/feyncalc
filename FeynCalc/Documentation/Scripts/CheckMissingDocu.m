(* ::Package:: *)

QuitAbort[]:=
If[$FrontEnd===Null,
	Quit[],
	Abort[]
];


If[!DirectoryQ[docuDir],
	Print["ERROR! The file ", indexFile, " does not exist!" ];
	QuitAbort[]
];


If[!FileExistsQ[indexFile],
	Print["ERROR! The file ", indexFile, " does not exist!" ];
	QuitAbort[]
];


$FeynCalcStartupMessages=False;
<<FeynCalc`


If[loadAddOns==="{}",
	fcSymbols=Names["FeynCalc`*"],
	
	fcSymbolsOld=Names["FeynCalc`*"];
	FCReloadAddOns[ToString/@ToExpression[loadAddOns]];
	fcSymbolsNew=Names["FeynCalc`*"];
	fcSymbols=Complement[fcSymbolsNew,fcSymbolsOld]
];


docFiles=FileBaseName/@FileNames["*.m",FileNameJoin[{docuDir,"Mathematica"}],Infinity];


mdFiles=FileNames["*.md",FileNameJoin[{docuDir,"Markdown"}],Infinity];
mdFilesImported=Import[#,"Text"]&/@mdFiles;
aux2=First[StringSplit[#,"\n"]]&/@mdFilesImported;
aux2=FileBaseName/@Extract[mdFiles,MapIndexed[If[StringFreeQ[#1,"#"],#2,Unevaluated[Sequence[]]]&,aux2]];
Print["Documentation pages missing titles:"];


Print[""]; Print[""];
Print["Symbols missing documentation pages:"];
Switch[FileBaseName[indexFile],
	"FeynCalc",
		Print[StringRiffle[Complement[SelectFree[fcSymbols,{"FerSolve","FeynCalc","SharedObjects","FCLoopBasis","ToSymbol"}],docFiles],"\n"]],
	_,
		Print[StringRiffle[Complement[SelectFree[fcSymbols,{}],docFiles],"\n"]]
	];


Print[""]; Print[""];
Print["Documentation pages for nonexisting symbols:"];
Print[StringRiffle[SelectFree[Complement[docFiles,fcSymbols],"Vectors"],"\n"]];

Switch[FileBaseName[indexFile],
	"FeynCalc",
		Print[StringRiffle[SelectFree[Complement[docFiles,fcSymbols],{"Vectors"}],"\n"]],
	_,
		Print[StringRiffle[SelectFree[Complement[docFiles,fcSymbols],{}],"\n"]]
	];



in=Import[indexFile,"Text"];
str=StringCases[in,"- "~~Shortest[x__]~~" - "/;!StringFreeQ[x,"["] && StringFreeQ[x,"\n"] :>x];

overviewSymbols=Switch[FileBaseName[indexFile],
	"FeynCalc",
		SelectFree[StringReplace[Union[Flatten[StringCases[#,"["~~Shortest[x__]~~"]":>x]&/@StringSplit[str,","]]],"\\"->""],
			{"Upper and lower indices","FeynArts sign conventions"}],
	_,
		SelectFree[StringReplace[Union[Flatten[StringCases[#,"["~~Shortest[x__]~~"]":>x]&/@StringSplit[str,","]]],"\\"->""],{}]
	];


Print[""]; Print[""];
Print["Symbols missing in the overview:"];

Switch[FileBaseName[indexFile],
	"FeynCalc",
		Print[StringRiffle[Complement[SelectFree[fcSymbols,{"FerSolve","FeynCalc","SharedObjects","FCLoopBasis","ToSymbol"}],overviewSymbols],"\n"]];,
	_,
		Print[StringRiffle[Complement[SelectFree[fcSymbols,{}],overviewSymbols],"\n"]]
	];


Print[""]; Print[""];
Print["Nonexisting symbols in the overview:"];
Print[StringRiffle[Complement[overviewSymbols,fcSymbols],"\n"]];

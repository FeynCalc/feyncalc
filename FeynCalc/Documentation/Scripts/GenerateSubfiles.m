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


docFiles=FileBaseName/@FileNames["*.m",FileNameJoin[{docuDir,"Mathematica"}],Infinity];


in=Import[indexFile,"Text"];
str=StringCases[in,"- "~~Shortest[x__]~~" - "/;!StringFreeQ[x,"["]:>x];
overviewSymbols=StringReplace[Union[Flatten[StringCases[#,"["~~Shortest[x__]~~"]":>x]&/@StringSplit[str,","]]],"\\"->""];


If[!DirectoryQ[outputDir],
	Print["ERROR! The directory ", outputDir, " does not exist!" ];
	QuitAbort[]
];


getTitlesFirst[str_]:=
Block[{tmp,res},
tmp=StringCases[str,"- "~~Shortest[x__]~~"\n"/;!StringFreeQ[x,"["]:>x];
res=StringReplace[DeleteDuplicates[Flatten[StringCases[#,"("~~Shortest[x__]~~".md)":>x]&/@StringSplit[tmp,","]]],"\\"->""];
res
]


getTitles[str_]:=
Block[{tmp,res},
tmp=StringCases[str,"- "~~Shortest[x__]~~" - "/;!StringFreeQ[x,"["]:>x];
res=StringReplace[DeleteDuplicates[Flatten[StringCases[#,"["~~Shortest[x__]~~"]":>x]&/@StringSplit[tmp,","]]],"\\"->""];
res
]


raw=Rest[StringSplit[in,"##"]];
out=StringSplit[#,"\n",2]&/@raw;


final=Join[{{StringTrim[out[[1]][[1]]],getTitlesFirst[out[[1]][[2]]]}},
{StringTrim[#[[1]]],getTitles[StringTrim[#[[2]]]]}&/@out[[2;;]]];


finalStr=StringRiffle[Flatten[Map[{"\n\\chapter{"<>#[[1]]<>"}\n",Function[x,{"\\subfile{pages/"<>x<>".tex}"}]/@#[[2]]}&,final]],"\n"];
finalStr=StringReplace[finalStr,{"/$"->"/Dollar"}];


finalStr


Print["Saving the output to ", FileNameJoin[{outputDir,"includes.tex"}]];
WriteString[FileNameJoin[{outputDir,"includes.tex"}],finalStr];

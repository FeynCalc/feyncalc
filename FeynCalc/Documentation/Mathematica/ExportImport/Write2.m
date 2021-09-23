(* ::Package:: *)

 


(* ::Section:: *)
(*Write2*)


(* ::Text:: *)
(*`Write2[file, val1 = expr1, val2 = expr2, ...]` writes the settings `val1 = expr1, val2 = expr2` in sequence followed by a newline, to the specified output file. Setting the option `FormatType` of `Write2` to `FortranForm` results in Fortran syntax output.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Isolate](Isolate.md), [PaVeReduce](PaVeReduce.md).*)


(* ::Subsection:: *)
(*Examples*)


FullForm[$FortranContinuationCharacter]


t=Collect[((a-c)^2+(a-b)^2)^2,a,Factor]


(* ::Text:: *)
(*This writes the assignment r=t to a file.*)


tempfilename=ToString[$SessionID]<>".s";
Write2[tempfilename,r=t];


(* ::Text:: *)
(*This shows the contents of the file.*)


TableForm[ReadList[If[$OperatingSystem==="MacOS",":",""]<>tempfilename,String]]


DeleteFile[If[$OperatingSystem==="MacOS",":",""]<>tempfilename]


t2=x+Isolate[t,a,IsolateNames->w]


Write2[tempfilename,r=t2];


TableForm[ReadList[If[$OperatingSystem==="MacOS",":",""]<>tempfilename,String]]


DeleteFile[If[$OperatingSystem==="MacOS",":",""]<>tempfilename]


(* ::Text:: *)
(*This is how to write out the expression `t2` in Fortran format.*)


Write2[tempfilename,r=t2,FormatType->FortranForm];


TableForm[ReadList[If[$OperatingSystem==="MacOS",":",""]<>tempfilename,String]]


DeleteFile[If[$OperatingSystem==="MacOS",":",""]<>tempfilename];
Clear[w,t,t2,r,tempfilename];

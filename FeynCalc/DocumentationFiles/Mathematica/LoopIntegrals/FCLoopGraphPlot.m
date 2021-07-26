(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopGraphPlot*)


(* ::Text:: *)
(*`FCLoopGraphPlot[{edges, labels}]` visualizes the graph of the given loop integral using the provided list of edges, styles and labels using the built-in function `Graph`. The Option `Graph` can be used to pass options to the `Graph` objects.*)


(* ::Text:: *)
(*By default, `FCLoopGraphPlot` returns a `Graph`. When using Mathematica 12.2 or newer, it is also possible to return a `Graphics` object created by `GraphPlot`. For this the option `GraphPlot` must be set to a list of options that will be passed to `GraphPlot`. An empty list is also admissible. For example, `FCLoopGraphPlot[int, GraphPlot -> {MultiedgeStyle -> 0.35, Frame -> True}]`.*)


(* ::Text:: *)
(*Given a list of `Graph` or `Graphics` objects created by `FCLoopGraphPlot`, a nice way to get a better overview is to employ `Magnify[Grid[(Partition[out, UpTo[4]])], 0.9]`.*)


(* ::Text:: *)
(*Notice that older Mathematica versions have numerous shortcomings in the graph drawing capabilities that cannot be reliably worked around. This why to use `FCLoopGraphPlot` you need to have at least Mathematica 11.0 or newer. For best results we recommend using Mathematica 12.2 or newer.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*FCLoopIntegralToGraph*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*1-loop tadpole*)


FCLoopIntegralToGraph[FAD[{p,m}],{p}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*1-loop massless bubble*)


FCLoopIntegralToGraph[FAD[p,p-q],{p}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*1-loop massless triangle*)


FCLoopIntegralToGraph[FAD[p,p+q1,p+q1+q2],{p}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*1-loop massless box*)


FCLoopIntegralToGraph[FAD[p,p+q1,p+q1+q2,p+q1+q2+q3],{p}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*1-loop massless pentagon*)


FCLoopIntegralToGraph[FAD[p,p+q1,p+q1+q2,p+q1+q2+q3,p+q1+q2+q3+q4],{p}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*2-loop massless self-energy*)


FCLoopIntegralToGraph[FAD[p1,p2,Q-p1-p2,Q-p1,Q-p2],{p1,p2}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*Same topology as before but now fully massive and with some dots*)


FCLoopIntegralToGraph[FAD[{p1,m},{p2,m2},{Q-p1-p2,m},{Q-p1,m,2},{Q-p2,m,2}],{p1,p2}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*3-loop massless self-energy*)


FCLoopIntegralToGraph[FAD[p1,p2,p3,Q-p1-p2-p3,Q-p1-p2,Q-p1,Q-p2,p1+p3],{p1,p2,p3}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*3-loop self-energy with two massive lines*)


FCLoopIntegralToGraph[Times@@{SFAD[{{p1,0},{m^2,1},1}],SFAD[{{p2,0},{0,1},1}],
SFAD[{{p3,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],
SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{m^2,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}]},{p1,p2,p3}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*2-loop triangle*)


FCLoopIntegralToGraph[FAD[p1,p2,Q1+p1,Q2-p1,Q1+p1+p2,Q2-p1-p2],{p1,p2}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*Not all loop integrals admit a graph representation. Furthermore, an integral may have a weird momentum routing that cannot be automatically recognized by*)
(*the employed algorithm*)


FCLoopIntegralToGraph[FCTopology[TRIX1, {SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1 + Q1, 0}, {0, 1}, 1}], 
SFAD[{{p1 + p2 + Q1, 0}, {0, 1}, 1}], SFAD[{{-p1 + Q2, 0}, {0, 1}, 1}], SFAD[{{-p1 - p2 + Q2, 0}, {0, 1}, 1}]}],{p1,p2}]


(* ::Text:: *)
(*Here `FCLoopIntegralToGraph` has no way to know that the actual momentum is Q1+Q2, i.e. it is a 2- and not 3-point function*)


FCLoopIntegralToGraph[FCTopology[TRIX1, {SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1 + Q1, 0}, {0, 1}, 1}], 
SFAD[{{p1 + p2 + Q1, 0}, {0, 1}, 1}], SFAD[{{-p1 + Q2, 0}, {0, 1}, 1}], SFAD[{{-p1 - p2 + Q2, 0}, {0, 1}, 1}]}],{p1,p2},Momentum->{Q1+Q2}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*And here is another example. This NRQCD integral from arXiv:1907.08227 looks like as if it has only one external momentum flowing in*)


FCLoopIntegralToGraph[FAD[{k, m}, l + p, l - p, k + l], {k, l}]


(* ::Text:: *)
(*while in reality there are two of them: `p` and `2p`*)


FCLoopIntegralToGraph[FAD[{k, m}, l + p, l - p, k + l], {k, l},Momentum -> {2 p, p}, FCE -> True]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*In this case we the correct form of the external momentum can be deduced upon performing some elementary shifts*)


ex=FCTopology[topo1X12679,{SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}]}]


FCLoopIntegralToGraph[ex,{p1,p2,p3}]


ex/.p2->p2-p3/.p3->p3-p1+Q


(* ::Text:: *)
(*Now we immediately see that the proper external momentum to consider is `2Q` instead of just `Q`*)


FCLoopIntegralToGraph[ex/.p2->p2-p3/.p3->p3-p1+Q,{p1,p2,p3},Momentum->{2Q}]
FCLoopGraphPlot[%]


(* ::Text:: *)
(*The `Style` option can be used to label lines carrying different masses in a particular way*)


OptionValue[FCLoopGraphPlot,Style]


FCLoopIntegralToGraph[ FAD[{k2,mb},{k3},{k1-q,mc},{k1-k2,mc},{k2-k3}],{k1,k2,k3}]
Magnify[FCLoopGraphPlot[%, GraphPlot-> {MultiedgeStyle->0.35,Frame->True},Style->{{"InternalLine",_,_,mm_/;!FreeQ[mm,mg]}->{Red,Thick,Dashed},
{"InternalLine",_,_,mm_/;!FreeQ[mm,mc]}->{Blue,Thick,Dashed}}],1.5]


FCLoopIntegralToGraph[ FAD[{k2,mg},{k3,mc},{k1,q},{k1-k2},{k2-k3,mc}],{k1,k2,k3}]
Magnify[FCLoopGraphPlot[%, GraphPlot-> {MultiedgeStyle->0.35,Frame->True},Style->{{"InternalLine",_,_,mm_/;!FreeQ[mm,mg]}->{Red,Thick,Dashed},
{"InternalLine",_,_,mm_/;!FreeQ[mm,mc]}->{Blue,Thick,Dashed}}],1.5]


FCLoopIntegralToGraph[ FAD[{k2,mg},{k3,mc},{k1-q},{k2-q,mb},{k1-k2},{k2-k3,mc}],{k1,k2,k3}]
Magnify[FCLoopGraphPlot[%, GraphPlot-> {MultiedgeStyle->0.35,Frame->True},Style->{{"InternalLine",_,_,mm_/;!FreeQ[mm,mg]}->{Red,Thick,Dashed},
{"InternalLine",_,_,mm_/;!FreeQ[mm,mc]}->{Blue,Thick,Dashed}}],1.5]


FCLoopIntegralToGraph[ FAD[{k2,0,2},{k1-q},{k1-k3,mc},{k2-k3,mc}],{k1,k2,k3}]
Magnify[FCLoopGraphPlot[%, GraphPlot-> {MultiedgeStyle->0.35,Frame->True},Style->{{"InternalLine",_,_,mm_/;!FreeQ[mm,mg]}->{Red,Thick,Dashed},
{"InternalLine",_,_,mm_/;!FreeQ[mm,mc]}->{Blue,Thick,Dashed}}],1.5]


(* ::Text:: *)
(*We can style a fully massive 1-loop box in a very creative way*)


FCLoopIntegralToGraph[FAD[{p,m1},{p+q1,m2},{p+q1+q2,m3},{p+q1+q2+q3,m4}],{p}]
FCLoopGraphPlot[%, GraphPlot-> {MultiedgeStyle->0.35,Frame->True},Style->{
{"InternalLine",_,_,mm_/;!FreeQ[mm,m1]}->{Red,Thick},
{"InternalLine",_,_,mm_/;!FreeQ[mm,m2]}->{Blue,Thick},
{"InternalLine",_,_,mm_/;!FreeQ[mm,m3]}->{Green,Thick},
{"InternalLine",_,_,mm_/;!FreeQ[mm,m4]}->{Purple,Thick},
{"ExternalLine",q1}->{Brown,Thick,Dashed}
}]





(* ::Text:: *)
(*The same goes for a 2-loop box with 3 massive lines*)


FCLoopIntegralToGraph[FAD[{p1,m1},{p2,m2},{Q1+p1,m3},Q2-p1,Q1+p1+p2,Q2-p1-p2,Q2+Q3-p1-p2],{p1,p2}]
FCLoopGraphPlot[%, GraphPlot-> {MultiedgeStyle->0.35,Frame->True},Style->{
{"InternalLine",_,_,mm_/;!FreeQ[mm,m1]}->{Red,Thick},
{"InternalLine",_,_,mm_/;!FreeQ[mm,m2]}->{Blue,Thick},
{"InternalLine",_,_,mm_/;!FreeQ[mm,m3]}->{Green,Thick},
{"InternalLine",_,_,mm_/;!FreeQ[mm,m4]}->{Purple,Thick},
{"ExternalLine",q1}->{Brown,Thick,Dashed}
}]





(* ::Text:: *)
(*One can also visualize the momentum flow, where we use powers to denote the dots*)


FCLoopIntegralToGraph[FCTopology[topo1X1,{SFAD[{{p2,0},{m1^2,1},2}],SFAD[{{p1,0},{m1^2,1},2}],
SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],
SFAD[{{p1+p3,0},{0,1},1}],SFAD[{{p1+p2+p3,0},{0,1},1}]}],{p1,p2,p3}]
FCLoopGraphPlot[%,GraphPlot-> {MultiedgeStyle->0.35,Frame->True},Labeled->{
{"InternalLine",x_,pow_,_}:>x^pow,
{"ExternalLine",_}:>{}}]




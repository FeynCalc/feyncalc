(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFindTopologyMappings*)


(* ::Text:: *)
(*`FCLoopFindTopologyMappings[{topo1, topo2, ...}, {p1, p2, ...}]` finds mappings between topologies (written as `FCTopology` objects) `topo1, topo2, ...` that depend on the loop momenta `p1, p2, ...`. For each source topology the function returns a list of loop momentum shits and a `GLI` replacement rule needed to map it to the given target topology. If you need to map everything to a particular set of target topologies, you can specify them via the `PreferredTopologies` option.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCTopology](FCTopology), [GLI](GLI), [FCLoopFindTopologies](FCLoopFindTopologies).*)


(* ::Subsection:: *)
(*Examples*)


topoList={FCTopology[fctopology1,{SFAD[{{p1+Q,0}
,{0,1},1}],SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[{{
p1+p2,0},{m1^2,1},1}],SFAD[{{p2-Q,0},{m1^2,1},1}]}],FCTopology[
fctopology2,{SFAD[{{p1+p2,0},{0,1},1}],SFAD[{{p2,0},{m1^2,1},1}],SFAD[
{{p1,0},{m1^2,1},1}],SFAD[{{p1+Q,0},{m1^2,1},1}],SFAD[{{p2-Q,0},{m1^2,
1},1}]}],FCTopology[fctopology3,{SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,
0},{m1^2,1},1}],SFAD[{{p1+Q,0},{m1^2,1},1}],SFAD[{{p1+p2,0},{m1^2,1},
1}],SFAD[{{p2-Q,0},{m1^2,1},1}]}],FCTopology[fctopology4,{SFAD[{{p2,0}
,{m1^2,1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[{{p2-Q,0},{0,1},1}],SFAD[
{{p1+Q,0},{m1^2,1},1}],SFAD[{{p1+p2,0},{m1^2,1},1}]}],FCTopology[
fctopology5,{SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[
{{p1-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{m1^2,1},1}],SFAD[{{p1-p2-Q,0},{m1^2,
1},1}]}],FCTopology[fctopology6,{SFAD[{{p2,0},{0,1},1}],SFAD[{{p1+Q,
0},{0,1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[{{p1+p2,0},{m1^2,1},1}],
SFAD[{{p2-Q,0},{m1^2,1},1}]}],FCTopology[fctopology7,{SFAD[{{p2+Q,0},{
0,1},1}],SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[{{
p1-Q,0},{m1^2,1},1}]}],FCTopology[fctopology8,{SFAD[{{p2+Q,0},{0,1},1}
],SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[{{p1-Q,0},{
0,1},1}]}],FCTopology[fctopology9,{SFAD[{{p1+Q,0},{0,1},1}],SFAD[{{p1+
p2,0},{0,1},1}],SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,1},1}]}],
FCTopology[fctopology10,{SFAD[{{p1+p2,0},{0,1},1}],SFAD[{{p2,0},{m1^2,
1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[{{p1-Q,0},{m1^2,1},1}]}],
FCTopology[fctopology11,{SFAD[{{p1+p2,0},{0,1},1}],SFAD[{{p2,0},{m1^2,
1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[{{p1-Q,0},{0,1},1}]}],
FCTopology[fctopology12,{SFAD[{{p1+p2,0},{0,1},1}],SFAD[{{p2,0},{m1^2,
1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[{{p1+p2,0},{m1^2,1},1}]}],
FCTopology[fctopology13,{SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,
1},1}],SFAD[{{p2+Q,0},{m1^2,1},1}],SFAD[{{p1-Q,0},{m1^2,1},1}]}],
FCTopology[fctopology14,{SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,
1},1}],SFAD[{{p1-p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{m1^2,1},1}]}],
FCTopology[fctopology15,{SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,
1},1}],SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p2+Q,0},{m1^2,1},1}]}],
FCTopology[fctopology16,{SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,
1},1}],SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p1+p2,0},{m1^2,1},1}]}],
FCTopology[fctopology17,{SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,
1},1}],SFAD[{{p1+p2,0},{m1^2,1},1}],SFAD[{{p1-Q,0},{m1^2,1},1}]}],
FCTopology[fctopology18,{SFAD[{{p1,0},{0,1},1}],SFAD[{{p2,0},{m1^2,1},
1}],SFAD[{{p1+Q,0},{m1^2,1},1}],SFAD[{{p1+p2,0},{m1^2,1},1}]}],
FCTopology[fctopology19,{SFAD[{{p1,0},{0,1},1}],SFAD[{{p2,0},{m1^2,1},
1}],SFAD[{{p1-p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{m1^2,1},1}]}],
FCTopology[fctopology20,{SFAD[{{p1,0},{0,1},1}],SFAD[{{p2,0},{m1^2,1},
1}],SFAD[{{p1-Q,0},{m1^2,1},1}],SFAD[{{p1-p2-Q,0},{m1^2,1},1}]}],
FCTopology[fctopology21,{SFAD[{{p1,0},{0,1},1}],SFAD[{{p2,0},{m1^2,1},
1}],SFAD[{{p1+p2,0},{m1^2,1},1}],SFAD[{{p1-Q,0},{m1^2,1},1}]}],
FCTopology[fctopology22,{SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,
1},1}],SFAD[{{p1+p2+Q,0},{m1^2,1},1}]}],FCTopology[fctopology23,{SFAD[
{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[{{p1+p2+Q,0},{0,1}
,1}]}],FCTopology[fctopology24,{SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{
m1^2,1},1}],SFAD[{{p1+p2+Q,0},{0,1},1}]}],FCTopology[fctopology25,{
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[{{p1+Q,0},{m1^2,
1},1}]}],FCTopology[fctopology26,{SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},
{m1^2,1},1}],SFAD[{{p1-Q,0},{m1^2,1},1}]}],FCTopology[fctopology27,{
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{m1^2,1},1}],SFAD[{{p1-Q,0},{0,1},
1}]}],FCTopology[fctopology28,{SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,
1},1}],SFAD[{{p1-Q,0},{m1^2,1},1}]}],FCTopology[fctopology29,{SFAD[{{
p1,0},{0,1},1}],SFAD[{{p2,0},{m1^2,1},1}],SFAD[{{p1,0},{m1^2,1},1}]}]}


FCLoopFindTopologyMappings[topoList,{p1,p2}]

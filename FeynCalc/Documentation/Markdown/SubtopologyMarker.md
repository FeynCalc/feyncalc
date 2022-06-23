`SubtopologyMarker` is an option for `FCLoopFindTopologies`, `FCLoopFindTopologyMappings` and other topology related functions. It denotes the symbol that is used to specify that the given topology is a subtopology of another topology and has
been obtained by removing some of the original propagators (i.e. there are no momenta shifts involved)

This information must be put into the very last list of the FCTopology object describing the corresponding subtopology. The syntax is `marker->topoID` where `topoID` is the ID of the larger topology.

Setting `SubtopologyMarker` to `False` means that the information about subtopologies will not be added when generating subtopologies and will be ignored by routines related to topology mappings.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md),
[FCLoopFindTopologyMappings](FCLoopFindSubtopologies.md).

### Examples
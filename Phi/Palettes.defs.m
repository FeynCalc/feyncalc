(*
Definitions for the package Palettes
*)

(*
Usage
*)

LoadConfiguration::"usage" = 
    "LoadConfiguration[c] loads a configuration file c.conf. c must be a string like e.g. \
\"QED\"";

LoadLagrangian::"usage" = "LoadLagrangian[l] loads the lagrangian l. l must be a non-string \
like e.g. QED[1] (or a string like e.g. \"QED1\")";

ReloadPhiFA::"usage" = "ReloadPhi[conf] reloads Phi with configuration conf, where conf must \
be given as a string, like e.g. \"QED\", and then reloads FeynArts";

RebuildConfigurationsPalette::"usage" = "RebuildConfigurationsPalette rebuilds, saves and \
(re)opens the configurations palette";

RebuildLagrangiansPalette::"usage" = "RebuildLagrangiansPalette rebuilds, saves and \
(re)opens the lagrangians palette";

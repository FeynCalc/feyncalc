# Master integrals

For the sake of the users that try to employ FeynCalc in calculations
beyond 1-loop but do not belong to the multiloop community, let us summarize
some basic facts about master integrals in multiloop calculations

## Analytic results

- Even at 2-loop there is no library containing analytic results for all
master integrals with arbitrary mass distributions. That is, there is simply
nothing similar to Package-X beyond 1-loop

- The available libraries usually focus on very specific integral families,
e.g. [Mincer](https://www.nikhef.nl/~form/maindir/packages/mincer/mincer.html) (3-loop massless 2-point functions), 
[Forcer](https://github.com/benruijl/forcer) (4-loop massless 2-point functions), [MATAD](https://www.ttp.kit.edu/~ms/software.html), [MATAD-ng](https://github.com/apik/matad-ng) (massive 3-loop tadpoles),
[FMFT](https://github.com/apik/fmft) (massive 4-loop tadpoles), [ON-SHELL2] (http://theor.jinr.ru/~kalmykov/onshell2/onshell2.html)
(on-shell 2-point functions with one mass scale)

- The main source for analytic results are scientific publications. When people calculate
new master integrals needed for their research, they often provide explicit analytic results in
the paper itself or put them into ancillary files accompanying the preprint. Unfortunately,
there is no compendium of all calculated integrals that would tell you where to find the 
corresponding expression. The [Loopedia](https://arxiv.org/abs/1709.01266) project is an
attempt to create something like a search engine for loop integrals, but its database
is still far from being comprehensive.

- Some relevant publications for 2-loop 2-point functions include (this list is far from being complete)
 [arXiv:hep-ph/9907431](https://arxiv.org/abs/hep-ph/9907431), [arXiv:hep-ph/0202123](https://arxiv.org/abs/hep-ph/0202123v2), [hep-ph/0307101](https://arxiv.org/abs/hep-ph/0307101v1)
 
## Numerical results

- Numerical results are much simpler to obtain and universal libraries
that can calculate almost any integral (given enough time and computing ressources)
are publicly available. Two prominent examples are [pySecDec](https://secdec.readthedocs.io/en/stable/) and [FIESTA](https://bitbucket.org/feynmanIntegrals/fiesta/src/master/)
- Apart from that, there are also libraries that cover specific integral families
and may offer better numerical stability due to the corresponding optimizations. Two
very useful tools are [TVID2](https://sites.pitt.edu/~afreitas/) for the evaluation of
3-loop 2-point functions with arbitrary masses and [3VIL](https://www.niu.edu/spmartin/3VIL/)
for the calculation of 3-loop tadpoles with arbitrary masses.

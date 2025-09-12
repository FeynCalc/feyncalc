## Dirac traces in 4 dimensions and Schouten identity

### See also

[Overview](FeynCalc.md).

Dirac traces computer in 4 dimensions are not unique. Depending on the way how such
traces are computerd, one may end up with expressions of different length. The reason
for this is the so-called Schouten identity.

The underlying issue comes from the fact that a totally antisymmetric tensor with 5 indices 
must identically vanish in 4-dimensional Minkowski space, where each index runs only from 0 to 3
This is an obvious statement, since with this setup you will always have two indices with the
same value between 0 and 3 and so the tensor will vanish by symmetry.

We can define such a 5-index totally antisymmetric tensor as

$$
\begin{align}
T^{\mu\nu\rho\sigma\tau} = \varepsilon^{\mu\nu\rho\sigma} p^\tau +
\varepsilon^{\nu \rho \sigma \tau} p^\mu +
\varepsilon^{\rho \sigma \tau \mu} p^\nu +
\varepsilon^{\sigma \tau \mu \nu} p^\rho +
\varepsilon^{\tau \mu \nu \rho} p^\sigma,
\end{align}
$$

where $p$ is just some arbitrary 4-vector.

It is easy to check that $T^{\mu\nu\rho\sigma\tau}$ is indeed totally antisymmetric by exchanging any of the two neighboring indices and adding that to the original expression, e.g.

$$
\begin{align}
T^{\mu\nu\rho\sigma\tau} + T^{\nu\mu\rho\sigma\tau} = 0
\end{align}
$$

According to the argument above the necessary vanishing of a 5-index totally antisymmetric tensor in the 4-dimensional spacetime, we must conclude that

$$
\begin{align}
\varepsilon^{\mu\nu\rho\sigma} p^\tau +
\varepsilon^{\nu \rho \sigma \tau} p^\mu +
\varepsilon^{\rho \sigma \tau \mu} p^\nu +
\varepsilon^{\sigma \tau \mu \nu} p^\rho +
\varepsilon^{\tau \mu \nu \rho} p^\sigma = 0
\end{align}
$$

This relation is known under the name of Schouten identity. We can also write it in a somewhat different way by noticing that

$$
\begin{align}
p^\alpha = g^{\alpha \beta} p_{\beta}
\end{align}
$$

which allows us to eliminate the arbitrary vector $p$. This yields

$$
\begin{align}
\varepsilon^{\mu \nu \rho \sigma} g^{\kappa \tau} +
\varepsilon^{\nu \rho \sigma \tau} g^{\kappa \mu} +
\varepsilon^{\rho \sigma \tau \mu} g^{\kappa \nu} +
\varepsilon^{\sigma \tau \mu \nu} g^{\kappa \rho} +
\varepsilon^{\tau \mu \nu \rho} g^{\kappa \sigma} = 0
\end{align}
$$

The above demonstrates a nontrivial linear combination of Levi-Civita and metric tensors that add up to zero. Furthermore, if all indices are contracted with other tensors or vectors, then recognizing a Schouten identity in a large expression with many terms becomes a formidable task. For example, consider multiplying Schouten identity with $\varepsilon^{\alpha \beta \gamma \delta}$. Using the formula for products of two Levi-Civita tensors, the amount of terms on the l.h.s. will increase to 120, still giving zero when added together.

In practice, not only Dirac traces but any kind of tensor calculations involving multiple products of Levi-Civita and metric tensors will sooner or later contain spurious zeros induced by the Schouten identity. There is no known systematic way to eliminate such redundancies.

The same issue arises when comparing two expressions that are expected to be the same but happen to look very different from each other and even contain a different number of terms. In many cases the equality can be demonstrated by applying Schouten identity multiple times in a particular way.

It is somewhat unfortunate that Schouten identity is never mentioned in most popular QFT books
and lectures apart from its variety used in the spinor-helicity formalism. Hence, there are  many particle physicists who have never heard of it. The FORM manual, however, [does mention it](https://www.nikhef.nl/~form/maindir/documentation/trace4).


FeynCalc has a function called [FCSchoutenBruteForce](..//FCSchoutenBruteForce.md) that tries to automatize the application of Schouten identity in a brute force by trying all possible combinations and checking which of them lead to the largest decrease in the number of terms. However, it there is absolutely no warranty that it can simplify all relevant cases. Often the brute force procedure gets stuck in the sense, that there is no single replacement possible that would decrease the number of terms. Surprisingly, in such cases it often helps to make a replacement that would actually make the given expression larger. But then one is out of the pit and the next replacement is most likely to be a good one, i.e. making the expression much shorter.

Another possibility to show the equality of two expression that differ by Schouten is to manifestly break the Lorentz invariance by rewriting everything in terms of single components.









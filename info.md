---
title: "Law of the Iterated Logarithm"
author: "LUQIN GAN"
date: "11/27/2017"
output: html_document
---


The APP aims to demonstrate the Law of the Iterated Logarithm. Users can select Normal, Bernoulli, Poisson distributions and set up corresponding parameters for random variable generation. 

Let $X_1,X_2$,.. be iid rv's with mean 0 and std 1. Let $S_n = X_1+..+X_n$. By the strong law of large numers we have $S_n/{n}\to 0$ almost surely,

and by the central limit theorem we have $S_n/\sqrt{n}$ converges in distribution to a standard normal random variable. 

So in some sense $S_n/n$ "squeezes" down to one
point whereas $S_n/\sqrt{n}$ "spreads out", roughly between -3 and 3, but a continous distirbution. 

It is a reasonable question then whether there is an in-between case, namely a sequence ${a_n}$ such that $\sqrt{n}$<$a_n$<$n$ converges to something between a constant and a distribution. The answer is given by

Theorem (Law of the Iterated Logarithm, Kolmogorov 1929)

$$
\limsup_{n\to{\infty}}\frac{S_n}{\sqrt{n\log{\log{n}}}} = \sqrt{2}~~~ \text{a.s.}
$$

by symmetry the corresponding liminf is $-\sqrt{2}$, so this sequence oscillates between ±$\sqrt{2}$. 

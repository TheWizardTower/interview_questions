# haskell

This is my solution to a hacker rank challenge in the Functional Programming section:

https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv/problem?h_r=profile

This took me a fair bit to get working, and I kinda juked the solution. Instead of taking successive numerical approximations, I just solved the integral algebraically. This was fairly trivial for the area under the curve. However, for the volume around the axis component, this required squaring the set of terms with itself.

However, both terms give the correct answer, within floating point rounding error, so I'm calling it a win. :)

I borrowed/translated the numeric solution from https://alexatnet.com/hr-f-area-under-curves-and-volume-of-revolving-a-curve/ . It took a fair bit to intuit how his approach worked, but I like some parts of his solution.

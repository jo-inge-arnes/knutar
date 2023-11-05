# Knutar
Knutar is an R-package for suggesting knot placements for restricted cubic spines, aiming for regression models with a good fit for low numbers of knots, presented in the paper Greedy Knot Selection Algorithm for Restricted Cubic Spline Regression.

It is not yet published at CRAN but can be built locally.

Build with:
```
./build.R
./build.sh
```

Note that some user may have to use ```sudo ./build.R```.

## Update 4th of November, 2023

Because of a breaking change between R version 4.2 and 4.3 regarding how ns() and bs()
works, the knutar-code for ```suggest_splines``` has been updated to reflect
these changes.

Information about the 4.3.0 changes can be found at [R 4.3.0 is released](https://stat.ethz.ch/pipermail/r-announce/2023/000691.html), where the change is described as:

    * bs() and ns() in the (typical) case of automatic knot construction, when some of the supposedly inner knots coincide with boundary knots, now moves them inside (with a warning), building on PR#18442 by Ben Bolker.

The PR that the change was based on can be found at
[Bug 18442 - ns() fails when quantiles end up on the boundary](https://bugs.r-project.org/show_bug.cgi?id=18442)

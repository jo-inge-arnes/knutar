# Knutar
Knutar is an R-package for suggesting knot placements for restricted cubic spines, aiming for regression models with a good fit for low numbers of knots, presented in the paper Greedy Knot Selection Algorithm for Restricted Cubic Spline Regression. 

The package has not yet been published at CRAN but can be built locally. A prebuilt version is also included in the repository.

## Pre-built version

[prebuilt/knutar_0.0.0.9098.tar.gz](https://github.com/jo-inge-arnes/knutar/raw/main/prebuilt/knutar_0.0.0.9098.tar.gz)

## Building

On Linux or WSL on Windows, you can build by running:

```
./build.R
./build.sh
```
Note that some users may have to use ```sudo ./build.R```.

## Installing
The built package will be placed in the parent directory of your locally cloned repository. After building the package, you can install it via the _Packages_ tab in RStudio by choosing "Install from: Package Archive File (.zip; .tar.gz)". 

## R Versions
The package has been tested on R version 4.2.2 and 4.3.2.

## Update 4th of November, 2023

Because of a breaking change between R version 4.2 and 4.3 regarding how ns() and bs()
works, the code for ```suggest_splines``` has been updated to reflect
these changes.

Information about the 4.3.0 changes can be found at [R 4.3.0 is released](https://stat.ethz.ch/pipermail/r-announce/2023/000691.html), where the change is described as:

    * bs() and ns() in the (typical) case of automatic knot construction, when some of the supposedly inner knots coincide with boundary knots, now moves them inside (with a warning), building on PR#18442 by Ben Bolker.

The PR that the change was based on can be found at
[Bug 18442 - ns() fails when quantiles end up on the boundary](https://bugs.r-project.org/show_bug.cgi?id=18442)

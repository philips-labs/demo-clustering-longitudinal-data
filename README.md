This repository contains the R scripts used in the analysis of the case study of the manuscript.

# Useful links
* MixTVEM source code used in the MixTVEM demo analysis - https://github.com/dziakj1/MixTVEM
* _lcmm_ R package, used for estimating GMM and GBTM - https://cran.r-project.org/package=lcmm
* _kml_ R package, used for estimating KmL - https://cran.r-project.org/package=kml
* _mclust_ R package, used for estimating LLPA - https://cran.r-project.org/package=mclust
* _latrend_ R package: The longitudinal clustering framework that we have created, originating from the learnings of this work - https://github.com/philips-software/latrend

# Getting started
1. Either load the Rstudio project file `demo.Rproj`, or start an R session with the working directory set to the root repository directory.
2. Install required packages and dependencies
```
install.packages(c("assertthat", "data.table", "ggdendro", "ggplot2", "IMIFA", "kml", "lcmm", "lpSolve", "magrittr", "MASS", "matrixStats", "mclust", "nlme", "scales"), dependencies = TRUE)
```
3. In case you want to run the MixTVEM analysis, you'll need to fetch `MixTVEM.R` from https://github.com/dziakj1/MixTVEM

You should now be able to run any of the analysis scripts.

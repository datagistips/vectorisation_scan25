# # VERIFIER LES LIBRAIRIES INSTALLEES
# packages = c(("RStoolbox"),
#              ("raster"),
#              ("rgdal"),
#              ("FNN"),
#              ("rgeos"),
#              ("spgrass6"),
#              ("maptools"),
#              ("gdalUtils"),
#              ("scatterplot3d"),
#              ("igraph"),
#              ("sp"),
#              ("R.utils"))

# for (package in packages) {
#   print(package)
#   c = find.package(package)
#   print(c)
# }

# for (package in packages) {
#   
#   if (!(package %in% rownames(installed.packages()))) {
#      install.packages(package)
#   }
# }

# LANCER LES LIBRAIRIES
suppressMessages(suppressWarnings(require("RStoolbox", quietly = TRUE)))
suppressMessages(suppressWarnings(require("raster", quietly = TRUE)))
suppressMessages(suppressWarnings(require("R.oo", quietly = TRUE)))
suppressMessages(suppressWarnings(require("R.methodsS3", quietly = TRUE)))
suppressMessages(suppressWarnings(require("R.utils", quietly = TRUE)))
suppressMessages(suppressWarnings(require("igraph", quietly = TRUE)))
suppressMessages(suppressWarnings(require("rgdal", quietly = TRUE)))
suppressMessages(suppressWarnings(require("FNN", quietly = TRUE)))
suppressMessages(suppressWarnings(require("rgeos", quietly = TRUE)))
suppressMessages(suppressWarnings(require("spgrass6", quietly = TRUE)))
suppressMessages(suppressWarnings(require("maptools", quietly = TRUE)))
suppressMessages(suppressWarnings(require("gdalUtils", quietly = TRUE)))
suppressMessages(suppressWarnings(require("scatterplot3d", quietly = TRUE)))
## FILES & COLORS ##
# SCAN 25
ecw = file.path(getwd(), ini$scan25)
if(!file.exists(ecw)) print("ERREUR : raster non trouvé"); error=TRUE

if(!file.exists(ini$cover)) {
  print("ERREUR : couche de couverture non trouvée")
  error=TRUE
} else {
#   expr = "(.*)/(.*).shp$"
#   bvs = readOGR(sub(expr, "\\1",ini$cover), sub(expr, "\\2",ini$cover))
  expr = "(.*)[/|\\](.*).shp$"
  bvs = crop(readOGR(sub(expr, "\\1",ini$cover), sub(expr, "\\2",ini$cover), verbose=FALSE), 
             getExtentFromFile(ecw)) # on découpe la couche de couverture par l'étendue du scan25  
}
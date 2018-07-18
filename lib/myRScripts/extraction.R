#########################################################################################
## Copyright (C) DREAL PACA > mathieu.rajerison@developpement-durable.gouv.fr         ##
## Licence Creative Commons BY NC SA                                                  ##
#########################################################################################

setwd(commandArgs(trailingOnly=TRUE)[1])

# setwd("D:/WKSP/151019_SBEP_JGL_TELETEDECTION_SCAN25/LOT_DISTRIBUTION")
# .Library = file.path(getwd(), "lib/R/R-3.1.2/library/")

cat("CHARGEMENT DE L'ENVIRONNEMENT\n")

source("lib//myRScripts/installAndLoadPackages.R") # on installe les paquets manquants
source("lib//myRScripts/initEnvironment.R") # on initialise l'environnement de travail
source("lib//myRScripts/generic.R") # fonctions spatiales génériques
source("lib//myRScripts/specific.R") # fonctions spatiales spécifiques au script d'extraction
source("lib/myRScripts/grassStuff.R")
source("lib//myRScripts/loadFiles.R") # chargement des fichiers couverture et scan25
source("lib//myRScripts/loadColors.R") # chargement des fichiers couverture et scan25

memLimit = memory.limit(size=as.numeric(ini$memorylimit)) # on augmente la taille mémoire allouée à R car cela va demander de la perf'

cat("\n")
for (idbv in 1:nrow(bvs)) {
  
  cat(paste0(">> OBJET DE COUVERTURE : ", idbv, " / ", nrow(bvs), "\n"))
  output = file.path(ini$outputfolder, paste(idbv, "shp", sep="."))
  
  if (!file.exists(output)) {
    
    bv = bvs[idbv, ]; writeOGR(bv, ini$tmpfolder, "cover", "ESRI Shapefile", overwrite=T) # on exporte l'objet de couverture
    
    cat(" DECOUPE\n")
    cutlineRaster(inputRasterPath = ecw,
                  outputRasterPath = file.path(ini$tmpfolder, "dalleCropped.tif"),
                  cutlineObjectFile = file.path(ini$tmpfolder, "cover.shp")) # on découpe le raster par l'objet de couverture
    
    dalle = raster::stack(file.path(ini$tmpfolder, "dalleCropped.tif")) # on lit la dalle
    
    cat(" CLASSIFICATION SUPERVISEE\n")
    classified <- RStoolbox::sam(dalle, endmembers, angles = T) # on  la classe
    
    gc(verbose=FALSE) # on libère de la mémoire
    
    cat(" UNIFICATION DES RESULTATS\n")
    covered = coverRasterStack(classified, threshs) # on combine tous les rasters classifiés
    r = covered$raster
    
    cat(" SUPPRESSION DES GROS AMAS (LACS, ETENDUES D'EAU)\n")
    noBigZones = removeBigPatches(r, nPixels=as.numeric(ini$bigpatch))
    
    cat(" SUPPRESSION DES PIXELS NOIRS\n")
    noBlack = removeBlack(noBigZones, dalle, as.numeric(ini$blackthresh)) # on peut avoir des pixels noirs compris dans la classif'
    
    cat(" LISSAGE\n")
    smoothed = smooth(noBlack, as.numeric(ini$ncells)) # on lisse le tout afin de connecter ce qui ne l'est pas et affiner les lignes
    
    ## GRASS STUFF
    cat(" VECTORISATION\n")
    l.detected = grassStuff(smoothed, layerName="smoothed") # du machin avec GRASS pour vectoriser du raster
    
    # SIMPLIFICATION
    l.simpl = gSimplify(l.detected , tol=as.numeric(ini$douglaspeucker)) # simplification de la ligne par algo Douglas-Peucker
    
    l.simpl = SpatialLinesDataFrame(l.simpl, 
                                    data=data.frame(id=1:length(l.simpl),
                                                    type="détection",
                                                    row.names=row.names(l.simpl))) # on rajoute des attributs élémentaires
    
    fun = function() {proj4string(l.simpl)=CRS("+init=EPSG:2154")} # on attribue le bon CRS RGF 93
    try(fun(), silent=TRUE) # make the command silent
    
    cat(" SNAPPING (ACCROCHAGE)\n")
    radarPars = sapply(strsplit(ini$radarhead, ",")[[1]], function(x) as.numeric(strsplit(x, ":")[[1]])) # on lit les paramètres de la tête chercheuse
    conns = directionalConnect(l.simpl, d=radarPars[1,], dang=radarPars[2,], tol=as.numeric(ini$angtol))$connections
    conns$type = "liaison"
    
    # MERGE DETECTED AND CONNECTIONS
    l.all = mergeTwoLayers(l.simpl, conns) # on fusionne les segments détectés et les liaisons
    l.all$idCov=idbv
    
    # PARAMETRES DE DETECTION
    l.all$parameters = paste(apply(cbind(apply(endmembers, 1, paste, collapse=","), threshs), 1, paste, collapse=":"), collapse="|") # on concatène tous les paramètres qui ont servi à la classification
    
    cat(" EXPORT\n")
    writeOGR(l.all, ini$outputfolder, as.character(idbv), "ESRI Shapefile", overwrite_layer=TRUE) # on exporte la donnée
    cat("OK\n")
    
    ## CLEAR ENVIRONMENT
    rm(list = c("dalle", "classified", "r", "noBlack", "smoothed", "thin", "l.simpl", "conns", "l.all", "noBigZones")) # on supprime les objets de l'environnement de travail afin de repartir de zéro.
    
  } else {
    cat(" deja calcule\n")
  }
  cat("\n") # retour à la ligne
  
}


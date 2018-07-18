grassStuff = function(r, layerName="smoothed") {
  
  #   set.suppressEchoCmdInFuncOption(TRUE)
  set.ignore.stderrOption(TRUE) # on rend le processus silencieux
  
  # INTEGRATE
  writeRAST(as(r, "SpatialGridDataFrame"), 
            layerName, 
            zcol="layer", 
            flags=c("overwrite"))
  
  execGRASS("g.region", rast=layerName)
  
  # NAMES
  LayerName.thin = paste(layerName, "thin", sep="_")
  layerName.clean = paste(layerName, "clean", sep="_")
  
  # PROCESS
  execGRASS("r.thin", parameters=list(input=layerName, 
                                      output=LayerName.thin, 
                                      iterations=as.numeric(ini$thinningiterations)), 
            flags=c("overwrite"))
  
  execGRASS("r.to.vect", parameters=list(input=LayerName.thin, 
                                         output=LayerName.thin, 
                                         feature="line"), 
            flags=c("overwrite"))
  
  execGRASS("v.clean", parameters=list(input=LayerName.thin, 
                                       tool="rmdangle", 
                                       output=layerName, 
                                       thresh=as.numeric(ini$danglethresh)), 
            flags=c("overwrite"))
  
  ## SIMPLIFY
  unlink(file.path(gmeta()$GISDBASE, gmeta()$LOCATION, gmeta()$MAPSET, ".tmp", paste0(layerName, ".*"))) # car GRASS peut ne pas être en mesure de supprimer le fichier temporaire lui-même lors de l'appel de la fonction readVECT
  
  return(readVECT(layerName))
  
}
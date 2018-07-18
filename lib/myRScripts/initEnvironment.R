# RASTER OPTIONS
rasterOptions(datatype = "FLT4S", 
              progress = "text", 
              tmpdir = file.path(getwd(), "tmp"), 
              tmptime = 3, 
              timer = TRUE,
              tolerance = 0.5,
              chunksize = 1e+05,
              maxmemory = 1e+09)

# READ INI FILE
f.ini = read.table(file.path(getwd(), "config.ini"), sep="=", stringsAsFactors=FALSE)
ini = list()
for (i in 1:nrow(f.ini)) ini[tolower(f.ini$V1[i])] = trim(f.ini$V2[i],1)

# GRASS INI
if(!(file.exists(ini$gisbase)) | !(file.exists(ini$gisdbase))) {
  cat(" ERREUR : les chemins vers GRASS sont mal renseignés\n"); error=TRUE
} else {
  initGRASS(gisBase=ini$gisbase,
            gisDbase=ini$gisdbase,
            location=ini$location,
            mapset=ini$mapset,
            home=tempdir(),
            override=TRUE)  
}

if(!(file.exists(ini$fwtools))) {
  cat(" ERREUR : le chemin vers GDAL est mal renseigné\n"); error=TRUE  
} else {
gdal_setInstallation(search_path=ini$fwtools,
                     rescan=T,
                     verbose=FALSE)
}

# CREATE OUTPUT DIRECTORY
if (!(file.exists(ini$outputfolder))){
  cat(paste(" INFO : création du dossier d'export", ini$outputfolder,"\n"))
  dir.create(ini$outputfolder, rec=TRUE)
}

# ADVANCED INI
advanced.ini = read.table(file.path(getwd(), "advanced.ini"), sep="=", stringsAsFactors=FALSE)
for (i in 1:nrow(f.ini)) ini[tolower(advanced.ini$V1[i])] = trim(advanced.ini$V2[i],1)
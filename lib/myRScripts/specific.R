#########################################################################################
## Copyright (C) DREAL PACA > mathieu.rajerison@developpement-durable.gouv.fr         ##
## Licence Creative Commons BY NC SA                                                  ##
#########################################################################################

cutlineRaster = function(inputRasterPath, outputRasterPath, cutlineObjectFile) {
  
  # DECOUPAGE SELON EXTENT
  bb = bbox(readOGR(sub("^(.*)/.*.shp$", "\\1", cutlineObjectFile), 
                    sub("^.*/(.*).shp$", "\\1", cutlineObjectFile), verbose=FALSE))
  
  
  gdal_translate(inputRasterPath, 
                 gsub(".tif", "_temp.tif", outputRasterPath),
                 of="GTiff",
                 projwin=paste(bb[1,1], bb[2,2], 
                               bb[1,2], bb[2,1]),
                 verbose=FALSE)
  
  # DECOUPAGE SELON LA FORME
  unlink(outputRasterPath) # on supprime le raster s'il existe déjà. 
  
  gdalwarp(srcfile = gsub(".tif", "_temp.tif", outputRasterPath), 
           dstfile = outputRasterPath, 
           cutline = cutlineObjectFile, 
           cl=sub("^.*/(.*).shp$", "\\1", cutlineObjectFile),
           srcnodata=0,
           dstnodata=0)  
  
}

removeBigPatches = function(r, nPixels) {
  cl = clump(r)
  frq = as.data.frame(freq(cl))
  cl[cl %in% frq$value[frq$count > nPixels]] = NA     
  return(cl)
}

coverRasterStack = function(s, threshs) {
  out = lapply(1:nlayers(s), function(i) clamp(s[[i]], upper = threshs[[i]], useValues = FALSE))
  
  r = out[[1]]  
  for(i in 2:length(out)) {
    r = cover(out[[i]], r)
  }    
  
  return(list(raster=r, list=out))
}

deleteIsolatedPixels = function(r, width, n) {
  
  cl = clump(r)
  # plot(noParasite, col="red")
  frq = as.data.frame(freq(cl))
  alone.r = cl
  alone.r[alone.r %in% frq$value[frq$count != 1]] = NA
  alone.pts = SpatialPoints(rasterToPoints(alone.r, fun=function(x){!is.na(x)}))
  alone.buf = gBuffer(alone.pts, width=width, byid=T)
  pts = SpatialPoints(rasterToPoints(r, fun=function(x){!is.na(x)}))
  # plot(pts, add=T)
  nCount = sapply(over(alone.buf, pts, returnList = TRUE), length)
  toDelete = coordinates(alone.pts[which(nCount <= (n+1))])
  # plot(SpatialPoints(toDelete), pch=16, col="red", add=T)
  r[cellFromXY(r, toDelete)] = NA
  
  return(r)
}

getExtentFromFile = function(ecw) {
  
  info = gdalinfo(ecw)
  info = info[grep("Upper Left|Lower Right", info)]
  
  myExtent = matrix(nrow=2, ncol=2)
  
  expr = "^Upper Left  [(]\\s*(.*),\\s*(.*)[)] .*$"
  myExtent[1,1] = as.numeric(sub(expr, "\\1", info[1]))
  myExtent[2,2] = as.numeric(sub(expr, "\\2", info[1]))
  
  expr = "^Lower Right [(]\\s*(.*),\\s*(.*)[)] .*$"
  myExtent[1,2] = as.numeric(sub(expr, "\\1", info[2]))
  myExtent[2,1] = as.numeric(sub(expr, "\\2", info[2]))
  
  return(myExtent)
  
}

radarHead = function(start, ang, d, dang, ID="radar") { # TETE CHERCHEUSE
  
  angs = seq(ang+pi-dang, ang+pi+dang, length.out=10) # on calcule les anglesde recherche
  pts = cbind((start[1] + d*cos(angs)), (start[2] + d*sin(angs))) # points qui constitueront la tête chercheuse     
  SpP = SpatialPolygons(list(Polygons(list(Polygon(rbind(start, pts, start))), ID))) 
  
  return(SpP)
  
}

# vérifier au niveau de boucles
# vérifier que pas d'intersection de ligne existante, même si connexion
directionalConnect = function(f, d=c(100,200), dang=c(0.5,0.1), tol=pi/4) { # ajouter les têtes chercheuses en résultat
  
  # print("construction du réseau")  # CONSTRUCTION DU RESEAU
  g = construireReseau(mergeThenSplitLine(f) ) # on construit un réseau de manière à déterminer, plus tard, tous les noeuds feuille
  
  # print("création des connexions") # REALISATION DES CONNEXIONS
  vs = V(g)[which(degree(g)==1)] # les noeuds de départ seront les noeuds feuille qui ne sont connectés qu'à un seul point
  
  # print("création des points") # CREATION DES POINTS  
  pts.g = SpatialPoints(cbind(vs$x, vs$y))
  
  out=list()
  targeted = NULL # contiendra la liste des ids de points déjà regardés
  for (i in 1:length(vs)) {
#   for (i in 1:4) {
    
    if((i %in% targeted)) {
      #print(paste("noeud", i, "déjà ciblé"))
    } else {# on ne lance le calcul que si le point de départ n'est pas déjà un point qui a été ciblé.
    
      #print(paste("noeud", i, "sur", length(vs)))
      if (i %% round(length(vs)/10) ==0) cat(paste0(round(i/length(vs),2)*100, "%...")) # on affiche l'avancement
      
      startNode = vs[[i]] # point qui nous intéresse
      e = E(g)[from(as.numeric(startNode))] # on récupère tous les arcs qui en partent
      endNodes = V(g)[ends(g, e, names=F)] # noeuds formant tous ces arcs, dont le noeud de départ qu'il faudra enlever
      endNode = endNodes[-which(startNode == endNodes)] # on récupère seulement le noeud de fin
      
      startSource = c(startNode$x, startNode$y) ; endSource = c(endNode$x, endNode$y) # coordonnées du début et de la fin du segment 
      ang = atan2((endSource[2]-startSource[2]) , (endSource[1]-startSource[1])) # angle du segment incriminé
      SpP = do.call(rbind, lapply(1:length(d), function(i) radarHead(startSource, ang, d[i], dang[i], i))); #plot(SpP)
      
      # SEARCH FOR INTERSECTIONS
      pts.subset = pts.g[which(!(1:length(pts.g)) %in% c(i, targeted))] # on filtre les points de la manière à ne pas travailler sur des points déjà ciblés. On enlève aussi le point de départ de cette liste
      w = which(!is.na(over(pts.subset, SpP))) # on récupère les points qui sont dans la zone
      
      # PLOTTING
#       plot(f, lwd=4); plot(SpP, add=T, col=rgb(1,0,0,0.2)); plot(pts.req, add=T, pch=21, col="red"); title(i)
    
      if (length(w) > 0) { #  on ne réalise le calcul seulement si un point se situe dans la zone de la tête chercheuse
#         print("ça touche")
        pts.touched = pts.subset[w] # il peut donc y avoir plusieurs points qui sont dans la zone de recherche
      
        # ANGLES DES CIBLES
        diffs = sapply(1:length(pts.touched), function(x) {
          
          w = which(V(g)$name == paste(pts.touched[x]@coords, collapse=" "))
          vs = V(g)[as.numeric(get.edges(g, E(g)[from(w)]))]
          iSource = which(paste(coordinates(pts.touched[x]), collapse=" ") == paste(vs$x, vs$y))
          iTarget = setdiff(c(1,2), iSource)
          # ANGLE
          startTarget = c(vs[iSource]$x, vs[iSource]$y) ; endTarget = c(vs[iTarget]$x, vs[iTarget]$y)
          angTarget = atan2((endTarget[2]-startTarget[2]) , (endTarget[1]-startTarget[1]))
          # print(angTarget)
          # DIFFERENCE BETWEEN ANGLES
          # http://stackoverflow.com/questions/1878907/the-smallest-difference-between-2-angles
          angN = ang + pi
          diffAng = abs(atan2(sin(angTarget-angN), cos(angTarget-angN)))
          
          return(diffAng)
          
        })
        #print(paste("différences d'angles : ", diffs))
        w = which(diffs < tol)
      
        if (length(w) > 0) { # l'angle d'attaque est tolérable
#           print("l'angle est tolérable")
          pts.goodAngle = pts.touched[w]
          
          dists = sapply(1:length(pts.goodAngle), function(x) { # on calcule les distances vers chacun des points
            coords = pts.goodAngle@coords[x, ]
            dx = coords[1] - startNode$x ; dy = coords[2] - startNode$y
            dist = sqrt(dx^2 + dy^2)
            return(dist)
          })
          
          pt.closest = pts.goodAngle[which.min(dists)] # on prend le point le plus proche  
          
          # CONNEXION
          # CREATION DE LA LIGNE
          l1 = rbind(c(startNode$x, startNode$y), c(pt.closest@coords[1], pt.closest@coords[2])) 
          Sl1 = Line(l1)
          S1 = Lines(list(Sl1), ID=paste("connection", i))
          Sl = SpatialLines(list(S1))

          if(length(which(gIntersects(Sl, f, byid=T))) <= 2) { # on vérifie que ça ne croise pas une autre ligne
            #print(paste("> snapping : la connexion fait ", round(gLength(Sl),2), "m", sep=""))
            
            OneRadarHead = SpP
            out[[i]] = Sl
            
            iTarget = which(vs$name == paste(pt.closest@coords, collapse=" ")) # ON RECUPERE L'ID DU VERTEX CORRESPONDANT AU POINT CIBLE SELECTIONNE
            targeted = c(i, iTarget, targeted) # on ajoute le point de départ ainsi que le point ciblé aux autres points ciblés
          } # IF touche une autre ligne
        } # IF
      } # IF
    } # IF
  } # FOR
  
  if(length(out) > 0) {
    w = which(!unlist(lapply(out, is.null)))  #   print(w)
    res = do.call(rbind, out[w])
    res = SpatialLinesDataFrame(res, data=data.frame(id=1:length(res), row.names=row.names(res))) 
  } else { res=NULL}
  
  cat("\n") # retour à la ligne pour l'affichage
  
  # plot(f); plot(res[1:10, ], add=T, col="red")
  
  return(list(connections = res, radar=OneRadarHead))
  
}

qgisExtent = function(ext) {
  
  ext.res = list()
  
  ext.res$xrange = as.numeric(c(strsplit(trim(strsplit(ext, ":")[[1]][[1]]), ",")[[1]][[1]], 
                                strsplit(trim(strsplit(ext, ":")[[1]][[2]]), ",")[[1]][[1]]))
  ext.res$yrange = as.numeric(c(strsplit(trim(strsplit(ext, ":")[[1]][[1]]), ",")[[1]][[2]], 
                                strsplit(trim(strsplit(ext, ":")[[1]][[2]]), ",")[[1]][[2]]))  
  ext.res$extent = c(ext.res$xrange, ext.res$yrange)
  
  return(ext.res)
}

plotColors = function(endmembers) {
  
  s3d <- scatterplot3d(endmembers[,1],endmembers[,2],endmembers[,3], 
                       xlim=c(0,255),  ylim=c(0,255), zlim=c(0,255),
                       xlab="R", ylab="G", zlab="B",
                       pch=15,
                       cex.symbols=5,
                       color=rgb(endmembers[,1]/255, endmembers[,2]/255, endmembers[,3]/255))  
  
  text(s3d$xyz.convert(endmembers[,1],endmembers[,2],endmembers[,3]), labels=1:nrow(endmembers))
  
}

extractTrain = function(s, train) {
  
  endmembers = raster::extract(s, train)           
  rownames(endmembers) = train$classe    
  colnames(endmembers) = c("R", "G", "B")
  
  plotColors(endmembers)
  
  return(endmembers)
}

sieve = function(r, thresh) {
  cl = clump(r)
  frq = as.data.frame(freq(cl))
  cl[cl %in% frq$value[frq$count <= thresh]] = NA  
  bin = cl
  bin[!is.na(cl)] = 1
  return(bin)
}

removeColors_OLD = function(r, mat, s, thresh) {
  
  cl = clump(r)
  pts = rasterToPoints(cl)
  e = raster::extract(s, pts[, 1:2])
  agg = aggregate(e, list(clump=pts[,3]), mean)
  dist = get.knnx(mat, agg[, c(2:4)], 1)$nn.dist
  w = which(dist < thresh)
  excludeID = agg[,1][w]
  cl[cl %in% excludeID] = NA
  bin = cl
  bin[!is.na(cl)] = 1
  
  return(bin)
}

removeBlack = function(r, dalle, thresh) {
  
  noblack = clamp(mean(dalle), lower=thresh, useValues=FALSE); summary(noblack)
  ok = mask(r, noblack)
  
  return(ok)
  
}

creerGrille = function(pol, res=5000) {
  r = raster(extent(pol))
  res(r) = res
  grilles = rasterToPolygons(r)
  grilles = grilles[which(gIntersects(grilles, pol, byid=T)), ]
  grilles$id = 1:nrow(grilles)
  return(grilles)
}

smooth = function(r, n=3) {
  
  smoothed = r
  values(smoothed)[is.na(values(r))] = 0
  
  smoothed = focal(smoothed, w=matrix(1,n,n), fun=mean)
  
  values(smoothed)[values(smoothed) > 0] = 1
  values(smoothed)[values(smoothed)==0] = NA
  
  return(smoothed)
}

createCluster <- function(ctrds, width) {
  buff = gBuffer(ctrds, width=(width/2), byid=T)
  nb = dnearneigh(ctrds, 0, width)
  ncs = n.comp.nb(nb)$nc
  ids = n.comp.nb(nb)$comp.id
  buffs = gUnaryUnion(buff, ids)
  npoints = as.numeric(table(ids))
  df = data.frame(id=1:ncs, npoints=npoints)
  buffs = SpatialPolygonsDataFrame(buffs, data=df)    
  return(buffs)
}

mergeShps = function(rootFolder) {
  
  l <- list.files(rootFolder, "*.shp$")
  
  layerNames = sapply(l, function(x) gsub(".shp", "", x))
  
  out=list()
  
  for (i in 1:length(l)) {
    print(i)
    spLine =  readOGR(rootFolder, layerNames[i])
    spLine = spChFIDs(spLine, paste(i, 1:nrow(spLine), sep="_"))
    spLine$groupe = i
    out[[i]] <- spLine 
  }
  
  res = do.call(rbind, out)
  return(res)
  
}

linesToComps = function(simpl) {
  # DENSITY OF LINES
  g = construireReseau(simpl)
  
  # SUBCOMPONENTS
  subgs = decompose.graph(g)
  
  # !! mettre i
  out = list()
  for (j in 1:length(subgs)) {
    
    print(paste(">> traitement du groupe de lignes", j, "sur", length(subgs)))
    
    subg = subgs[[j]]
    w = which(!is.na(match(E(g)$name, E(subg)$name)))
    spl = gLineMerge(simpl[w, ])
    
    spl = spChFIDs(spl, as.character(j))
    df = data.frame(longueur=gLength(spl), 
                    complexite=graph.density(subg), 
                    row.names=as.character(j))
    out[[j]] = SpatialLinesDataFrame(spl, data=df)
    
  }  
  comps = do.call(rbind, out)
  
  return(comp)
  
}

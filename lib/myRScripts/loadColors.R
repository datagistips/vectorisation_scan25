# COLORS FROM INI FILE
colors = read.table("couleurs.ini", sep=",", header=TRUE, stringsAsFactors=FALSE)
endmembers = as(colors[,1:3], "matrix"); row.names(endmembers)=1:nrow(endmembers)
threshs = colors[,4]
if(any(is.na(threshs))) cat("ERREUR : vous n'avez pas affecté de seuil à toutes les couleurs\n")
cat(paste(" INFO : ", nrow(endmembers), "variations de teinte\n"))
# plotColors(endmembers)
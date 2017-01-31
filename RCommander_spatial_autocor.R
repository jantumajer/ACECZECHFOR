#####################################################################
### Vypocet prostorove autokorelace (Moranovo I) korelacniho keficientu mezi TRW(obs) a TRW(mod)
#####################################################################

library(ape)
pearson <- (readXL("F:/IFER/###VSLite/Octave/model/model.xlsx", rownames=FALSE, header=TRUE, na="", sheet="#Pearson_z1_73_acevedo_rd500", stringsAsFactors=TRUE))[c(1:129),c(2,3)] # Pearsonovy korelacni koeficienty pro jednotlive plochy
plots <- readXL("F:/IFER/###VSLite/Klima_zdroj/Phi/ID_Plots_JT.xlsx", rownames=FALSE, header=TRUE, na="", sheet="phi", stringsAsFactors=TRUE) # Data o plochach (X, Y, nadmorska vyska)
merge <- merge(plots, pearson, by.x="ID_Plots", by.y="ID")

### Vytvorim inverzni matici vzdalenosti (rozdilu nadmorskych vysek), na jejiz diagonalu manualne doplnim nuly
dists <- as.matrix(dist(cbind(merge$LAT_DEG_WG, merge$LON_DEG_WG)))
dists.inv <- 1/dists
diag(dists.inv) <- 0

elevs <- as.matrix(dist(merge$Z_M))
elevs.inv <- 1/elevs
diag(elevs.inv) <- 0

Moran.I(merge$X.40.2012., dists.inv)
Moran.I(merge$X.40.2012., elevs.inv)
scatterplot(LON_DEG_WG~LAT_DEG_WG, reg.line=lm, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=merge)
scatterplot(LAT_DEG_WG~LON_DEG_WG, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=merge)


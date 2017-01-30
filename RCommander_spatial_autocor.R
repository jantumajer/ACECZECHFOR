# Nacitam data z //Dendro_zdroj/Sesit1.xlsx - list proR

strom.vek <- read.table("clipboard", header=TRUE, sep="\t", na.strings="NA",  dec=".", strip.white=TRUE)
plot.pocet <- read.table("clipboard", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
plot.vyska <- read.table("clipboard", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
plot.vyska <- within(plot.vyska, { VS_factor <- as.factor(Vyskovy.stupeò)}) # Vysku je nutne prevest na faktor

### Normalita residualu

res <- read.table("clipboard", header=FALSE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE) # Transponovana rada residualu z model.xls
with(res, shapiro.test(V1))


### Rozdily ve veku mezi vyskovymi stupni

vek.vyska <- merge(strom.vek, plot.vyska[,c(1,10,13)], by="IDPlot") # Vsechny stromy
vek.vyska.2 <- subset(vek.vyska, subset=Vìk>74) # Jenom ty, ktere jsou starsi nez 74 let (tj. od 1940)


par(mfrow=c(2,1))
Boxplot(Vìk~VS_factor, data=vek.vyska, id.method="n", xlab="Elevation belt", ylab="Age", ylim=c(20,160))
Boxplot(Vìk~VS_factor, data=vek.vyska.2, id.method="n", xlab="Elevation belt", ylab="Age", ylim=c(70,160))

AnovaModel.1 <- aov(Vìk ~ VS_factor, data=vek.vyska) # Signifikantni ANOVA, nesignifikantni post-hoc
summary(AnovaModel.1); TukeyHSD(x=AnovaModel.1)

AnovaModel.2 <- aov(Vìk ~ VS_factor, data=vek.vyska.2) # rozdil mezi 6 a 1
summary(AnovaModel.2); TukeyHSD(x=AnovaModel.2)

### Rozdily v poctu stromu mezi vyskovymi stupni

pocet.vyska <- merge(plot.pocet, plot.vyska[,c(1,10,13)], by="IDPlot")

Boxplot(Pocet_stromu~VS_factor, data=pocet.vyska, id.method="n", xlab="Elevation belt", ylab="Number of trees in chronology", ylim=c(0,10))

AnovaModel.3 <- aov(Pocet_stromu ~ VS_factor, data=pocet.vyska) # Nesignifikantni ANOVA
summary(AnovaModel.3); TukeyHSD(x=AnovaModel.3)

### Kontingencni tabulka Vyska-Svetova strana
# Mezi jednotlivymi vyskovymi stupni se nelisi zastoupeni V-Z chronologii (chi-kvadrat testy)


local({
  .Table <- xtabs(~S_J+V_Z+VS_factor, data=plot.vyska)
  cat("\nFrequency table:\n")
  print(.Table) 
})



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


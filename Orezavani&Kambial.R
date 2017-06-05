### Funkce, ktera orizne serie prednastavenym oknem a nasledne z oriznuteho sourboru vypocita chronologii

#######################################

library(dplR); library(treeclim)

### Cesta ke zdrojovemu rwl souboru
serie.zdroj <- read.rwl("C:/Documents and Settings/Administrator/Desktop/Trendy/picea_older_30_trend.txt", format="auto") 
### Cesta k tabulce s odhadem poctu chybejicich letokruhu
chybejici.let <- readXL("C:/Documents and Settings/Administrator/Desktop/Trendy/Chybejici_letokruhy.xls", rownames=FALSE, header=TRUE, na="", sheet="List2", stringsAsFactors=TRUE)


###########################################################################
### Overeni, zda je stejne poradi serii v rwl a souboru chybejicich let ###
###########################################################################
merge <- merge(rwl.stats(serie.zdroj)[c(1,4)], chybejici.let, by.x="series", by.y="ID", all.x=TRUE)
merge[is.na(merge)] <- 0 # Stromy, u kterych nebyl odhadnut pocet chybejicich letokruhu, povazuji za vrtane do stredu.
max(na.omit(chybejici.let[5])) # Maximalni pocet chybejicich letokruhu < dolni okraj = nemusim resit vymazani celych chronologii


###############################################################
### Funkce pro oriznuti serii (nutne celou nacist najednou) ###
###############################################################

# jako input parameter pouzivam merge
orez <- function(vstupni.serie, chyb.let, dolni.okraj, horni.okraj) {

	# Do skriptu vstoupi jenom ty serie, ktere jsou dostatecne dlouhe (setreni casu)				
	AGE <- (which(chyb.let[,2]+chyb.let[,6]>horni.okraj))
	trw.analyza <- vstupni.serie[,AGE]; chyb.analyza <- merge[AGE,]

	max.chybejicich.podkornich.letokruhu <- max(rwl.stats(trw.analyza)[,3])-min(rwl.stats(trw.analyza)[,3]) 						# Maximalni pocet chybejicich podkornich letokruhu

	data.table <- trw.analyza				 # Soubor pro ulozeni oriznutych serii ma stejnou strukturu jako vstupni soubor ...
	data.table[data.table>=0] <- NA 					# ... jenom ho pred spustenim vyprazdnim.

	na.table <- data.frame(yr=c(1:(horni.okraj-dolni.okraj+1)))

	for (i in 1:ncol(trw.analyza)) {

			na.table[,1] <- data.frame(na.omit(trw.analyza[,i]))[c((dolni.okraj-chyb.analyza[i,6]):(horni.okraj-chyb.analyza[i,6])),]				 # Vybere pouze non-NA letokruhy, ktere jsou mezi hornim a dolnim okrajem okna
			poradi.prvniho.letokruhu <- sum(is.na(trw.analyza[c(1:(nrow(trw.analyza)-max.chybejicich.podkornich.letokruhu)),i]))-chyb.analyza[i,6]

		for (k in c(1:(horni.okraj-dolni.okraj+1))) {

			data.table[(poradi.prvniho.letokruhu+dolni.okraj+k-1),i] <- na.table[k,1]
		
		}
	}
	
#	EPS <- rwi.stats.running(data.table, min.corr.overlap=0, window.length=31, window.overlap=20)
	chron <- chron(data.table, prewhitten=T)[c(1:nrow(trw.analyza)),]
	plot(chron, add.spline=T, ylim=c(0,2))
return(list(chron=chron, data.table=data.table))
}


##############################################################################
### Funkce pro vypocet linearniho trendu v oriznute casti serie (A. Buras) ###
##############################################################################

TRENDS<-function(rwl)
{
  RESP<-vector(mode="numeric")
  for(i in 1:ncol(rwl))
  {
    M<-lm(rwl[,i]~as.numeric(rownames(rwl)))
    if(summary(M)$coef[2,4]<0.05&&sign(summary(M)$coef[2,1])<0)
    {
      RESP[i]<-"NEG_sig"
    }
    if(summary(M)$coef[2,4]<0.05&&sign(summary(M)$coef[2,1])>0)
    {
      RESP[i]<-"POS_sig"
    }
    if(summary(M)$coef[2,4]>=0.05&&sign(summary(M)$coef[2,1])<0)
    {
      RESP[i]<-"NEG_nesig"
    }
    if(summary(M)$coef[2,4]>=0.05&&sign(summary(M)$coef[2,1])>0)
    {
      RESP[i]<-"POS_nesig"
    }
  }
  return(RESP)		
}


#################################################################################
### Funkce pro serazeni podle kambialniho stari (nutne celou nacist najednou) ###
#################################################################################

kambial <- function(vstupni.serie, start.year=86, end.year=95, pocet.kroku=5, step=1) {

		chronologie <- data.frame(cambial.age=c(1:(end.year+12))) # Radeji pridam 12 radku - maximalni pocet chybejicich letokruhu u jednoho stromu (viz tabulka merge)

		for (i in c(0:(pocet.kroku-1))) {

			kambialni.stari <- data.frame(N=rep(NA,end.year+12))
			VYBER <- (intersect(which(rwl.stats(vstupni.serie)[,4]+merge[,6]>=(start.year+i*step)), which(rwl.stats(vstupni.serie)[,4]+merge[,6]<=(end.year+i*step))))
			serie.vyber <- vstupni.serie[,VYBER]; merge.vyber <- merge[VYBER,]

			for (k in c(1:ncol(serie.vyber))) {			
			for (l in c(1:nrow(data.frame(na.omit(serie.vyber[,k]))))) {
				
				chybi.let <- merge.vyber[k,6]
				kambialni.stari[(l+chybi.let),k] <- data.frame(na.omit(serie.vyber[,k]))[l,]			} 
								}

			chronologie <- cbind(chronologie, chron(kambialni.stari, prefix=paste("+",((i)*step), sep=""),biweight=F))				
		}

	return(list(chron=chronologie, groups=pocet.kroku))

}


kambial.plot <- function(vysledek, start.year=NULL) {

		colvec <- vector(mode="numeric")
		pocet.chronologii <- vysledek$groups
			mezikrok <- vysledek$chron[,seq(2,2*pocet.chronologii, by=2)]
			(mezikrok[is.na(mezikrok)] <- 100); min <- min(mezikrok)
			(mezikrok[mezikrok==100] <- -100); max <- max(mezikrok)
		plot(NA,xlim=c(1,nrow(vysledek$chron)), ylim=c(min,max), type="l", xlab="cambial age", ylab="TRW")
		for (i in c(1:pocet.chronologii)) {
			colvec[i] <- hsv(i/(pocet.chronologii*1.2))
			lines(vysledek$chron[,(2*i)], col=colvec[i]) }
		legend('topright', paste(start.year, names(vysledek$chron[,seq(2,2*pocet.chronologii, by=2)]), sep=""), lty=1, col=colvec)
}

###########################
## Vypocet pomoci funkce ##
###########################

### Zde si uprav (detrendovani, BAI,..) serie tak, jak chces aby vstupovaly do vypoctu
hruba.data <- serie.zdroj # hruba data
Spline.det <- detrend(serie.zdroj, method="Spline", nyrs=100) # 100lety spline
Mean.det <- detrend(serie.zdroj, method="Mean") # Prumer
Exp.det <- detrend(serie.zdroj, method="ModNegExp") # Negativni exponenciele
Bai <- bai.in(serie.zdroj) # BAI
# ... <- ...

### Vlastni vypocet vytvori nove tabulky s vystupy
vypocet.1 <- orez(hruba.data, merge, 31, 50) # syntax: (zdrojova serie, merge-chybejici letokruhy, dolni okraj, horni okraj)


vypocet.2 <- orez(hruba.data, merge, 51, 70)

# ... <- ...

kam <- kambial(hruba.data, 86, 95, 5, 1) # 40-lete v roce 1961
kam <- kambial(hruba.data, 36, 45, 5, 1) # 40-lete v roce 2011

kambial.plot(kam, "36")
write.table(kam$chron, "C:/Documents and Settings/Administrator/Desktop/Trendy/kambial.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")


### Ulozeni tabulky do *.txt (oddelovac tabulator)
write.table(vypocet.1$chron, "F:/IFER/trendy/chronologie/chron_31_50.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")
write.table(vypocet.1$EPS, "F:/IFER/trendy/chronologie/eps_31_50.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")
write.table(vypocet.1$data.table, "F:/IFER/trendy/chronologie/31_50.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")
write.rwl(vypocet.1$data.table, "F:/IFER/trendy/chronologie/31_50.rwl", format=c("tucson"))

write.table(vypocet.2$chron, "F:/IFER/trendy/chronologie/chron_51_70.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")
write.table(vypocet.2$EPS, "F:/IFER/trendy/chronologie/eps_51_70.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")
write.table(vypocet.2$data.table, "F:/IFER/trendy/chronologie/51_70.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")
write.rwl(vypocet.2$data.table, "F:/IFER/trendy/chronologie/51_70.rwl", format=c("tucson"))
# ...

### Trendy
trendy.1 <- data.frame(TRENDS(vypocet.1$data.table))
trendy.2 <- data.frame(TRENDS(vypocet.2$data.table))


local({
  .Table <- with(trendy.2, table(TRENDS.vypocet.2.data.table.))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})


##############
## Treeclim ##
##############
# Pro usetreni casu nacitam chronologie z XLS souboru

# Chronologie
Dataset <- readXL("F:/IFER/trendy/klimadata/Database_1961to2013.xlsx", rownames=FALSE, header=TRUE, na="", sheet="trend+vysokofrekvencni", stringsAsFactors=TRUE)

cr_A_All <- chron(data.frame(subset(Dataset, subset=Pasmo=="ALL")[,3])); rownames(cr_A_All) <- c(1961:2013)
cr_B_All <- chron(data.frame(subset(Dataset, subset=Pasmo=="ALL")[,5])); rownames(cr_B_All) <- c(1961:2013)
cr_A_p1 <- chron(data.frame(subset(Dataset, subset=Pasmo==1)[,3])); rownames(cr_A_p1) <- c(1961:2013)
cr_B_p1 <- chron(data.frame(subset(Dataset, subset=Pasmo==1)[,5])); rownames(cr_B_p1) <- c(1961:2013)
cr_A_p2 <- chron(data.frame(subset(Dataset, subset=Pasmo==2)[,3])); rownames(cr_A_p2) <- c(1961:2013)
cr_B_p2 <- chron(data.frame(subset(Dataset, subset=Pasmo==2)[,5])); rownames(cr_B_p2) <- c(1961:2013)
cr_A_p3 <- chron(data.frame(subset(Dataset, subset=Pasmo==3)[,3])); rownames(cr_A_p3) <- c(1961:2013)
cr_B_p3 <- chron(data.frame(subset(Dataset, subset=Pasmo==3)[,5])); rownames(cr_B_p3) <- c(1961:2013)

# Klimadata (zatim jednotna pro vsechny vyskove urovne)
temp <- readXL("F:/IFER/trendy/klimadata/priprava/Clima_ALL_T_vazeno.xlsx", rownames=FALSE, header=TRUE, na="", sheet="T", stringsAsFactors=TRUE)
prec <- readXL("F:/IFER/trendy/klimadata/priprava/Clima_ALL_Prec_vazeno.xlsx", rownames=FALSE, header=TRUE, na="", sheet="P", stringsAsFactors=TRUE)
SPEI <- readXL("F:/IFER/trendy/klimadata/priprava/SPEI_1961to2015_Crosstab_SPEI01.xlsx", rownames=FALSE, header=TRUE, na="", sheet="SPEI_1961to2015_Crosstab_SPEI01", stringsAsFactors=TRUE)

tempA <- temp[,c(1:13)]; tempB <- temp[,c(1,14:25)]
precA <- prec[,c(1:13)]; precB <- prec[,c(1,14:25)]

climA <- list(tempA=tempA, precA=precA); climB <- list(tempB=tempB, precB=precB)
climA <- list(tempA=tempA, precA=precA, SPEI=SPEI); climB <- list(tempB=tempB, precB=precB, SPEI=SPEI)

############

climA_ALL <- dcc(cr_A_All, climA, method=c("correlation"))
climA_p1 <- dcc(cr_A_p1, climA, method=c("correlation"))
climA_p2 <- dcc(cr_A_p2, climA, method=c("correlation"))
climA_p3 <- dcc(cr_A_p3, climA, method=c("correlation"))

climB_ALL <- dcc(cr_B_All, climB, method=c("correlation"))
climB_p1 <- dcc(cr_B_p1, climB, method=c("correlation"))
climB_p2 <- dcc(cr_B_p2, climB, method=c("correlation"))
climB_p3 <- dcc(cr_B_p3, climB, method=c("correlation"))

plot(climA_ALL)
plot(climA_p1)
plot(climA_p2)
plot(climA_p3)

plot(climB_ALL)
plot(climB_p1)
plot(climB_p2)
plot(climB_p3)

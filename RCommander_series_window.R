### Funkce, ktera orizne serie prednastavenym oknem a nasledne z oriznuteho sourboru vypocita chronologii

#######################################

library(dplR)

### Zde zadej cestu ke zdrojovemu rwl souboru
serie.zdroj <- read.rwl("F:/IFER/###VSLite/Dendro_zdroj/picea_older_30.rwl", format="auto") 			

### Zde nastav rozpeti let, ktera se maji oriznout
dolni.okraj <- 50; horni.okraj <- 70 

### Zde urci, jaky typ serie ma vstupovat do oriznuti (vyber si)
serieTRW <- serie.zdroj # hruba data
serieTRW <- detrend(serie.zdroj, method="Spline", nyrs=100) # 100lety spline
serieTRW <- bai.in(serie.zdroj) # BAI
# serieTRW <- ...

#################################
### Funkce pro oriznuti serii ###
#################################

AGE <- (which(rwl.stats(serieTRW)[,4]>horni.okraj)) 						# Do skriptu vstupi jenom ty serie, ktere jsou dostatecne dlouhe (setreni casu)
trw.analyza <- serieTRW[,AGE]
max.chybejicich.podkornich.letokruhu <- max(rwl.stats(trw.analyza)[,3])-min(rwl.stats(trw.analyza)[,3]) 						# Maximalni pocet chabejicich podkornich letokruhu

data.table <- trw.analyza				 # Soubor pro ulozeni oriznutych serii ma stejnou strukturu jako vstupni soubor ...
data.table[data.table>0] <- NA 					# ... jenom ho pred spustenim vyprazdnim.

na.table <- data.frame(yr=c(1:(horni.okraj-dolni.okraj+1)))

for (i in 1:ncol(trw.analyza)) {

		na.table[,1] <- data.frame(na.omit(serieTRW[,i]))[c(dolni.okraj:horni.okraj),]				 # Vybere pouze non-NA letokruhy, ktere jsou mezi hornim a dolnim okrajem okna
		poradi.prvniho.letokruhu <- sum(is.na(serieTRW[c(1:(nrow(trw.analyza)-max.chybejicich.podkornich.letokruhu)),i]))

	for (k in c(1:(horni.okraj-dolni.okraj+1))) {

		data.table[(poradi.prvniho.letokruhu+dolni.okraj+k-1),i]<-na.table[k,1]
		
	}

}


##############################
chron <- chron(data.table)[c(1:nrow(trw.analyza)),]
crn.plot(chron, add.spline=T)

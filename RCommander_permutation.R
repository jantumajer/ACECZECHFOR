
perm <- readXL("F:/IFER/###VSLite/Dendro_zdroj/permutace/permutace.xlsx", rownames=TRUE, header=TRUE, na="", sheet="permutationR", stringsAsFactors=TRUE)
plots <- read.table("F:/IFER/###VSLite/Dendro_zdroj/permutace/pocty.txt", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

split <- strsplit(colnames(perm),"_")
df <- data.frame(split)
plot.names <- as.character(plots[,1])
sub.n <- data.frame(C=c(1:73))


# cely cyklus probehne pro vsechny plochy, ktere maji 7 nebo 8 vrtanych stromu
for (name in plot.names) { 

j <- 1
subset <-data.frame(C=c(1:73))
sub.1 <-subset; sub.3 <-subset; sub.5 <-subset; sub.all <-subset

### 1] vyber serii z dane plochy
for (i in (1:ncol(perm))) {
	if (as.character(df[1,i])==name) {subset[,j]<-perm[,i];
						 j<-j+1}
}
### 2] permutace - 5x nahodny vyber 1, 3 a 5 stromu (rovnou zprumerovano) + celkova chronologie ze vsech stromu
for (nperm in c(1:5)) {
	sub.1[,nperm] <- rbind(sample(subset, 1))
	sub.3[,nperm] <- data.frame(rowMeans(sample(subset, 3)))
	sub.5[,nperm] <- data.frame(rowMeans(sample(subset, 5)))
	sub.all <- data.frame(rowMeans(sample(subset, ncol(subset))))
	}

colnames(sub.1) <- c(paste(name,"_11",sep=""), paste(name,"_12",sep=""), paste(name,"_13",sep=""), paste(name,"_14",sep=""), paste(name,"_15",sep="")) # Pojmenovani chronologii
colnames(sub.3) <- c(paste(name,"_31",sep=""), paste(name,"_32",sep=""), paste(name,"_33",sep=""), paste(name,"_34",sep=""), paste(name,"_35",sep=""))
colnames(sub.5) <- c(paste(name,"_51",sep=""), paste(name,"_52",sep=""), paste(name,"_53",sep=""), paste(name,"_54",sep=""), paste(name,"_55",sep=""))
colnames(sub.all) <- (paste(name,"_all",sep="")) 

sub.n <- cbind(sub.n, sub.1, sub.3, sub.5, sub.all) # Propojeni chronologii

}


# Ulozeni
write.table(sub.n, "F:/IFER/###VSLite/Dendro_zdroj/permutace/perm_chronologie.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")


# Teplota

for (i in (formatC(c(0021,0022,0023,0026,0033,0035,0042,0044,0047,0048,0061,0066,0076,0081,0100,0106,0107,0118,0124,0125,0126,0127,0128,0138,0140,0150,0165,0189,0192,0213,0231,0239,0259,0292,0294,0301,0307,0342,0344,0349,0361,0365,0373,0376,0383,0390,0391,0425,0436,0437,0444,0491,0499,0501,0506,0512,0534,0539,0541,0548,0566,0574,0600,0610,0613,0616,0633,0634,0640,0667,0668,0675,0678,0679,0703,0706,0712,0729,0762,0765,0796,0804,0818,0819,0826,0860,0887,0888,0896,0904,0938,0948,0990,0992,0996,0997,1062,1100,1121,1134,1158,1163,1180,1203,1204,1211,1214,1232,1263,1288,1310,1333,1334,1356,1377,1381,1420,1429,1448,1475,1482,1498,1532,1555,1569,1576,1577,1584,1587), width=4, flag='0'))) 

{

Dataset <- read.table(paste("C:/honzaT/aceczechfor/###VSLite/Klima_zdroj/Temperature/",i,".txt", sep=""), header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Dataset <- data.frame(t(Dataset[141:213,2:13]))
Dataset$var <- (";"); Dataset[12,74] <- (""); # Dataset$var2 <- (""); Dataset[1,75] <- ("["); Dataset2 <- Dataset[,c(75, 1:74)]
write.table(Dataset, paste("C:/honzaT/aceczechfor/###VSLite/Klima_zdroj/Temperature/uprava/",i,".csv", sep=""), sep=",", col.names=FALSE, row.names=FALSE, quote=TRUE, na="NA")
}

# Pripojeni vyskoveho gradientu



phi <- readXL("C:/honzaT/aceczechfor/###VSLite/Klima_zdroj/Phi/ID_Plots_JT.xlsx",rownames=FALSE, header=TRUE, na="", sheet="phi", stringsAsFactors=TRUE)
elev <- readXL("C:/honzaT/aceczechfor/###VSLite/Klima_zdroj/Phi/Grid_original.xlsx",rownames=FALSE, header=TRUE, na="", sheet="Grid_original", stringsAsFactors=TRUE)
merge <- merge(phi, elev, by="ID_Plots")
write.table(merge, paste("C:/honzaT/aceczechfor/###VSLite/Klima_zdroj/Phi/elev.txt", sep=""), sep=",", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")


# Vykresleni histogramu



hist(phi$Z_M, main="", xlab="", ylab="",col="gray", xlim=c(200,1200), ylim=c(0,40), breaks=seq(200,1200,by=100), cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)



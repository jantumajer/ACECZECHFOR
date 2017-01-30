
Dataset <- read.table("clipboard", header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE) # Dilci rustove limitace z GR_Acevedo.xls/ limitace_vahy
# Subset pro zohledneni trendu v reziduich Dataset <- Dataset[1:61,]
Dataset$s1 <- with(Dataset, m1+t1+un1+mix1)
Dataset$s2 <- with(Dataset, m2+t2+un2+mix2)
Dataset$s3 <- with(Dataset, m3+t3+un3+mix3)
Dataset$s4 <- with(Dataset, m4+t4+un4+mix4)
Dataset$s5 <- with(Dataset, m5+t5+un5+mix5)
Dataset$s6 <- with(Dataset, m6+t6+un6+mix6)

RegModel.1 <- lm(t1~Year, data=Dataset);RegModel.2 <- lm(t2~Year, data=Dataset);RegModel.3 <- lm(t3~Year, data=Dataset)
RegModel.4 <- lm(t4~Year, data=Dataset);RegModel.5 <- lm(t5~Year, data=Dataset);RegModel.6 <- lm(t6~Year, data=Dataset)

RegModel.1 <- lm(m1~Year, data=Dataset);RegModel.2 <- lm(m2~Year, data=Dataset);RegModel.3 <- lm(m3~Year, data=Dataset)
RegModel.4 <- lm(m4~Year, data=Dataset);RegModel.5 <- lm(m5~Year, data=Dataset);RegModel.6 <- lm(m6~Year, data=Dataset)

RegModel.1 <- lm(un1~Year, data=Dataset);RegModel.2 <- lm(un2~Year, data=Dataset);RegModel.3 <- lm(un3~Year, data=Dataset)
RegModel.4 <- lm(un4~Year, data=Dataset);RegModel.5 <- lm(un5~Year, data=Dataset);RegModel.6 <- lm(un6~Year, data=Dataset)

RegModel.1 <- lm(mix1~Year, data=Dataset);RegModel.2 <- lm(mix2~Year, data=Dataset);RegModel.3 <- lm(mix3~Year, data=Dataset)
RegModel.4 <- lm(mix4~Year, data=Dataset);RegModel.5 <- lm(mix5~Year, data=Dataset);RegModel.6 <- lm(mix6~Year, data=Dataset)

RegModel.1 <- lm(s1~Year, data=Dataset);RegModel.2 <- lm(s2~Year, data=Dataset);RegModel.3 <- lm(s3~Year, data=Dataset)
RegModel.4 <- lm(s4~Year, data=Dataset);RegModel.5 <- lm(s5~Year, data=Dataset);RegModel.6 <- lm(s6~Year, data=Dataset)

summary(RegModel.1)
summary(RegModel.2)
summary(RegModel.3)
summary(RegModel.4)
summary(RegModel.5)
summary(RegModel.6)


###########################################
Dataset.2 <- read.table("clipboard", header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE) # Celkove (bez ohledu na vysku) z GR_Acevedo.xls/ limitace_vahy

RegModel.T <- lm(Temp~Year, data=Dataset.2); RegModel.M <- lm(Moist~Year, data=Dataset.2); RegModel.U <- lm(Unlim~Year, data=Dataset.2); RegModel.Mix <- lm(Mix~Year, data=Dataset.2); RegModel.Sum <- lm(SUMA~Year, data=Dataset.2)

summary(RegModel.T)
summary(RegModel.M)
summary(RegModel.U)
summary(RegModel.Mix)
summary(RegModel.Sum)


######################################################################################
# Trendy v Raw seriich

library(dplR)

serieTRW <- read.rwl("F:/IFER/###VSLite/Dendro_zdroj/picea_older_30.rwl", format="auto")
stats <- rwl.stats(serieTRW[c(1:(nrow(serieTRW)-2)),])

sub <- data.frame(ID=c(1:155))
j <- 1
for (i in (1:nrow(stats))){
	if (stats[i,4]>72) 
		{sub[,j]<-(serieTRW[,i]); colnames(sub[,j]) <- colnames(serieTRW[,i]); j<-j+1}
}

sub <- sub[c(81:153),] # Jenom obdobi 1940-2012

########
# Alanova funkce
########

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

out <- data.frame(TRENDS(sub))
out.10 <- data.frame(TRENDS(sub[c((nrow(sub)-10):nrow(sub)),]))
out.20 <- data.frame(TRENDS(sub[c((nrow(sub)-20):nrow(sub)),]))
out.30 <- data.frame(TRENDS(sub[c((nrow(sub)-30):nrow(sub)),]))
out.40 <- data.frame(TRENDS(sub[c((nrow(sub)-40):nrow(sub)),]))
out.32.12 <- data.frame(TRENDS(sub[c((nrow(sub)-32):(nrow(sub)-12)),]))


.Table20 <- with(out.20, table(TRENDS.sub.c..nrow.sub....20..nrow.sub......))
.Table <- with(out, table(TRENDS.sub.))


  


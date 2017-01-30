library(abind, pos=14)
library(e1071, pos=15)
library(mvtnorm, pos=16)
library(survival, pos=16)
library(MASS, pos=16)
library(TH.data, pos=16)
library(multcomp, pos=16)

parametry <- readXL("F:/IFER/###VSLite/Octave/parametry/Bayes_parametry.xlsx", rownames=TRUE, header=TRUE, na="", sheet="souhRn", stringsAsFactors=TRUE)

parametry <- within(parametry, {Vyskovy.stupeò <- as.factor(Vyskovy.stupeò)})


### Prumer + sd pro celek a jednotlive vyskove urovne
numSummary(parametry[,c("X73_M1_rd500", "X73_M2_rd500", "X73_T1_rd500", "X73_T2_rd500")], statistics=c("mean", "sd"), quantiles=c(0,.25,.5,.75,1))
numSummary(parametry[,c("X73_M1_rd500", "X73_M2_rd500", "X73_T1_rd500", "X73_T2_rd500")], 
  groups=parametry$Vyskovy.stupeò, statistics=c("mean", "sd"), quantiles=c(0,.25,.5,.75,1))

### ANOVA
AnovaModel.1 <- aov(X73_M1_rd500 ~ Vyskovy.stupeò, data=parametry)
summary(AnovaModel.1)
AnovaModel.2 <- aov(X73_M2_rd500 ~ Vyskovy.stupeò, data=parametry)
summary(AnovaModel.2)
AnovaModel.3 <- aov(X73_T1_rd500 ~ Vyskovy.stupeò, data=parametry)
summary(AnovaModel.3)
AnovaModel.4 <- aov(X73_T2_rd500 ~ Vyskovy.stupeò, data=parametry)
summary(AnovaModel.4)
parametry <- read.table("clipboard", header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
library(relimp, pos=21)

### t-testy mezi dvojicemi parametru za ruzna obdobi kalibrace
# Vychazeji rozdily mezi M2

attach(parametry)
t.test(X26_M1_rd500, X51_M1_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)
t.test(X26_M1_rd500, X73_M1_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)
t.test(X51_M1_rd500, X73_M1_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)

t.test(X26_M2_rd500, X51_M2_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)
t.test(X26_M2_rd500, X73_M2_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)
t.test(X51_M2_rd500, X73_M2_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)

t.test(X26_T1_rd500, X51_T1_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)
t.test(X26_T1_rd500, X73_T1_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)
t.test(X51_T1_rd500, X73_T1_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)

t.test(X26_T2_rd500, X51_T2_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)
t.test(X26_T2_rd500, X73_T2_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)
t.test(X51_T2_rd500, X73_T2_rd500, alternative='two.sided', conf.level=.95, paired=TRUE)

### ANOVA mezi dvojicemi parametru za ruzna obdobi kalibrace
# Nesignifikantni

parametry.2 <- readXL("F:/IFER/###VSLite/Octave/parametry/Bayes_parametry.xlsx", rownames=FALSE, header=TRUE, na="", sheet="R2", stringsAsFactors=TRUE)

AnovaModel.1 <- aov(M1 ~ Priod, data=parametry.2)
summary(AnovaModel.1)
with(parametry.2, numSummary(M1, groups=Priod, statistics=c("mean", "sd")))
AnovaModel.3 <- aov(M2 ~ Priod, data=parametry.2)
summary(AnovaModel.3)
with(parametry.2, numSummary(M2, groups=Priod, statistics=c("mean", "sd")))
AnovaModel.4 <- aov(T1 ~ Priod, data=parametry.2)
summary(AnovaModel.4)
with(parametry.2, numSummary(T1, groups=Priod, statistics=c("mean", "sd")))
AnovaModel.5 <- aov(T2 ~ Priod, data=parametry.2)
summary(AnovaModel.5)
with(parametry.2, numSummary(T2, groups=Priod, statistics=c("mean", "sd")))


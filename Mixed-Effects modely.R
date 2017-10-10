library(lme4); library(nlme)
library(piecewiseSEM) 

Dataset <- readXL("C:/honzaT/aceczechfor/Mean_paper/Database_1961to2013.xlsx", rownames=FALSE, header=TRUE, na="", sheet="trend+vysokofrekvencni", stringsAsFactors=TRUE) # Cesta na Applu

#### Regrese ####
#################
# Pocitana jenom za obdobi 1961-2010

Dataset_GLM <- Dataset[c(54:212),] # do roku 2013

########################

################################################
## GLM s interakcemi nezavislych promennych a vysky
################################################

GLM.A.pasmo <- lm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A + NDEP_A + NDEP_A:Pasmo + CO2, data=Dataset_GLM) # Varianta se zachovanim NDEP v interakci se srazkami - nizsi AIC, vyssi BIC
summary(GLM.A.pasmo)

GLM.B.pasmo <- lm(TRW_B ~ Pasmo + T.3.5._B + Veg_Prec_B + NDEP_B + NDEP_B:Pasmo + CO2, data=Dataset_GLM)
summary(GLM.B.pasmo)


# Model se zohlednenim casove autokorelace
GLM.A.pasmo.autocor <- gls(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A + NDEP_A + NDEP_A:Pasmo + CO2, data=Dataset_GLM, correlation=corAR1(,form=~YEAR|Pasmo), na.action=na.omit)
summary(GLM.A.pasmo.autocor)
AIC(GLM.A.pasmo, GLM.A.pasmo.autocor)

GLM.B.pasmo.autocor <- gls(TRW_B ~ Pasmo + T.3.5._B + Veg_Prec_B + NDEP_B + NDEP_B:Pasmo + CO2, data=Dataset_GLM, correlation=corAR1(,form=~YEAR|Pasmo), na.action=na.omit)
summary(GLM.B.pasmo.autocor)
AIC(GLM.B.pasmo, GLM.B.pasmo.autocor)

################################################
## Srovnani alternativnich modelu podle AIC

A_full <- glm(TRW_A ~ Pasmo + T.3.5._A:Pasmo + Veg_Prec_A:Pasmo + NDEP_A:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
A_I <- glm(TRW_A ~ T.3.5._A:Pasmo + Veg_Prec_A:Pasmo + NDEP_A:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
A_T <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A:Pasmo + NDEP_A:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
A_P <- glm(TRW_A ~ Pasmo + T.3.5._A:Pasmo + Veg_Prec_A + NDEP_A:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
A_N <- glm(TRW_A ~ Pasmo + T.3.5._A:Pasmo + Veg_Prec_A:Pasmo + NDEP_A + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
A_C <- glm(TRW_A ~ Pasmo + T.3.5._A:Pasmo + Veg_Prec_A:Pasmo + NDEP_A:Pasmo + CO2, family=Gamma(identity), data=Dataset_GLM)

AIC(A_full, A_I, A_T, A_P, A_N, A_C) # Nejlepsi AIC pro model bez vlivu Pasma na srazky
###############

A_PI <- glm(TRW_A ~ T.3.5._A:Pasmo + Veg_Prec_A + NDEP_A:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
A_PT <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A + NDEP_A:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
A_PN <- glm(TRW_A ~ Pasmo + T.3.5._A:Pasmo + Veg_Prec_A + NDEP_A + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
A_PC <- glm(TRW_A ~ Pasmo + T.3.5._A:Pasmo + Veg_Prec_A + NDEP_A:Pasmo + CO2, family=Gamma(identity), data=Dataset_GLM)

AIC(A_P, A_PI, A_PT, A_PN, A_PC) # Nejlepsi AIC pro model bez vlivu Pasma na srazky + teplotu
###############

A_PTI <- glm(TRW_A ~ T.3.5._A + Veg_Prec_A + NDEP_A:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
A_PTN <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A + NDEP_A + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
A_PTC <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A + NDEP_A:Pasmo + CO2, family=Gamma(identity), data=Dataset_GLM)

AIC(A_PT, A_PTI, A_PTN, A_PTC) # Nejlepsi AIC pro model bez vlivu Pasma na srazky + teplotu + CO2
###############

A_PTCI <- glm(TRW_A ~ T.3.5._A + Veg_Prec_A + NDEP_A:Pasmo + CO2, family=Gamma(identity), data=Dataset_GLM)
A_PTCN <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A + NDEP_A + CO2, family=Gamma(identity), data=Dataset_GLM)

AIC(A_PTC, A_PTCI, A_PTCN) # Vyrazeni dalsich interakci zhorsuje AIC -> nejlepsi je model s interakcemi pro vliv pasma na intercept a N
############################################################

B_full <- glm(TRW_B ~ Pasmo + T.3.5._B:Pasmo + Veg_Prec_B:Pasmo + NDEP_B:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
B_I <- glm(TRW_B ~ T.3.5._B:Pasmo + Veg_Prec_B:Pasmo + NDEP_B:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
B_T <- glm(TRW_B ~ Pasmo + T.3.5._B + Veg_Prec_B:Pasmo + NDEP_B:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
B_P <- glm(TRW_B ~ Pasmo + T.3.5._B:Pasmo + Veg_Prec_B + NDEP_B:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
B_N <- glm(TRW_B ~ Pasmo + T.3.5._B:Pasmo + Veg_Prec_B:Pasmo + NDEP_B + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
B_C <- glm(TRW_B ~ Pasmo + T.3.5._B:Pasmo + Veg_Prec_B:Pasmo + NDEP_B:Pasmo + CO2, family=Gamma(identity), data=Dataset_GLM)

AIC(B_full, B_I, B_T, B_P, B_N, B_C) # Nejlepsi AIC pro model bez vlivu Pasma na teploty
###############

B_TI <- glm(TRW_B ~ T.3.5._B + Veg_Prec_B:Pasmo + NDEP_B:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
B_TP <- glm(TRW_B ~ Pasmo + T.3.5._B + Veg_Prec_B + NDEP_B:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
B_TN <- glm(TRW_B ~ Pasmo + T.3.5._B + Veg_Prec_B:Pasmo + NDEP_B + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
B_TC <- glm(TRW_B ~ Pasmo + T.3.5._B + Veg_Prec_B:Pasmo + NDEP_B:Pasmo + CO2, family=Gamma(identity), data=Dataset_GLM)

AIC(B_T, B_TI, B_TP, B_TN, B_TC) # Nejlepsi AIC pro model bez vlivu Pasma na teplotu + srazky
###############

B_TPI <- glm(TRW_B ~ T.3.5._B + Veg_Prec_B + NDEP_B:Pasmo + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
B_TPN <- glm(TRW_B ~ Pasmo + T.3.5._B + Veg_Prec_B + NDEP_B + CO2:Pasmo, family=Gamma(identity), data=Dataset_GLM)
B_TPC <- glm(TRW_B ~ Pasmo + T.3.5._B + Veg_Prec_B + NDEP_B:Pasmo + CO2, family=Gamma(identity), data=Dataset_GLM)

AIC(B_TP, B_TPI, B_TPN, B_TPC) # Nejlepsi AIC pro model bez vlivu Pasma na teplotu + srazky + CO2

B_TPCI <- glm(TRW_B ~ T.3.5._B + Veg_Prec_B + NDEP_B:Pasmo + CO2, family=Gamma(identity), data=Dataset_GLM)
B_TPCN <- glm(TRW_B ~ Pasmo + T.3.5._B + Veg_Prec_B + NDEP_B + CO2, family=Gamma(identity), data=Dataset_GLM)

AIC(B_TPC, B_TPCI, B_TPCN) # Vyrazeni dalsich interakci zhorsuje AIC -> nejlepsi je model s interakcemi pro vliv pasma na intercept a N
###############

A_PTCI <- glm(TRW_A ~ T.3.5._A + Veg_Prec_A + NDEP_A:Pasmo + CO2, family=Gamma(identity), data=Dataset_GLM)
A_PTCN <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A + NDEP_A + CO2, family=Gamma(identity), data=Dataset_GLM)

AIC(A_PTC, A_PTCI, A_PTCN) # Vyrazeni dalsich interakci zhorsuje AIC -> nejlepsi je model s interakcemi pro vliv pasma na intercept a N

################################################
################################################
## Puvodni cast skriptu
################################################
################################################

# Konstrukce modelu (podle Bates (2010): lme4:Mixed-effects modeling with R, kapitola 3.2.):
# fixni efekty - populacni intercept a populacni smernice nezavislych promennych
# nahodne efekty (...|...) - odchylky od populacniho interceptu a smernice nezavisle promenne pro danou kategorii
# | pouzit, pokud korelace mezi nahodnymi efekty je vysoka (viz summary modelu), jinak ||


MELM.A <- lmer(TRW_A ~ (1 + T.3.5._A + Veg_Prec_A * NDEP_A + CO2|Pasmo) + 1 + T.3.5._A + Veg_Prec_A * NDEP_A + CO2, data=Dataset_GLM) 						# Model zalozeny na REML
summary(MELM.A) # Shrnuti
coef(MELM.A) # Koeficienty (fixni+nahodny)
fixef(MELM.A); ranef(MELM.A, condVar=T) # Koeficienty (zvlast fixni a nahodny)

shapiro.test(residuals(MELM.A)) # Normalita residualu

###
MELM.B <- lmer(TRW_B ~ (1 + T.3.5._B + NDEP_B + Veg_Prec_B + CO2|Pasmo) + 1 + T.3.5._B + Veg_Prec_B + NDEP_B + CO2, data=Dataset_GLM)
summary(MELM.B)
coef(MELM.B)
fixef(MELM.B); ranef(MELM.B)

shapiro.test(residuals(MELM.B))

##################
### GLM modely ###

GLM.A <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A * NDEP_A + CO2, family=Gamma(identity), data=Dataset_GLM) # Varianta se zachovanim NDEP v interakci se srazkami - nizsi AIC, vyssi BIC
# GLM.A <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A + CO2, family=Gamma(identity), data=Dataset_GLM) # Varianta s vypustenim NDEP - vyssi AIC, nizsi BIC

summary(GLM.A)
coef(GLM.A)
vif(GLM.A)

###
GLM.B <- glm(TRW_B ~ Pasmo + T.3.5._B  + Veg_Prec_B + NDEP_B + CO2, family=Gamma(identity), data=Dataset_GLM)
summary(GLM.B)
coef(GLM.B)
vif(GLM.B)

###################
### Citlivostni analyza

# Priprava modelu
MELM.A_co2 <- lmer(TRW_A ~ (1 + T.3.5._A + Veg_Prec_A * NDEP_A |Pasmo) + 1 + T.3.5._A + Veg_Prec_A * NDEP_A , data=Dataset_GLM)
MELM.A_N <- lmer(TRW_A ~ (1 + T.3.5._A + Veg_Prec_A + CO2|Pasmo) + 1 + T.3.5._A + Veg_Prec_A + CO2, data=Dataset_GLM)
MELM.A_co2_N <- lmer(TRW_A ~ (1 + T.3.5._A + Veg_Prec_A |Pasmo) + 1 + T.3.5._A + Veg_Prec_A, data=Dataset_GLM)

MELM.B_co2 <- lmer(TRW_B ~ (1 + T.3.5._B + NDEP_B + Veg_Prec_B |Pasmo) + 1 + T.3.5._B + Veg_Prec_B + NDEP_B , data=Dataset_GLM)
MELM.B_N <- lmer(TRW_B ~ (1 + T.3.5._B + Veg_Prec_B + CO2|Pasmo) + 1 + T.3.5._B + Veg_Prec_B + CO2, data=Dataset_GLM)
MELM.B_co2_N <- lmer(TRW_B ~ (1 + T.3.5._B + Veg_Prec_B |Pasmo) + 1 + T.3.5._B + Veg_Prec_B, data=Dataset_GLM)

GLM.A_co2 <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A * NDEP_A, family=Gamma(identity), data=Dataset_GLM)
GLM.A_N <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A + CO2, family=Gamma(identity), data=Dataset_GLM)
GLM.A_co2_N <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A, family=Gamma(identity), data=Dataset_GLM)

GLM.B_co2 <- glm(TRW_B ~ Pasmo + T.3.5._B  + Veg_Prec_B + NDEP_B, family=Gamma(identity), data=Dataset_GLM)
GLM.B_N <- glm(TRW_B ~ Pasmo + T.3.5._B  + Veg_Prec_B + CO2, family=Gamma(identity), data=Dataset_GLM)
GLM.B_co2_N <- glm(TRW_B ~ Pasmo + T.3.5._B  + Veg_Prec_B, family=Gamma(identity), data=Dataset_GLM)

# Doplnek o klimadata
MELM.A_P <- lmer(TRW_A ~ (1 + T.3.5._A +  NDEP_A + CO2|Pasmo) + 1 + T.3.5._A +  NDEP_A + CO2 , data=Dataset_GLM)
MELM.A_T <- lmer(TRW_A ~ (1  + Veg_Prec_A * NDEP_A + CO2|Pasmo) + 1 + Veg_Prec_A * NDEP_A + CO2, data=Dataset_GLM)

MELM.B_P <- lmer(TRW_B ~ (1 + T.3.5._B + NDEP_B + CO2|Pasmo) + 1 + T.3.5._B + NDEP_B + CO2 , data=Dataset_GLM)
MELM.B_T <- lmer(TRW_B ~ (1 + Veg_Prec_B + NDEP_B + CO2|Pasmo) + 1 + Veg_Prec_B + NDEP_B + CO2, data=Dataset_GLM)

GLM.A_P <- glm(TRW_A ~ Pasmo + T.3.5._A + NDEP_A + CO2, family=Gamma(identity), data=Dataset_GLM)
GLM.A_T <- glm(TRW_A ~ Pasmo + Veg_Prec_A * NDEP_A + CO2, family=Gamma(identity), data=Dataset_GLM)

GLM.B_P <- glm(TRW_B ~ Pasmo + T.3.5._B + NDEP_B + CO2, family=Gamma(identity), data=Dataset_GLM)
GLM.B_T <- glm(TRW_B ~ Pasmo + Veg_Prec_B + NDEP_B + CO2, family=Gamma(identity), data=Dataset_GLM)

### funkce
p <- function(model, AB, typ="G") {
	dataframe <- data.frame(ID=NA)

	if (typ=="M") {model <- update(model, REML=FALSE)}

	if (AB=="A")	{dataframe[1,1] <- (summary(lm(TRW_A ~ predict(model), data=Dataset_GLM))$adj.r.squared)}
	if (AB=="B")	{dataframe[1,1] <- (summary(lm(TRW_B ~ predict(model), data=Dataset_GLM))$adj.r.squared)}
	dataframe[2,1] <- (AIC(model))
	dataframe[3,1] <- (BIC(model))
	
	rownames(dataframe) <- c("R2", "AIC", "BIC")
	return(dataframe)
	}

### vypocet
modely <- cbind(p(MELM.A, "A", "M"), p(MELM.A_co2, "A", "M"), p(MELM.A_N, "A", "M"), p(MELM.A_co2_N, "A", "M"), p(MELM.B, "B", "M"), p(MELM.B_co2, "B", "M"), p(MELM.B_N, "B", "M"), p(MELM.B_co2_N, "B", "M"),
	p(GLM.A, "A"), p(GLM.A_co2, "A"), p(GLM.A_N, "A"), p(GLM.A_co2_N, "A"), p(GLM.B, "B"), p(GLM.B_co2, "B"), p(GLM.B_N, "B"), p(GLM.B_co2_N, "B"))

colnames(modely) <- c("MELM.A", "MELM.A_co2", "MELM.A_N", "MELM_A_co2_N", "MELM.B", "MELM.B_co2", "MELM.B_N", "MELM_B_co2_N", 
			"GLM.A", "GLM.A_co2", "GLM.A_N", "GLM_A_co2_N", "GLM.B", "GLM.B_co2", "GLM.B_N", "GLM_B_co2_N")

write.table(modely, "C:/Users/PC/Desktop/modely.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")

# doplneni o klima
modely_doplneni <- cbind(p(MELM.A, "A", "M"), p(MELM.A_T, "A", "M"), p(MELM.A_P, "A", "M"), p(MELM.B, "B", "M"), p(MELM.B_T, "B", "M"), p(MELM.B_P, "B", "M"),
	p(GLM.A, "A"), p(GLM.A_T, "A"), p(GLM.A_P, "A"), p(GLM.B, "B"), p(GLM.B_T, "B"), p(GLM.B_P, "B"))

colnames(modely_doplneni) <- c("MELM.A", "MELM.A_T", "MELM.A_P", "MELM.B", "MELM.B_T", "MELM.B_P", 
			"GLM.A", "GLM.A_T", "GLM.A_P", "GLM.B", "GLM.B_T", "GLM.B_P")


##################
### Citlivostni analyza - puvodni

citlivost <- function(model, odchylka, AB, transf=FALSE) {

	nd.CO2 <- Dataset_GLM; nd.CO2$CO2 <- with(nd.CO2, CO2*(1+odchylka))
	if (AB=="A") {
		nd.Veg_Prec <- Dataset_GLM; nd.Veg_Prec$Veg_Prec_A <- with(nd.Veg_Prec, Veg_Prec_A*(1+odchylka))
		nd.T <- Dataset_GLM; nd.T$T.3.5._A <- with(nd.T, T.3.5._A*(1+odchylka))
		nd.NDEP <- Dataset_GLM; nd.NDEP$NDEP_A <- with(nd.NDEP, NDEP_A*(1+odchylka))}

	if (AB=="B") {
		nd.Veg_Prec <- Dataset_GLM; nd.Veg_Prec$Veg_Prec_B <- with(nd.Veg_Prec, Veg_Prec_B*(1+odchylka))
		nd.T <- Dataset_GLM; nd.T$T.3.5._B <- with(nd.T, T.3.5._B*(1+odchylka))
		nd.NDEP <- Dataset_GLM; nd.NDEP$NDEP_B <- with(nd.NDEP, NDEP_B*(1+odchylka))}

	if (transf==TRUE) {
		nd.CO2[,c("CO2","NDEP_A","NDEP_B","T.3.5._A","T.3.5._B","Veg_Prec_A","Veg_Prec_B")] <- scale(nd.CO2[,c("CO2","NDEP_A","NDEP_B","T.3.5._A","T.3.5._B","Veg_Prec_A","Veg_Prec_B")])
		nd.Veg_Prec[,c("CO2","NDEP_A","NDEP_B","T.3.5._A","T.3.5._B","Veg_Prec_A","Veg_Prec_B")] <- scale(nd.Veg_Prec[,c("CO2","NDEP_A","NDEP_B","T.3.5._A","T.3.5._B","Veg_Prec_A","Veg_Prec_B")])
		nd.T[,c("CO2","NDEP_A","NDEP_B","T.3.5._A","T.3.5._B","Veg_Prec_A","Veg_Prec_B")] <- scale(nd.T[,c("CO2","NDEP_A","NDEP_B","T.3.5._A","T.3.5._B","Veg_Prec_A","Veg_Prec_B")])
		nd.NDEP[,c("CO2","NDEP_A","NDEP_B","T.3.5._A","T.3.5._B","Veg_Prec_A","Veg_Prec_B")] <- scale(nd.NDEP[,c("CO2","NDEP_A","NDEP_B","T.3.5._A","T.3.5._B","Veg_Prec_A","Veg_Prec_B")])}


	pred.orig <- predict(model)
	pred.CO2 <- predict(model, newdata=nd.CO2)
	pred.Veg_Prec <- predict(model, newdata=nd.Veg_Prec)
	pred.T <- predict(model, newdata=nd.T)
	pred.NDEP <- predict(model, newdata=nd.NDEP)

	podil.CO2 <- 100*((pred.CO2/pred.orig)-1)
	podil.Veg_Prec <- 100*((pred.Veg_Prec/pred.orig)-1)
	podil.T <- 100*((pred.T/pred.orig)-1)
	podil.NDEP <- 100*((pred.NDEP/pred.orig)-1)

	return(list(CO2=cbind(VSE=mean(podil.CO2), Pasmo1=mean(podil.CO2[1:53]), Pasmo2=mean(podil.CO2[54:106]), Pasmo3=mean(podil.CO2[107:159])),
		PREC=cbind(VSE=mean(podil.Veg_Prec), Pasmo1=mean(podil.Veg_Prec[1:53]), Pasmo2=mean(podil.Veg_Prec[54:106]), Pasmo3=mean(podil.Veg_Prec[107:159])),
		TEMP=cbind(VSE=mean(podil.T), Pasmo1=mean(podil.T[1:53]), Pasmo2=mean(podil.T[54:106]), Pasmo3=mean(podil.T[107:159])),
		NDEP=cbind(VSE=mean(podil.NDEP), Pasmo1=mean(podil.NDEP[1:53]), Pasmo2=mean(podil.NDEP[54:106]), Pasmo3=mean(podil.NDEP[107:159]))
		# ,chrono=cbind(podil.CO2, podil.Veg_Prec, podil.T, podil.NDEP)
	)) }


t <- FALSE

citlivost(GLM.A, 0.1, "A", t)
citlivost(GLM.B, 0.1, "B", t)
citlivost(MELM.A, 0.1, "A", t)
citlivost(MELM.B, 0.1, "B", t)

citlivost(GLM.A.pasmo, 0.1, "A", t)
citlivost(GLM.B.pasmo, 0.1, "B", t)


################# Normalita vstupnich dat ######################

Dataset_GLM <- Dataset[c(54:212),] # do roku 2013
Dataset_GLM <- subset(Dataset_GLM, subset=Pasmo==1)

with(Dataset_GLM, shapiro.test(CO2))
with(Dataset_GLM, shapiro.test(NDEP_A))
with(Dataset_GLM, shapiro.test(NDEP_B))
with(Dataset_GLM, shapiro.test(T.3.5._A))
with(Dataset_GLM, shapiro.test(T.3.5._B))
with(Dataset_GLM, shapiro.test(TRW_A))
with(Dataset_GLM, shapiro.test(TRW_B))
with(Dataset_GLM, shapiro.test(Veg_Prec_A))
with(Dataset_GLM, shapiro.test(Veg_Prec_B))

shapiro.test(residuals(GLM.A.Pasmo))

################# Kvalita modelu ######################

# Grafy
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(GLM.A.pasmo)
par(oldpar)

avPlots(GLM.A.pasmo, id.method="mahal", id.n=2)


library(effects, pos=18)
plot(allEffects(GLM.B.pasmo, partial.residuals=FALSE), span=0.5)

# Data
shapiro.test(residuals(GLM.A.pasmo))
shapiro.test(residuals(GLM.B.pasmo))


plot(residuals(GLM.A.pasmo) ~ Dataset_GLM$Pasmo); abline(0,0, col="red")
summary(lm(residuals(GLM.A.pasmo) ~ Dataset_GLM$Pasmo))


names(Dataset_GLM)
# VIF
vif(GLM.A.pasmo)
vif(GLM.B.pasmo)
#### Standardizace ####
#######################
# nepouzivame #

Dataset_out <- t(data.frame(rep(NA, 25))); colnames(Dataset_out) <- colnames(Dataset_GLM)

for (i in c(1:3)) {

Dataset_GLM_subset <- subset(Dataset_GLM, subset=Pasmo==i)

Dataset_GLM_subset <- local({
  .Z <- scale(Dataset_GLM_subset[,c("CO2","NDEP_A","NDEP_B","T.3.5._A","T.3.5._B","TRW_A","TRW_B","Veg_Prec_A","Veg_Prec_B","P.3.9._A","P.3.9._B")])
  within(Dataset_GLM_subset, {
    P.3.9._B <- .Z[,11]
    P.3.9._A <- .Z[,10]
    Veg_Prec_B <- .Z[,9]
    Veg_Prec_A <- .Z[,8]
  #  TRW_B <- .Z[,7]
  #  TRW_A <- .Z[,6]
    T.3.5._B <- .Z[,5]
    T.3.5._A <- .Z[,4]
    NDEP_B <- .Z[,3]
    NDEP_A <- .Z[,2]
    CO2 <- .Z[,1] 
  })
})
Dataset_out <- rbind(Dataset_out, Dataset_GLM_subset)
}

Dataset_GLM <- Dataset_out[c(2:nrow(Dataset_out)),]


########################
########################


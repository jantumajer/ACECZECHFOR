library(lme4); library(nlme)

Dataset <- readXL("F:/IFER/trendy/klimadata/Database_1961to2013.xlsx", rownames=FALSE, header=TRUE, na="", sheet="trend+vysokofrekvencni", stringsAsFactors=TRUE)

#### Regrese ####
#################
# Pocitana jenom za obdobi 1961-2010

Dataset_GLM <- Dataset[c(54:103, 107:156, 160:209),]

#### Standardizace ####

Dataset_GLM <- local({
  .Z <- scale(Dataset_GLM[,c("CO2","NDEP_A","NDEP_B","T.3.5._A","T.3.5._B","TRW_A","TRW_B","Veg_Prec_A","Veg_Prec_B")])
  within(Dataset_GLM, {
    Veg_Prec_B <- .Z[,9]
    Veg_Prec_A <- .Z[,8]
    TRW_B <- .Z[,7]
    TRW_A <- .Z[,6]
    T.3.5._B <- .Z[,5]
    T.3.5._A <- .Z[,4]
    NDEP_B <- .Z[,3]
    NDEP_A <- .Z[,2]
    CO2 <- .Z[,1] 
  })
})

########################
# Konstrukce modelu (podle Bates (2010): lme4:Mixed-effects modeling with R, kapitola 3.2.):
# fixni efekty - populacni intercept a populacni smernice nezavislych promennych
# nahodne efekty (...|...) - odchylky od populacniho interceptu a smernice nezavisle promenne pro danou kategorii
# | pouzit, pokud korelace mezi nahodnymi efekty je vysoka (viz summary modelu), jinak ||


MELM.A <- lmer(TRW_A ~ (1 + NDEP_A + T.3.5._A + Veg_Prec_A|Pasmo) + 1 + NDEP_A + T.3.5._A + Veg_Prec_A + CO2  , data=Dataset_GLM) 						# Model zalozeny na REML
summary(MELM.A) # Shrnuti
coef(MELM.A) # Koeficienty (fixni+nahodny)
fixef(MELM.A); ranef(MELM.A) # Koeficienty (zvlast fixni a nahodny)
# (conf.A <- confint(MELM.A, "beta_", 0.95)) # p-value - prechod pres nulu

pred.A <- predict(MELM.A) # Modelovane hodnoty + residualy
res.A <- residuals(MELM.A)
cor(Dataset_GLM$TRW_A,pred.A)
cor(Dataset_GLM$TRW_A,abs(res.A))
lineplot(Dataset_GLM$YEAR,res.A)
shapiro.test(res.A) # Normalita residualu

###
MELM.B <- lmer(TRW_B ~ (1 + NDEP_B + T.3.5._B + Veg_Prec_B|Pasmo) + 1 + NDEP_B + T.3.5._B + Veg_Prec_B + CO2, data=Dataset_GLM)
summary(MELM.B)
coef(MELM.B)
fixef(MELM.B); ranef(MELM.B)
# (conf.B <- confint(MELM.B, "beta_", 0.95))

pred.B <- predict(MELM.B)
res.B <- residuals(MELM.B)
cor(Dataset_GLM$TRW_B,pred.B)
cor(Dataset_GLM$TRW_B,res.B)
lineplot(Dataset_GLM$YEAR,res.B)
shapiro.test(res.B)

##################
### Mixed modely s Maximum-likelyhood

MELM.A.ml <- update(MELM.A, REML=FALSE)				# Mixed-effect model zalozeny na ML
MELM.B.ml <- update(MELM.B, REML=FALSE)

##################
### GLM modely ###
GLM.A <- glm(TRW_A ~ Pasmo + T.3.5._A + Veg_Prec_A + CO2 + NDEP_A , family=gaussian(identity), data=Dataset_GLM)
summary(GLM.A)
coef(GLM.A)
vif(GLM.A)
cor(Dataset_GLM$TRW_A,predict(GLM.A))

GLM.B <- glm(TRW_B ~ Pasmo + T.3.5._B + Veg_Prec_B + CO2 + NDEP_B  , family=gaussian(identity), data=Dataset_GLM)
summary(GLM.B)
coef(GLM.B)
vif(GLM.B)
cor(Dataset_GLM$TRW_B,predict(GLM.B))

################
### Srovnani ###

extractAIC(MELM.A.ml)
extractAIC(MELM.B.ml)
extractAIC(GLM.A)
extractAIC(GLM.B)

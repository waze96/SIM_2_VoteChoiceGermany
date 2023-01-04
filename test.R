# Load Required Packages: to be increased over the course "ggmap""plotly""xlsx""kableExtra",
options(contrasts=c("contr.treatment","contr.treatment"))
requiredPackages <- c("missMDA","effects","FactoMineR","car",
                      "factoextra","RColorBrewer","ggplot2",
                      "dplyr","ggthemes","knitr","MASS",
                      "chemometrics","rpart","ROCR","corrr",
                      "readxl","RColorBrewer","ggplot2","PerformanceAnalytics",                           "corrplot","reshape2","scales","stargazer",
                      "cowplot","rapportools", "packHV", "chemometrics","lmtest",
                      "AER","nnet","MNLpred")


package.check <- lapply(requiredPackages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

search()  #verify they are loaded


if(!is.null(dev.list())) dev.off() # Clear plots
rm(list=ls()) # Clean workspace

load("./gles.RData")

df <- gles

df$idx <- c(1:length(df$vote)) # create index by individual

summary(df)
str(df)


intervals <- c(0,2,4,6,8,10)

df$gegopos_immi <- factor(cut(df$egoposition_immigration,breaks=intervals))
table(df$gegopos_immi)

# mediana dintre de cada grup
cintervals<- tapply(df$egoposition_immigration,df$gegopos_immi,median)

# ngincome: numeric, mediana segons el grup al que pertany
df$ngegopos_immi<-factor(cut(df$egoposition_immigration,breaks=intervals))
df$ngegopos_immi<-factor(df$ngegopos_immi,labels=cintervals)
df$ngegopos_immi<-as.numeric(levels(df$ngegopos_immi))[df$ngegopos_immi]

# Set Factors
df$vote=as.factor(df$vote)
df$egoposition_immigration=as.factor(df$egoposition_immigration)
df$ostwest=as.factor(df$ostwest)
df$political_interest=as.factor(df$political_interest)
df$income=as.factor(df$income)
df$gender=as.factor(df$gender)
table(df$income)

# Rename levels for each factor
levels(df$vote) <- c("vote.AfD", "vote.CDU/CSU", "vote.FDP", "vote.Gruene", "vote.LINKE", "vote.SPD")
levels(df$egoposition_immigration) <- c("immgration.0", "immgration.1",
                                        "immgration.2", "immgration.3", "immgration.4", "immgration.5", "immgration.6", "immgration.7", "immgration.8", "immgration.9", "immgration.10")
levels(df$ostwest) <- c("ostwest.no", "ostwest.yes")
levels(df$political_interest) <- c("political_interest.low", "political_interest.low_medium","political_interest.medium", "political_interest.medium_high", "political_interest.high")
levels(df$income) <- c("income.low", "income.low_medium","income.medium", "income.medium_high", "income.high")
levels(df$gender) <- c("gender.male", "gender.female")

#Final structure
str(df)


#Dimension reduction of egoposition immigration from 10 to 3.
df$egoposition_immigration_red <- df$egoposition_immigration

levels(df$egoposition_immigration_red) <- list("immigration.open" = c("immgration.0", "immgration.1", "immgration.2"), 
                                               "immigration.mid" = c("immgration.3", "immgration.4","immgration.5"),
                                               "immigration.restrictive" = c("immgration.6", "immgration.7", "immgration.8", "immgration.9", "immgration.10"))

df$egoposition_immigration_red2 <- df$egoposition_immigration

levels(df$egoposition_immigration_red2) <- list("immigration.open" = c("immgration.0", "immgration.1", "immgration.2"), 
                                                "immigration.open_moderate" = c("immgration.3", "immgration.4"),
                                                "immigration.moderate" = c("immgration.3", "immgration.5"),
                                                "immigration.moderate_restrictive" = c("immgration.6", "immgration.7"),  "immigration.restrictive" = c("immgration.8", "immgration.9", "immgration.10"))

# Political Orientation
df$politicalOrientation <- df$vote

levels(df$politicalOrientation) <- list("politicalOrientation.right"=c("vote.AfD"),
                                        "politicalOrientation.center"=c("vote.CDU/CSU", "vote.FDP","vote.SPD"),
                                        "politicalOrientation.left"=c("vote.Gruene", "vote.LINKE"))





options(contrasts=c("contr.treatment","contr.treatment"))

###########################################################################################################
###########################################################################################################
###########################################################################################################
# LEFT+CENTER OR RIGHT MODEL
set.seed(1234567)

df$politicalOrientationBinary<-ifelse(df$politicalOrientation=="politicalOrientation.right",1,0)
df$politicalOrientationBinary<-factor(df$politicalOrientationBinary,labels=c("left_center","right"))

llwork <- sample(1:nrow(df),round(0.8*nrow(df),dig=0))
dfwork <- df[llwork,]
dftest <- df[-llwork,]

bm1.1 <- glm( politicalOrientationBinary ~ 1, family="binomial",data=dfwork)
bm1.2 <- glm( politicalOrientationBinary ~ egoposition_immigration+political_interest+income+gender+ostwest, family="binomial",data=dfwork)
bm1.3 <- glm( politicalOrientationBinary ~ egoposition_immigration + gender + ostwest, family="binomial",data=dfwork)

# Necesari? 
#bm2$null.dev - bm2$dev
#bm3$null.dev - bm3$dev


AIC(bm1.2, bm1.3) # bm3 288.8330
step(bm1.2)
anova(bm1.2, bm1.3, test="Chisq") # After step seems better to keep bm2??
plot(allEffects(bm1.3))


pred <- predict(bm1.3, newdata = dftest, type = "response")

dftest$predictions <- pred

ggplot(dftest, aes(x=idx, y=predictions, color=politicalOrientationBinary)) +
  geom_point() +
  geom_hline(yintercept=0.22)

predClass <- ifelse(pred > 0.22,'Right','Left_Center')
dftest$predictedClass<-predClass

tt <- table(predClass, dftest$politicalOrientationBinary)
tt

(accuracy <- sum(diag(tt)) / sum(tt))
(precision <- diag(tt) / rowSums(tt))
(recall <- (diag(tt) / colSums(tt)))
(f1 <- (2*precision*recall/(precision+recall)))

###########################################################################################################
###########################################################################################################
###########################################################################################################
# LEFT OR CENTER
set.seed(1234567)

newdf <- df[df$politicalOrientation %in% c('politicalOrientation.left','politicalOrientation.center'), ]
newdf$politicalOrientationBinary<-ifelse(newdf$politicalOrientation=="politicalOrientation.left",1,0)
newdf$politicalOrientationBinary<-factor(newdf$politicalOrientationBinary,labels=c("left","center"))

llwork <- sample(1:nrow(newdf),round(0.8*nrow(newdf),dig=0))
dfwork <- newdf[llwork,]
dftest <- newdf[-llwork,]

bm2.1 <- glm( politicalOrientation ~ 1, family="binomial",data=dfwork)
bm2.2 <- glm( politicalOrientation ~ egoposition_immigration+political_interest+income+gender+ostwest, family="binomial",data=dfwork)
bm2.3 <- glm( politicalOrientation ~ egoposition_immigration + gender + ostwest, family="binomial",data=dfwork)


# Necesari? 
#bm2.2$null.dev - bm2.2$dev
#bm2.3$null.dev - bm2.3$dev


AIC(bm2.2, bm2.3) # bm3 288.8330
step(bm2.2)
anova(bm2.2, bm2.3, test="Chisq") # After step seems better to keep bm2??
plot(allEffects(bm2.3))


pred <- predict(bm2.3, newdata = dftest, type = "response")

dftest$predictions <- pred
dftest$idx <- c(1:length(dftest$predictions))

ggplot(dftest, aes(x=idx, y=predictions, color=politicalOrientation)) +
  geom_point() +
  geom_hline(yintercept=0.31)

predClass <- ifelse(pred > 0.31,'Left','Center')
dftest$predictedClass <- predClass

tt <- table(dftest$predictedClass, dftest$politicalOrientationBinary)
tt

(accuracy <- sum(diag(tt)) / sum(tt))
(precision <- diag(tt) / rowSums(tt))
(recall <- (diag(tt) / colSums(tt)))
(f1 <- (2*precision*recall/(precision+recall)))


###########################################################################################################
###########################################################################################################
###########################################################################################################
# LEFT Party
set.seed(1234567)

newdf <- df[df$politicalOrientation =='politicalOrientation.left', ]
newdf$voteBinary<-ifelse(newdf$vote=="vote.Gruene",1,0)
newdf$voteBinary<-factor(newdf$voteBinary,labels=c("LINKE","GRUENE"))

llwork <- sample(1:nrow(newdf),round(0.8*nrow(newdf),dig=0))
dfwork <- newdf[llwork,]
dftest <- newdf[-llwork,]

bm3.1 <- glm( vote ~ 1, family="binomial",data=dfwork)
bm3.2 <- glm( vote ~ egoposition_immigration+political_interest+income+gender+ostwest, family="binomial",data=dfwork)
bm3.3 <- glm( vote ~ egoposition_immigration + gender + ostwest, family="binomial",data=dfwork)

# NECESARI?
bm3.2$null.dev - bm3.2$dev
bm3.3$null.dev - bm3.3$dev


AIC(bm3.2, bm3.3) # bm3 288.8330
step(bm3.2)
anova(bm3.2, bm3.3, test="Chisq") # After step seems better to keep bm2??
plot(allEffects(bm3.3))


pred <- predict(bm3.3, newdata = dftest, type = "response")

dftest$predictions <- pred
dftest$idx <- c(1:length(dftest$predictions))

ggplot(dftest, aes(x=idx, y=predictions, color=vote)) +
  geom_point() +
  geom_hline(yintercept=0.45)

predClass <- ifelse(pred <= 0.45,'GRUENE', 'LINKE')
dftest$predictedClass <- predClass

tt <- table(dftest$predictedClass, dftest$voteBinary)
tt

(accuracy <- sum(diag(tt)) / sum(tt))
(precision <- diag(tt) / rowSums(tt))
(recall <- (diag(tt) / colSums(tt)))
(f1 <- (2*precision*recall/(precision+recall)))


###########################################################################################################
###########################################################################################################
###########################################################################################################
# CENTER Party
set.seed(1234567)

newdf <- df[df$politicalOrientation =='politicalOrientation.center', ]
newdf$votMult<-ifelse(newdf$vote=="vote.SPD","vote.SPD",ifelse(newdf$vote=="vote.CDU/CSU","vote.CDU/CSU","vote.FDP"))
newdf$votMult<-as.factor(newdf$votMult)

llwork <- sample(1:nrow(newdf),round(0.8*nrow(newdf),dig=0))
dfwork <- newdf[llwork,]
dftest <- newdf[-llwork,]

ones<-ifelse(dfwork$votMult=="vote.FDP",2,1)
mm1 <- multinom( votMult ~ 1,data=dfwork,weight=ones )
mm2 <- multinom( votMult ~ egoposition_immigration+political_interest+income+gender+ostwest, data=dfwork,weight=ones )
mm3 <- multinom( votMult ~ egoposition_immigration + gender + ostwest, data=dfwork,weight=ones )



AIC(mm2, mm3) # bm3 288.8330
step(mm2)
anova(mm2, mm3, test="Chisq") # After step seems better to keep bm2??
plot(allEffects(mm3))


pred <- predict(mm3, newdata = dftest, type = "probs")

dftest$predictionsCDUCSU <- pred[,1]
dftest$predictionsFDP <- pred[,2]
dftest$predictionsSPD <- pred[,3]
dftest$idx <- c(1:length(dftest$predictionsCDUCSU))

ggplot(dftest, aes(x=idx, y=predictionsCDUCSU, color=votMult)) +
  geom_point() +
  geom_hline(yintercept=0.41)

ggplot(dftest, aes(x=idx, y=predictionsFDP, color=votMult)) +
  geom_point() +
  geom_hline(yintercept=0.1)

ggplot(dftest, aes(x=idx, y=predictionsSPD, color=votMult)) +
  geom_point() +
  geom_hline(yintercept=0.41)

#predClass <- ifelse(pred <= 0.45,'GRUENE', 'LINKE')
#dftest$predictedClass <- predClass

tt <- table(predict(mm3, newdata = dftest, type = "class"),dftest$votMult)
tt

(accuracy <- sum(diag(tt)) / sum(tt))
(precision <- diag(tt) / rowSums(tt))
(recall <- (diag(tt) / colSums(tt)))
(f1 <- (2*precision*recall/(precision+recall)))

###########################################################################################################3
###########################################################################################################3
## CHECK HIERARCHICAL MODEL
## (RIGHT OR LEFT/CENTER):                     [BINOMIAL]
          # RIGHT: AfD
          ## (LEFT OR CENTER):                 [BINOMIAL]
              # LEFT: (GRUENE OR LINKE)            [BINOMIAL]
              # CENTER: (SPD OR CDU/CSU OR FDP)    [MULTINOMIAL]

set.seed(1234567)
df$idx <- c(1:length(df$vote))

#########################
# (RIGHT OR LEFT/CENTER)
#########################
df$politicalOrientationBinary<-as.factor(ifelse(df$politicalOrientation=="politicalOrientation.right","right","left_center"))
pred <- predict(bm1.3, newdata = df, type = "response")
df$predictions <- pred

ggplot(df, aes(x=idx, y=predictions, color=politicalOrientationBinary)) +
  geom_point() +
  geom_hline(yintercept=0.22)

df$predictedPoliticalOrientation <- as.factor(ifelse(pred > 0.22,'pred.Right','pred.Left_Center')) # mantenir el inicial?
df$predictedParty <- as.factor(ifelse(df$predictedPoliticalOrientation == 'pred.Right','pred.AfD','-'))

#########################
# (LEFT OR CENTER)
#########################
df1<-df[df$predictedPoliticalOrientation=='pred.Left_Center',]
pred <- predict(bm2.3, newdata = df1, type = "response")
df1$predictions <- pred

ggplot(df1, aes(x=idx, y=predictions, color=politicalOrientation)) +
  geom_point() +
  geom_hline(yintercept=0.31)

df1$predictedPoliticalOrientation <- ifelse(pred > 0.31,'pred.Left','pred.Center')       # mantenir el inicial?


#########################
# (GRUENE OR LINKE)
#########################
df2<-df1[df1$predictedPoliticalOrientation=='pred.Left',]
pred <- predict(bm3.3, newdata = df2, type = "response")
df2$predictedParty <- ifelse(pred <= 0.45,'pred.Gruene','pred.LINKE')       # mantenir el inicial?

#########################
# (SPD OR CDU/CSU OR FDP)
#########################
df3<-df1[df1$predictedPoliticalOrientation=='pred.Center',]
pred <- predict(mm3, newdata = df3, type = "probs")
df3$predictedParty <- predict(mm3, newdata = df3, type = "class")

#########################
# JOIN DATASETS AND VALIDATE HIERARCHICAL MODEL
#########################
dfFull = rbind(df2,df3,df[df$predictedPoliticalOrientation=='pred.Right',])
dfFull$predictedPoliticalOrientation<-as.factor(dfFull$predictedPoliticalOrientation)
dfFull$predictedParty<-as.factor(dfFull$predictedParty)
levels(dfFull$predictedParty) <- c("pred.AfD","pred.Gruene","pred.LINKE","pred.CDU/CSU","pred.FDP","pred.SPD")

tt1 <- table(dfFull$politicalOrientation,dfFull$predictedPoliticalOrientation)
tt1 <-tt1[order(row.names(x = tt1)), order(colnames(x = tt1))]; tt1

(accuracy <- sum(diag(tt1)) / sum(tt1))
(precision <- diag(tt1) / rowSums(tt1))
(recall <- (diag(tt1) / colSums(tt1)))
(f1 <- (2*precision*recall/(precision+recall)))


tt2 <- table(dfFull$vote,dfFull$predictedParty)
tt2 <-tt2[order(row.names(x = tt2)), order(colnames(x = tt2))]; tt2
(accuracy <- sum(diag(tt2)) / sum(tt2))
(precision <- diag(tt2) / rowSums(tt2))
(recall <- (diag(tt2) / colSums(tt2)))
(f1 <- (2*precision*recall/(precision+recall)))




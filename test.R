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
set.seed(123456)

df$politicalOrientationBinary<-ifelse(df$politicalOrientation=="politicalOrientation.right",1,0)
df$politicalOrientationBinary<-factor(df$politicalOrientationBinary,labels=c("left_center","right"))

llwork <- sample(1:nrow(df),round(0.8*nrow(df),dig=0))
dfwork <- df[llwork,]
dftest <- df[-llwork,]

bm1 <- glm( politicalOrientationBinary ~ 1, family="binomial",data=dfwork)
bm2 <- glm( politicalOrientationBinary ~ egoposition_immigration+political_interest+income+gender+ostwest, family="binomial",data=dfwork)
bm3 <- glm( politicalOrientationBinary ~ egoposition_immigration + gender + ostwest, family="binomial",data=dfwork)


bm2$null.dev - bm2$dev
bm3$null.dev - bm3$dev


AIC(bm2, bm3) # bm3 288.8330
step(bm2)
anova(bm2, bm3, test="Chisq") # After step seems better to keep bm2??
plot(allEffects(bm3))


pred <- predict(bm3, newdata = dftest, type = "response")

dftest$predictions <- pred
dftest$idx <- c(1:length(dftest$predictions))

ggplot(dftest, aes(x=idx, y=predictions, color=politicalOrientationBinary)) +
  geom_point()

predClass <- ifelse(pred > 0.185,'Right','Left_Center')
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
set.seed(123456)

newdf <- df[df$politicalOrientation %in% c('politicalOrientation.left','politicalOrientation.center'), ]
newdf$politicalOrientationBinary<-ifelse(newdf$politicalOrientation=="politicalOrientation.left",1,0)
newdf$politicalOrientationBinary<-factor(newdf$politicalOrientationBinary,labels=c("left","center"))

llwork <- sample(1:nrow(newdf),round(0.8*nrow(newdf),dig=0))
dfwork <- newdf[llwork,]
dftest <- newdf[-llwork,]

bm1 <- glm( politicalOrientation ~ 1, family="binomial",data=dfwork)
bm2 <- glm( politicalOrientation ~ egoposition_immigration+political_interest+income+gender+ostwest, family="binomial",data=dfwork)
bm3 <- glm( politicalOrientation ~ egoposition_immigration + gender + ostwest, family="binomial",data=dfwork)


bm2$null.dev - bm2$dev
bm3$null.dev - bm3$dev


AIC(bm2, bm3) # bm3 288.8330
step(bm2)
anova(bm2, bm3, test="Chisq") # After step seems better to keep bm2??
plot(allEffects(bm3))


pred <- predict(bm3, newdata = dftest, type = "response")

dftest$predictions <- pred
dftest$idx <- c(1:length(dftest$predictions))

ggplot(dftest, aes(x=idx, y=predictions, color=politicalOrientation)) +
  geom_point()

predClass <- ifelse(pred > 0.31,'Left','Center')
dftest$predictedClass <- predClass

tt <- table(dftest$predictedClass, dftest$politicalOrientationBinary)
tt

(accuracy <- sum(diag(tt)) / sum(tt))
(precision <- diag(tt) / rowSums(tt))
(recall <- (diag(tt) / colSums(tt)))
(f1 <- (2*precision*recall/(precision+recall)))




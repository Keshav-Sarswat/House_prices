library(MASS)	# "Modern Applied Statistics in S"
library(psych)	# contains pairs.panels() function
library(car) 	# contains vif function
library(MPV)  	# contains PRESS function
library(Hmisc)

## reading the prepared data
setwd("C:/Users/kesha/OneDrive/Desktop/2020/S2/DATA_analysis_subject/group project/data nba")
fdata <- read.table("2018-2019 total edited")
mdata <- read.table("2018-2019 advance edited")
tdata <- read.table("2017-2018 total edited")
## renaming and selecting the main variables
fdata1 <- fdata[fdata[,"MIN"]==48,]
attach(fdata1)
train_data <- data.frame(PTS,"margin"=X..., "FG%" = FG., FT., FTA,X3P.,X3PA,OREB,OREBA,DREB,DREBA,AST,ASTA
                  ,BLK,BLKA,STL,STLA,TOV,TOVA,PF,PFA,PACE,PACEA,PIE,PIEA,
                    PTS.OFF.TO,PTS.OFF.TOA,X2ND.PTS,X2ND.PTSA,PITP,PITPA,"HT"=as.factor(H_I))

detach(fdata1)
tdata1 <- tdata[tdata[,"MIN"]==48,]
attach(tdata1)
test_data <- data.frame(PTS,"margin"=X..., "FG%" = FG., FT., FTA,X3P.,X3PA,OREB,OREBA,DREB,DREBA,AST,ASTA
                         ,BLK,BLKA,STL,STLA,TOV,TOVA,PF,PFA,PACE,PACEA,PIE,PIEA,
                         PTS.OFF.TO,PTS.OFF.TOA,X2ND.PTS,X2ND.PTSA,PITP,PITPA,"HT"=as.factor(H_I))

detach(tdata1)

## exploratory analysis
sum(fdata[,"MIN"]!=48)/nrow(fdata)
boxplot(margin~HT, data = train_data)
boxplot(PTS~HT, data = train_data)
cor(train_data[,-32])
cont1 <- c("PTS","margin","FG.", "FT.", "FTA","X3P.","X3PA","OREB","OREBA","DREB","DREBA","AST","ASTA")
cont2 <- c("PTS","margin","BLK","BLKA","STL","STLA","TOV","TOVA","PF","PFA","PACE","PACEA","PIE")
cont3 <- c("PTS","margin","PTS.OFF.TO","PTS.OFF.TOA","X2ND.PTS","X2ND.PTSA","PITP","PITPA","PIEA")
rcorr(as.matrix(train_data), type = "pearson")
## significant for PTS
# FG., FT., FTA, X3P., X3PA, OREB, OREBA, DREB, DREBA, AST, ASTA, BLK, BLKA, STL, STLA, TOV, TOVA, PF, PFA, PACE, PACEA, PIE, PIEA, PTS.OFF.TO, PTS.OFF.TOA, X2ND.PTS, X2ND.PTSA, PITP, PITPA, HT    
## significant for margin
# FG., FT., FTA, X3P., X3PA, DREB, DREBA, AST, ASTA, BLK, BLKA, STL, STLA, TOV, TOVA, PIE, PIEA, PTS.OFF.TO, PTS.OFF.TOA, X2ND.PTS, X2ND.PTSA, PITP, PITPA, HT
pairs.panels(train_data[,cont1], pch='.', gap=0, ellipses=F, col.points='grey'
             , lwd=2, hist.col='aliceblue')   
pairs.panels(train_data[,cont2], pch='.', gap=0, ellipses=F, col.points='grey'
             , lwd=2, hist.col='aliceblue')   
pairs.panels(train_data[,cont3], pch='.', gap=0, ellipses=F, col.points='grey'
             , lwd=2, hist.col='aliceblue')   

## fitting models
mod1 <- lm(margin~FG.+FT.+FTA+X3P.+X3PA+DREB+DREBA+AST+ASTA+BLK+BLKA+STL+STLA+TOV+TOVA+
             PIE+PIEA+PTS.OFF.TO+PTS.OFF.TOA+X2ND.PTS+X2ND.PTSA+PITP+PITPA+HT,data = train_data)
summary(mod1)
plot(mod1, cex=0.5)
vif(mod1)
train_error <- summary(mod1)$sigma^2
test_error <- sum((test_data$margin-predict(mod1, test_data))^2)/mod1$df.residual
train_error
test_error
##remove pie and piea big VIF and insignificant

mod2 <- update(mod1, .~. - PIE -PIEA)
summary(mod2)
plot(mod2,cex=0.5)
vif(mod2)
train_error2 <- summary(mod2)$sigma^2
test_error2 <- sum((test_data$margin-predict(mod2, test_data))^2)/mod2$df.residual
train_error2
test_error2
anova(mod1, mod2)
AIC(mod1)
AIC(mod2)
#residual and qq plots are good, not overfitting, need to look into 921, 1115, 1977 as potential outliers
# even though anova and AIC suggest mod1 due to the high VIF we will choose mod2
mod2.1 <- lm(margin~FG.+FT.+FTA+X3P.+X3PA+DREB+DREBA+AST+ASTA+BLK+BLKA+STL+STLA+TOV+TOVA
             +PTS.OFF.TO+PTS.OFF.TOA+X2ND.PTS+X2ND.PTSA+PITP+PITPA+HT,data = train_data[c(-921,-1115,-1977),])
summary(mod2.1)
train_error2.1 <- summary(mod2.1)$sigma^2
test_error2.1 <- sum((test_data$margin-predict(mod2.1, test_data))^2)/mod2.1$df.residual
train_error2.1
test_error2.1

mod3 <- lm(margin~PIE, data=train_data)
summary(mod3)
plot(mod3)
vif(mod3)
train_error3 <- sum((train_data$margin-predict(mod3, train_data))^2)
test_error3 <- sum((test_data$margin-predict(mod3, test_data))^2)
train_error3
test_error3

stepAIC(mod2,direction="backward", k=2)

mod4 <-lm(formula = margin ~ FG. + FT. + FTA + X3P. + X3PA + DREB + 
        DREBA + AST + ASTA + BLK + STL + STLA + TOV + TOVA + PTS.OFF.TO + 
        PTS.OFF.TOA + X2ND.PTS + X2ND.PTSA + PITP + PITPA + HT, data = train_data)
summary(mod4)
plot(mod4, cex=0.5)
vif(mod4)
train_error4 <- summary(mod4)$sigma^2
test_error4 <- sum((test_data$margin-predict(mod4, test_data))^2)/mod4$df.residual
train_error4
test_error4
anova(mod2, mod4)
anova(mod4, mod1)
AIC(mod4)
#mod4 better

# test the two borderline points
anova(update(mod4, .~. - STL), mod4)
anova(update(mod4, .~. - PTS.OFF.TO), mod4)
anova(update(mod4, .~. - STL -PTS.OFF.TO), mod4)
anova(update(mod4, .~. - STL -PTS.OFF.TO), update(mod4, .~. - STL))
#dont remove both try remove STL

AIC(mod4) # win
AIC(update(mod4, .~. - STL))
sum((train_data$margin-predict(update(mod4, .~. - STL), train_data))^2) #SSQ is bigger than mod4
sum((test_data$margin-predict(update(mod4, .~. - STL), test_data))^2)
#stick with mod4


length(train_data)
str(train_data)
attach(fdata)
str(fdata)
hist(X...)
model_1 <- lm(X...~FG.+FT.+FTA+X3P.+X3PA+OREB+OREBA+DREB+DREBA+AST+ASTA
              + BLK+BLKA+STL+STLA+TOV+TOVA+PF+PFA+PACE+PACEA+PIE+PIEA+
                PTS.OFF.TO+PTS.OFF.TOA+X2ND.PTS+X2ND.PTSA+PITP+PITPA+as.factor(H_I))
summary(model_1)

plot(model_1)


check <- stepAIC(model_1,direction="backward", k=2)

vif(model_2)
model_2 <- lm(X... ~ FG. + FT. + FTA + X3P. + X3PA + OREB + OREBA + DREB + 
                DREBA + AST + ASTA + BLK + BLKA + STL + STLA + TOV + TOVA + 
                PF + PFA + PACE + PIE + PTS.OFF.TOA + X2ND.PTS + X2ND.PTSA + 
                PITPA + as.factor(H_I))
summary(model_2)
plot(model_2)


model_3 <- lm(X... ~ FG. + FT. + FTA + X3P. + X3PA + OREB + OREBA + DREB + 
                           DREBA + AST + ASTA + BLK + BLKA + STL + STLA + TOV + TOVA + 
                           PF + PFA + PACE + PTS.OFF.TOA + X2ND.PTS + X2ND.PTSA + 
                           PITPA + as.factor(H_I))
check <- stepAIC(model_3,direction="backward", k=2)
model_4 <- lm(X... ~ FG. + FT. + FTA + X3P. + X3PA + OREB + DREB + DREBA + 
                AST + ASTA + BLK + BLKA + STL + STLA + TOV + TOVA + PF + 
              PACE + PTS.OFF.TOA + X2ND.PTS + X2ND.PTSA + PITPA + as.factor(H_I), data = fdata)
vif(model_4)
check <- stepAIC(lm(X...~1),direction="forward",scope=list(upper=model_4,lower=lm(X...~1)), k=2)
summary(model_4)
model4 <- lm(X... ~ PIE)
summary(model4)
summary(model_3)
fdata[c(1,96, 63, 2048, 2000),]
sum((fdata[,"X..."] - predict(model_3, fdata)))^2
anova(model_3)


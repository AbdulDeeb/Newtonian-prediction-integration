library(ggplot2)
library(cowplot)
library(ez)

rm(list=ls())
setwd('~/Documents/') #Directory where files Exp1-5 are held
setwd('~/Documents/BilliardProject/Data/')
load('Exp2')

kissData = Exp2

kissData$deflectionAngle_inv10 = kissData$deflectionAngle
kissData$responseAngle_inv10 = kissData$responseAngle
kissData$deflectionAngle_inv10[as.character(kissData$aimingAngle)=='10'] = -as.numeric(as.character(kissData$deflectionAngle))[as.character(kissData$aimingAngle)=='10']
kissData$responseAngle_inv10[as.character(kissData$aimingAngle)=='10'] = -kissData$responseAngle_inv10[as.character(kissData$aimingAngle)=='10']

# Appears to be a small bias even in Kiss Shot
dat0 = with(kissData,as.table(by(responseAngle_inv10,list(subjName,deflectionAngle_inv10,occlusion),mean)))
kissMeans=as.data.frame.table(dat0)
names(kissMeans) = c('Subject','Deflection','Duration','Response')
kissMeans$Deflection = as.numeric(as.character(kissMeans$Deflection))
ggplot(data=kissMeans,aes(x=Deflection,y=Response,col=Duration)) +
  stat_summary(geom='pointrange') +
  geom_abline(slope=1,intercept=0) +
  geom_abline(slope=0,intercept=22.8,col='red') +
  theme_cowplot()

# Here we take the within-SD for each cell, then do the x-axis flip for +10, then combine AimingAngles
# i.e., We merge SDs for Displayed that have opposite sign, but same eccentricity and bias wrt Newtonian
dat00 = with(kissData,as.table(by(responseAngle,list(subjName,deflectionAngle,occlusion,aimingAngle),sd)))
kissSD=as.data.frame.table(dat00)
names(kissSD) = c('Subject','Deflection','Duration','Aiming','SD')
kissSD$Deflection = round(as.numeric(as.character(kissSD$Deflection)),2)
kissSD$Deflection_inv = kissSD$Deflection
kissSD$Deflection_inv[as.character(kissSD$aimingAngle)=='10'] = -kissSD$Deflection[as.character(kissSD$aimingAngle)=='10']
dat000 = with(kissSD,as.table(by(SD,list(Subject,Deflection_inv,Duration),mean)))
kissSD = as.data.frame.table(dat000)
names(kissSD) = c('Subject','Deflection','Duration','SD')
kissSD$Deflection = as.numeric(as.character(kissSD$Deflection))

# Fit a linear model to sigma_S, we will use this instead of cell means
kissSD$Eccentricity = abs(kissSD$Deflection)
sigma_S_mdl = lm(SD~Eccentricity+Duration,data=kissSD)
summary(sigma_S_mdl)
coef = sigma_S_mdl$coef
sd_intercept = unname(coef[1])
b_ecc = unname(coef[2])
b_dur80 = unname(coef[3])

# Uses the linear fit above to draw lines on Figure 2b
drawLinearFit = function(x,durationIs80){
  sd_intercept + b_ecc*abs(x) + durationIs80*b_dur80
}

###THIS ONE HERE FOR SD GRAPH
ggplot(data=kissSD,aes(x=Deflection,y=SD,col=Duration)) +
  scale_color_manual(values=c('black','darkgray')) +
  stat_function(fun=drawLinearFit,args=list(0),lty='solid',col='black') +
  stat_function(fun=drawLinearFit,args=list(1),lty='solid',col='darkgray') +
  geom_abline(slope=0,intercept=0) +
  stat_summary(fun.data='mean_se',geom='point') +
  stat_summary(fun.data='mean_se',geom='errorbar',width=3) +
  xlab("Displayedº" ) + ylab("Standard Deviation" )+
  theme_minimal()

# New ANOVA on Standard Deviations for effects of Eccentricity and Duration
library(ez)
kissSD$Eccentricity = as.ordered(kissSD$Eccentricity)
ezANOVA(data=kissSD,dv=SD,within=list(Duration,Eccentricity),wid=Subject)

load('Exp1')
allData <- Exp1
# Invert the +10 aiming angle data (displayed and response)
allData$deflectionAngle_inv10 = allData$deflectionAngle
allData$responseAngle_inv10 = allData$responseAngle
allData$deflectionAngle_inv10[as.character(allData$aimingAngle)=='10'] = -as.numeric(as.character(allData$deflectionAngle))[as.character(allData$aimingAngle)=='10']
allData$responseAngle_inv10[as.character(allData$aimingAngle)=='10'] = -allData$responseAngle_inv10[as.character(allData$aimingAngle)=='10']

# Get the quantities we need for the model
dat1=with(allData,as.table(by(responseAngle_inv10,list(subjName,deflectionAngle_inv10,occlusion),mean)))
se = function(x){return (sd(x)/sqrt(length(x)))}
Response = apply(dat1,c(2,3),mean)
SE = apply(dat1,c(2,3),se)
Duration = rep(c(40,80),each=13)
Sensory = rep(as.numeric(rownames(Response)),2)
Newtonian = rep(22.86,26)

preddat=data.frame(Deflection=rep(unique(kissSD$Deflection),2),
                   Duration=rep(unique(kissSD$Duration),each=13))
preddat$Eccentricity = abs(preddat$Deflection)
preddat$Duration = as.factor(preddat$Duration)
preddat[,c('fit','lwr','upr')]=predict(sigma_S_mdl,newdata=preddat,interval='confidence',level=.68)

sigS_pred = preddat$fit
sigS_pred_lwr = preddat$lwr
sigS_pred_upr = preddat$upr

lol=data.frame(x = as.numeric(Response)-Sensory,
               y = preddat$fit^2 * (Newtonian-as.numeric(Response)),
               ylwr = preddat$lwr^2 * (Newtonian-as.numeric(Response)),
               yupr = preddat$upr^2 * (Newtonian-as.numeric(Response)))
ggplot(data=lol,aes(x=x,y=y)) +
  geom_point()
mdl = lm(y ~ 0+x,data=lol)
mdl_out = summary(mdl)
coef = mdl_out$coefficients
sigma_N_sq_ols = coef[1]
sigma_N_sq_SE = coef[2]

mdl = lm(ylwr ~ 0+x,data=lol)
mdl_out = summary(mdl)
coef = mdl_out$coefficients
sigma_N_sq_lwr = coef[1]-coef[2]

mdl = lm(yupr ~ 0+x,data=lol)
mdl_out = summary(mdl)
coef = mdl_out$coefficients
sigma_N_sq_upr = coef[1]+coef[2]
sigma_N_sq = sigma_N_sq_ols

# Determine the weights based on the fitted value of sigma_N_sq
wN = sigS_pred^2/(sigS_pred^2 + sigma_N_sq)
wN_UB = sigS_pred_upr^2/(sigS_pred_upr^2 + sigma_N_sq_lwr)
wN_LB = sigS_pred_lwr^2/(sigS_pred_lwr^2 + sigma_N_sq_upr)
Predicted = wN*Newtonian + (1-wN)*Sensory
PredLB = wN_LB*Newtonian + (1-wN_LB)*Sensory
PredUB = wN_UB*Newtonian + (1-wN_UB)*Sensory
PredictedSE = PredUB - PredLB

PredictedBias = Predicted-Sensory
PredBiasLB = PredLB-Sensory
PredBiasUB = PredUB-Sensory
PredBiasSE = PredBiasUB-PredBiasLB

newData = data.frame(Response = as.numeric(Response),
                     SE = as.numeric(SE),
                     Duration = as.factor(Duration),
                     Sensory = Sensory,
                     Newtonian = Newtonian,
                     Predicted = Predicted,
                     PredLB = PredLB,
                     PredUB = PredUB,
                     PredictedSE = PredictedSE)

# Fit to data
ggplot(data=newData,aes(x=Sensory,y=Predicted,ymax=PredUB,ymin=PredLB,fill=Duration)) +
  geom_abline(slope=1,intercept=0,lty='longdash') + 
  geom_ribbon(alpha=.67) +
  geom_line(aes(col=Duration)) +
  geom_pointrange(aes(y=Response,ymax=Response+SE,ymin=Response-SE,col=Duration)) +
  xlim(-50, 50) + ylim(-50, 50) + coord_equal() +
  theme_minimal()

# Plot fitted weights over empirical weights
newData2 <- newData[, c(3,4)] 
newData2$sigS_pred <- sigS_pred
newData2$prediction <- 1/((( (sqrt(sigma_N_sq))/newData2$sigS_pred)^2) +1)

# Empirical Newtonian Weights
disp = aperm(array(rep(Sensory[1:13],22),dim=c(13,11,2)),c(2,1,3));
newt = array(22.86,c(11,13,2));

# On the individual subject data, then average
wn = (dat1-disp)#/(newt-disp);
apply(wn,c(2,3),mean)
apply(wn,c(2,3),se)


# Biases (observed with model fit)
empiricalWeights = as.data.frame(wn)
empiricalWeights$Var2 = as.numeric(as.character(empiricalWeights$Var2))
ggplot(data=newData) +
  geom_ribbon(aes(x=Sensory,ymax=PredBiasUB,ymin=PredBiasLB,fill=Duration),alpha=.67) +
  geom_line(aes(x=Sensory,y=PredictedBias,col=Duration)) +
  theme_minimal() +
  stat_summary(data=empiricalWeights,aes(x=Var2,y=Freq,col=Var3),fun.data=mean_se) + 
  coord_cartesian(xlim =c(-50, 50), ylim=c(-15, 30)) +
  theme(aspect.ratio=1)

# Correlation of predicted biases (x) with observed biases (y)
empiricalWeights$Bias = empiricalWeights$Freq
empiricalWeights$Duration = empiricalWeights$Var3
Predicted_Bias =c(rep(PredictedBias[1:13],each=11),rep(PredictedBias[14:26],each=11))
ggplot(data=newData) +
  scale_color_manual(values=c('black','darkgray')) +
  geom_abline(slope=1,lty='longdash') +
  stat_summary(data=empiricalWeights,aes(x=Predicted_Bias,y=Bias,col=Duration),fun.data=mean_se) + 
  stat_summary(data=empiricalWeights,aes(x=Predicted_Bias,y=Bias,col=Duration),geom='errorbar',width=2) +
  theme_minimal() +
  xlab("Predicted Bias" ) + ylab("Emperical Bias")+
  theme(aspect.ratio=1)

# Weights (empirical vs. model fit)
wn = (dat1-disp)/(newt-disp);
empiricalWeights = as.data.frame(wn)
empiricalWeights$Var2 = as.numeric(as.character(empiricalWeights$Var2))
ggplot(data=newData) +
  geom_ribbon(aes(x=Sensory,ymax=wN_UB,ymin=wN_LB,fill=Duration),alpha=.67) +
  geom_line(aes(x=Sensory,y=wN,col=Duration)) +
  theme_minimal() +
  stat_summary(data=empiricalWeights,aes(x=Var2,y=Freq,col=Var3),fun.data=mean_se) + 
  coord_cartesian(xlim =c(-50, 50), ylim=c(0, 1)) +
  theme(aspect.ratio=1)

#Exp 3

load('Exp3')




allData2 <- as.data.frame(with(Exp3,as.table(by(compAngle,list(subjName,aimingAngle,deflectionAngle),mean,na.rm=T))) )
names(allData2) = c('subjName', 'aimingAngle','deflectionAngle', 'compAngle')
allData2$deflectionAngle <-as.numeric(as.character(allData2$deflectionAngle)) 


levels(allData2$aimingAngle) = c('-10°','+10°')

allData2$responseAngle = allData2$compAngle
featData <- allData2
featData <- as.data.frame.table(with(featData,as.table(by(responseAngle,list(subjName,deflectionAngle, aimingAngle),mean,na.rm=T))))
names(featData) = c('subject','deflectionAngle','aimingAngle', 'responseAngle')
featData$deflectionAngle = as.numeric(as.character(featData$deflectionAngle))


featData2 = featData
featData2$responseAngle[featData2$aimingAngle=='+10°'] = -1*featData2$responseAngle[featData2$aimingAngle=='+10°']
featData2$deflectionAngle[featData2$aimingAngle=='+10°'] = -1*featData2$deflectionAngle[featData2$aimingAngle=='+10°']
featData2 = as.data.frame.table(with(featData2,as.table(by(responseAngle,list(subject,deflectionAngle),mean,na.rm=T))))
names(featData2) = c('subject','deflectionAngle','responseAngle')
featData2$deflectionAngle = as.numeric(as.character(featData2$deflectionAngle))

featData3 <- with(featData2,aggregate(responseAngle, 
                                      by=list(deflectionAngle = deflectionAngle
                                      ), mean))


allData2$aimingAngle = as.factor(allData2$aimingAngle)
allData2$deflectionAngle <- as.factor(allData2$deflectionAngle)
aov = ezANOVA(data = allData2, dv = compAngle, wid = subjName, within = .(aimingAngle,deflectionAngle))

#LINE GRAPH

ggplot(featData2,aes(x=deflectionAngle,y=responseAngle)) +
  geom_abline(intercept=0,slope=1,lty='longdash',col='gray') +
  geom_abline(intercept=22.86,slope=0,lty='longdash',col='#F8766D') +
  stat_summary(fun.data='mean_se',geom='point') +
  stat_summary(fun.data='mean_se',geom='errorbar',width=1.5) +
  stat_summary(fun.data='mean_se',geom='line') +
  coord_cartesian(xlim=c(-25,25),ylim=c(-25,25)) +
  theme_minimal()


#Exp 4

load('Exp4')

ggplot(Exp4,  aes(x=(deflectionAngle), y=as.numeric(responseAngle), color = occlusion, group = occlusion)) +
  scale_color_manual(values=c('black','darkgray')) +
  stat_summary(fun.data='mean_se',geom='point') + 
  xlab("Deflection Angle" ) + ylab("Response Angle" ) +
  theme_minimal()


#Exp 5

load('Exp5')
featData <- Exp5
featData$deflectionAngle <- as.numeric(as.character(featData$deflectionAngle))
levels(featData$occlusion) = c('40 ms','80 ms')
levels(featData$aimingAngle) = c('-10°','+10°')
# PLOT 1: Two curves, 40 ms condition only
ggplot(subset(featData,occlusion=='40 ms'),aes(x=deflectionAngle,y=responseAngle,col=aimingAngle,group=aimingAngle)) +
  geom_abline(intercept=0,slope=1,lty='longdash') +
  geom_abline(intercept=22.86,slope=0,lty='longdash',col='#F8766D') +
  geom_abline(intercept=-22.86,slope=0,lty='longdash',col='#00BFC4') +
  stat_summary(fun.data='mean_se',geom='point') +
  stat_summary(fun.data='mean_se',geom='errorbar',width=3) +
  stat_summary(fun.data='mean_se',geom='line') +
  coord_cartesian(xlim=c(-50,50),ylim=c(-50,50)) +
  theme_minimal()

# Invert the +10 aiming angle data
featData2 = featData
featData2$responseAngle[featData2$aimingAngle=='+10°'] = -1*featData2$responseAngle[featData2$aimingAngle=='+10°']
featData2$deflectionAngle[featData2$aimingAngle=='+10°'] = -1*featData2$deflectionAngle[featData2$aimingAngle=='+10°']

# PLOT 2: Two curves, +10 aiming angle inverted, 40 ms condition only
ggplot(subset(featData2,occlusion=='40 ms'),aes(x=deflectionAngle,y=responseAngle,col=aimingAngle,group=aimingAngle)) +
  geom_abline(intercept=0,slope=1,lty='longdash') +
  geom_abline(intercept=22.86,slope=0,lty='longdash',col='#F8766D') +
  stat_summary(fun.data='mean_se',geom='point') +
  stat_summary(fun.data='mean_se',geom='errorbar',width=3) +
  stat_summary(fun.data='mean_se',geom='line') +
  coord_cartesian(xlim=c(-50,50),ylim=c(-50,50)) +
  theme_minimal()

# Now that we've inverted, average over aiming angles
featData2 = as.data.frame.table(with(featData2,as.table(by(responseAngle,list(subjName,deflectionAngle,occlusion),mean,na.rm=T))))
names(featData2) = c('subject','deflectionAngle','occlusion','responseAngle')
featData2$deflectionAngle = as.numeric(as.character(featData2$deflectionAngle))

featData2$difference <- featData2$deflectionAngle -22.86
featData2$differenceSign <-sign(featData2$difference)

ggplot(featData2,aes(x=deflectionAngle,y=responseAngle,col=occlusion,group=occlusion)) +
  scale_color_manual(values=c('black','darkgray')) +
  geom_abline(intercept=0,slope=1,lty='longdash') +
  geom_abline(intercept=22.86,slope=0,lty='longdash',col='#F8766D') +
  stat_summary(fun.data='mean_se',geom='point') +
  stat_summary(fun.data='mean_se',geom='errorbar',width=3) +
  stat_summary(fun.data='mean_se',geom='line') +
  coord_cartesian(xlim=c(-50,50),ylim=c(-50,50)) +
  theme_minimal()



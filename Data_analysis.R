rm(list=ls())
setwd('~/Documents/R/abdul')

library(ggplot2)
library(cowplot)

load('allData, kissShot')
kissData = allData

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

load('allData, Main')

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
#sigS_pred = (sd_intercept+b_ecc*abs(Sensory)+b_dur80*(Duration==80))

preddat=data.frame(Deflection=rep(unique(kissSD$Deflection),2),
                   Duration=rep(unique(kissSD$Duration),each=13))
preddat$Eccentricity = abs(preddat$Deflection)
preddat$Duration = as.factor(preddat$Duration)
preddat[,c('fit','lwr','upr')]=predict(sigma_S_mdl,newdata=preddat,interval='confidence',level=.68)

#sigS_pred = (sd_intercept+b_ecc*abs(Sensory)+b_dur80*(Duration==80))
sigS_pred = preddat$fit
sigS_pred_lwr = preddat$lwr
sigS_pred_upr = preddat$upr

# See Evan's notebook for derivation of this linear model...
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

#----Since both X and Y are transformations of the data, maybe we want slope of PC1 line?
# varcovmat = with(lol,rbind(c(var(x),cov(x,y)),
#                            c(cov(x,y),var(y))))
# firstEigen = eigen(varcovmat)$vectors[,1]
# sigma_N_sq_pc1 = firstEigen[2]/firstEigen[1]
#----...They are really similar

sigma_N_sq = sigma_N_sq_ols

# Determine the weights based on the fitted value of sigma_N_sq
wN = sigS_pred^2/(sigS_pred^2 + sigma_N_sq)
#wN_UB = sigS_pred^2/(sigS_pred^2 + sigma_N_sq-sigma_N_sq_SE)
wN_UB = sigS_pred_upr^2/(sigS_pred_upr^2 + sigma_N_sq_lwr)
#wN_LB = sigS_pred^2/(sigS_pred^2 + sigma_N_sq+sigma_N_sq_SE)
wN_LB = sigS_pred_lwr^2/(sigS_pred_lwr^2 + sigma_N_sq_upr)
Predicted = wN*Newtonian + (1-wN)*Sensory
PredLB = wN_LB*Newtonian + (1-wN_LB)*Sensory
PredUB = wN_UB*Newtonian + (1-wN_UB)*Sensory
PredictedSE = PredUB - PredLB

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
wn = (dat1-disp)/(newt-disp);
apply(wn,c(2,3),mean)
apply(wn,c(2,3),se)
# On the averages (it's the same as above)
#(Response-matrix(Sensory,13,2))/(matrix(Newtonian,13,2)-matrix(Sensory,13,2))
# We could try a constrained fit...
#library(pracma)

empiricalWeights = as.data.frame(wn)
empiricalWeights$Var2 = as.numeric(as.character(empiricalWeights$Var2))
ggplot(data=newData) +
  #scale_color_manual(values=c('black','darkgray')) +
  geom_ribbon(aes(x=Sensory,ymax=wN_UB,ymin=wN_LB,fill=Duration),alpha=.67) +
  geom_line(aes(x=Sensory,y=wN,col=Duration)) +
  theme_minimal() +
  stat_summary(data=empiricalWeights,aes(x=Var2,y=Freq,col=Var3)) + 
  coord_cartesian(xlim =c(-50, 50), ylim=c(0, 1)) +
  theme(aspect.ratio=1)

# Fit Weights
# ggplot(data=newData,aes(x=Sensory,y=wN,ymax=wN_UB,ymin=wN_LB,fill=Duration)) +
#   geom_abline(slope=1,intercept=0) + 
#   geom_ribbon(alpha=.33) +
#   geom_line(aes(col=Duration)) +
#   #geom_pointrange(aes(y=Response,ymax=Response+SE,ymin=Response-SE,col=Duration)) +
#   theme_cowplot()
# mod <- lm(SD ~ Eccentricity*Duration, data = kissSD)
# summary(mod)
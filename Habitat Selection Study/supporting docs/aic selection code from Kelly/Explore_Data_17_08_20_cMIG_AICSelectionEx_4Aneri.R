setwd("/Users/KellyPC/Documents/Documents/GIS_MODIFIED/Layers_MODIFIED/ANALYSIS/Ch2_RSFs/Explore_Data_17_08_20")
# Use Points you wish to extract values to
df <- read.csv("ALL_USE_AVAIL_LocData_wCov_rem2400mAllNoVeg_17_08_20_v2.csv")
# Install Packages
library(dplyr)
# Use v2 csv, has both standardized and unstandardized covariates
# The first version was made to get the mean and sd to make the standardized covariates


# Data Structure and Filter -----------------------------------------------------------
str(df)
df$STUDY <- as.factor(df$STUDY)
df$M1R0 <- as.factor(df$M1R0)
df$USE <- as.factor(df$USE)
str(df)


# No Home Ranges made for 1993, all ELK_IDYY have <5 USE locations
df <- subset(df,!(YY==1993)) %>% 
  droplevels()


# Remove Elk_IDYY if less than 5 locations
# Already did this for AVAIL Locs as I didn't have a HR to project points
# all pts removed should be USE pts
df <- df %>% 
  group_by(ELK_IDYY) %>% 
  filter(n()>5) %>% 
  ungroup() %>% 
  droplevels()


# Lump Study 1+2 together, make 3.1 to 3
# Combine study 1+2 -> heb, 3.1 -> tara, 4 -> stent
levels(df$STUDY)
levels(df$STUDY)[4] <-"stent"

levels(df$STUDY)
levels(df$STUDY)[3] <-"tara"

levels(df$STUDY)
levels(df$STUDY)[2] <-"heb"

levels(df$STUDY)
levels(df$STUDY)[1] <-"heb"


# Omit Rows with NA
df <- na.omit(df)

# Drop RIPR and ROADDEN bcs not available to all my animals
# Drop all non standardized values
df$RIPR <- NULL
df$ROADDEN <- NULL
df$sROADDEN <- NULL
df$SOLRAD <- NULL
df$ROADDEC<- NULL
df$ELEV <- NULL


# Make Datasets
# Use and Avail
use <- subset(df,USE=="1") %>% 
  droplevels()
avl <- subset(df,USE=="0") %>% 
  droplevels()

# Mig and Res
mig <- subset(df,M1R0=="1") %>% 
  droplevels()
res <- subset(df,M1R0=="0") %>% 
  droplevels()

# Mig Use and Avail
mig_use <- subset(mig,USE=="1") %>% 
  droplevels()
mig_avl <- subset(mig,USE=="0") %>% 
  droplevels()

# Res Use and Avail
res_use <- subset(res,USE=="1") %>% 
  droplevels()
res_avl <- subset(res,USE=="0") %>% 
  droplevels()

write.csv(mig ,"migmodeldatatest.csv", row.names=F)###HERE###
write.csv(res ,"resmodeldatatest.csv", row.names=F)###HERE###

# Data Histograms ---------------------------------------------------------
# Graph the histograms
# Splits the graphic window or two graphs
par(mfrow=c(2,2))
# Use if "figure margins too large" error
### par(mar=c(2.1,4.1,4.1,2.1)) 
# Histograms of Use and Avail for each covariate

#sSOLRAD
hist(mig_avl$sSOLRAD)
hist(mig_use$sSOLRAD)

hist(res_avl$sSOLRAD)
hist(res_use$sSOLRAD)

#sELEV
hist(mig_avl$sELEV)
hist(mig_use$sELEV)

hist(res_avl$sELEV)
hist(res_use$sELEV)

#NOVEG aka HRBSHR
hist(mig_avl$HRBSHR)
hist(mig_use$HRBSHR)

hist(res_avl$HRBSHR)
hist(res_use$HRBSHR)

#TREE
hist(mig_avl$TREE)
hist(mig_use$TREE)

hist(res_avl$TREE)
hist(res_use$TREE)

#CB
hist(mig_avl$CB)
hist(mig_use$CB)

hist(res_avl$CB)
hist(res_use$CB)

#WF
hist(mig_avl$WF)
hist(mig_use$WF)

hist(res_avl$WF)
hist(res_use$WF)

#sROADDEC
hist(mig_avl$sROADDEC)
hist(mig_use$sROADDEC)

hist(res_avl$sROADDEC)
hist(res_use$sROADDEC)




# Pearson's Correlation Matrix (Migrant) ----------------------------------
mig_newdata <- mig[c(7:13)]
#make a subset dataframe with just our numeric variables
cor(mig_newdata[sapply(mig_newdata, is.numeric)], method = c("pearson"))
library(Hmisc)
# rcorr(as.matrix(mig_newdata))
# 
# > rcorr(as.matrix(mig_newdata))
#          sSOLRAD sELEV HRBSHR  TREE    CB    WF sROADDEC
# sSOLRAD     1.00  0.10  -0.06  0.06 -0.01  0.01     0.10
# sELEV       0.10  1.00  -0.28  0.27 -0.12  0.03     0.65
# HRBSHR     -0.06 -0.28   1.00 -0.81 -0.16 -0.11    -0.32
# TREE        0.06  0.27  -0.81  1.00 -0.32 -0.20     0.29
# CB         -0.01 -0.12  -0.16 -0.32  1.00 -0.04    -0.05
# WF          0.01  0.03  -0.11 -0.20 -0.04  1.00     0.06
# sROADDEC    0.10  0.65  -0.32  0.29 -0.05  0.06     1.00
# 
# n= 515536 


#  Correlated variables
# -0.81 = HRBSHR + TREE
#  0.65 = (s)ELEV + (s)ROADDEC



# Pearson's Correlation Matrix (Resident) ---------------------------------
res_newdata <- res[c(7:13)]
#make a subset dataframe with just our numeric variables
cor(res_newdata[sapply(res_newdata, is.numeric)], method = c("pearson"))
library(Hmisc)
rcorr(as.matrix(res_newdata))

# > rcorr(as.matrix(res_newdata))
#           sSOLRAD sELEV HRBSHR  TREE    CB    WF sROADDEC
# sSOLRAD     1.00  0.12  -0.09  0.08  0.01  0.00     0.12
# sELEV       0.12  1.00  -0.39  0.37  0.03 -0.02     0.57
# HRBSHR     -0.09 -0.39   1.00 -0.77 -0.33 -0.06    -0.35
# TREE        0.08  0.37  -0.77  1.00 -0.31 -0.06     0.29
# CB          0.01  0.03  -0.33 -0.31  1.00 -0.02     0.12
# WF          0.00 -0.02  -0.06 -0.06 -0.02  1.00    -0.03
# sROADDEC    0.12  0.57  -0.35  0.29  0.12 -0.03     1.00

#n= 48735 

#  Correlated variables
# -0.77 = HRBSHR + TREE
#  0.57 = (s)ELEV + (s)ROADDEC
# -0.39 = (s)ELEV + HRBSHR


###### APPENDIX ####### -----------------------------------------------------------
#mig  1                      Null      -----------------------------------------
library(AICcmodavg)
library(lme4)
Cand.models <- list()



Cand.models[[1]]<-glmer(USE~ 1 + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[1]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ 1 + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90319.9  90342.2 -45157.9  90315.9   515534 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.6872 -0.1678 -0.1114 -0.0796 26.0181 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.31     1.145   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.33453    0.09421  -46.01   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



#mig  2                      HRBSHR + CB + WF + sELEV + I(sELEV^2) + sROADDEC + sSOLRAD   ---------------------------------
Cand.models[[2]]<-glmer(USE~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + sROADDEC + sSOLRAD + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[2]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + sROADDEC + sSOLRAD +      (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86710.9  86811.3 -43346.5  86692.9   515527 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.225 -0.146 -0.104 -0.066 46.556 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.378    1.174   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.9507504  0.1169224 -42.342  < 2e-16 ***
#   HRBSHR       1.4303363  0.0282474  50.636  < 2e-16 ***
#   CB           1.7826350  0.0365101  48.826  < 2e-16 ***
#   WF           0.4525612  0.1038408   4.358 1.31e-05 ***
#   sELEV        0.1730234  0.0153010  11.308  < 2e-16 ***
#   I(sELEV^2)  -0.1117106  0.0124451  -8.976  < 2e-16 ***
#   sROADDEC     0.0998088  0.0180506   5.529 3.21e-08 ***
#   sSOLRAD     -0.0003774  0.0100968  -0.037     0.97    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     WF     sELEV  I(ELEV sROADD
#                                        HRBSHR     -0.111                                          
#                                        CB         -0.083  0.384                                   
#                                        WF         -0.058  0.113  0.097                            
#                                        sELEV       0.038  0.181  0.143  0.036                     
#                                        I(sELEV^2) -0.096 -0.187  0.052  0.018 -0.301              
#                                        sROADDEC   -0.096  0.045 -0.001 -0.028 -0.677  0.461       
#                                        sSOLRAD    -0.008  0.013  0.000  0.002 -0.061  0.050 -0.010







#mig  3                      HRBSHR + CB    -----------------------------------------
Cand.models[[3]]<-glmer(USE~ HRBSHR + CB + (1|ELK_IDYY),    family=binomial, data=mig)
#  > summary(Cand.models[[3]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + (1 | ELK_IDYY)
# Data: mig
#                                        
#  AIC      BIC   logLik deviance df.resid 
#  87384.5  87429.1 -43688.2  87376.5   515532 
#                                        
# Scaled residuals: 
# Min     1Q Median     3Q    Max 
# -1.084 -0.144 -0.109 -0.068 36.414 
#                                        
#  Random effects:
# Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.27     1.127   
# Number of obs: 515536, groups:  ELK_IDYY, 108
#                                        
# Fixed effects
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.87141    0.09980  -48.81   <2e-16 ***
# HRBSHR       1.12519    0.02562   43.91   <2e-16 ***
# CB           1.64869    0.03484   47.32   <2e-16 ***
#                                          ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#                                        
# Correlation of Fixed Effects:
# (Intr) HRBSHR
# HRBSHR -0.106       
# CB     -0.061  0.365



#mig  4                      HRBSHR + WF      -----------------------------------------
Cand.models[[4]]<-glmer(USE~ HRBSHR + WF + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[4]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + WF + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 89136.5  89181.1 -44564.2  89128.5   515532 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.9609 -0.1551 -0.1089 -0.0709 31.2505 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.295    1.138   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.64343    0.10261 -45.253   <2e-16 ***
#   HRBSHR       0.84308    0.02394  35.221   <2e-16 ***
#   WF           0.19525    0.09735   2.006   0.0449 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR
# HRBSHR -0.085       
# WF     -0.053  0.101



#mig  5                      CB + WF      -----------------------------------------
Cand.models[[5]]<-glmer(USE~ CB + WF + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[5]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ CB + WF + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 89262.9  89307.5 -44627.5  89254.9   515532 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.0375 -0.1606 -0.1104 -0.0769 27.8659 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.297    1.139   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.42421    0.09631 -45.938   <2e-16 ***
#   CB           1.18109    0.03219  36.695   <2e-16 ***
#   WF           0.02049    0.08281   0.247    0.805    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) CB    
# CB -0.040       
# WF -0.011  0.052



#mig  6                      HRBSHR + CB + WF      -----------------------------------------
Cand.models[[6]]<-glmer(USE~ HRBSHR + CB + WF + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[6]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + WF + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 87367.8  87423.5 -43678.9  87357.8   515531 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.087 -0.144 -0.108 -0.068 36.639 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.278    1.131   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.89829    0.10647 -46.007  < 2e-16 ***
#   HRBSHR       1.13763    0.02577  44.145  < 2e-16 ***
#   CB           1.66120    0.03490  47.594  < 2e-16 ***
#   WF           0.47580    0.09815   4.848 1.25e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB    
# HRBSHR -0.111              
# CB     -0.070  0.368       
# WF     -0.046  0.106  0.075



#mig  7                      HRBSHR + CB + WF + sSOLRAD      -----------------------------------------
Cand.models[[7]]<-glmer(USE~ HRBSHR + CB + WF + sSOLRAD + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[7]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + WF + sSOLRAD + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 87361.5  87428.4 -43674.8  87349.5   515530 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.115 -0.144 -0.108 -0.068 35.214 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.279    1.131   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.901463   0.111959 -43.779  < 2e-16 ***
#   HRBSHR       1.142794   0.026085  43.810  < 2e-16 ***
#   CB           1.662828   0.035519  46.815  < 2e-16 ***
#   WF           0.476138   0.103566   4.597 4.28e-06 ***
#   sSOLRAD      0.028439   0.009868   2.882  0.00395 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     WF    
# HRBSHR  -0.119                     
# CB      -0.077  0.380              
# WF      -0.060  0.124  0.092       
# sSOLRAD -0.011  0.068  0.016  0.001



#mig  8                      HRBSHR + CB + WF + sELEV      -----------------------------------------
Cand.models[[8]]<-glmer(USE~ HRBSHR + CB + WF + sELEV + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[8]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + WF + sELEV + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86909.9  86976.8 -43448.9  86897.9   515530 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.282 -0.146 -0.105 -0.066 43.453 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.348    1.161   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.98394    0.11497 -43.349  < 2e-16 ***
#   HRBSHR       1.33153    0.02748  48.450  < 2e-16 ***
#   CB           1.81379    0.03646  49.744  < 2e-16 ***
#   WF           0.51813    0.10326   5.018 5.23e-07 ***
#   sELEV        0.23199    0.01067  21.739  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     WF    
# HRBSHR -0.126                     
# CB     -0.082  0.415              
# WF     -0.060  0.126  0.095       
# sELEV  -0.041  0.314  0.205  0.024









#mig  9                      HRBSHR + CB + sELEV + I(sELEV^2)  -----------------------------------------
Cand.models[[9]]<-glmer(USE~ HRBSHR + CB + sELEV + I(sELEV^2) + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[9]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + sELEV + I(sELEV^2) + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86754.3  86821.2 -43371.1  86742.3   515530 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.203 -0.146 -0.104 -0.066 46.119 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.339    1.157   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.86142    0.11460  -42.42   <2e-16 ***
#   HRBSHR       1.41116    0.02805   50.31   <2e-16 ***
#   CB           1.76990    0.03636   48.68   <2e-16 ***
#   sELEV        0.23022    0.01122   20.52   <2e-16 ***
#   I(sELEV^2)  -0.14546    0.01106  -13.15   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     sELEV 
# HRBSHR     -0.103                     
# CB         -0.080  0.377              
# sELEV      -0.040  0.293  0.194       
# I(sELEV^2) -0.058 -0.239  0.059  0.033



#mig  10                     HRBSHR + WF  + sELEV + I(sELEV^2)    -----------------------------------------
Cand.models[[10]]<-glmer(USE~ HRBSHR + WF  + sELEV + I(sELEV^2) + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[10]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + WF + sELEV + I(sELEV^2) + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 88675.0  88742.0 -44331.5  88663.0   515530 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.131 -0.155 -0.110 -0.071 37.874 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.355    1.164   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.58820    0.11508 -39.868   <2e-16 ***
#   HRBSHR       1.08639    0.02644  41.095   <2e-16 ***
#   WF           0.16033    0.10271   1.561    0.119    
# sELEV        0.16024    0.01120  14.311   <2e-16 ***
#   I(sELEV^2)  -0.16898    0.01109 -15.234   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR WF     sELEV 
# HRBSHR     -0.082                     
# WF         -0.054  0.085              
# sELEV      -0.031  0.295  0.015       
# I(sELEV^2) -0.054 -0.270  0.032  0.033



#mig  11                     CB + WF + sELEV + I(sELEV^2)   -----------------------------------------
Cand.models[[11]]<-glmer(USE~ CB + WF + sELEV + I(sELEV^2) + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[11]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ CB + WF + sELEV + I(sELEV^2) + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 89231.1  89298.1 -44609.6  89219.1   515530 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.0814 -0.1604 -0.1098 -0.0762 29.2596 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.313    1.146   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.417726   0.113079 -39.068  < 2e-16 ***
#   CB           1.201752   0.033346  36.039  < 2e-16 ***
#   WF           0.010917   0.101950   0.107    0.915    
# sELEV        0.063010   0.010712   5.882 4.05e-09 ***
#   I(sELEV^2)  -0.005128   0.010848  -0.473    0.636    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) CB     WF     sELEV 
# CB         -0.045                     
# WF         -0.048  0.053              
# sELEV      -0.009  0.134 -0.003       
# I(sELEV^2) -0.088  0.135  0.058  0.108



#mig  12                     HRBSHR + CB + WF + sELEV + I(sELEV^2)      -----------------------------------------
Cand.models[[12]]<-glmer(USE~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[12]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86738.1  86816.1 -43362.0  86724.1   515529 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.207 -0.146 -0.104 -0.066 46.387 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.348    1.161   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.88945    0.11520  -42.44  < 2e-16 ***
#   HRBSHR       1.42339    0.02825   50.38  < 2e-16 ***
#   CB           1.78331    0.03653   48.81  < 2e-16 ***
#   WF           0.46941    0.10361    4.53 5.89e-06 ***
#   sELEV        0.23125    0.01122   20.61  < 2e-16 ***
#   I(sELEV^2)  -0.14398    0.01107  -13.01  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     WF     sELEV 
# HRBSHR     -0.109                            
# CB         -0.085  0.384                     
# WF         -0.062  0.114  0.097              
# sELEV      -0.041  0.293  0.195  0.024       
# I(sELEV^2) -0.059 -0.235  0.061  0.034  0.033








#mig  13                     HRBSHR + CB + sROADDEC   -----------------------------------------
Cand.models[[13]]<-glmer(USE~ HRBSHR + CB + sROADDEC  + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[13]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86882.2  86937.9 -43436.1  86872.2   515531 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.233 -0.145 -0.105 -0.066 43.226 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.405    1.185   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -5.04459    0.11723  -43.03   <2e-16 ***
#   HRBSHR       1.33784    0.02738   48.87   <2e-16 ***
#   CB           1.73439    0.03567   48.62   <2e-16 ***
#   sROADDEC     0.27212    0.01263   21.54   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB    
# HRBSHR   -0.128              
# CB       -0.077  0.385       
# sROADDEC -0.071  0.323  0.112



#mig  14                     HRBSHR + WF  + sROADDEC     -----------------------------------------
Cand.models[[14]]<-glmer(USE~ HRBSHR + WF  + sROADDEC  + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[14]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + WF + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 88778.3  88834.0 -44384.1  88768.3   515531 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.069 -0.154 -0.110 -0.071 35.854 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.414    1.189   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.77748    0.11734 -40.717   <2e-16 ***
#   HRBSHR       1.01625    0.02564  39.628   <2e-16 ***
#   WF           0.14984    0.10279   1.458    0.145    
# sROADDEC     0.22690    0.01239  18.315   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR WF    
# HRBSHR   -0.109              
# WF       -0.049  0.085       
# sROADDEC -0.064  0.335 -0.022



#mig  15                     CB + WF + sROADDEC    -----------------------------------------
Cand.models[[15]]<-glmer(USE~ CB + WF + sROADDEC  + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[15]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ CB + WF + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 89228.5  89284.2 -44609.2  89218.5   515531 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.0719 -0.1601 -0.1086 -0.0765 29.8342 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.327    1.152   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.44323    0.11333 -39.208  < 2e-16 ***
#   CB           1.18422    0.03278  36.125  < 2e-16 ***
#   WF          -0.01099    0.10195  -0.108    0.914    
# sROADDEC     0.07020    0.01178   5.960 2.52e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) CB     WF    
# CB       -0.035              
# WF       -0.043  0.046       
# sROADDEC -0.030  0.021 -0.050



#mig  16                     HRBSHR + CB + WF + sROADDEC      -----------------------------------------
Cand.models[[16]]<-glmer(USE~ HRBSHR + CB + WF + sROADDEC + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[16]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + WF + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86868.7  86935.7 -43428.4  86856.7   515530 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.235 -0.145 -0.105 -0.066 43.435 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.41     1.187   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -5.06884    0.11696 -43.340  < 2e-16 ***
#   HRBSHR       1.34881    0.02756  48.942  < 2e-16 ***
#   CB           1.74567    0.03581  48.746  < 2e-16 ***
#   WF           0.43076    0.10359   4.158  3.2e-05 ***
#   sROADDEC     0.27132    0.01264  21.471  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     WF    
# HRBSHR   -0.132                     
# CB       -0.081  0.391              
# WF       -0.057  0.113  0.090       
# sROADDEC -0.070  0.319  0.110 -0.017








#mig  17                     HRBSHR + sELEV + I(sELEV^2) + sROADDEC     ---------------------------------
Cand.models[[17]]<-glmer(USE~ HRBSHR + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[17]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + sELEV + I(sELEV^2) + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 88646.7  88713.6 -44317.4  88634.7   515530 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.132 -0.154 -0.110 -0.071 37.909 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.384    1.177   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.64007    0.11651 -39.827  < 2e-16 ***
#   HRBSHR       1.08999    0.02633  41.400  < 2e-16 ***
#   sELEV        0.10320    0.01513   6.819 9.18e-12 ***
#   I(sELEV^2)  -0.13703    0.01249 -10.975  < 2e-16 ***
#   sROADDEC     0.09753    0.01779   5.481 4.22e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR sELEV  I(ELEV
# HRBSHR     -0.081                     
# sELEV       0.044  0.183              
# I(sELEV^2) -0.091 -0.219 -0.299       
# sROADDEC   -0.096  0.048 -0.674  0.466



#mig  18                     CB + sELEV + I(sELEV^2) + sROADDEC      ---------------------------------
Cand.models[[18]]<-glmer(USE~ CB + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[18]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ CB + sELEV + I(sELEV^2) + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 89222.9  89289.8 -44605.5  89210.9   515530 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.0882 -0.1602 -0.1098 -0.0762 29.7302 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.327    1.152   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.44757    0.11390 -39.047  < 2e-16 ***
#   CB           1.19977    0.03329  36.045  < 2e-16 ***
#   sELEV        0.03357    0.01480   2.268  0.02336 *  
#   I(sELEV^2)   0.01154    0.01227   0.940  0.34700    
# sROADDEC     0.05095    0.01783   2.858  0.00426 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) CB     sELEV  I(ELEV
# CB         -0.041                     
# sELEV       0.059  0.109              
# I(sELEV^2) -0.119  0.108 -0.261       
# sROADDEC   -0.094 -0.017 -0.690  0.473



#mig  19                     WF + sELEV + I(sELEV^2) + sROADDEC      ---------------------------------
Cand.models[[19]]<-glmer(USE~ WF + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[19]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ WF + sELEV + I(sELEV^2) + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90289.2  90356.1 -45138.6  90277.2   515530 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.7107 -0.1652 -0.1126 -0.0786 28.1130 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.333    1.155   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.323936   0.114011 -37.926  < 2e-16 ***
#   WF          -0.137567   0.101719  -1.352  0.17624    
# sELEV       -0.005971   0.014685  -0.407  0.68428    
# I(sELEV^2)  -0.025059   0.012237  -2.048  0.04058 *  
#   sROADDEC     0.057910   0.017636   3.284  0.00102 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) WF     sELEV  I(ELEV
# WF         -0.043                     
# sELEV       0.060  0.018              
# I(sELEV^2) -0.115  0.031 -0.261       
# sROADDEC   -0.092 -0.034 -0.689  0.479



#mig  20                     HRBSHR + CB + sELEV + I(sELEV^2) + sROADDEC     ---------------------------------
Cand.models[[20]]<-glmer(USE~ HRBSHR + CB + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[20]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + sELEV + I(sELEV^2) + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86723.9  86801.9 -43354.9  86709.9   515529 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.221 -0.146 -0.104 -0.066 46.275 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.372    1.171   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.92487    0.11631 -42.344  < 2e-16 ***
#   HRBSHR       1.41864    0.02804  50.588  < 2e-16 ***
#   CB           1.76967    0.03632  48.722  < 2e-16 ***
#   sELEV        0.17089    0.01526  11.202  < 2e-16 ***
#   I(sELEV^2)  -0.11250    0.01242  -9.056  < 2e-16 ***
#   sROADDEC     0.10175    0.01804   5.640  1.7e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     sELEV  I(ELEV
# HRBSHR     -0.106                            
# CB         -0.079  0.377                     
# sELEV       0.039  0.180  0.141              
# I(sELEV^2) -0.095 -0.190  0.051 -0.299       
# sROADDEC   -0.098  0.048  0.002 -0.679  0.462



#mig  21                     HRBSHR + WF + sELEV + I(sELEV^2) + sROADDEC     ---------------------------------
Cand.models[[21]]<-glmer(USE~ HRBSHR + WF + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[21]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + WF + sELEV + I(sELEV^2) + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 88646.9  88724.9 -44316.4  88632.9   515529 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.133 -0.155 -0.110 -0.070 37.957 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.387    1.178   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.64797    0.11668 -39.834  < 2e-16 ***
#   HRBSHR       1.09291    0.02642  41.361  < 2e-16 ***
#   WF           0.14395    0.10285   1.400    0.162    
# sELEV        0.10381    0.01514   6.855 7.13e-12 ***
#   I(sELEV^2)  -0.13678    0.01249 -10.953  < 2e-16 ***
#   sROADDEC     0.09686    0.01780   5.442 5.26e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR WF     sELEV  I(ELEV
# HRBSHR     -0.085                            
# WF         -0.050  0.084                     
# sELEV       0.043  0.185  0.030              
# I(sELEV^2) -0.091 -0.217  0.015 -0.299       
# sROADDEC   -0.095  0.046 -0.028 -0.674  0.466



#mig  22                     CB + WF + sELEV + I(sELEV^2) + sROADDEC     ---------------------------------
Cand.models[[22]]<-glmer(USE~ CB + WF + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[22]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ CB + WF + sELEV + I(sELEV^2) + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 89224.9  89303.0 -44605.5  89210.9   515529 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.0882 -0.1602 -0.1098 -0.0762 29.7302 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.327    1.152   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.4476045  0.1140785 -38.987  < 2e-16 ***
#   CB           1.1997828  0.0333368  35.990  < 2e-16 ***
#   WF           0.0005839  0.1020146   0.006  0.99543    
# sELEV        0.0335695  0.0148067   2.267  0.02338 *  
#   I(sELEV^2)   0.0115423  0.0122767   0.940  0.34712    
# sROADDEC     0.0509471  0.0178377   2.856  0.00429 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) CB     WF     sELEV  I(ELEV
# CB         -0.043                            
# WF         -0.045  0.054                     
# sELEV       0.058  0.110  0.022              
# I(sELEV^2) -0.120  0.109  0.034 -0.260       
# sROADDEC   -0.092 -0.019 -0.035 -0.690  0.471



#mig  23                     HRBSHR + CB + WF + sELEV + I(sELEV^2) + sROADDEC     ---------------------------------
Cand.models[[23]]<-glmer(USE~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# DELETED TOP MODEL RESULTS FOR SECURITY REASONS :)



#mig  24                      HRBSHR + CB + WF - sELEV + sROADDEC     ---------------------------------
Cand.models[[24]]<-glmer(USE~ HRBSHR + CB + WF + sELEV + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[24]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + WF + sELEV + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86789.0  86867.1 -43387.5  86775.0   515529 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.277 -0.146 -0.105 -0.066 44.948 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.401    1.184   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -5.05484    0.11722 -43.122  < 2e-16 ***
#   HRBSHR       1.38084    0.02777  49.729  < 2e-16 ***
#   CB           1.80017    0.03644  49.403  < 2e-16 ***
#   WF           0.47013    0.10379   4.530  5.9e-06 ***
#   sELEV        0.12940    0.01420   9.111  < 2e-16 ***
#   sROADDEC     0.17632    0.01621  10.874  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     WF     sELEV 
# HRBSHR   -0.131                            
# CB       -0.079  0.404                     
# WF       -0.057  0.119  0.096              
# sELEV     0.009  0.126  0.171  0.044       
# sROADDEC -0.059  0.158 -0.027 -0.041 -0.644

summary(Cand.models[[24]])


# APPENDIX  AIC Table models 1->23  (mig) -------------------------------------------
Modnames <- paste ("mod", 1:length(Cand.models), sep = " ")
# generate AICc table
aictab(cand.set = Cand.models, modnames= Modnames, sort=TRUE)                

# Model selection based on AICc:
#   
#                                                                      AICc      Delta_AICc  AICcWt Cum.Wt    LL
# mod 23   #HRBSHR + CB + WF + sELEV - I(sELEV^2) + sROADDEC           86708.91       0.00   0.73   0.73 -43346.45
# mod 2    #HRBSHR + CB + WF + sELEV - I(sELEV^2) + sROADDEC - sSOLRAD 86710.91       2.00   0.27   1.00 -43346.45
# mod 20   HRBSHR + CB + sELEV - I(sELEV^2) + sROADDEC                 86723.87      14.96   0.00   1.00 -43354.93
# mod 12   HRBSHR + CB + WF + sELEV - I(sELEV^2)                       86738.06      29.16   0.00   1.00 -43362.03
# mod 9    HRBSHR + CB + sELEV - I(sELEV^2)                            86754.29      45.38   0.00   1.00 -43371.14
# mod 24   HRBSHR + CB + WF + sELEV + sROADDEC                         86789.05      80.14   0.00   1.00 -43387.52
# mod 16   HRBSHR + CB + WF + sROADDEC                                 86868.74     159.84   0.00   1.00 -43428.37
# mod 13   HRBSHR + CB + sROADDEC                                      86882.16     173.25   0.00   1.00 -43436.08
# mod 8    HRBSHR + CB + WF + sELEV                                    86909.87     200.96   0.00   1.00 -43448.93
# mod 7    HRBSHR + CB + WF + sSOLRAD                                  87361.51     652.60   0.00   1.00 -43674.75
# mod 6    HRBSHR + CB + WF                                            87367.77     658.87   0.00   1.00 -43678.89
# mod 3    HRBSHR + CB                                                 87384.49     675.58   0.00   1.00 -43688.24
# mod 17   HRBSHR + sELEV - I(sELEV^2) + sROADDEC                      88646.73    1937.82   0.00   1.00 -44317.36
# mod 21   HRBSHR + WF + sELEV - I(sELEV^2) + sROADDEC                 88646.86    1937.95   0.00   1.00 -44316.43
# mod 10   HRBSHR + WF  + sELEV - I(sELEV^2)                           88675.04    1966.13   0.00   1.00 -44331.52
# mod 14   HRBSHR + WF  + sROADDEC                                     88778.28    2069.37   0.00   1.00 -44384.14
# mod 4    HRBSHR + WF                                                 89136.50    2427.59   0.00   1.00 -44564.25
# mod 18   #CB + sELEV + I(sELEV^2) + sROADDEC                         89222.92    2514.02   0.00   1.00 -44605.46
# mod 22   CB + WF + sELEV + I(sELEV^2) + sROADDEC7                    89224.92    2516.02   0.00   1.00 -44605.46
# mod 15   CB - WF + sROADDEC                                          89228.47    2519.56   0.00   1.00 -44609.23
# mod 11   CB + WF + sELEV - I(sELEV^2)                                89231.15    2522.24   0.00   1.00 -44609.57
# mod 5    CB + WF                                                     89262.92    2554.01   0.00   1.00 -44627.46
# mod 19   -WF - sELEV - I(sELEV^2) + sROADDEC                         90289.19    3580.29   0.00   1.00 -45138.60
# mod 1    #Null                                                       90319.89    3610.99   0.00   1.00 -45157.95

# K     AICc Delta_AICc AICcWt Cum.Wt        LL
# mod 23 8 86708.91       0.00   0.73   0.73 -43346.45
# mod 2  9 86710.91       2.00   0.27   1.00 -43346.45
# mod 20 7 86723.87      14.96   0.00   1.00 -43354.93
# mod 12 7 86738.06      29.16   0.00   1.00 -43362.03
# mod 9  6 86754.29      45.38   0.00   1.00 -43371.14
# mod 24 7 86789.05      80.14   0.00   1.00 -43387.52
# mod 16 6 86868.74     159.84   0.00   1.00 -43428.37
# mod 13 5 86882.16     173.25   0.00   1.00 -43436.08
# mod 8  6 86909.87     200.96   0.00   1.00 -43448.93
# mod 7  6 87361.51     652.60   0.00   1.00 -43674.75
# mod 6  5 87367.77     658.87   0.00   1.00 -43678.89
# mod 3  4 87384.49     675.58   0.00   1.00 -43688.24
# mod 17 6 88646.73    1937.82   0.00   1.00 -44317.36
# mod 21 7 88646.86    1937.95   0.00   1.00 -44316.43
# mod 10 6 88675.04    1966.13   0.00   1.00 -44331.52
# mod 14 5 88778.28    2069.37   0.00   1.00 -44384.14
# mod 4  4 89136.50    2427.59   0.00   1.00 -44564.25
# mod 18 6 89222.92    2514.02   0.00   1.00 -44605.46
# mod 22 7 89224.92    2516.02   0.00   1.00 -44605.46
# mod 15 5 89228.47    2519.56   0.00   1.00 -44609.23
# mod 11 6 89231.15    2522.24   0.00   1.00 -44609.57
# mod 5  4 89262.92    2554.01   0.00   1.00 -44627.46
# mod 19 6 90289.19    3580.29   0.00   1.00 -45138.60
# mod 1  2 90319.89    3610.99   0.00   1.00 -45157.95



###### UNIVARIATE ####### -----------------------------------------------------------
#mig  1                      NULL ---------------------------------------------------
library(lme4)
Cand.models <- list()



Cand.models[[1]]<-glmer(USE~ 1 + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[1]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ 1 + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90319.9  90342.2 -45157.9  90315.9   515534 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.6872 -0.1678 -0.1114 -0.0796 26.0181 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.31     1.145   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.33453    0.09421  -46.01   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#mig  2                      TREE      -----------------------------------------
Cand.models[[2]]<-glmer(USE~ TREE + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[2]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ TREE + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 87754.6  87788.0 -43874.3  87748.6   515533 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -0.924 -0.144 -0.108 -0.068 36.936 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.322    1.15    
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -3.74972    0.10869  -34.50   <2e-16 ***
#   TREE        -1.20617    0.02423  -49.78   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# TREE -0.085


#mig  3                      HRBSHR      -----------------------------------------
Cand.models[[3]]<-glmer(USE~ HRBSHR + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[3]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 89137.9  89171.4 -44566.0  89131.9   515533 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.9593 -0.1556 -0.1090 -0.0715 31.1867 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.29     1.136   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.63283    0.10277  -45.08   <2e-16 ***
#   HRBSHR       0.83894    0.02387   35.14   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# HRBSHR -0.085


#mig  4                      CB      -----------------------------------------
Cand.models[[4]]<-glmer(USE~ CB + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[4]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ CB + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 89261.0  89294.4 -44627.5  89255.0   515533 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.0374 -0.1606 -0.1104 -0.0769 27.8619 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.297    1.139   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.42324    0.10437  -42.38   <2e-16 ***
#   CB           1.18078    0.03233   36.52   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# CB -0.048


#mig  5                      WF      -----------------------------------------
Cand.models[[5]]<-glmer(USE~ WF + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[5]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ WF + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90320.9  90354.4 -45157.5  90314.9   515533 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.6872 -0.1678 -0.1115 -0.0792 26.5799 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.307    1.143   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.32969    0.10103  -42.86   <2e-16 ***
#   WF          -0.09884    0.08447   -1.17    0.242    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# WF 0.038 


#mig  6                      sSOLRAD      -----------------------------------------
Cand.models[[6]]<-glmer(USE~ sSOLRAD + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[6]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ sSOLRAD + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90321.9  90355.4 -45157.9  90315.9   515533 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.6872 -0.1678 -0.1114 -0.0796 26.0196 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.31     1.145   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.334e+00  1.122e-01 -38.626   <2e-16 ***
#   sSOLRAD     -3.832e-05  9.987e-03  -0.004    0.997    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# sSOLRAD -0.003


#mig  7                      sELEV      -----------------------------------------
Cand.models[[7]]<-glmer(USE~ sELEV + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[7]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ sELEV + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90312.3  90345.8 -45153.2  90306.3   515533 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.7031 -0.1654 -0.1117 -0.0794 26.6081 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.32     1.149   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.33324    0.11292 -38.373  < 2e-16 ***
#   sELEV        0.03219    0.01039   3.098  0.00195 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# sELEV 0.003 


#mig  8                      sELEV + I(sELEV^2)      -----------------------------------------
Cand.models[[8]]<-glmer(USE~ sELEV + I(sELEV^2) + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[8]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ sELEV + I(sELEV^2) + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90297.6  90342.3 -45144.8  90289.6   515532 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.6978 -0.1652 -0.1127 -0.0787 27.4322 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.32     1.149   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.29629    0.11296 -38.033  < 2e-16 ***
#   sELEV        0.02749    0.01064   2.584  0.00976 ** 
#   I(sELEV^2)  -0.04376    0.01075  -4.072 4.67e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) sELEV 
# sELEV      -0.006       
# I(sELEV^2) -0.078  0.116


#mig  9                      sROADDEC      -----------------------------------------
Cand.models[[9]]<-glmer(USE~ sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[9]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90290.0  90323.4 -45142.0  90284.0   515533 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.7082 -0.1649 -0.1127 -0.0789 27.6865 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.341    1.158   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.35352    0.11364 -38.308  < 2e-16 ***
#   sROADDEC     0.06431    0.01152   5.581 2.39e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# sROADDEC -0.031



# UNIVARIATE AIC Table models 1->9 (mig) -------------------------------------------
Modnames <- paste ("mod", 1:length(Cand.models), sep = " ")
# generate AICc table
aictab(cand.set = Cand.models, modnames= Modnames, sort=TRUE)                
# Model selection based on AICc:
#   K     AICc Delta_AICc AICcWt Cum.Wt        LL
# mod 2 3 87754.59       0.00      1      1 -43874.29
# mod 3 3 89137.91    1383.32      0      1 -44565.96
# mod 4 3 89260.96    1506.37      0      1 -44627.48
# mod 9 3 90289.98    2535.40      0      1 -45141.99
# mod 8 4 90297.65    2543.06      0      1 -45144.82
# mod 7 3 90312.35    2557.76      0      1 -45153.17
# mod 1 2 90319.89    2565.31      0      1 -45157.95
# mod 5 3 90320.94    2566.35      0      1 -45157.47
# mod 6 3 90321.89    2567.31      0      1 -45157.95



####### Testing WF interaction ###### ------------
#(Decided not to include in my thesis)
# We did not look at elev. inter  bcs... split...
# Map with Elev or Elev2, dark around cut out areas, means select beyond what I eliminated. Look at high elev that are not masked out. they are bringing down my selection. Mention in discussion what that effect is. 
#mig  1                      WF  ---------------------------------
library(lme4)
Cand.models <- list()


Cand.models[[1]]<-glmer(USE~ WF + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[1]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ WF + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90320.9  90354.4 -45157.5  90314.9   515533 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.6872 -0.1678 -0.1115 -0.0792 26.5799 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.307    1.143   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -4.32969    0.10103  -42.86   <2e-16 ***
#   WF          -0.09884    0.08447   -1.17    0.242    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# WF 0.038 

#mig  2                      sELEV    ---------------------------------
Cand.models[[2]]<-glmer(USE~ sELEV + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[2]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ sELEV + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90312.3  90345.8 -45153.2  90306.3   515533 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.7031 -0.1654 -0.1117 -0.0794 26.6081 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.32     1.149   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -4.33324    0.11292 -38.373  < 2e-16 ***
#   sELEV        0.03219    0.01039   3.098  0.00195 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# sELEV 0.003 

#mig  3                      sELEV + I(sELEV^2)   ---------------------------------
Cand.models[[3]]<-glmer(USE~ sELEV + I(sELEV^2)  + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[3]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ sELEV + I(sELEV^2) + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90297.6  90342.3 -45144.8  90289.6   515532 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.6978 -0.1652 -0.1127 -0.0787 27.4322 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.32     1.149   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.29629    0.11296 -38.033  < 2e-16 ***
#   sELEV        0.02749    0.01064   2.584  0.00976 ** 
#   I(sELEV^2)  -0.04376    0.01075  -4.072 4.67e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) sELEV 
# sELEV      -0.006       
# I(sELEV^2) -0.078  0.116

#mig  4                      (WF*sELEV)  ---------------------------------
Cand.models[[4]]<-glmer(USE~ (WF*sELEV) + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[4]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ (WF * sELEV) + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90306.9  90362.6 -45148.4  90296.9   515531 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.7053 -0.1659 -0.1119 -0.0790 28.2811 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.313    1.146   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.32544    0.11235 -38.501  < 2e-16 ***
#   WF          -0.11657    0.10278  -1.134 0.256742    
#   sELEV        0.03442    0.01041   3.305 0.000949 ***
#   WF:sELEV    -0.38938    0.13414  -2.903 0.003698 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) WF     sELEV 
# WF       -0.042              
# sELEV     0.003 -0.007       
# WF:sELEV -0.010  0.158 -0.067

#mig  5                      (WF*sELEV) + (WF*(I(sELEV^2)))  ---------------------------------
Cand.models[[5]]<-glmer(USE~ (WF*sELEV) + (WF*(I(sELEV^2))) + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[5]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ (WF * sELEV) + (WF * (I(sELEV^2))) + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90290.7  90368.8 -45138.3  90276.7   515529 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -0.698 -0.166 -0.113 -0.078 37.668 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.314    1.146   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -4.29032    0.11274 -38.054  < 2e-16 ***
#   WF            -0.02626    0.11995  -0.219  0.82672    
#   sELEV          0.02999    0.01066   2.814  0.00489 ** 
#   I(sELEV^2)    -0.04283    0.01080  -3.967 7.27e-05 ***
#   WF:sELEV      -0.46541    0.16078  -2.895  0.00380 ** 
#   WF:I(sELEV^2) -0.25923    0.15467  -1.676  0.09375 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) WF     sELEV  I(ELEV WF:ELE
#                          WF          -0.050                            
#                          sELEV       -0.006  0.007                     
#                          I(sELEV^2)  -0.082  0.092  0.115              
#                          WF:sELEV    -0.004  0.003 -0.059 -0.007       
#                          WF:I(ELEV^2  0.017 -0.498 -0.013 -0.069  0.296

#mig  6                      (WF*(I(sELEV^2)))  ---------------------------------
Cand.models[[6]]<-glmer(USE~ (WF*(I(sELEV^2))) + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[6]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ (WF * (I(sELEV^2))) + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90302.5  90358.3 -45146.3  90292.5   515531 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.6959 -0.1654 -0.1124 -0.0788 30.1433 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.309    1.144   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)   -4.29057    0.11247 -38.149  < 2e-16 ***
#   WF            -0.02768    0.11843  -0.234    0.815    
#   I(sELEV^2)    -0.04673    0.01077  -4.340 1.43e-05 ***
#   WF:I(sELEV^2) -0.20586    0.14292  -1.440    0.150    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) WF     I(ELEV
#                   WF          -0.049              
#                   I(sELEV^2)  -0.082  0.094       
#                   WF:I(ELEV^2  0.015 -0.509 -0.074                                                

#mig  7                      HRBSHR + CB + WF + sELEV + I(sELEV^2) + sROADDEC     ---------------------------------
Cand.models[[7]]<-glmer(USE~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# DELETED RESULTS FOR SECURITY REASONS :)

#mig  8                      WF + sELEV + I(sELEV^2) + sROADDEC     ---------------------------------
Cand.models[[8]]<-glmer(USE~ WF + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[8]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ WF + sELEV + I(sELEV^2) + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90289.2  90356.1 -45138.6  90277.2   515530 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.7107 -0.1652 -0.1126 -0.0786 28.1130 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.333    1.155   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.323936   0.114011 -37.926  < 2e-16 ***
#   WF          -0.137567   0.101719  -1.352  0.17624    
#   sELEV       -0.005971   0.014685  -0.407  0.68428    
#   I(sELEV^2)  -0.025059   0.012237  -2.048  0.04058 *  
#   sROADDEC     0.057910   0.017636   3.284  0.00102 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) WF     sELEV  I(ELEV
#                          WF         -0.043                     
#                          sELEV       0.060  0.018              
#                          I(sELEV^2) -0.115  0.031 -0.261       
#                          sROADDEC   -0.092 -0.034 -0.689  0.479

#mig  9                      WF + sELEV + I(sELEV^2)     ---------------------------------
Cand.models[[9]]<-glmer(USE~ WF + sELEV + I(sELEV^2) + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[9]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ WF + sELEV + I(sELEV^2) + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90298.1  90353.8 -45144.0  90288.1   515531 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.6979 -0.1653 -0.1127 -0.0786 27.9822 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.316    1.147   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.28949    0.11330 -37.859  < 2e-16 ***
#   WF          -0.12622    0.10178  -1.240  0.21493    
#   sELEV        0.02759    0.01064   2.593  0.00952 ** 
#   I(sELEV^2)  -0.04450    0.01077  -4.134 3.57e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) WF     sELEV 
# WF         -0.048              
# sELEV      -0.005 -0.007       
# I(sELEV^2) -0.082  0.054  0.115


# WF INTERACTION  AIC Table models 1->9 WF (mig) -------------------------------------------
Modnames <- paste ("mod", 1:length(Cand.models), sep = " ")
# generate AICc table
aictab(cand.set = Cand.models, modnames= Modnames, sort=TRUE)                
# Forget about the interaction, don't need these anymore
# Model selection based on AICc:
#   
#   K     AICc Delta_AICc AICcWt Cum.Wt        LL
# mod 7 8 86708.91       0.00      1      1 -43346.45
# mod 8 6 90289.19    3580.29      0      1 -45138.60
# mod 5 7 90290.70    3581.79      0      1 -45138.35
# mod 3 4 90297.65    3588.74      0      1 -45144.82
# mod 9 5 90298.08    3589.17      0      1 -45144.04
# mod 6 5 90302.50    3593.60      0      1 -45146.25
# mod 4 5 90306.88    3597.98      0      1 -45148.44
# mod 2 3 90312.35    3603.44      0      1 -45153.17
# mod 1 3 90320.94    3612.03      0      1 -45157.47










summary(Cand.models[[9]])

confint(Cand.models[[12]])
###### SUBSET 4 PAPER ####### -----------------------------------------------------------
#mig  1                      NULL ---------------------------------------------------
library(lme4)
Cand.models <- list()

Cand.models[[1]]<-glmer(USE~ 1 + (1|ELK_IDYY),   family=binomial, data=mig)

# > summary(Cand.models[[1]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ 1 + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 90319.9  90342.2 -45157.9  90315.9   515534 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.6872 -0.1678 -0.1114 -0.0796 26.0181 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.31     1.145   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.33453    0.09421  -46.01   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



#mig  2                      HRBSHR + CB + WF - sELEV - I(sELEV^2) + sROADDEC + sSOLRAD   ---------------------------------
Cand.models[[2]]<-glmer(USE~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + sROADDEC + sSOLRAD + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[2]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + sROADDEC + sSOLRAD +      (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86710.9  86811.3 -43346.5  86692.9   515527 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.225 -0.146 -0.104 -0.066 46.556 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.378    1.174   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.9507504  0.1169224 -42.342  < 2e-16 ***
#   HRBSHR       1.4303363  0.0282474  50.636  < 2e-16 ***
#   CB           1.7826350  0.0365101  48.826  < 2e-16 ***
#   WF           0.4525612  0.1038408   4.358 1.31e-05 ***
#   sELEV        0.1730234  0.0153010  11.308  < 2e-16 ***
#   I(sELEV^2)  -0.1117106  0.0124451  -8.976  < 2e-16 ***
#   sROADDEC     0.0998088  0.0180506   5.529 3.21e-08 ***
#   sSOLRAD     -0.0003774  0.0100968  -0.037     0.97    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     WF     sELEV  I(ELEV sROADD
#                                        HRBSHR     -0.111                                          
#                                        CB         -0.083  0.384                                   
#                                        WF         -0.058  0.113  0.097                            
#                                        sELEV       0.038  0.181  0.143  0.036                     
#                                        I(sELEV^2) -0.096 -0.187  0.052  0.018 -0.301              
#                                        sROADDEC   -0.096  0.045 -0.001 -0.028 -0.677  0.461       
#                                        sSOLRAD    -0.008  0.013  0.000  0.002 -0.061  0.050 -0.010


#mig  16                      HRBSHR + CB + WF + sROADDEC      -----------------------------------------
Cand.models[[3]]<-glmer(USE~ HRBSHR + CB + WF + sROADDEC + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[3]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + WF + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86868.7  86935.7 -43428.4  86856.7   515530 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.235 -0.145 -0.105 -0.066 43.435 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.41     1.187   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -5.06884    0.11696 -43.340  < 2e-16 ***
#   HRBSHR       1.34881    0.02756  48.942  < 2e-16 ***
#   CB           1.74567    0.03581  48.746  < 2e-16 ***
#   WF           0.43076    0.10359   4.158  3.2e-05 ***
#   sROADDEC     0.27132    0.01264  21.471  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     WF    
# HRBSHR   -0.132                     
# CB       -0.081  0.391              
# WF       -0.057  0.113  0.090       
# sROADDEC -0.070  0.319  0.110 -0.017


#mig  20                      HRBSHR + CB - sELEV - I(sELEV^2) + sROADDEC     ---------------------------------
Cand.models[[4]]<-glmer(USE~ HRBSHR + CB + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[4]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + sELEV + I(sELEV^2) + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86723.9  86801.9 -43354.9  86709.9   515529 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.221 -0.146 -0.104 -0.066 46.275 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.372    1.171   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.92487    0.11631 -42.344  < 2e-16 ***
#   HRBSHR       1.41864    0.02804  50.588  < 2e-16 ***
#   CB           1.76967    0.03632  48.722  < 2e-16 ***
#   sELEV        0.17089    0.01526  11.202  < 2e-16 ***
#   I(sELEV^2)  -0.11250    0.01242  -9.056  < 2e-16 ***
#   sROADDEC     0.10175    0.01804   5.640  1.7e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     sELEV  I(ELEV
#                                 HRBSHR     -0.106                            
#                                 CB         -0.079  0.377                     
#                                 sELEV       0.039  0.180  0.141              
#                                 I(sELEV^2) -0.095 -0.190  0.051 -0.299       
#                                 sROADDEC   -0.098  0.048  0.002 -0.679  0.462


#mig  21                      HRBSHR + WF - sELEV - I(sELEV^2) + sROADDEC     ---------------------------------
Cand.models[[5]]<-glmer(USE~ HRBSHR + WF + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[5]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + WF + sELEV + I(sELEV^2) + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 88646.9  88724.9 -44316.4  88632.9   515529 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.133 -0.155 -0.110 -0.070 37.957 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.387    1.178   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.64797    0.11668 -39.834  < 2e-16 ***
#   HRBSHR       1.09291    0.02642  41.361  < 2e-16 ***
#   WF           0.14395    0.10285   1.400    0.162    
# sELEV        0.10381    0.01514   6.855 7.13e-12 ***
#   I(sELEV^2)  -0.13678    0.01249 -10.953  < 2e-16 ***
#   sROADDEC     0.09686    0.01780   5.442 5.26e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR WF     sELEV  I(ELEV
#                                 HRBSHR     -0.085                            
#                                 WF         -0.050  0.084                     
#                                 sELEV       0.043  0.185  0.030              
#                                 I(sELEV^2) -0.091 -0.217  0.015 -0.299       
#                                 sROADDEC   -0.095  0.046 -0.028 -0.674  0.466


#mig  22                      CB + WF - sELEV - I(sELEV^2) + sROADDEC     ---------------------------------
Cand.models[[6]]<-glmer(USE~ CB + WF + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[6]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ CB + WF + sELEV + I(sELEV^2) + sROADDEC + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 89224.9  89303.0 -44605.5  89210.9   515529 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.0882 -0.1602 -0.1098 -0.0762 29.7302 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.327    1.152   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.4476045  0.1140785 -38.987  < 2e-16 ***
#   CB           1.1997828  0.0333368  35.990  < 2e-16 ***
#   WF           0.0005839  0.1020146   0.006  0.99543    
# sELEV        0.0335695  0.0148067   2.267  0.02338 *  
#   I(sELEV^2)   0.0115423  0.0122767   0.940  0.34712    
# sROADDEC     0.0509471  0.0178377   2.856  0.00429 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) CB     WF     sELEV  I(ELEV
#                                 CB         -0.043                            
#                                 WF         -0.045  0.054                     
#                                 sELEV       0.058  0.110  0.022              
#                                 I(sELEV^2) -0.120  0.109  0.034 -0.260       
#                                 sROADDEC   -0.092 -0.019 -0.035 -0.690  0.471


#mig  23                      HRBSHR + CB + WF - sELEV - I(sELEV^2) + sROADDEC     ---------------------------------
Cand.models[[7]]<-glmer(USE~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + sROADDEC + (1|ELK_IDYY),   family=binomial, data=mig)
# > summary(Cand.models[[7]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + sROADDEC + (1 |      ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86708.9  86798.1 -43346.5  86692.9   515528 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.225 -0.146 -0.104 -0.066 46.530 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.378    1.174   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.95078    0.11675 -42.407  < 2e-16 ***
#   HRBSHR       1.43035    0.02824  50.652  < 2e-16 ***
#   CB           1.78264    0.03650  48.840  < 2e-16 ***
#   WF           0.45259    0.10368   4.365 1.27e-05 ***
#   sELEV        0.17298    0.01527  11.328  < 2e-16 ***
#   I(sELEV^2)  -0.11169    0.01243  -8.986  < 2e-16 ***
#   sROADDEC     0.09981    0.01805   5.530 3.20e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     WF     sELEV  I(ELEV
#                                        HRBSHR     -0.111                                   
#                                        CB         -0.084  0.384                            
#                                        WF         -0.060  0.113  0.097                     
#                                        sELEV       0.037  0.182  0.143  0.036              
#                                        I(sELEV^2) -0.096 -0.188  0.052  0.018 -0.299       
#                                        sROADDEC   -0.096  0.045 -0.001 -0.028 -0.679  0.462


#mig  12                      HRBSHR + CB + WF - sELEV - I(sELEV^2)      -----------------------------------------
Cand.models[[8]]<-glmer(USE~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + (1|ELK_IDYY),    family=binomial, data=mig)
# > summary(Cand.models[[8]])
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: USE ~ HRBSHR + CB + WF + sELEV + I(sELEV^2) + (1 | ELK_IDYY)
# Data: mig
# 
# AIC      BIC   logLik deviance df.resid 
# 86738.1  86816.1 -43362.0  86724.1   515529 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -1.207 -0.146 -0.104 -0.066 46.387 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ELK_IDYY (Intercept) 1.348    1.161   
# Number of obs: 515536, groups:  ELK_IDYY, 108
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.88945    0.11520  -42.44  < 2e-16 ***
#   HRBSHR       1.42339    0.02825   50.38  < 2e-16 ***
#   CB           1.78331    0.03653   48.81  < 2e-16 ***
#   WF           0.46941    0.10361    4.53 5.89e-06 ***
#   sELEV        0.23125    0.01122   20.61  < 2e-16 ***
#   I(sELEV^2)  -0.14398    0.01107  -13.01  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) HRBSHR CB     WF     sELEV 
# HRBSHR     -0.109                            
# CB         -0.085  0.384                     
# WF         -0.062  0.114  0.097              
# sELEV      -0.041  0.293  0.195  0.024       
# I(sELEV^2) -0.059 -0.235  0.061  0.034  0.033






# SUBSET 4 PAPER  AIC Table models 1->23  (res) -------------------------------------------
Modnames <- paste ("mod", 1:length(Cand.models), sep = " ")
# generate AICc table
aictab(cand.set = Cand.models, modnames= Modnames, sort=TRUE) 

# Model selection based on AICc:
#   
#   K     AICc Delta_AICc AICcWt Cum.Wt        LL
# mod(23)  7 8 86708.91       0.00   0.73   0.73 -43346.45
# mod(2)   2 9 86710.91       2.00   0.27   1.00 -43346.45
# mod(20)  4 7 86723.87      14.96   0.00   1.00 -43354.93
# mod(12)  8 7 86738.06      29.16   0.00   1.00 -43362.03
# mod(16)  3 6 86868.74     159.84   0.00   1.00 -43428.37
# mod(21)  5 7 88646.86    1937.95   0.00   1.00 -44316.43
# mod(22)  6 7 89224.92    2516.02   0.00   1.00 -44605.46
# mod(1)   1 2 90319.89    3610.99   0.00   1.00 -45157.95
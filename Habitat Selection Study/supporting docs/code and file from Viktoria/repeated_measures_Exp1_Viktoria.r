
################################################################################
#                                                                              #
#                        Scap Adapt analysis                                   #
#                  Repeated measures of Experiment 1                           #
#                    home soil and control soil                                #
#                                                                              #
#################################################################################

adapt_time <- read_csv("supporting docs/code and file from Viktoria/adapt_time.csv")

rm(list=ls())
objects()

adapt_time<-read.table("C:\\Rdata\\scap_adapt\\final_analysis_PloS\\scapadapt_timeseries.txt", header=T)
attach(adapt_time)
names(adapt_time)
str(adapt_time)

ind<-factor(ind)
block<-factor(block)
hist(height)

# Initial plotting with interaction.plot to see the overall curve structure of the data

par(mfrow=c(1,2))
interaction.plot(time[region=="EUR"],factor(geo_dist[region=="EUR"]),height[region=="EUR"],col=1:5)
interaction.plot(time[region=="KZ"],factor(geo_dist[region=="KZ"]),height[region=="KZ"],col=1:5)
par(mfrow = c(1,1))

# The curves seem to be a non-linear curve Michaelis-Menten curve or a simple asymptotic curve (Crawley p. 661)
# three solutions are possible:
# 1) transform data to linearize curve--> doesnt work, i tried it, except for (height)^10 which is not really elegant
# 2) modeling the whole curve with pre-defined curves (e.g. Crawley 674): too complicated because one needs to find out the starting values
# 3) GAM: too complicated and oversophisticated
# 4) analyzing data before the asymptotic curve when it is still linear: initial growth rate --> best solution because it is clear and crisp

# inspection of the interaction plot (s.a.) showed that plant height - time relationship becomes asymptotic at 9 weeks
# omitting time values for week 12, 15, 18

initial<-adapt_time[time==c("3","6","9"),]

# handling repeated measures
# either specify correlation structure in gls (which i prefer, see Zuur et al. p. 158, UCLA script)
# or use lmer or lme and specify correlation structure in random intercept and/or random slope



#  This is the data we want to analyze. TAKE GREAT CARE to specify the following dataframe                                 ## !!!

initial_0_contr<-adapt_time[time==c("3","6","9")&geo_dist%in%c("0","Contr"),]      # data set with only control and own soil
adapt_time[time==c("3","6","9")&geo_dist%in%c("0","Contr"),]


library(nlme)

# 1) first construct a groupedData object so that R knows that individuals are repeated (see Pinheiro&Bates 1998 online script: CO2 undorthodong example)
initial_0_contr_grouped<-groupedData(height~time|plant/ind, outer=~factor(region)*factor(geo_dist), data=initial_0_contr)
initial_0_contr_grouped


# 2) testing different autocorrelation structures according to UCLA script & Zuur et al. 158 without any random effects  ____________________________________________
#     block is incorporated as a covariable (a 4way interactin did not work maybe because of overparametrization)


Form1<-formula(height ~ time * factor(geo_dist) * region + time * block,data=initial_0_contr_grouped)

# compound symmetry
M1<-gls(Form1,corr=corCompSymm(),method="ML",data=initial_0_contr_grouped)

# unstructured covariance matrix
M2<-gls(Form1,corr=corSymm(),weights=varIdent(form=~1|time),method="ML",data=initial_0_contr_grouped)
# funktioniert immer wieder nicht!

# autoregressive var-cov matrix
M3<-gls(Form1,corr=corAR1(),method="ML",data=initial_0_contr_grouped)

# autoregressie with heterogeneous variance var-cov matrix
M4<-gls(Form1,corr=corAR1(),weights=varIdent(form=~1|time),method="ML",data=initial_0_contr_grouped)


anova(M1,M3,M4)                 # M4 is best model --> the best var-cov structure is autoregressive with heterogeneous variances


# the data has an autoregressive var-cov structure with heterogeneous variances
# what does it mean? it means observations that are more proximate are correlated and variances change over time



# 3) after having found the optimum correlation and variance structure, need to find the right random effects structure with lme (Zuur et al. 160)

M5<-lme(Form1,random=~1|ind, corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)         # only ind as random effect
M6<-lme(Form1,random=~1|plant/ind, corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)   # block and ind as random effects

M5a<-lme(Form1,random=~1|region/ind, corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)
M6a<-lme(Form1,random=~1|region/plant/ind, corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)

M5b<-lme(Form1,random=~time|ind, corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)         # only ind as random effect
M6b<-lme(Form1,random=~time|plant/ind, corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)

M5c<-lme(Form1,random=~time|region/ind, corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)
M6c<-lme(Form1,random=~time|region/plant/ind, corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)       # doesnt work

M9<-gls(Form1,corr=corAR1(),weights=varIdent(form=~1|time),data=initial_0_contr_grouped)

anova(M5,M6,M5a,M6a,M5b, M6b, M5c, M9)                          # model6b with plant pop as a random intercept and slope is best


anova(M6b,M9)                          # M6a ist best (retain time|pop/ibd)


# is block a necessary fixed effects factor?

M10<-lme(Form1,random=~time|plant/ind, corr=corAR1(), method="ML",weights=varIdent(form=~1|time),data=initial_0_contr_grouped)
# choosing method="ML" leaads to convergence problems

M10a<-lme(Form1,random=~time|plant/ind, corr=corAR1(),weights=varIdent(form=~1|time), control = list(opt="optim"), method="ML",data=initial_0_contr_grouped)

#does not work because of the time in the random effect, so i will use the next best model

anova(M5,M6,M5a,M6a,M5b, M5c, M9)                           # model 5b is the best model (time|ind)

anova(M10)

M10b<-lme(Form1,random=~time|ind, corr=corAR1(),weights=varIdent(form=~1|time), control = list(opt="optim"), method="ML",data=initial_0_contr_grouped)
anova(M10b)

M11<-lme(height ~ time * factor(geo_dist) * factor(region) + factor(block),random=~time|ind, method="ML",
         corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)

anova(M10b,M11)           # its cose, but removing time * block according to the p value is justified

M12<-lme(height ~ time * factor(geo_dist) * factor(region),random=~time|ind, method="ML",
         corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)

anova(M11,M12)           # removing block is better according to the AIC
anova(M10b,M12)


## FINAL model: measurments have an auto-regressive correlation structure (obs. closer to each other are more similar),
#               variance at each time point can be different, each individual growth can have different intercepts


##_____________________________________________________________________________________________________________________________________________________________

final_control<-lme(height ~ time * factor(geo_dist) * factor(region),random=~time|ind, method="ML",                      # final model
                corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)

anova(final_control)

##_____________________________________________________________________________________________________________________________________________________________


# testing the significance of the random effect time x ind

final_control_a<-lme(height ~ time * factor(geo_dist) * factor(region),random=~time|ind, method="ML",
                corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)

final_control_b<-gls(height ~ time * factor(geo_dist) * factor(region), method="ML",
                corr=corAR1(), weights=varIdent(form=~1|time),data=initial_0_contr_grouped)
                
anova(final_control_a,final_control_b)        # p < 0.0001

0.5 * (1 - pchisq(35.42928, 2))



# inspecting heteroscedacity of residuals
H1<-resid(final_control,type="normalized")
H2<-fitted(final_control)
par(mfrow=c(2,2))
plot(x=H2,y=H1, xlab="fitted values", ylab="residuals")
boxplot(H1~region, data=initial_0_contr_grouped, main="region",ylab="residuals")
boxplot(H1~geo_dist,data=initial_0_contr_grouped, main="distances",ylab="residuals")
plot(x=time$initial_0_contr_grouped,y=H1,data=initial_0_contr_grouped, main="time",ylab="residuals")  # geht nicht, aber sonst alles ok


# checking for heteroscedacity, second option

plot(final_control,resid(., type="n")~height|region)       # no pattern emerging
plot(final_control,resid(., type="n")~height|geo_dist)     # no pattern emerging
plot(final_control,resid(., type="n")~height|geo_dist*region)   # no pattern emerging
plot(final_control,resid(., type="n")~height|time)         # variation seems to vary among time points, but we accounted for that in our model!

# checking for normality of residulas
qqnorm(M9,~resid(.,type="n"))



# use summary() and not anova() according to Zuur et al. (p. 135) as this implements sequential testing
# region is not significant but we cannot get rig of it because we have the interaction and each factor will be listed seperately, automatically
#WARNING: R provides Type I sequential SS, not the default Type III marginal SS reported by SAS and SPSS.
# In a nonorthogonal design with more than one term on the right hand side of the equation order will matter
# (i.e., A+B and B+A will produce different results)! We will need use the drop1( ) function to produce the familiar Type III results.
# It will compare each term with the full model. See also Crawley p. 504+p.507
anova(final_initial_0_contr,type="marginal")
anova(final_initial_0_contr)
drop1(final_initial_0_contr,~.,test="F")

# which table to choose? anova() or summary? See Crawley p. 364 and Zuur et al. p. 135: anova() applies sequential testing.
# Better use summary() for the manuscript and try different orders in interaction term, here diff. orders dont matter





# OPTIONAL: plotting the results: height growth seems to have a different slope in "own soil" und "control" (geo_dist levels): need to display data
par(mfrow=c(1,2))
interaction.plot(time[time==c("3","6","9")&geo_dist%in%c("0","Contr")&region%in%c("EUR")],
                 factor(geo_dist[time==c("3","6","9")&geo_dist%in%c("0","Contr")&region%in%c("EUR")]),
                 height[time==c("3","6","9")&geo_dist%in%c("0","Contr")&region%in%c("EUR")],
                 main="Europe", ylab="mean height (cm)",xlab="time (weeks)",legend=FALSE, cex.axis=1.5,cex.lab=1.8)
legend(locator(1), legend=c("control", "own soil"), cex=1.0, col=c("black", "black"), lty=1:4)


interaction.plot(time[time==c("3","6","9")&geo_dist%in%c("0","Contr")&region%in%c("KZ")],
                 factor(geo_dist[time==c("3","6","9")&geo_dist%in%c("0","Contr")&region%in%c("KZ")]),
                 height[time==c("3","6","9")&geo_dist%in%c("0","Contr")&region%in%c("KZ")],
                 main="Kazakhstan", ylab="mean height (cm)",xlab="time (weeks)",legend=FALSE, cex.axis=1.5,cex.lab=1.8)
legend(locator(1), legend=c("control", "own soil"), cex=1.0, col=c("black", "black"), lty=1:4)


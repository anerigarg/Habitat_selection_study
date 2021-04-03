#gamma glmm code for Iris


# packages  ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(psych)
library(glmmTMB)
library(ggeffects)
library(emmeans)
library(stargazer)

# tips I like in R:
#`%notin%` = negate(`%in%`)   #this makes a function that's kind of like the opposite of %in% in tidyverse
# write.csv(name, "name.csv", row.names = FALSE)      #this imports df without that weird extra col

# keyboard shortcuts:
# ctrl+shift+M = quickly adds the pipe symbols (%>%)
# ctrl+shift+R = makes a new drop-down section
# Alt+O = closes all the open sections 


# data exporation ---------------------------------------------------------

ARD_3_relrich <- read_csv("data/standardize to control calculations/ARD_3_relrich.csv")

ARD3rr <- ARD_3_relrich %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                        labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  mutate(plot = as.factor(plot)) %>% 
  rename(Tr = treatment) %>% 
  rename(C = complexity)

hist(ARD3rr$rrich)
range(ARD3rr$rrich) #-1.750 to 6.625 (so should add 2 to make all positive values)

describeBy(ARDrr, group = list(ARDrr$C, ARDrr$Tr)) #for vars
describeBy(ARDrr, group = (ARDrr$C)) #for one var
describeBy(ARDrr, group = (ARDrr$Tr)) 

#look at variance
boxplot(rrich~plot, data = ARD3rr) 
boxplot(rrich~visit, data = ARD3rr)  
boxplot(rrich~Tr, data = ARD3rr)

#add constant to make positive
ARD3rr1 <- ARD3rr %>% 
  mutate(rrich1 = rrich + 2)

# assess gamma over data distribution ________________________________________________________________________________________

r.mean1 = mean(ARD3rr1$rrich1)
r.var1 = var(ARD3rr1$rrich1)
gamma.beta1r = r.mean1/r.var1
gamma.alpha1r = r.mean1*gamma.beta1r

hist(ARD3rr1$rrich1, prob = T)
curve(dgamma(x, gamma.alpha1r, gamma.beta1r),0,25,add = T, col = 'red')

#looks to see how the red gamma dist curve fits on top of data

# choose random effect structure________________________________________________________________________________________________

M0glmmTMBr <- glmmTMB(rrich1~Tr*C, family=Gamma(link="log"), data = ARD3rr1)
M1glmmTMBr <- glmmTMB(rrich1~Tr*C + (1|visit), family=Gamma(link="log"), data = ARD3rr1) #just visit as re
M2glmmTMBr <- glmmTMB(rrich1~Tr*C + (1|plot), family=Gamma(link="log"), data = ARD3rr1) #just plot sa re
M3glmmTMBr <- glmmTMB(rrich1~Tr*C + (1|plot) + (1|visit), family=Gamma(link="log"), data = ARD3rr1) #both visit and plot as re

#recommended to use link = log in family, apparently it's less weird than using "inverse"

AIC(M0glmmTMBr, M1glmmTMBr, M2glmmTMBr, M3glmmTMBr) #M1 is the best

summary(M1glmmTMBr)

# post-hoc test using emmeans, there is a way to convert back from log (it didn't make a dif to the t.ration and p-value i was after) so i did not
# but if you do, I thik the term is called "contrast"

M1glmmTMBremm <- emmeans(M1glmmTMBr, pairwise ~ Tr) #just between treatments
pairs(M1glmmTMBremm)
M1glmmTMBremm1 <- emmeans(M1glmmTMBr, pairwise ~ C) #just complexity
pairs(M1glmmTMBremm1)
M1glmmTMBremm2 <- emmeans(M1glmmTMBr, pairwise ~ Tr|C) #comp*treat
pairs(M1glmmTMBremm2)


# model assessment with dharma --------------------------------------------

#dispersion test
testDispersion(M1glmmTMBr)
# p > 0.05, not oversdispersed, p-value = 0.616

simouttmb3r <- simulateResiduals(fittedModel = M1glmmTMBr, plot = T) # assess like qqplot
# it will also display results of uniformity, dispersion and outlier tests on the plot

residuals(simouttmb3r)
plotResiduals(simouttmb3r, ARD3rr1$Tr) #looks good
plotResiduals(simouttmb3r, ARD3rr1$C) #looks good
hist(simouttmb3r) # I think just ok, a couple outliers (not surprised),but outlier test looks ok

#goodness of fit tests
testResiduals(simouttmb3r)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # non sig, p-value = 0.6721
# 2) testOutliers: if there are more simulation outliers than expected        # non sig, p = 1
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig, p-value = 0.616


# visualize ---------------------------------------------------------------

# just data plotted w s.e.:
ARDrr_sum <- ARD3rr %>% 
  group_by(Tr, C) %>% 
  summarize(rrich.mean = mean(rrich), rrich.sd = sd(rrich)) %>%
  mutate(rrich.se = rrich.sd/sqrt(1280))

ggplot() +
  geom_col(data = ARDrr_sum,
           aes(x = Tr,
               y = rrich.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.9) +
  geom_errorbar(data =ARDrr_sum,
                aes(x = Tr,
                    ymin = rrich.mean+rrich.se,
                    ymax = rrich.mean-rrich.se),
                width = 0.3) +
  scale_fill_manual(values = c("#FFB000", "#FE6100", "#DC267F", "#785EF0", "#648FFF")) + #hex codes that are colour-blind friendly
  labs(x = expression(Percent~living~coral),
       y = expression(Relative~richness~(number~of~species))) +
  facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")

# extract predictions from model and plot on top of data
predM3glmmr <- ggpredict(M1glmmTMBr, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group) %>% 
  mutate(pred2 = predicted - 2) #got to take away 2 again since I added it as a constant before

ggplot() +                             
  geom_col(data = ARDrr_sum, #data
           aes(x = Tr,
               y = rrich.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_col(data = predM3glmmr , #model prediction
           aes(x = Tr,
               y = pred2,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM3glmmr , #model errorbars (could also put in conf int)
                aes(x = Tr,
                    ymin = pred2+std.error,
                    ymax = pred2-std.error),
                width = 0.3) +
  ggtitle("predicted richness (outline) over data (colour)") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 


# create model table ------------------------------------------------------

# I couldn't get this to work with glmmTMB, but Noelle was able to, so you also may be able to! 

stargazer(M1glmmTMBr, type = "text") #this will give a basic output in the console of what the summary table can look like

# below code is for making a bigger table with multiple model inputs and some code that customizers certain features:
stargazer(lmmM1as,M3glmmTMBe1,glm.gam, #you can add more than 1 model output here
          title = "Table 1. Model Resuls", #changes the main title of the table
          out = "table1.doc", type = "html", #this exports the table into a word doc in your working environment
          dep.var.labels = c("Relative Recruitment Rate", "Relative Saturation Density", "Relative Final Density"), #to change the names of the dependent variables in the table
          intercept.bottom = FALSE, #since the automatic outut puts the intercept (constant) in the last row, this will put it back to the top like how it's dispalyed in the summary table
          # order = c("Constant "), #to change the order of variables
          covariate.labels = c("Low, 0% (intercept)","Low, 30%", "Low, 50%", "Low, 70%", "Low, 100%","High, 0%", "High, 30%", "High, 50%", "High, 70%", "High, 100%"), #to rename the variables
          omit.stat = c("LL", "bic")) #choose which summary stats you want to include/exclude
# dep.var.caption = "AR1 lmm", "glmm") 
# ci = TRUE, ci.level = 0.90) #if you want to input confidence intervals instead of standard error in the summary table



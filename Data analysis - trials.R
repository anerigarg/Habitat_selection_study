# data analysis - habitat selection

# 1) BACKGROUND COMPLEXITY ---------------------------------------------------



 _________________________________________________

# A. RECRUITMENT RATE -----------------------------------------------------


# option 1) t-test --------------------------------------------------------


ARD_3_rate <- read_csv("data/rate calculations/ARD_3_rate.csv", 
                       col_types = cols(X1 = col_skip())) %>% 
  dplyr::mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  dplyr::mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  dplyr::mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                               labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  dplyr::mutate(rate = as.numeric(rate)) %>% 
  dplyr::mutate(plot = as.factor(plot)) %>% 
  dplyr::rename(Tr = treatment) %>% 
  dplyr::rename(C = complexity) %>% 
  dplyr::filter(visit != "1")

#checks out, 1440 observations (24 clusters * 4 plots * 15 visits)

ARDrbc <- ARD_3_rate %>% 
  filter(Tr == "control")

#check normality: 
hist(ARDrbc$rate) #normalish, a few v big or small responses, not surprising given variability in recruitment rate
shapiro.test(ARDrbc$rate) # nope, p-value = 9.134e-14
range(ARDrbc$rate) #-7.6 to 7

# homogeneity of variance
boxplot(rate~plot, data = ARDrbc) #hmm may have some spatial cor
boxplot(rate~visit, data = ARDrbc) #not as extreme cheese wedge, but still there
boxplot(rate~C, data = ARDrbc) # looks hetero
leveneTest(ARDrbc$rate, ARDrbc$C) # nope, 0.00145 **

describeBy(ARDrbc, group=ARDrbc$C) 


# ok but assumptions matter more if you have unequal sample sizes (this one is equal)
# Note that if you don’t specify equal variances, R does a Welch’s Two Sample t-test or what Zar
# calls the Welch’s approximate t, or Behrens-Fisher test (Zar 5th ed. pp.138-142), with a
# downwards adjusted degrees of freedom (i.e., more conservative). You could try running the
# Welch’s test  to see the difference (although it’s hardly anything in this case) by setting
# var.equal=FALSE, or by simply leaving the argument out of the command.

ttest.rbc <- t.test(ARDrbc$rate ~ ARDrbc$C, var.equal=FALSE)
ttest.rbc

# You can visualize the test results using boxplots. Run the command:
boxplot(ARDrbc$rate ~ ARDrbc$C, ylim=c(-10,10))

# effect size
# cohen's d for student t-test

ttest.rbc %>% 
  cohen.d(rate ~ C, var.equal = FALSE) # not working

# mean rate for low was 0.08 (SD = 1.33), and mean rate for high was 0.04 (SD = 1.98).
# A Welch's two-sample t-tes showed that the difference was not sig different (t = 0.20058, df = 208.23, p-value = 0.8412)


# option 2) Wilcoxon non-parametric test --------------------------------------------------

wilcox.test(ARDrbc$rate ~ ARDrbc$C)
# W = 7563.5, p-value = 0.4969


# visualize: --------------------------------------------------------------

# data
ARDrbc_sum <- ARDrbc %>% 
  group_by(C) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>%
  mutate(rate.se = rate.sd/sqrt(240))

ggplot() +
  geom_col(data = ARDrbc_sum,
           aes(x = C,
               y = rate.mean,
               group = C,
               fill = C),
           alpha = 0.5) +
  geom_errorbar(data =ARDrbc_sum,
                aes(x = C,
                    ymin = rate.mean+rate.se,
                    ymax = rate.mean-rate.se),
                width = 0.3) +
  ggtitle("just data")
  # ylim(-0.4,0.7) +
  # facet_grid(.~C) 


# still figuring out how to extract predicted values from a t-test:
# #ttest rate
# predt <- ggpredict(ttest.rbc, terms = "C") %>% 
#   rename(C = x)
# pred <-ggpredict(ttest.rbc)
# 
# ARDr_sum <- ARDr %>% 
#   group_by(Tr, C) %>% 
#   summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
#   mutate(rrate.se = rrate.sd/sqrt(1280))
# 
# #same graph: ()
# ggplot() +
#   geom_col(data = ARDr_sum,
#            aes(x = Tr,
#                y = rrate.mean,
#                group = Tr,
#                fill = Tr),
#            alpha = 0.35) +
#   geom_col(data = predM1a,
#            aes(x = Tr,
#                y = predicted,
#                group = Tr),
#            colour = "black",
#            fill = "transparent",
#            size = 1.2) +
#   geom_errorbar(data = predM1a,
#                 aes(x = Tr,
#                     ymin = predicted+std.error,
#                     ymax = predicted-std.error),
#                 width = 0.3) +
#   ggtitle("predicted AR1 lmm (outline) over data (colour)") +
#   # ylim(-0.4,0.7) +
#   facet_grid(.~C) 



# B. FINAL DENSITY ---------------------------------------------------------------------

ARD_4to6 <- read_csv("data/filter 0 values/ARD_4to6.csv") %>% 
  dplyr::mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  dplyr::mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  dplyr::mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                               labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  dplyr::mutate(density = as.numeric(abundance)) %>% 
  dplyr::mutate(plot = as.factor(plot)) %>% 
  dplyr::rename(Tr = treatment) %>% 
  dplyr::rename(C = complexity)


#checks out, 1536 observations (24 clusters * 4 plots * 16 visits)

ARD4bc <- ARD_4to6 %>% 
  filter(Tr == "control") %>% 
  filter(visit %in% c("14", "15", "16")) %>% 
  mutate(density1 = sqrt(density)) %>% 
  mutate(density2 = log(density)) %>% 
  mutate(density3 = Math.cbrt(density))
  

#check normality: 
hist(ARD4bc$density) #that's a poisson dist
hist(ARD4bc$density1)
hist(ARD4bc$density2)
hist(ARD4bc$density3) #they all kind of suck, maybe a non-parametric test right away?

shapiro.test(ARD4bc$density) # nope, p-value = 8.086e-08
range(ARD4bc$density) #0 to 13

# homogeneity of variance
boxplot(density~plot, data = ARD4bc) #quite a bit of variation
boxplot(density~visit, data = ARD4bc) 
boxplot(density~C, data = ARD4bc) # looks hetero
leveneTest(ARD4bc$density, ARD4bc$C) #homogeneous, 0.0525

describeBy(ARD4bc, group=ARD4bc$C) 
# mean final densit for Low is 1.54 (SD = 1.74) and mean final density for High is   3.00 (SD = 3.62)
# Low median is 1, H median is 

# option 1) t.test --------------------------------------------------------


# ok but assumptions matter more if you have unequal sample sizes (this one is equal)
# Note that if you don’t specify equal variances, R does a Welch’s Two Sample t-test or what Zar
# calls the Welch’s approximate t, or Behrens-Fisher test (Zar 5th ed. pp.138-142), with a
# downwards adjusted degrees of freedom (i.e., more conservative). You could try running the
# Welch’s test  to see the difference (although it’s hardly anything in this case) by setting
# var.equal=FALSE, or by simply leaving the argument out of the command.

ttest.4bc <- t.test(ARD4bc$density ~ ARD4bc$C, var.equal=TRUE)
ttest.4bc
ttest.4bc1 <- t.test(ARD4bc$density ~ ARD4bc$C, var.equal=FALSE) #same output
ttest.4bc1

# not significant (t = -1.7766, df = 46, p-value = 0.08225)

# option 2) Wilcoxon non-parametric test --------------------------------------------------

wilcox.test(ARD4bc$density ~ ARD4bc$C)
# not significant  (W = 228, p-value = 0.2094)
# I'm surprised these aren't sig different considering mean and sd


#  option 3) glmm ---------------------------------------------------------

glmm4bc <- glmmTMB(density~C, data = ARD4bc)
glmm4bc1 <- glmmTMB(density~C + (1|plot), data = ARD4bc)
glmm4bc2 <- glmmTMB(density~C + (1|visit), data = ARD4bc)
glmm4bc3 <- glmmTMB(density~C + (1|visit) + (1|plot), data = ARD4bc)

AIC(glmm4bc, glmm4bc1, glmm4bc2, glmm4bc3) #the lm was the best, so could do a glm?

glm4bc <- glm(density~C, data = ARD4bc) #same as above

# except for these need numerical var (0 or 1)

ARD4bc1 <- ARD4bc %>% 
  mutate(Cnum = ifelse(C == "High", 1,0))

glmm4bc <- glmmTMB(density~C, data = ARD4bc1)
glmm4bc1 <- glmmTMB(density~C + (1|plot), data = ARD4bc1)
glmm4bc2 <- glmmTMB(density~C + (1|visit), data = ARD4bc1)
glmm4bc3 <- glmmTMB(density~C + (1|visit) + (1|plot), data = ARD4bc1)

AIC(glmm4bc, glmm4bc1, glmm4bc2, glmm4bc3) #the glm is the best

# I feel like this is dumb....gonna just move on


# visualize:  -------------------------------------------------------------

# data
ARD4bc_sum <- ARD4bc %>% 
  group_by(C) %>% 
  summarize(dens.mean = mean(density), dens.sd = sd(density)) %>%
  mutate(dens.se = dens.sd/sqrt(48))

ggplot() +
  geom_col(data = ARD4bc_sum,
           aes(x = C,
               y = dens.mean,
               group = C,
               fill = C),
           alpha = 0.5) +
  geom_errorbar(data =ARD4bc_sum,
                aes(x = C,
                    ymin = dens.mean+dens.se,
                    ymax = dens.mean-dens.se),
                width = 0.3) +
  ggtitle("just data - final density 4-6")



# C. OVERALL DENSITY ------------------------------------------------------


ARD_3 <- read_csv("data/filter 0 values/ARD_3.csv") %>% 
  dplyr::mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  dplyr::mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  dplyr::mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                               labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  dplyr::mutate(density = as.numeric(abundance)) %>% 
  dplyr::mutate(plot = as.factor(plot)) %>% 
  dplyr::rename(Tr = treatment) %>% 
  dplyr::rename(C = complexity)


#checks out, 1536 observations (24 clusters * 4 plots * 16 visits)

ARD3bc <- ARD_3 %>% 
  filter(Tr == "control") %>% 
  mutate(density1 = sqrt(density)) %>% 
  mutate(density2 = log(density)) %>% 
  mutate(density3 = Math.cbrt(density))


#check normality: 
hist(ARD3bc$density) #that's a neg binom
hist(ARD3bc$density1)
hist(ARD3bc$density2) 
hist(ARD3bc$density3) #they all kind of suck, maybe a non-parametric test right away?

shapiro.test(ARD3bc$density) # nope, p-value = 8.086e-08
shapiro.test(ARD3bc$density3)
range(ARD4bc$density) #0 to 13

# homogeneity of variance
boxplot(density~plot, data = ARD3bc) #quite a bit of variation bw HS and HN
boxplot(density~visit, data = ARD3bc) 
boxplot(density~C, data = ARD3bc) # looks hetero
leveneTest(ARD3bc$density, ARD3bc$C) #hetero, 0.0006589 ***

describeBy(ARD3bc, group=ARD3bc$C) 
# mean density for Low is  1.48 (SD = 2.14) and mean density for High is 2.76 (SD = 3.42)
# Low median is 1, H median is 2

# option 1) t.test --------------------------------------------------------


# ok but assumptions matter more if you have unequal sample sizes (this one is equal)
# Note that if you don’t specify equal variances, R does a Welch’s Two Sample t-test or what Zar
# calls the Welch’s approximate t, or Behrens-Fisher test (Zar 5th ed. pp.138-142), with a
# downwards adjusted degrees of freedom (i.e., more conservative). You could try running the
# Welch’s test  to see the difference (although it’s hardly anything in this case) by setting
# var.equal=FALSE, or by simply leaving the argument out of the command.

# ttest.4bc <- t.test(ARD4bc$density ~ ARD4bc$C, var.equal=TRUE)
# ttest.4bc
ttest.3bc1 <- t.test(ARD3bc$density ~ ARD3bc$C, var.equal=FALSE) #same output
ttest.3bc1

# significant (t = -3.5674, df = 213.4, p-value = 0.0004452)

# option 2) Wilcoxon non-parametric test --------------------------------------------------

w3bc <- wilcox.test(ARD3bc$density ~ ARD3bc$C)
# overall recruit density is significantly different between High and Low complexity  (W = 6214, p-value = 0.0006028)
w3bc

# visualise:  -------------------------------------------------------------

#data

ARD3bc_sum <- ARD3bc %>% 
  group_by(C) %>% 
  summarize(dens.mean = mean(density), dens.sd = sd(density)) %>%
  mutate(dens.se = dens.sd/sqrt(256))

ggplot() +
  geom_col(data = ARD3bc_sum,
           aes(x = C,
               y = dens.mean,
               group = C,
               fill = C),
           alpha = 0.5) +
  geom_errorbar(data =ARD3bc_sum,
                aes(x = C,
                    ymin = dens.mean+dens.se,
                    ymax = dens.mean-dens.se),
                width = 0.3) +
  ggtitle("just data - overall density (0-3")


# D. DIVERSITY METRICS ----------------------------------------------------

ARD_3_rich <- read_csv("data/filter 0 values/ARD_3_rich.csv") %>% 
  dplyr::mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  dplyr::mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  dplyr::mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                               labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  dplyr::mutate(plot = as.factor(plot)) %>% 
  dplyr::rename(Tr = treatment) %>% 
  dplyr::rename(C = complexity)

#checks out, 1536 observations (24 clusters * 4 plots * 16 visits)

ARD3bcr <- ARD_3_rich %>% 
  filter(Tr == "control") %>% 
  mutate(rich1 = sqrt(rich)) %>% 
  mutate(rich2 = log(rich)) %>% 
  mutate(rich3 = Math.cbrt(rich))


#check normality: 
hist(ARD3bcr$rich) #that's poisson-y
hist(ARD3bcr$rich1) #nope
hist(ARD3bcr$rich2)  #meh
hist(ARD3bcr$rich3) #they all kind of suck, maybe a non-parametric test right away?

shapiro.test(ARD3bcr$rich) # nope,p-value = 1.137e-15
range(ARD3bcr$rich) #0 to 4

# homogeneity of variance
boxplot(rich~plot, data = ARD3bcr) #quite a bit of variation bw HS and HN
boxplot(rich~visit, data = ARD3bcr) 
boxplot(rich~C, data = ARD3bcr) # looks homo and very similar lol
leveneTest(ARD3bcr$rich, ARD3bcr$C) #homo, 0.1422

describeBy(ARD3bcr, group=ARD3bcr$C) 
# mean density for Low is  0.92  (SD = ) 0.90) and mean density for High is 0.98 (SD = 0.81)
# Low median is 1, H median is 1

# option 1) t.test --------------------------------------------------------

ttest.3bcr <- t.test(ARD3bcr$rich ~ ARD3bcr$C, var.equal=TRUE)
ttest.3bcr


# non significant (t = -0.58244, df = 254, p-value = 0.5608)

# option 2) Wilcoxon non-parametric test --------------------------------------------------

w3bcr <- wilcox.test(ARD3bcr$rich ~ ARD3bcr$C)
# species richness is not significantly different between High and Low complexity  (W = 7707.5, p-value = 0.3821)
w3bcr

# visualise:  -------------------------------------------------------------

#data

ARD3bcr_sum <- ARD3bcr %>% 
  group_by(C) %>% 
  summarize(rich.mean = mean(rich), rich.sd = sd(rich)) %>%
  mutate(rich.se = rich.sd/sqrt(256))

ggplot() +
  geom_col(data = ARD3bcr_sum,
           aes(x = C,
               y = rich.mean,
               group = C,
               fill = C),
           alpha = 0.5) +
  geom_errorbar(data =ARD3bcr_sum,
                aes(x = C,
                    ymin = rich.mean+rich.se,
                    ymax = rich.mean-rich.se),
                width = 0.3) +
  ggtitle("just data - overall rich (0-3")



# 2) STRUCTURE VS NO STRUCTURE --------------------------------------------

# uneven sample size, consider kruskall wallis test


# code from lab 5 of 430: 
options(contrasts=c("contr.sum","contr.poly"))
# You only need to run it once every R session.
# This command is needed to generate so-called Type III sums of squares, which are appropriate 
# for the ANOVA in this lab as well as all other ANOVA analyses you will do in the other labs
# for this course.
# Type III SS allow for F-tests for any term or factor in the analysis, including interactions,
# after controlling for all other factors in the ANOVA model. Type I and Type II SS are for
# other specific purposes that are not appropriate for any ANOVA analyses you will do in this
# course.
# This isn’t much of a concern when data are balanced (i.e., equal sample sizes among groups or
# subgroups) because Type I, Type II, and Type III SSs are all equal under those conditions.
# However, they can differ widely when analyzing unbalanced data, so it is a good practice to
# use the options command above whenever you are doing ANOVA analyses in R _______________________________________________

# A. RECRUITMENT RATE -----------------------------------------------------
# B. FINAL DENSITY ---------------------------------------------------------------------
# C. DIVERSITY METRICS ----------------------------------------------------



# 3) COMPOSITION ----------------------------------------------------------
 ______________________________________________________________

# A. RELATIVE RECRUITMENT RATE -----------------------------------------------------



# test modelling 1:  ----------------------------------------------------------


# lm 

#following code from Andy Field textbook ch 12, factorial anova

# # install.packages("car")
# install.packages("compute.es")
# # install.packages("ggplot2"); 
# install.packages("multcomp") 
# install.packages("pastecs") 
# install.packages("reshape") 
# install.packages("WRS", repos="http://R-Forge.R-project.org")

library(car); library(compute.es); library(ggplot2); library(multcomp);
library(pastecs); library(reshape); library(WRS)

### visuals and assumptions testing ###

# df
ARD_3_rate_effect <- read_csv("data/standardize to control calculations/ARD_3_rate_effect.csv") %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"),
                            labels = c("0", "0.3", "0.5", "0.7", "1.0"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))
         
hist(ARD_3_rate_effect$effect.size)
ggplot(data = ARD_3_rate_effect) +
  geom_boxplot(aes(x = treatment,
                   y = effect.size)) +
  # facet_grid(treatment~complexity) +
  facet_grid(complexity~treatment)+
  ylim(-0.5,0.5)
#looks normalish...

ARD_3_rate_effect$treat_comp <- paste(ARD_3_rate_effect$treatment, "-", ARD_3_rate_effect$complexity)

shapiro.test(ARD_3_rate_effect$effect.size) 
# data:  ARD_3_rate_effect$effect.size
# W = 0.94471, p-value = 6.491e-06 #not normal

leveneTest(ARD_3_rate_effect$effect.size, ARD_3_rate_effect$treatment) #variance is homogenous
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   4  0.8924 0.4701
#       155  
leveneTest(ARD_3_rate_effect$effect.size, ARD_3_rate_effect$complexity)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   1  0.4632 0.4971
#       158 
leveneTest(ARD_3_rate_effect$effect.size, ARD_3_rate_effect$treat_comp)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   9   1.152 0.3301
#       150 
leveneTest(ARD_3_rate_effect$effect.size, interaction(ARD_3_rate_effect$treatment,
                                                      ARD_3_rate_effect$complexity)) #interaction, variance is homogenous
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   9   1.152 0.3301
#       150 

by(ARD_3_rate_effect$effect.size, ARD_3_rate_effect$treatment, stat.desc)
by(ARD_3_rate_effect$effect.size, ARD_3_rate_effect$complexity, stat.desc)
by(ARD_3_rate_effect$effect.size, list(ARD_3_rate_effect$treatment,
                                       ARD_3_rate_effect$complexity), stat.desc)

# general lm (anova) - notes from andy field book Ch 12 Factorial anovas 
# same assumptions as other parametric tests (homogeneity of variance , independent observations, distributions within groups are normally distributed)
# "robust" test, but mainly when there are equal sample sizes (works for this data set)
# post-hoc tests to see what's the actual difference could be Tukey's HSD test (equal sample sizes and similar group variances)
# ok so for this df and question, I think since my response is relative rate it would be an independent factorial design


### contrasts ###

# this is so that we can later detect the main effects and where the difs bw groups are
# we're going to look at type III sums of squares - need orthogonal contrasts.... jane box 11.1

# complexity has 2 levels, can code orthogonal contrast as -1(low) and 1(high)
# since I've made treatment continuous don't have to set contrasts, it's in there already (0, 0.3, -.5, 0.7, 1.0)

contrasts(ARD_3_rate_effect$complexity)<-c(-1, 1)
ARD_3_rate_effect$complexity #at the bottom it shows -1 for low and 1 for high


### fitting the model ###

composition.rate.lm <- aov(effect.size ~ treatment + complexity + treatment*complexity, data = ARD_3_rate_effect)
Anova(composition.rate.lm, type = "III")

# output:

# Anova Table (Type III tests)
# 
# Response: effect.size
# Sum Sq  Df F value Pr(>F)
# (Intercept)           0.088   1  0.2075 0.6494
# treatment             0.245   4  0.1446 0.9652
# complexity            0.023   1  0.0546 0.8155
# treatment:complexity  0.067   4  0.0396 0.9970
# Residuals            63.539 150

# so there is no difference in relative recruitment rate between treatments, background complexities, or their interactions
# not that surprising given wide error bars of the s.e.

summary.lm(composition.rate.lm)

# Call:
#   aov(formula = effect.size ~ treatment + complexity + treatment * 
#         complexity, data = ARD_3_rate_effect)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.23737 -0.32845 -0.00807  0.36048  2.65000 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)              -0.0524089  0.1150538  -0.456    0.649
# treatment0.3              0.0876302  0.1627107   0.539    0.591
# treatment0.5              0.1140625  0.1627107   0.701    0.484
# treatment0.7              0.0488281  0.1627107   0.300    0.765
# treatment1.0              0.0810547  0.1627107   0.498    0.619
# complexity1               0.0268880  0.1150538   0.234    0.816
# treatment0.3:complexity1  0.0002604  0.1627107   0.002    0.999
# treatment0.5:complexity1  0.0114583  0.1627107   0.070    0.944
# treatment0.7:complexity1 -0.0359375  0.1627107  -0.221    0.825
# treatment1.0:complexity1 -0.0380859  0.1627107  -0.234    0.815
# 
# Residual standard error: 0.6508 on 150 degrees of freedom
# Multiple R-squared:  0.005406,	Adjusted R-squared:  -0.05427 
# F-statistic: 0.09059 on 9 and 150 DF,  p-value: 0.9997

# ok so no post-hoc analysis for this one since it's non-signficiant, and adjusted R-square is v low...




### Model Validation ###



plot(composition.rate.lm)

# from zuur: pg 23 book, 41 pdf
op <- par(mfrow = c(2, 3), mar = c(5, 4, 1, 2)) ##pannels and spacing between
plot(composition.rate.lm, add.smooth = FALSE, which = 1)
E <- resid(composition.rate.lm)
hist(E, xlab = "Residuals", main = "") # 1)normality: looks normal-ish (why are we assessing normality of resids like this??)
plot(ARD_3_rate_effect$effect.size, E, xlab = "rel.recruit.rate)", #2) independence -> i mean it looks llike the spread is the same? 
       ylab = "Residuals")
plot(ARD_3_rate_effect$treatment, E, xlab = "treatment", #2) homogeneity: looks like there's different spread of resids in each treatment? maybe i don't actually have homogeneous variances?
       ylab = "Residuals")
plot(ARD_3_rate_effect$complexity, E, xlab = "complexity", #2) and between complexities it's different
     ylab = "Residuals")
par(op) #returns to default settings


# REMEMBER: residuals are observed - fitted values. can do a test for homogeneity: F-ratio test:
E1 <- E[ARD_3_rate_effect$effect.size <= 0]
E2 <- E[ARD_3_rate_effect$effect.size < 0]
var.test(E1, E2)

# output: #not a very convicning p-valye (0.9951) --> so yeah not the right model to use. *can play around with whcih cutoff to use to see difs (still nt good)
  
#   F test to compare two variances
# 
# data:  E1 and E2
# F = 1.0004, num df = 85, denom df = 69, p-value = 0.9951
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.6328597 1.5645538
# sample estimates:
#   ratio of variances 
# 1.000428 


# can also use Bartlett test to see homogeneity bw groups: (pg 43 of pdf) *remmeber you assigned E to be residuals of the first lm
# this test is sensitive to non-normalisy

bartlett.test(E, ARD_3_rate_effect$treatment)
# data:  E and ARD_3_rate_effect$treatment
# Bartlett's K-squared = 14.832, df = 4, p-value = 0.005063  # implies they're homogenous vars
bartlett.test(E, ARD_3_rate_effect$complexity)
# data:  E and ARD_3_rate_effect$complexity
# Bartlett's K-squared = 6.0459, df = 1, p-value = 0.01394 # implies non-homogenous vars

# OK --> so i don't think a lm is a suitable choice given that there appears to be heteroscedastity in the variances and potentially some non-normality


# NEXT: 

# 1) could also try dropping first survey for "training" effects (or reducing samples in first half since there are more)
# 2) could do generalized lm for non-linear relationships (i.e. specify poisson --> but these data don't look that non-normal, it's the variance that's the issue)
# 3) could do a generalized least squares estimation ( allows different variances)
# 4) could consider doing a mixed effects model with observer and plot as random effects? take into account spatial structure 


# lm but drop first visit
# lm but only first half of visits
# test modelling 2: -------------------------------------------------------
# lmm1 - plot and days separate, treatment "standardized", , with outlier 
# lmm2 - like lmm1, but no scaling treatment 
# lmm3 - treat scaled, just days as random
# lmm4 - treat scaled, just plot, no outlier  
# lmm5 - treat scaled, just days, no outlier 
# lmm6 - days and plot_grid nested in plot
# lmm7 
# following Viktoria code for autocorelation check 
# test modelling 3: -------------------------------------------------------


# nlme - transition to using nlme - random intercept just plot and just days
# lme1 
# lme2 
# lme3 
# lme4 
# lme5 - plot and days as random effects - days nested in plot
# lme6 - plot nested in days
# test modelling 4: -------------------------------------------------------

library(lme4)
library(lmerTest)

# 1) import data (ARD_3_rate_effect) and make treatment a factor, make complexity a factor (Low then High),
#     rename visit as plot_grid_day to be more descriptive of what it actually is,
#     rename days_since_outplanting as visit (to explicitly treat it as random effect, not
#    as something I'm interested in) , rename effect.size to rrate (relative recruitment rate),
#     and rename treatment to Tr and complexity to C to make it faster for modeling

# 2) check distribution and dispersion 

# 3) test random effects structure using following formulas:
#     M0r <- (rrate ~ Tr*C) <- null model, no random effect
#     M1r <- (rrate ~ Tr*C + (1|plot)) <- random intercept
#     M2r <- (rrate ~ Tr*C + (1|visit)) <- random intercept
#     M3r <- (rrate ~ Tr*C + (1|plot) + (1|visit)) <- crossed random intercepts
#     M4r <- (rrate ~ Tr*C + (1 + rrate|visit)) <- random slope and random intercept
#     M5r <- (rrate ~ Tr*C + (1 + rrate|visit) + (1|plot)) <- random slope and intercept for daysf, random intercept for plot

# 4) use AIC for model selection (Check out MuMin) to select random effect structure

# 5) test fixed effects models:
#     M0 <- (rrate ~ "random effect structure that is best)
#     M1 <- (rrate ~ Tr*C)
#     M2 <- (rrate ~ Tr*C + "random effect structure that is best)

# 6) use AIC for model selection


#1) Import data and do the renaming:
library(readr)
library(tidyverse)
library(tidyverse)
library(ggeffects)
library(lme4)

ARD_3_rate_effect1 <- read_csv("data/standardize to control calculations/ARD_3_rate_effect1.csv") 
  
ARDr <- ARD_3_rate_effect1 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  rename(plot_grid_day = visit) %>% 
  # filter(plot_grid_day != "HS - 8 - 3") %>% 
  mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                        labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  mutate(rrate = as.numeric(effect.size)) %>% 
  rename(Tr = treatment) %>% 
  rename(C = complexity) %>% 
  filter(visit != "1") # cause they're all obv 0 on first day...

  
#df for treating time as continuous variable:
  ARD_3_rate_effect2 <- ARD_3_rate_effect1 %>% 
    mutate(treatment1 = as.numeric(treatment)) %>% 
    mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                          labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
    mutate(rrate = as.numeric(effect.size)) %>% 
    rename(Tr = treatment) %>% 
    rename(C = complexity) %>% 
    filter(visit != "1") # cause they're all obv 0 on first day...
is.numeric( ARD_3_rate_effect2$treatment1)
  

#2) Check distribution (not going to check overdispersion structure since it's nornal)


hist(ARDr$rrate) #looks normalish, according to Harrison (forget year) lmms can handle a degree of non-normality

shapiro.test(ARD_3_rate_effect1$rrate) 
# data:  ARD_3_rate_effect1$rrate
# W = 0.883, p-value < 2.2e-16 
# according to this it's non-normal distribution, but I don't think so, it's also a large sample size so may be more sensitive
# going to work on assumption of lepto-kurtotic, but normal enough for an lmm

range(ARD_3_rate_effect1$rrate) #-9 to 9, cool

#3) test random effects structure

#     M0r <- (rrate ~ Tr*C) <- null model, no random effect
#     M1r <- (rrate ~ Tr*C + (1|plot)) <- random intercept
#     M2r <- (rrate ~ Tr*C + (1|visit)) <- random intercept
#     M3r <- (rrate ~ Tr*C + (1|plot) + (1|visit)) <- crossed random intercepts
#     M4r <- (rrate ~ Tr*C + (1 + rrate|visit)) <- random slope and random intercept
#     M5r <- (rrate ~ Tr*C + (1 + rrate|visit) + (1|plot)) <- random slope and intercept for daysf, random intercept for plot

# linear model
Mt <- lm(rrate~Tr*C, data = ARDr)
summary(Mt)
Mtresids <- resid(Mt)

plot(Mt, which = 1)
plot(Mt, which = 2)

hist(Mtresids) # yup looks normal
qqnorm(Mtresids)
qqline(Mtresids)
acf(Mtresids,na.action = na.pass, main = "autocorrelation plot for resids")

boxplot(rrate~plot, data = ARDr) # doesn't look like there's much going on here, there's 4 levels, the plots are on the same reef so not that far apart, may not end up being important is my guess
boxplot(rrate~visit, data = ARDr) #def looks like something going on here, way more variation at first and differences depending on visit


M0rr <- lmer(rrate ~ Tr*C, data = ARDr) #error, wants me to specify random effects. I think Mt would be my null model (above)
M1rr <- lmer(rrate ~ Tr*C + (1|plot), data = ARDr) # getting isSingular error     ### variances of one or more liear combinations of effects close to 0 ###
M2rr <- lmer(rrate ~ Tr*C + (1|visit), data = ARDr)
M3rr <- lmer(rrate ~ Tr*C + (1|plot) + (1|visit), data = ARDr) # getting isSingular error
M4rr <- lmer(rrate ~ Tr*C + (1 + C|visit), data = ARDr) 
M5rr <- lmer(rrate ~ Tr*C + (1 + Tr|visit), data = ARDr) # model failed to converge
M6rr <- lmer(rrate ~ Tr*C + (1 + C|visit) + (1|plot), data = ARDr) # getting isSingular error
M7rr <- lmer(rrate ~ Tr*C + (1 + Tr|visit) + (1|plot), data = ARDr) # getting isSingular error and failed to converge

# Try using Control command to force the models with errors to run

Mrrc <- lmerControl(optimizer = "nloptwrap",optCtrl = list(5000))

M1rr.c <- lmer(rrate ~ Tr*C + (1|plot), data = ARDr, control = Mrrc) # still getting ?isSingular
M3rr.c <- lmer(rrate ~ Tr*C + (1|plot) + (1|visit), data = ARDr, control = Mrrc)
M5rr.c <- lmer(rrate ~ Tr*C + (1 + Tr|visit), data = ARDr, control = Mrrc)
M6rr.c <- lmer(rrate ~ Tr*C + (1 + C|visit) + (1|plot), data = ARDr, control = Mrrc) 
M7rr.c <- lmer(rrate ~ Tr*C + (1 + Tr|visit) + (1|plot), data = ARDr, control = Mrrc)

# getting same errors, going to keep using M2rr as it best fits my hypothesis with ranom effecy struture 

# treatment as continuous:
M2rr1 <- lmer(rrate ~ treatment1*C + (1|visit), data = ARD_3_rate_effect2)
summary(M2rr1)

# 4) use AIC for model selection (Check out MuMin) to select random effect structure

# the way that went, it would seem that the only viable random effect structure is from M2rr
# I don't think I can even apply AIC selection process to this...

# 5) test fixed effects models:
#     M0 <- (rrate ~ "random effect structure that is best)
#     M1 <- (rrate ~ Tr*C)
#     M2 <- (rrate ~ Tr*C + "random effect structure that is best)

M0r <-lm(rrate ~ visit, data = ARDr) # I don't think this actually makese sense to use as a null model
M1r <- lm(rrate ~ Tr*C, data = ARDr) 
M2r <- lmer(rrate ~ Tr*C + (1|visit), data = ARDr)
# M3r <- lmer(rrate ~ Tr*C + plot + (1|visit), data = ARDr)

anova(M1r, M2r)
AIC(M0r) 
AIC(M1r) #interestingly this has lower AIC compared to model with random effect --> since visit only exmplains 3% of variation, do i use lm?
AIC(M2r) # or do I have to still keep this one as top since it's at least taking hierarchical structure in consideration?
# I'm not sure I can actually compare AIC from a lm to lmm

summary(M2r)
summary(M1r)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: rrate ~ Tr * C + (1 | visit)
#    Data: ARDr
# 
# REML criterion at convergence: 4586.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -5.6803 -0.3764 -0.0043  0.3549  5.6592 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  visit    (Intercept) 0.08287  0.2879           ### so visit explains (0.082787/(0.08287 + 2.62322)) = 0.030593, only 3% of variance... ###
#  Residual             2.62322  1.6196  
# Number of obs: 1199, groups:  visit, 15
# 
# Fixed effects:
#                Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)  -8.458e-02  1.655e-01  1.639e+02  -0.511    0.610
# Tr30%         9.319e-02  2.091e-01  1.175e+03   0.446    0.656
# Tr50%         1.094e-01  2.091e-01  1.175e+03   0.523    0.601
# Tr70%         9.042e-02  2.091e-01  1.175e+03   0.432    0.666
# Tr100%        1.271e-01  2.091e-01  1.175e+03   0.608    0.543
# CHigh         5.736e-02  2.091e-01  1.175e+03   0.274    0.784
# Tr30%:CHigh   5.556e-04  2.957e-01  1.175e+03   0.002    0.999
# Tr50%:CHigh   2.444e-02  2.957e-01  1.175e+03   0.083    0.934
# Tr70%:CHigh  -2.135e-01  2.960e-01  1.175e+03  -0.721    0.471
# Tr100%:CHigh -8.125e-02  2.957e-01  1.175e+03  -0.275    0.784
# 
# Correlation of Fixed Effects:
#             (Intr) Tr30%  Tr50%  Tr70%  Tr100% CHigh  T30%:C T50%:C T70%:C
# Tr30%       -0.632                                                        
# Tr50%       -0.632  0.500                                                 
# Tr70%       -0.632  0.500  0.500                                          
# Tr100%      -0.632  0.500  0.500  0.500                                   
# CHigh       -0.632  0.500  0.500  0.500  0.500                            
# Tr30%:CHigh  0.447 -0.707 -0.354 -0.354 -0.354 -0.707                     
# Tr50%:CHigh  0.447 -0.354 -0.707 -0.354 -0.354 -0.707  0.500              
# Tr70%:CHigh  0.446 -0.353 -0.353 -0.706 -0.353 -0.706  0.499  0.499       
# Tr100%:CHgh  0.447 -0.354 -0.354 -0.354 -0.707 -0.707  0.500  0.500  0.499


# extract predictions from df for M2r
predM2r <- ggpredict(M2r, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

(Mrplot <- ggplot(data = predM2r) +
  geom_col(aes(x = Tr,
               y = predicted,
               group = Tr,
               fill = Tr)) +
  geom_errorbar(aes(x = Tr,
                    ymin = predicted+conf.low,
                    ymax = predicted+conf.high),
                width = 0.3) +
  facet_grid(.~C) +
    ylim(-0.10, 0.15) +
    ggtitle("Predicted values - recruitment rate") +
    theme_classic())

ARDr_sum <- ARDr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1280))

(Drplot <- ggplot(data = ARDr_sum)+
    geom_col(aes(x = Tr,
                 y = rrate.mean,
                 group = Tr,
                 fill = Tr),
             alpha = 0.8)+
    geom_errorbar(aes(x = Tr,
                      ymin = rrate.mean - rrate.se,
                      ymax = rrate.mean + rrate.se),
                  width = 0.3)+
    facet_grid(.~C)+
    ggtitle("data values - recruitment rate") +
    theme_classic())

#same graph: ()
ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.35) +
  geom_col(data = predM2r,
           aes(x = Tr,
               y = predicted,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM2r,
                aes(x = Tr,
                    ymin = predicted+std.error,
                    ymax = predicted-std.error),
                width = 0.3) +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

# I think I'll do an rbind and plot them next to each other...

ARDr_pred<- rbind(ARDr_sum, predM2r)

ggplot() +
  geom_col(data = predM2r,
           aes(x = Tr,
               y = predicted,
               fill = Tr),
           alpha = 0.3) +
  geom_errorbar(data = predM2r,
                aes(x = Tr,
                    ymin = predicted+conf.low,
                    ymax = predicted+conf.high)) +
  facet_grid(.~C) +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr))


# what about extracting predictions from lm to compare:

predM1r <- ggpredict(M1r, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

(Mrplot1 <- ggplot(data = predM1r) +
    geom_col(aes(x = Tr,
                 y = predicted,
                 group = Tr,
                 fill = Tr)) +
    geom_errorbar(aes(x = Tr,
                      ymin = predicted+conf.low,
                      ymax = predicted+conf.high),
                  width = 0.3) +
    facet_grid(.~C) +
    # ylim(-0.10, 0.15) +
    ggtitle("Predicted values - recruitment rate lm") +
    theme_classic())





M2rr1 <- lmer(rrate ~ Tr*C + (1|visit), data = ARD_3_rate_effect2)
predM1r1 <- ggpredict(M2rr1, terms = c("C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)


# now plot predicted values on top of data to see fit:


# test modelling 5: - lmm reduce sampling days - come back to this after autocorrelation-------------------------------------------------------

library(lmerTest)
library(readr)
library(tidyverse)
library(ggeffects)
library(lme4)

ARD_3_rate_effect1 <- read_csv("data/standardize to control calculations/ARD_3_rate_effect1.csv") 

ARDr2 <- ARD_3_rate_effect1 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  rename(plot_grid_day = visit) %>% 
  # filter(plot_grid_day != "HS - 8 - 3") %>% 
  mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                        labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  mutate(rrate = as.numeric(effect.size)) %>% 
  rename(Tr = treatment) %>% 
  rename(C = complexity) %>% 
  filter(visit %notin% c("1", "3", "5", "7")) # cause they're all obv 0 on first day... and then to make the spacing between visits more even take out 3, 7, 11
# this is also taking out the day with the outlier
# WAIT NO, this doesn't make sense. taking out these days would impact the rate value, so would have to go back and change those calculations then import here



hist(ARDr2$rrate) #looks normalish, according to Harrison (forget year) lmms can handle a degree of non-normality

shapiro.test(ARDr2$rrate) # non normal (maybe cause of outlier)

range(ARDr2$rrate) #-9 to 7

Mt2 <- lm(rrate~Tr*C, data = ARDr2)
summary(Mt2)
Mtresids2 <- resid(Mt2)

plot(Mt2, which = 1)
hist(Mtresids2) # yup looks normal
qqnorm(Mtresids2)
qqline(Mtresids2)
acf(Mtresids2,na.action = na.pass, main = "autocorrelation plot for resids") # still something funky in first few days, i think since there's large variance infirst few

boxplot(rrate~plot, data = ARDr2) #no major patterns
boxplot(rrate~visit, data = ARDr2) #still trend of more variance in first half of experiment

M1rr2 <- lmer(rrate ~ Tr*C + (1|plot), data = ARDr2) # getting isSingular error     ### variances of one or more liear combinations of effects close to 0 ###
M2rr2 <- lmer(rrate ~ Tr*C + (1|visit), data = ARDr2)
M3rr <- lmer(rrate ~ Tr*C + (1|plot) + (1|visit), data = ARDr) # getting isSingular error

# Try using Control command to force the models with errors to run

Mrrc <- lmerControl(optimizer = "nloptwrap", optCtrl = list(5000))

M1rr.c <- lmer(rrate ~ Tr*C + (1|plot), data = ARDr, control = Mrrc) # still getting ?isSingular
M3rr.c <- lmer(rrate ~ Tr*C + (1|plot) + (1|visit), data = ARDr, control = Mrrc)
M5rr.c <- lmer(rrate ~ Tr*C + (1 + Tr|visit), data = ARDr, control = Mrrc)
M6rr.c <- lmer(rrate ~ Tr*C + (1 + C|visit) + (1|plot), data = ARDr, control = Mrrc) 
M7rr.c <- lmer(rrate ~ Tr*C + (1 + Tr|visit) + (1|plot), data = ARDr, control = Mrrc)

# getting same errors, going to keep using M2rr as it best fits my hypothesis with ranom effecy struture 

# treatment as continuous:
M2rr1 <- lmer(rrate ~ treatment1*C + (1|visit), data = ARD_3_rate_effect2)
summary(M2rr1)

# 4) use AIC for model selection (Check out MuMin) to select random effect structure

# the way that went, it would seem that the only viable random effect structure is from M2rr
# I don't think I can even apply AIC selection process to this...

# 5) test fixed effects models:
#     M0 <- (rrate ~ "random effect structure that is best)
#     M1 <- (rrate ~ Tr*C)
#     M2 <- (rrate ~ Tr*C + "random effect structure that is best)

M0r <-lm(rrate ~ visit, data = ARDr) # I don't think this actually makese sense to use as a null model
M1r <- lm(rrate ~ Tr*C, data = ARDr) 
M2r <- lmer(rrate ~ Tr*C + (1|visit), data = ARDr)
# M3r <- lmer(rrate ~ Tr*C + plot + (1|visit), data = ARDr)

anova(M1r, M2r)
AIC(M0r) 
AIC(M1r) #interestingly this has lower AIC compared to model with random effect --> since visit only exmplains 3% of variation, do i use lm?
AIC(M2r) # or do I have to still keep this one as top since it's at least taking hierarchical structure in consideration?
# I'm not sure I can actually compare AIC from a lm to lmm

summary(M2r)
summary(M1r)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: rrate ~ Tr * C + (1 | visit)
#    Data: ARDr
# 
# REML criterion at convergence: 4586.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -5.6803 -0.3764 -0.0043  0.3549  5.6592 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  visit    (Intercept) 0.08287  0.2879           ### so visit explains (0.082787/(0.08287 + 2.62322)) = 0.030593, only 3% of variance... ###
#  Residual             2.62322  1.6196  
# Number of obs: 1199, groups:  visit, 15
# 
# Fixed effects:
#                Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)  -8.458e-02  1.655e-01  1.639e+02  -0.511    0.610
# Tr30%         9.319e-02  2.091e-01  1.175e+03   0.446    0.656
# Tr50%         1.094e-01  2.091e-01  1.175e+03   0.523    0.601
# Tr70%         9.042e-02  2.091e-01  1.175e+03   0.432    0.666
# Tr100%        1.271e-01  2.091e-01  1.175e+03   0.608    0.543
# CHigh         5.736e-02  2.091e-01  1.175e+03   0.274    0.784
# Tr30%:CHigh   5.556e-04  2.957e-01  1.175e+03   0.002    0.999
# Tr50%:CHigh   2.444e-02  2.957e-01  1.175e+03   0.083    0.934
# Tr70%:CHigh  -2.135e-01  2.960e-01  1.175e+03  -0.721    0.471
# Tr100%:CHigh -8.125e-02  2.957e-01  1.175e+03  -0.275    0.784
# 
# Correlation of Fixed Effects:
#             (Intr) Tr30%  Tr50%  Tr70%  Tr100% CHigh  T30%:C T50%:C T70%:C
# Tr30%       -0.632                                                        
# Tr50%       -0.632  0.500                                                 
# Tr70%       -0.632  0.500  0.500                                          
# Tr100%      -0.632  0.500  0.500  0.500                                   
# CHigh       -0.632  0.500  0.500  0.500  0.500                            
# Tr30%:CHigh  0.447 -0.707 -0.354 -0.354 -0.354 -0.707                     
# Tr50%:CHigh  0.447 -0.354 -0.707 -0.354 -0.354 -0.707  0.500              
# Tr70%:CHigh  0.446 -0.353 -0.353 -0.706 -0.353 -0.706  0.499  0.499       
# Tr100%:CHgh  0.447 -0.354 -0.354 -0.354 -0.707 -0.707  0.500  0.500  0.499


# extract predictions from df for M2r
predM2r <- ggpredict(M2r, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

(Mrplot <- ggplot(data = predM2r) +
    geom_col(aes(x = Tr,
                 y = predicted,
                 group = Tr,
                 fill = Tr)) +
    geom_errorbar(aes(x = Tr,
                      ymin = predicted+conf.low,
                      ymax = predicted+conf.high),
                  width = 0.3) +
    facet_grid(.~C) +
    ylim(-0.10, 0.15) +
    ggtitle("Predicted values - recruitment rate") +
    theme_classic())

ARDr_sum <- ARDr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1280))

(Drplot <- ggplot(data = ARDr_sum)+
    geom_col(aes(x = Tr,
                 y = rrate.mean,
                 group = Tr,
                 fill = Tr),
             alpha = 0.8)+
    geom_errorbar(aes(x = Tr,
                      ymin = rrate.mean - rrate.se,
                      ymax = rrate.mean + rrate.se),
                  width = 0.3)+
    facet_grid(.~C)+
    ggtitle("data values - recruitment rate") +
    theme_classic())

#same graph: ()
ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.35) +
  geom_col(data = predM2r,
           aes(x = Tr,
               y = predicted,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM2r,
                aes(x = Tr,
                    ymin = predicted+std.error,
                    ymax = predicted-std.error),
                width = 0.3) +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 



# what about extracting predictions from lm to compare:

predM1r <- ggpredict(M1r, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

(Mrplot1 <- ggplot(data = predM1r) +
    geom_col(aes(x = Tr,
                 y = predicted,
                 group = Tr,
                 fill = Tr)) +
    geom_errorbar(aes(x = Tr,
                      ymin = predicted+conf.low,
                      ymax = predicted+conf.high),
                  width = 0.3) +
    facet_grid(.~C) +
    # ylim(-0.10, 0.15) +
    ggtitle("Predicted values - recruitment rate lm") +
    theme_classic())





M2rr1 <- lmer(rrate ~ Tr*C + (1|visit), data = ARD_3_rate_effect2)
predM1r1 <- ggpredict(M2rr1, terms = c("C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)


# now plot predicted values on top of data to see fit:



# test modelling 6: - dealing with temporal correlation - USE THIS -------------------

library(nlme)
library(lmerTest)
library(readr)
library(tidyverse)
library(ggeffects)
library(lme4)

ARD_3_rate_effect1 <- read_csv("data/standardize to control calculations/ARD_3_rate_effect1.csv") 

ARDr <- ARD_3_rate_effect1 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  rename(plot_grid_day = visit) %>% 
  # filter(plot_grid_day != "HS - 8 - 3") %>% 
  mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                        labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  mutate(rrate = as.numeric(effect.size)) %>% 
  mutate(plot = as.factor(plot)) %>% 
  rename(Tr = treatment) %>% 
  rename(C = complexity) %>% 
  filter(visit != "1")
# cause they're all obv 0 on first day...


hist(ARDr$rrate)

M0gls <- gls(rrate~ Tr*C, data = ARDr) #since there's no correlation term this is essentiallya lm (I think this is my "mull model"?)
summary(M0gls)
#can't actually trust these values since violating assumption of independence...

#make an acf plot to visualize if there's any autocorrelation
E <- residuals(M0gls, type = "normalized")
I1 <- !is.na(ARDr$rrate)
Efull <- vector(length = length(ARDr$rrate))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
      main = "Auto-correlation plot for residuals")

#ok so i still don't quite get what this means, but i think i have temporal autocorrelation, so I'm going to use gls to account for this:
# and then compare the one with and without the autocorrelaton term with AIC
# zuur recommends not spending too much time finding the optimal autocorrelation structure

hist(ARDr$rrate)

# test 4 dif autocorrelation strucutres: (following code from Zuur)____________________________________________________________________


# compound summetry autocorrelation structure
# form tells R that order of data determined by var visit
M1gls <- gls(rrate~ Tr*C, correlation = corCompSymm(form = ~ visit), data = ARDr) #without time as fixed effect, use this (Essentially same AIC)
summary(M1gls)
AIC(M1gls)
# M1.1gls <- gls(rrate~ Tr*C + visit, correlation = corCompSymm(form = ~ visit), data = ARDr) 
# summary(M1.1gls)
# AIC(M1.1gls)

# AR-1 auto-regressive model of order I, models residuals at time sas a function of the residual of time s-1 
M2gls <- gls(rrate ~ Tr*C, correlation = corAR1(form = ~ visit|plot_grid), na.action = na.omit, data= ARDr)
#getting error: 
# Error in Initialize.corAR1(X[[i]], ...) : 
#   covariate must have unique values within groups for "corAR1" objects
# can't have two or more samples taken on te same day for a corAR1 structure, so need to specify that plot grid is the repeated "site" measurement
# ok first try making plot#grid a factor
ARDr <- ARDr %>% 
  mutate(plot_grid = as.factor(plot_grid))
#now try treating visit as numeric
ARDr <- ARDr %>% 
  mutate(visit = as.numeric(visit))
#yup that worked, need to keep plot_grid in there
summary(M2gls)
# parameter estimate is -0.4697 = residuals separated by each visit have are negatively correlated by -47
# generally this should be a positve number though
# if it's negative could mean model is missing an important explanatory variable or abundances go from high values in one year to values in the next year
# I think this is being driven by the variability in the first few visits --> check ch 4 of zuur for dealing with heteroscedastity 
AIC(M2gls) # WAY better AIC value

# ARMA error structure
# like AR-1, but instead had a moving average for the resids
# a bit more complicated, need so specify p a nd q values, and based on what he's saying it seems like those can be kinda arbitrary? and depends on your data?

cs1 <- corARMA(c(0.2), p = 1, q = 1)
M3arma1 <-gls(rrate~Tr*C,
                # na.action = na.omit,
                correlation = cs1, data = ARDr)
# M3arma2 <- gls(rrate~Tr*C,                        #this one wsn't working)
#                  # na.action = na.omit,
#                  correlation = cs2, data = ARDr)
AIC(M3arma1) # well, this is worse...

# I don't actually know what this means tho... seems a bit odd to "try" different p and q values, fishing for values?

# gonna try the code from viktoria to test the 4 structures: _______________________________________________________________________

# compound symmetry
# glsM1<-gls(Tr*C,corr=corCompSymm(),method="ML",data = ARDr) # old code
M1gls <- gls(rrate~ Tr*C, correlation = corCompSymm(form = ~ visit), data = ARDr)  #new one

# unstructured covariance matrix
glsM2<-gls(Tr*C,corr=corSymm(form = ~ visit),weights=varIdent(form=~1|visit),method="ML",data=ARDr)
# funktioniert immer wieder nicht!
# it also ist nicht funktionen for me...

# autoregressive var-cov matrix
# glsM3<-gls(Tr*C,corr=corAR1(),method="ML",data=ARDr)
M2gls <- gls(rrate ~ Tr*C, correlation = corAR1(form = ~ visit|plot_grid), na.action = na.omit, data= ARDr) #new

# autoregressie with heterogeneous variance var-cov matrix
# glsM4<-gls(Tr*C,corr=corAR1(),weights=varIdent(form=~1|visit),method="ML",data=ARDr)
M3gls <- gls(rrate ~ Tr*C, corr = corAR1(), weights = varIdent(form = ~ 1|visit), data = ARDr) #this one worked

#from her code:
# the data has an autoregressive var-cov structure with heterogeneous variances
# what does it mean? it means observations that are more proximate are correlated and variances change over time (obs. closer to each other are more similar)
# I think I can now include this correlation structure using nlme? --> do I have to now find the best random effect structure? (She did that)

#best structure: and check plots
M3gls <- gls(rrate ~ Tr*C, corr = corAR1(), weights = varIdent(form = ~ 1|visit), data = ARDr)

anova(M1gls,M2gls, M3gls) #M3gls has best structure

M3glsresids <- resid(M3gls)

plot(M3gls, which = 1)
hist(M3glsresids)# yup looks normal
qqnorm(M3glsresids)
qqline(M3glsresids)
acf(M3glsresids,na.action = na.pass, main = "autocorrelation plot for resids")

boxplot(rrate~plot, data = ARDr) # doesn't look like there's much going on here, there's 4 levels, the plots are on the same reef so not that far apart, may not end up being important is my guess
boxplot(rrate~visit, data = ARDr) # vury interesting, perhaps the autoregressive w het var-cov may actually makes sense looking at this! :)

#OK So the best autoregressive structure is M3gls, with autoregressive variance-covariance structure with heterogeneous variances

# do a quick check of Ch.4 of Zuur 


#Next: Check random effects with the correct autocorrelation structre (from M3gls) _________________________________________
lmmM1 <- lme(rrate~Tr*C, random = ~1|visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDr)
lmmM2 <- lme(rrate~Tr*C, random = ~1|plot/visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDr) #convergence issues
lmmM3 <- lme(rrate~Tr*C, random = ~1|plot, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDr) #convergence issues

# I think all the ones with plot weren't converging because as I saw before with the ?iSSingular error with lme4, plot was not explaining any of the 
# variation, and I saw that from the boxplots too. 

#LmmM1 is the only one that converges, checks out though with what I've seen so far: 

# now check if adding plot as a "nuissance" fixed effect matters__________________________________________________________

lmmM1.1 <- lmmM1 <- lme(rrate~Tr*C + plot, random = ~1|visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDr)
lm2 <- gls(rrate~ Tr*C + plot, data = ARDr)
# getting Singularity error: it looks like this is because it's not telling you new informaiton (confounding variables)

# test significance of random effect visit and AR1 structure__________________________________________________________________________________

M0gls <- gls(rrate~ Tr*C, data = ARDr) #the null model (lm) - no random effect or autoregression structure
lmmM1a <- lme(rrate~Tr*C, random = ~1|visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDr) #AR1 structure and random effect
lmmM1b <- gls(rrate~Tr*C, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDr) # just AR1 structure
lmmM1c <- lme(rrate~Tr*C, random = ~1|visit, data=ARDr) #just random effect

anova(M0gls,lmmM1a, lmmM1b, lmmM1c) 

# Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# M0gls      1 11 4723.407 4779.306 -2350.704                        
# lmmM1a     2 27 4169.527 4306.733 -2057.763 1 vs 2 585.8803  <.0001
# lmmM1b     3 26 3895.571 4027.696 -1921.786 2 vs 3 271.9555  <.0001
# lmmM1c     4 12 4710.318 4771.299 -2343.159 3 vs 4 842.7471  <.0001

# it looks like the just AR1 structure is my best fitting model (without the random effect), then the model with just the random effect, then the AR1 lmm
# but I need to think about if it makes sense to use visit defnied in AR1 AND as a random effect
# ie is that just trying to deal with heteroscedastity twice in the same model?
summary(lmmM1a) #AR1 structre and random effect
summary(lmmM1b) #just AR1 structure

# inspect models: _________________________________________________________________________________________________________

# inspecting heteroscedacity of residuals - lmm w AR1
H1a<-resid(lmmM1a,type="normalized")
H2a<-fitted(lmmM1a)
par(mfrow=c(2,2))
plot(x=H2a,y=H1a, xlab="fitted values", ylab="residuals")
boxplot(H1a~visit, data=ARDr, main="visit",ylab="residuals")
boxplot(H1a~Tr, data=ARDr, main="treatment",ylab="residuals")
boxplot(H1a~C, data=ARDr, main="complexity",ylab="residuals")

# inspecting heteroscedacity of residuals - gls (just AR1)
H1b<-resid(lmmM1b,type="normalized")
H2b<-fitted(lmmM1b)
par(mfrow=c(2,2))
plot(x=H2b,y=H1b, xlab="fitted values", ylab="residuals")
boxplot(H1b~visit, data=ARDr, main="visit",ylab="residuals")
boxplot(H1b~Tr, data=ARDr, main="treatment",ylab="residuals")
boxplot(H1b~C, data=ARDr, main="complexity",ylab="residuals")

# inspecting heteroscedacity of residuals - just lmm 
H1c<-resid(lmmM1c,type="normalized")
H2c<-fitted(lmmM1c)
par(mfrow=c(2,2))
plot(x=H2c,y=H1c, xlab="fitted values", ylab="residuals")
boxplot(H1c~visit, data=ARDr, main="visit",ylab="residuals")
boxplot(H1c~Tr, data=ARDr, main="treatment",ylab="residuals")
boxplot(H1c~C, data=ARDr, main="complexity",ylab="residuals")

par(mfrow=c(1,1))

# checking for normality of residulas

qqnorm(H1a, main = "AR1 lmm") # I feel like this one is less worse
qqline(H1a)
qqnorm(H1b, main = "just gls")
qqline(H1b)
qqnorm(H1c, main = "just lmm")
qqline(H1c)

acf(H1a,na.action = na.pass, main = "autocorrelation plot for resids AR1 lmm")
acf(H1b,na.action = na.pass, main = "autocorrelation plot for resids gls") # this is the only one that looks dif
acf(H1c,na.action = na.pass, main = "autocorrelation plot for resids just lmm")


# use summary() and not anova() according to Zuur et al. (p. 135) as this implements sequential testing
# region is not significant but we cannot get rig of it because we have the interaction and each factor will be listed seperately, automatically
#WARNING: R provides Type I sequential SS, not the default Type III marginal SS reported by SAS and SPSS.
# In a nonorthogonal design with more than one term on the right hand side of the equation order will matter
# (i.e., A+B and B+A will produce different results)! We will need use the drop1( ) function to produce the familiar Type III results.
# It will compare each term with the full model. See also Crawley p. 504+p.507
anova(lmmM1a,type="marginal")
anova(lmmM1a)
drop1(lmmM1a,~.,test="F")
summary(lmmM1a)

anova(lmmM1b,type="marginal")
anova(lmmM1b)
drop1(lmmM1b,~.,test="F")
summary(lmmM1b)

anova(lmmM1c,type="marginal")
anova(lmmM1c)
drop1(lmmM1c,~.,test="F")
summary(lmmM1c)
# plot model results over data _____________________________________________________________________________________________________

# lmm
predM1a <- ggpredict(lmmM1a, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

ARDr_sum <- ARDr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1280))

#same graph: ()
ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.35) +
  geom_col(data = predM1a,
           aes(x = Tr,
               y = predicted,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM1a,
                aes(x = Tr,
                    ymin = predicted+std.error,
                    ymax = predicted-std.error),
                width = 0.3) +
  ggtitle("predicted AR1 lmm (outline) over data (colour)") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

# lgls
predM1b <- ggpredict(lmmM1b, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

ARDr_sum <- ARDr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1280))

#same graph: ()
ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.35) +
  geom_col(data = predM1b,
           aes(x = Tr,
               y = predicted,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM1b,
                aes(x = Tr,
                    ymin = predicted+std.error,
                    ymax = predicted-std.error),
                width = 0.3) +
  ggtitle("predicted gls (outline) over data (colour)") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 


# just lmm
predM1c <- ggpredict(lmmM1c, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

ARDr_sum <- ARDr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1280))

#same graph: ()
ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.35) +
  geom_col(data = predM1c,
           aes(x = Tr,
               y = predicted,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM1c,
                aes(x = Tr,
                    ymin = predicted+std.error,
                    ymax = predicted-std.error),
                width = 0.3) +
  ggtitle("predicted just lmm (outline) over data (colour)") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

summary(lmmM1c)

# null
predM0 <- ggpredict(M0gls, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

ARDr_sum <- ARDr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1280))

#same graph: ()
ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.35) +
  geom_col(data = predM0,
           aes(x = Tr,
               y = predicted,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM0,
                aes(x = Tr,
                    ymin = predicted+std.error,
                    ymax = predicted-std.error),
                width = 0.3) +
  ggtitle("predicted just lm") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

#wow this fits my model WAY better than the other two...

# just data plotted w s.e.:
ARDr_sum <- ARDr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1280))

ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_errorbar(data =ARDr_sum,
                aes(x = Tr,
                    ymin = rrate.mean+rrate.se,
                    ymax = rrate.mean-rrate.se),
                width = 0.3) +
  ggtitle("just data") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 


# gls and lmm with AR1 *USE THIS ------------------------------------------


library(nlme)
library(lmerTest)
library(readr)
library(tidyverse)
library(ggeffects)
library(lme4)

ARD_3_relrate <- read_csv("data/standardize to control calculations/ARD_3_relrate.csv")

ARDrr <- ARD_3_relrate %>% 
  dplyr::mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  dplyr::mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  # dplyr::rename(plot_grid_day = visit) %>% 
  # filter(plot_grid_day != "HS - 8 - 3") %>% 
  dplyr::mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                        labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  dplyr::mutate(rrate = as.numeric(rrate)) %>% 
  dplyr::mutate(plot = as.factor(plot)) %>% 
  dplyr::rename(Tr = treatment) %>% 
  dplyr::rename(C = complexity) %>% 
  dplyr::filter(visit != "1")
# cause they're all obv 0 on first day...


hist(ARDrr$rrate)

M0gls <- gls(rrate~ Tr*C, data = ARDrr) #since there's no correlation term this is essentiallya lm (I think this is my "mull model"?)
summary(M0gls)
#can't actually trust these values since violating assumption of independence...

#make an acf plot to visualize if there's any autocorrelation
E <- residuals(M0gls, type = "normalized")
I1 <- !is.na(ARDr$rrate)
Efull <- vector(length = length(ARDr$rrate))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")

#some kidn of temporal autocorrelation structure

# test 4 dif autocorrelation strucutres: (following code from Zuur)____________________________________________________________________

# compound symmetry
M1gls <- gls(rrate~ Tr*C, correlation = corCompSymm(form = ~ visit), data = ARDrr)  #new one

# unstructured covariance matrix
glsM2<-gls(Tr*C,corr=corSymm(form = ~ visit),weights=varIdent(form=~1|visit),method="ML",data=ARDrr)
# funktioniert immer wieder nicht!
# it also ist nicht funktionen for me...

# autoregressive var-cov matrix
M2gls <- gls(rrate ~ Tr*C, correlation = corAR1(form = ~ visit|plot_grid), na.action = na.omit, data= ARDrr) #new

# autoregressie with heterogeneous variance var-cov matrixr
M3gls <- gls(rrate ~ Tr*C, corr = corAR1(), weights = varIdent(form = ~ 1|visit), data = ARDrr) #this one worked

#best structure: and check plots
M3gls <- gls(rrate ~ Tr*C, corr = corAR1(), weights = varIdent(form = ~ 1|visit), data = ARDr)

anova(M1gls,M2gls, M3gls) #M3gls has best structure

M3glsresids <- resid(M3gls)

plot(M3gls, which = 1)
hist(M3glsresids)# yup looks normal
qqnorm(M3glsresids)
qqline(M3glsresids)
acf(M3glsresids,na.action = na.pass, main = "autocorrelation plot for resids")

boxplot(rrate~plot, data = ARDrr) # doesn't look like there's much going on here, there's 4 levels, the plots are on the same reef so not that far apart, may not end up being important is my guess
boxplot(rrate~visit, data = ARDrr) # vury interesting, perhaps the autoregressive w het var-cov may actually makes sense looking at this! :)

#OK So the best autoregressive structure is M3gls, with autoregressive variance-covariance structure with heterogeneous variances
#from her code:
# the data has an autoregressive var-cov structure with heterogeneous variances
# what does it mean? it means observations that are more proximate are correlated and variances change over time (obs. closer to each other are more similar)

#Next: Check random effects with the correct autocorrelation structre (from M3gls) _________________________________________
lmmM1 <- lme(rrate~Tr*C, random = ~1|visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr)
lmmM2 <- lme(rrate~Tr*C, random = ~1|plot/visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr) #convergence issues
lmmM3 <- lme(rrate~Tr*C, random = ~1|plot, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr) #convergence issues

# I think all the ones with plot weren't converging because as I saw before with the ?iSSingular error with lme4, plot was not explaining any of the 
# variation, and I saw that from the boxplots too. 

#LmmM1 is the only one that converges, checks out though with what I've seen so far: 

# now check if adding plot as a "nuissance" fixed effect matters__________________________________________________________

lmmM1.1 <- lmmM1 <- lme(rrate~Tr*C + plot, random = ~1|visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr)
lm2 <- gls(rrate~ Tr*C + plot, data = ARDrr)
# getting Singularity error: it looks like this is because it's not telling you new informaiton (confounding variables)

# test significance of random effect visit and AR1 structure__________________________________________________________________________________

M0gls <- gls(rrate~ Tr*C, data = ARDrr) #the null model (lm) - no random effect or autoregression structure
lmmM1a <- lme(rrate~Tr*C, random = ~1|visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr) #AR1 structure and random effect
lmmM1b <- gls(rrate~Tr*C, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr) # just AR1 structure
lmmM1c <- lme(rrate~Tr*C, random = ~1|visit, data=ARDrr) #just random effect

anova(M0gls,lmmM1a, lmmM1b, lmmM1c) 

# Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# M0gls      1 11 4723.407 4779.306 -2350.704                        
# lmmM1a     2 27 4169.527 4306.733 -2057.763 1 vs 2 585.8803  <.0001
# lmmM1b     3 26 3895.571 4027.696 -1921.786 2 vs 3 271.9555  <.0001
# lmmM1c     4 12 4710.318 4771.299 -2343.159 3 vs 4 842.7471  <.0001

# it looks like the just AR1 structure is my best fitting model (without the random effect), then the model with just the random effect, then the AR1 lmm
# but I need to think about if it makes sense to use visit defnied in AR1 AND as a random effect
# ie is that just trying to deal with heteroscedastity twice in the same model?
summary(lmmM1a) #AR1 structre and random effect
summary(lmmM1b) #just AR1 structure

# inspect models: _________________________________________________________________________________________________________

# inspecting heteroscedacity of residuals - lmm w AR1
H1a<-resid(lmmM1a,type="normalized")
H2a<-fitted(lmmM1a)
par(mfrow=c(2,2))
plot(x=H2a,y=H1a, xlab="fitted values", ylab="residuals")
boxplot(H1a~visit, data=ARDr, main="visit",ylab="residuals")
boxplot(H1a~Tr, data=ARDr, main="treatment",ylab="residuals")
boxplot(H1a~C, data=ARDr, main="complexity",ylab="residuals")

# inspecting heteroscedacity of residuals - gls (just AR1)        #this looks best
H1b<-resid(lmmM1b,type="normalized")
H2b<-fitted(lmmM1b)
par(mfrow=c(2,2))
plot(x=H2b,y=H1b, xlab="fitted values", ylab="residuals")
boxplot(H1b~visit, data=ARDr, main="visit",ylab="residuals")
boxplot(H1b~Tr, data=ARDr, main="treatment",ylab="residuals")
boxplot(H1b~C, data=ARDr, main="complexity",ylab="residuals")

# inspecting heteroscedacity of residuals - just lmm (no AR1)
H1c<-resid(lmmM1c,type="normalized")
H2c<-fitted(lmmM1c)
par(mfrow=c(2,2))
plot(x=H2c,y=H1c, xlab="fitted values", ylab="residuals")
boxplot(H1c~visit, data=ARDr, main="visit",ylab="residuals")
boxplot(H1c~Tr, data=ARDr, main="treatment",ylab="residuals")
boxplot(H1c~C, data=ARDr, main="complexity",ylab="residuals")

par(mfrow=c(1,1))

# checking for normality of residulas

qqnorm(H1a, main = "AR1 lmm") 
qqline(H1a)
qqnorm(H1b, main = "just gls")  # I feel like this one is less worse
qqline(H1b)
qqnorm(H1c, main = "just lmm")
qqline(H1c)

acf(H1a,na.action = na.pass, main = "autocorrelation plot for resids AR1 lmm")
acf(H1b,na.action = na.pass, main = "autocorrelation plot for resids gls")  # this is the only one that looks dif
acf(H1c,na.action = na.pass, main = "autocorrelation plot for resids just lmm")


# use summary() and not anova() according to Zuur et al. (p. 135) as this implements sequential testing
# region is not significant but we cannot get rig of it because we have the interaction and each factor will be listed seperately, automatically
#WARNING: R provides Type I sequential SS, not the default Type III marginal SS reported by SAS and SPSS.
# In a nonorthogonal design with more than one term on the right hand side of the equation order will matter
# (i.e., A+B and B+A will produce different results)! We will need use the drop1( ) function to produce the familiar Type III results.
# It will compare each term with the full model. See also Crawley p. 504+p.507
anova(lmmM1a,type="marginal")
anova(lmmM1a)
drop1(lmmM1a,~.,test="F")
summary(lmmM1a)

anova(lmmM1b,type="marginal")
anova(lmmM1b)
# drop1(lmmM1b,~.,test="F")
# summary(lmmM1b)


# plot model results over data _____________________________________________________________________________________________________

# lmm
predM1a <- ggpredict(lmmM1a, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

ARDr_sum <- ARDr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1280))

#same graph: ()
ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.35) +
  geom_col(data = predM1a,
           aes(x = Tr,
               y = predicted,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM1a,
                aes(x = Tr,
                    ymin = predicted+std.error,
                    ymax = predicted-std.error),
                width = 0.3) +
  ggtitle("predicted AR1 lmm (outline) over data (colour)") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

# lgls
predM1b <- ggpredict(lmmM1b, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

ARDr_sum <- ARDrr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1200))

#same graph: ()                       #honestly plot looks so bad, but they all do? 
ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.35) +
  geom_col(data = predM1b,
           aes(x = Tr,
               y = predicted,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM1b,
                aes(x = Tr,
                    ymin = predicted+std.error,
                    ymax = predicted-std.error),
                width = 0.3) +
  ggtitle("predicted gls (outline) over data (colour)") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 


# just lmm
predM1c <- ggpredict(lmmM1c, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

ARDr_sum <- ARDrr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1200))

#same graph: ()                 #this plot looks suspiciously good? like wayyyy too much...
ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.35) +
  geom_col(data = predM1c,
           aes(x = Tr,
               y = predicted,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM1c,
                aes(x = Tr,
                    ymin = predicted+std.error,
                    ymax = predicted-std.error),
                width = 0.3) +
  ggtitle("predicted just lmm (outline) over data (colour)") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

summary(lmmM1c)

# null
predM0 <- ggpredict(M0gls, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group)

ARDr_sum <- ARDr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1280))

#same graph: ()
ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.35) +
  geom_col(data = predM0,
           aes(x = Tr,
               y = predicted,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM0,
                aes(x = Tr,
                    ymin = predicted+std.error,
                    ymax = predicted-std.error),
                width = 0.3) +
  ggtitle("predicted just lm") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

#wow this fits my model WAY better than the other two...

# just data plotted w s.e.:
ARDr_sum <- ARDrr %>% 
  group_by(Tr, C) %>% 
  summarize(rrate.mean = mean(rrate), rrate.sd = sd(rrate)) %>%
  mutate(rrate.se = rrate.sd/sqrt(1200))

ggplot() +
  geom_col(data = ARDr_sum,
           aes(x = Tr,
               y = rrate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_errorbar(data =ARDr_sum,
                aes(x = Tr,
                    ymin = rrate.mean+rrate.se,
                    ymax = rrate.mean-rrate.se),
                width = 0.3) +
  ggtitle("just data") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 


# B. RELATIVE FINAL DENSITY ---------------------------------------------------------------------

# for the relative final density bit I could either consider repeated measures factorial design (if assumptions are met) or glmm like originally planned

library(MASS)
library(psych)

ARD_4to6_relabun <- read_csv("data/standardize to control calculations/ARD_4to6_relabun.csv")

ARD4f <- ARD_4to6_relabun %>% 
mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                        labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  mutate(plot = as.factor(plot)) %>% 
  rename(Tr = treatment) %>% 
  rename(C = complexity) %>% 
  filter(visit %in% c("14","15","16"))

hist(ARD4f$rabun)
shapiro.test(ARD4f$rabun) #not normal
# data:  ARD4f$rabun
# W = 0.87297, p-value = 2.988e-13
mean(ARD4f$rabun) #0.7625
var(ARD4f$rabun) #13.23, yeah we prob have over-dispersion...

# since it's the last 3 visits, don't need to worry about visit as random effect
# could test plot for random effect
# but will most likely be glm or ancova (transformed data)

glm.poiss <- glm(rabun~Tr*C, family = poisson(link = "log"), data = ARD4f)
#can't have negative values in the poisson family, maybe add a standard to make it all above 0?

range(ARD4f$rabun)
#-4.75 18.75



# all data ----------------------------------------------------------------



# transform data __________________________________________________________________________________________________________________________

# can also try a cube root on the data:
Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}


ARD4f1 <- ARD4f %>% 
  mutate(rabun1 = (rabun + 5)) %>% # add 5 to all values to make above 1
  mutate(rabun1sqrt = sqrt(rabun1)) %>% # do sqrt transformation on data with a constant
  mutate(rabun1log = log(rabun1)) %>% #do log transformaiton on data with constant
  mutate(rabuncsqrt = Math.cbrt(rabun)) #use formula above for cube root transformation

# examine normality and homogeneity of variances for the transformed data ___________________________________________________________________

hist(ARD4f1$rabun1)
shapiro.test(ARD4f1$rabun1) #W = 0.87297, p-value = 2.988e-13 

hist(ARD4f1$rabun1sqrt) 
#this looks normalish, turns out all the H complexity ones are normal, but not L complexity
shapiro.test(ARD4f1$rabun1sqrt) #still not normal according to this, but I'll try anyways
shapiro.test(ARD4f1$rabun1sqrt[ARD4f1$C == "Low"])
shapiro.test(ARD4f1$rabun1sqrt[ARD4f1$C == "High"])
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "High") &(ARD4f1$Tr == "0%")]) #norm
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "High") &(ARD4f1$Tr == "30%")]) #norm
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "High") &(ARD4f1$Tr == "50%")]) #norm
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "High") &(ARD4f1$Tr == "70%")]) #norm
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "High") &(ARD4f1$Tr == "100%")]) #norm
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "Low") &(ARD4f1$Tr == "0%")]) 
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "Low") &(ARD4f1$Tr == "30%")])
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "Low") &(ARD4f1$Tr == "50%")])
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "Low") &(ARD4f1$Tr == "70%")])
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "Low") &(ARD4f1$Tr == "100%")])

leveneTest(ARD4f1$rabun1sqrt ~ as.factor(ARD4f1$Tr)*as.factor(ARD4f1$C), horizontal = FALSE) # variances aren't sig different (p > 0.05)
var.test(ARD4f1$rabun1sqrt ~ ARD4f1$C) #ratio bw variances is 0.55 (less than 1, so likely dif variances) -> makes sense based on resid boxplots, 
# however when grouped with Tr (in levenes test), results indicate that they aren't significantly different, move on

hist(ARD4f1$rabun1log) #this does not look great, there's a neg outlier
shapiro.test(ARD4f1$rabun1log) #not normal

hist(ARD4f1$rabuncsqrt)# o def NO aha, that's now binomial 
shapiro.test(ARD4f1$rabuncsqrt) #nope

boxplot(rabun1~plot, data = ARD4f1) #heterosc between H and L plots, not really within plots but try as re
boxplot(rabun1~visit, data = ARD4f1) #some heterosc

# going to go ahead and use the sqrt transformation in a lm


# run models --------------------------------------------------------------


# run the models & model selection ______________________________________________________________________________________________________________

# glm.poiss <- glm(rabun1~Tr*C, family = poisson(link = "log"), data = ARD4f1)
# wait, they're all non-integer values... so can't use poisson at all

# the standardizing to background complexity step makes all the integer values of abundance into non-integer values
# I think in this case I would need to use gamma distribution... and would have to use the one with 5 added to each value...
# neg binom is also integers that are positive (pg 199 zuur)
# could also try quasi-posson (not an actual distibution, just tells R that it's over-dispersed) - pg 226 for how to report -> I think maybe come back
#   to this but for now not going to include
# glm.quap <- glm(rabun1~Tr*C, family = quasipoisson(), data = ARD4f1) #quasi poisson

#first need to check if I can use a gamma distribution: (lab notes from Jenny)

ab.mean = mean(ARD4f1$rabun1)
ab.var = var(ARD4f1$rabun1)
gamma.beta = ab.mean/ab.var
gamma.alpha = ab.mean*gamma.beta

hist(ARD4f1$rabun1, prob = T)
curve(dgamma(x, gamma.alpha, gamma.beta),0,25,add = T, col = 'red')

# it actually looks like it fits decently, maybe the gamma dist is a bit under?...

# glm.gam.3 <- glm(rabun1~Tr*C, family = Gamma(link = "identity"), data = ARD4f1) #from jenny, made no dif to AIC
# glm.invg <- glm(rabun1~Tr*C, family = inverse.gaussian(), data = ARD4f1)# apparently this behaves similarly to gamma, ok it's much worse in AIC comparison

# untransformed data: 
lm.0 <- glm(rabun1~Tr*C, family = gaussian(), data = ARD4f1) #null model, lm of untransformed data
glm.gam <- glm(rabun1~Tr*C, family = Gamma(), data = ARD4f1) # gamma glm
# glm.gam.1 <- glm(rabun1~Tr*C, family = Gamma(link = "log"), data = ARD4f1) #gamma glm, specify log link - guess generally recommended?, no dif 
# glm.gam.2 <- glm(rabun1~Tr*C, family = Gamma(link = "inverse"), data = ARD4f1) #inverse link, no dif
glmm.gam <- glmmTMB(rabun1~Tr*C + (1|plot), family = Gamma(link="log"), data = ARD4f1)
glmm.gam1 <- glmmTMB(rabun1~Tr*C + (1|visit), family = Gamma(link="log"), data = ARD4f1)
glmm.gam2 <- glmmTMB(rabun1~Tr*C + (1|visit) + (1|plot), family = Gamma(link="log"), data = ARD4f1)

#transformed data: 
lmmsq <- lme(rabun1sqrt~Tr*C,random = ~1|plot, data = ARD4f1) #lmm with sqrt tranformed data,  plot as random effect
lmsq <- glm(rabun1sqrt~Tr*C, family = gaussian(), data = ARD4f1) #lm with sqrt tranformed data
lmsq2 <- glm(rabun1sqrt~Tr*C + plot, family = gaussian(), data = ARD4f1) #plot as "nuissance" fixed var
lmmsq2 <- lme(rabun1sqrt~Tr*C,random = ~1|visit, data = ARD4f1) #lmm with sqrt transformed data, visit as random effect

AIC(lm.0, glm.gam, glmm.gam, glmm.gam1, glmm.gam2) #glm.gam better, log link and inverse made no dif (maybe it's the default?)
AIC(lmmsq, lmsq, lmsq2, lmmsq2) #lmsq better

# lmsq best for transformed data, so just regular lm with no random effect
# glm.gam and glmm.gam1 (visit as re) as re) best for untransformed data,

car::Anova(glmm.gam)
car::Anova(glmm.gam)
car::Anova(glm.gam)
anova(lmsq)


# lmsq model evaluation ---------------------------------------------------



# top model evaluation - sqrt transformed _______________________________________________________________________________________________________

anova(lmsq)
summary(lmsq)

#ok what if I compare using glm and lm and aov for ancova...

# lmsq <- glm(rabun1sqrt~Tr*C, family = gaussian(), data = ARD4f1)
# lmsq1 <- lm(rabun1sqrt~Tr*C, data = ARD4f1)
# anc1 <- aov(rabun1sqrt~Tr*C, data = ARD4f1)
# AIC(lmsq,lmsq1, anc1)
# they're the exact same... so since Tr and C are factors need to test heterogeneity of slopes also (if doing ancova)

# model visual evaluation:
# inspecting heteroscedacity of residuals - lmsq
J1a<-resid(lmsq)
J2a<-fitted(lmsq)
par(mfrow=c(2,2))
plot(x=J2a,y=J1a, xlab="fitted values", ylab="residuals")
boxplot(J1a~Tr, data=ARD4f1, main="treatment",ylab="residuals") # looks a bit like there's some heterosc
boxplot(J1a~C, data=ARD4f1, main="complexity",ylab="residuals") #looks good
qqnorm(J1a) # looks just ok
qqline(J1a)
par(mfrow=c(1,1))

# test residuals for normality:
shapiro.test(J1a) #according to this, not normal (p < 0.05)
ks.test(J1a, "pnorm", mean = mean(J1a), sd = sd(J1a)) # p < 0.05, but not by much, p = 0.02
# getting warning message: ties should not be present for the Kolmogorov-Smirnov test
hist(J1a, prob = T) # I mean, they look normal...
curve(dnorm(x, mean = mean(J1a), sd = sd(J1a)), add = T)

# test for heteroscedastitiy of residuals
bartlett.test(J1a ~as.factor(ARD4f1$Tr)) #homogeneous
bartlett.test(J1a ~as.factor(ARD4f1$C)) #hetero...
leveneTest(J1a ~as.factor(ARD4f1$Tr), center = median) #homo
leveneTest(J1a ~as.factor(ARD4f1$C), center = median) #hetero...

# so the lm on sqrt transformed data did not perform great, some model violations, 

# glm gam model evaluation ------------------------------------------------


# model evaluation of glm.gam ___________________________________________________________________________________________________________

#remember this is non-transformed data, but pure relative abundance for last 3 visits (+ 6 to make it positive)
#but need to exponentiate the coeficients since they're logged in the modeling process? (sean anderson webste)
summary(glm.gam) # sig for just 0% L
summary(lmsq) #sig for 0% H and L
exp(coef(glm.gam))

coef(glm.gam)
#these match remarkably well...
# check dharma package for generalized linear models, not sure I can use normal means of assessing models 

#notes from jenny lab:
#assess model adequacy by plotting deviance residuals vs rabun1, this uses residual deviance
plot(ARD4f1$rabun1, resid(glm.gam), log = "x", xlab = "rabun", ylab = "deviance residuals")
# I feel like this is showing a pattern... maybe my data didn't follow the gamma distribution enough?

#compare gamma m with lm model
AIC(lm.0, glm.gam) # I mean, the gamma is better...
anova(lm.0, glm.gam) #resid deviance is better than lm, 

# this is how youèd assess lm, not sure if it applies to gamma glm (don't think so but try to see)
K1a<-resid(glm.gam)
K2a<-fitted(glm.gam)
par(mfrow=c(2,2))
plot(x=K2a,y=K1a, xlab="fitted values", ylab="residuals")
boxplot(K1a~Tr, data=ARD4f1, main="treatment",ylab="residuals") # looks a bit like there's some heterosc
boxplot(K1a~C, data=ARD4f1, main="complexity",ylab="residuals") #looks good
qqnorm(K1a) # looks worse than the lmsq
qqline(K1a)
par(mfrow=c(1,1))

# test residuals for normality:
shapiro.test(K1a) #according to this, not normal (p < 0.05)
ks.test(K1a, "pnorm", mean = mean(K1a), sd = sd(K1a)) # not normal
# getting warning message: ties should not be present for the Kolmogorov-Smirnov test
hist(J1a, prob = T) # I mean, they look normalish, they look almost exactly like my data input actually
curve(dnorm(x, mean = mean(K1a), sd = sd(K1a)), add = T)

# test for heteroscedastitiy of residuals
bartlett.test(K1a ~as.factor(ARD4f1$Tr)) #homogeneous
bartlett.test(K1a ~as.factor(ARD4f1$C)) #hetero...
leveneTest(K1a ~as.factor(ARD4f1$Tr), center = median) #homo
leveneTest(K1a ~as.factor(ARD4f1$C), center = median) #hetero.. so same as lmsq


# dharma for glm ------------------------------------------------------------------

install.packages("DHARMa")
library(DHARMa)
citation("DHARMa")

#uses simulations to calculate residuals

#dispersion test
testDispersion(glm.gam)
# p > 0.05, not overdispersed

simoutglm <- simulateResiduals(fittedModel = glm.gam, plot = T) #plots scaled resid
residuals(simoutglm)
plot(simoutglm) # qq looks like the deviation from uniformity is significant :S

# the first is a qq plot to detect deviation from expected distribution (deafault is KS test)
# outliers are those outside the simulation envelope
# residuals plots plots resudlas against predicted value. simulation outliers have red stars (don't kow how much they deviate from model expectations)

plotResiduals(simoutglm, ARD4f1$Tr)
plotResiduals(simoutglm, ARD4f1$C)
hist(simoutglm) # doesn't look great? 


#goodness of fit tests
testResiduals(simoutglm)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # sig, deviation present
# 2) testOutliers: if there are more simulation outliers than expected        # sig outliers
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig, dispersion good
testUniformity(simoutglm) #KS test, p < 0.05, so not uniform

# Heteoscedastity 

# temporal autocorrelation
testTemporalAutocorrelation(simulationOutput = simoutglm, time = ARD4f1$visit) #can only do this if there's unique values per time step


# dharma for glmm.gam1 (visit as re) *USE THIS -------------------------

summary(glmm.gam1)
car::Anova(glmm.gam1)
summary(glmm.gam)
car::Anova(glmm.gam)
# summary(glm.gam)
# anova(glm.gam)
# summary(lmsq)
# anova(lmsq)

# install.packages("DHARMa")
# library(DHARMa)
# citation("DHARMa")

#uses simulations to calculate residuals

#dispersion test
testDispersion(glmm.gam1)
# p > 0.05, not overdispersed, p-value = 0.544

simoutglmm.gam1 <- simulateResiduals(fittedModel = glmm.gam1, plot = T) #plots scaled resid
residuals(simoutglmm.gam1)
plot(simoutglmm.gam1) # qq looks like the deviation from uniformity is significant :S

# the first is a qq plot to detect deviation from expected distribution (deafault is KS test)
# outliers are those outside the simulation envelope
# residuals plots plots resudlas against predicted value. simulation outliers have red stars (don't kow how much they deviate from model expectations)

plotResiduals(simoutglmm.gam1, ARD4f1$Tr)
plotResiduals(simoutglmm.gam1, ARD4f1$C) #some hetero, but not as bad as lmsq
hist(simoutglmm.gam1) # doesn't look the best, but better than the glm
hist(simoutglm)
hist(simoutglmm.ga)


#goodness of fit tests
testResiduals(simoutglmm.gam1)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # only somewhat sig (p-value = 0.01076)
# 2) testOutliers: if there are more simulation outliers than expected        # non sig outliers, p-value = 0.18
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig, dispersion good, p-value = 0.544
testUniformity(simoutglmm.gam1) #KS test, p-value = 0.01076, so not uniform, but less worse lol

# Heteoscedastity 

# temporal autocorrelation
testTemporalAutocorrelation(simulationOutput = simoutglmm.gam1, time = ARD4f1$visit) #can only do this if there's unique values per time step

ARD4.time <- ARD4f1 %>% 
  group_by(visit) %>% 
  summarize(mean.ab = mean (rabun1))

testTemporalAutocorrelation(simulationOutput = simoutglmm.gam1, time = ARD4.time$visit)

#no temporal autocorrelation (I guess makes sense since it's only 3 visits)


# visualize predictions ---------------------------------------------------


# visualize model predictions _________________________________________________________________________________________________________

# lm with sqrt transformed data
# ok so since I did the sqrt on a constant, need to backtransform coefficients 
# I added a constant then did sqrt
# so to back-transform I should first exponentiate, then add 5? mayn not make a dif check

predlmsq <- ggpredict(lmsq, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group) %>% 
  mutate(pred.exp = predicted^2) %>% #back transform the sqrt first
  mutate(pred = pred.exp - 5) #then add the constant


#same graph: ()                       #this acutally doesn't look too bad. they're all consistently under-estimated tho...hmmm
ggplot() +
  geom_col(data = ARD4f_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_col(data = predlmsq,
           aes(x = Tr,
               y = pred,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predlmsq,
                aes(x = Tr,
                    ymin = pred+std.error,
                    ymax = pred-std.error),
                width = 0.3) +
  ggtitle("predicted on data - final density lm sqrt") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

#gam.glm                   #this is uncannily good looking? but also I am skept.  Do i need to back-transform a gamma-distribution? 
                            # I feel like Emma said something about not being able to back transform estimates, but predictions, yes

predgam <- ggpredict(glm.gam1, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group) %>% 
  mutate(pred = predicted - 5)

ggplot() +
  geom_col(data = ARD4f_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_col(data = predgam,
           aes(x = Tr,
               y = pred,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predgam,
                aes(x = Tr,
                    ymin = pred+std.error,
                    ymax = pred-std.error),
                width = 0.3) +
  ggtitle("predicted on data - final density glm gam") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 


# glmm.gam
predglmmgam <- ggpredict(glmm.gam, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group) %>% 
  mutate(pred = predicted - 5)

ggplot() +
  geom_col(data = ARD4f_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_col(data = predglmmgam,
           aes(x = Tr,
               y = pred,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predglmmgam,
                aes(x = Tr,
                    ymin = pred+std.error,
                    ymax = pred-std.error),
                width = 0.3) +
  ggtitle("predicted on data - final density glm gam") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C)

# just data
ARD4f_sum <- ARD4f %>%
  group_by(Tr, C) %>% 
  summarize(rabun.mean = mean(rabun), rabun.sd = sd(rabun)) %>%
  mutate(rabun.se = rabun.sd/sqrt(240))

ggplot() +
  geom_col(data = ARD4f_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_errorbar(data =ARD4f_sum,
                aes(x = Tr,
                    ymin = rabun.mean+rabun.se,
                    ymax = rabun.mean-rabun.se),
                width = 0.3) +
  ggtitle("just data - final rel abun 4-6") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 





# remove outliers - haven't done, come back later if needed ---------------------------------------------------------

ARD_4to6_relabun <- read_csv("data/standardize to control calculations/ARD_4to6_relabun.csv")

ARD4f <- ARD_4to6_relabun %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                        labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  mutate(plot = as.factor(plot)) %>% 
  rename(Tr = treatment) %>% 
  rename(C = complexity) %>% 
  filter(visit %in% c("14","15","16"))

ARD4f$plot_grid_visit <- paste(ARD4f$plot_grid, "-", ARD4f$visit)

ARD4fo <- ARD4f %>% 
  filter(plot_grid_visit %notin% c('LN - 4 - 16', 'HN - 2 - 16'))

hist(ARD4f$rabun)
shapiro.test(ARD4f$rabun) #not normal
# data:  ARD4f$rabun
# W = 0.87297, p-value = 2.988e-13
mean(ARD4f$rabun) #0.7625
var(ARD4f$rabun) #13.23, yeah we prob have over-dispersion...

# since it's the last 3 visits, don't need to worry about visit as random effect
# could test plot for random effect
# but will most likely be glm or ancova (transformed data)

glm.poiss <- glm(rabun~Tr*C, family = poisson(link = "log"), data = ARD4f)
#can't have negative values in the poisson family, maybe add a standard to make it all above 0?

range(ARD4f$rabun)
#-4.75 18.75



# transform data __________________________________________________________________________________________________________________________

# can also try a cube root on the data:
Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}


ARD4f1 <- ARD4f %>% 
  mutate(rabun1 = (rabun + 5)) %>% # add 5 to all values to make above 1
  mutate(rabun1sqrt = sqrt(rabun1)) %>% # do sqrt transformation on data with a constant
  mutate(rabun1log = log(rabun1)) %>% #do log transformaiton on data with constant
  mutate(rabuncsqrt = Math.cbrt(rabun)) #use formula above for cube root transformation

# examine normality and homogeneity of variances for the transformed data ___________________________________________________________________

hist(ARD4f1$rabun1)
shapiro.test(ARD4f1$rabun1) #W = 0.87297, p-value = 2.988e-13 

hist(ARD4f1$rabun1sqrt) 
#this looks normalish, turns out all the H complexity ones are normal, but not L complexity
shapiro.test(ARD4f1$rabun1sqrt) #still not normal according to this, but I'll try anyways
shapiro.test(ARD4f1$rabun1sqrt[ARD4f1$C == "Low"])
shapiro.test(ARD4f1$rabun1sqrt[ARD4f1$C == "High"])
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "High") &(ARD4f1$Tr == "0%")]) #norm
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "High") &(ARD4f1$Tr == "30%")]) #norm
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "High") &(ARD4f1$Tr == "50%")]) #norm
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "High") &(ARD4f1$Tr == "70%")]) #norm
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "High") &(ARD4f1$Tr == "100%")]) #norm
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "Low") &(ARD4f1$Tr == "0%")]) 
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "Low") &(ARD4f1$Tr == "30%")])
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "Low") &(ARD4f1$Tr == "50%")])
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "Low") &(ARD4f1$Tr == "70%")])
shapiro.test(ARD4f1$rabun1sqrt[(ARD4f1$C == "Low") &(ARD4f1$Tr == "100%")])

leveneTest(ARD4f1$rabun1sqrt ~ as.factor(ARD4f1$Tr)*as.factor(ARD4f1$C), horizontal = FALSE) # variances aren't sig different (p > 0.05)
var.test(ARD4f1$rabun1sqrt ~ ARD4f1$C) #ratio bw variances is 0.55 (less than 1, so likely dif variances) -> makes sense based on resid boxplots, 
# however when grouped with Tr (in levenes test), results indicate that they aren't significantly different, move on

hist(ARD4f1$rabun1log) #this does not look great, there's a neg outlier
shapiro.test(ARD4f1$rabun1log) #not normal

hist(ARD4f1$rabuncsqrt)# o def NO aha, that's now binomial 
shapiro.test(ARD4f1$rabuncsqrt) #nope

# going to go ahead and use the sqrt transformation in a lm

# run the models & model selection ______________________________________________________________________________________________________________

glm.poiss <- glm(rabun1~Tr*C, family = poisson(link = "log"), data = ARD4f1)
# wait, they're all non-integer values... so can't use poisson at all

# the standardizing to background complexity step makes all the integer values of abundance into non-integer values
# I think in this case I would need to use gamma distribution... and would have to use the one with 5 added to each value...
# neg binom is also integers that are positive (pg 199 zuur)
# could also try quasi-posson (not an actual distibution, just tells R that it's over-dispersed) - pg 226 for how to report -> I think maybe come back
#   to this but for now not going to include
# glm.quap <- glm(rabun1~Tr*C, family = quasipoisson(), data = ARD4f1) #quasi poisson

# untransformed data: 
lm.0 <- glm(rabun1~Tr*C, family = gaussian(), data = ARD4f1) #null model, lm of untransformed data
glm.gam <- glm(rabun1~Tr*C, family = Gamma(), data = ARD4f1) # gamma glm
glm.gam.1 <- glm(rabun1~Tr*C, family = Gamma(link = "log"), data = ARD4f1) #gamma glm, specify log link - guess generally recommended? 
glm.gam.2 <- glm(rabun1~Tr*C, family = Gamma(link = "inverse"), data = ARD4f1) #inverse link

#transformed data: 
lmmsq <- lme(rabun1sqrt~Tr*C,random = ~1|plot, data = ARD4f1) #lmm with sqrt tranformed data,  plot as random effect
lmsq <- glm(rabun1sqrt~Tr*C, family = gaussian(), data = ARD4f1) #lm with sqrt tranformed data
lmsq2 <- glm(rabun1sqrt~Tr*C + plot, family = gaussian(), data = ARD4f1) #plot as "nuissance" fixed var

AIC(lm.0, glm.gam, glm.gam.1, glm.gam.2) #glm.gam better, log link and inverse made no dif (maybe it's the default?)
AIC(lmmsq, lmsq, lmsq2) #lmsq better

# lmsq best for transformed data


# lmsq model evaluation ---------------------------------------------------



# top model evaluation - sqrt transformed _______________________________________________________________________________________________________

anova(lmsq)
summary(lmsq)

#ok what if I compare using glm and lm and aov for ancova...

lmsq <- glm(rabun1sqrt~Tr*C, family = gaussian(), data = ARD4f1)
lmsq1 <- lm(rabun1sqrt~Tr*C, data = ARD4f1)
anc1 <- aov(rabun1sqrt~Tr*C, data = ARD4f1)
AIC(lmsq,lmsq1, anc1)
# they're the exact same... so since Tr and C are factors need to test heterogeneity of slopes also

# model visual evaluation:
# inspecting heteroscedacity of residuals - lmsq
J1a<-resid(lmsq)
J2a<-fitted(lmsq)
par(mfrow=c(2,2))
plot(x=J2a,y=J1a, xlab="fitted values", ylab="residuals")
boxplot(J1a~Tr, data=ARD4f1, main="treatment",ylab="residuals") # looks a bit like there's some heterosc
boxplot(J1a~C, data=ARD4f1, main="complexity",ylab="residuals") #looks good
qqnorm(J1a) # looks just ok
qqline(J1a)

# test residuals for normality:
shapiro.test(J1a) #according to this, not normal (p < 0.05)
ks.test(J1a, "pnorm", mean = mean(J1a), sd = sd(J1a)) # p < 0.05, but not by much, p = 0.02
# getting warning message: ties should not be present for the Kolmogorov-Smirnov test
hist(J1a, prob = T) # I mean, they look normal...
curve(dnorm(x, mean = mean(J1a), sd = sd(J1a)), add = T)

# test for heteroscedastitiy of residuals
bartlett.test(J1a ~as.factor(ARD4f1$Tr)) #homogeneous
bartlett.test(J1a ~as.factor(ARD4f1$C)) #hetero...
leveneTest(J1a ~as.factor(ARD4f1$Tr), center = median) #homo
leveneTest(J1a ~as.factor(ARD4f1$C), center = median) #hetero...



# glm gam model evaluation ------------------------------------------------


# model evaluation of glm.gam ___________________________________________________________________________________________________________

#remember this is non-transformed data, but pure relative abundance for last 3 visits
summary(glm.gam) # sig for just 0% L
summary(lmsq) #sig for 0% H and L
exp(coef(glm.gam))

coef(glm.gam)
#these match remarkably well...
# check dharma package for generalized linear models, not sure I can use normal means of assessing models 

K1a<-resid(glm.gam)
K2a<-fitted(glm.gam)
par(mfrow=c(2,2))
plot(x=K2a,y=K1a, xlab="fitted values", ylab="residuals")
boxplot(K1a~Tr, data=ARD4f1, main="treatment",ylab="residuals") # looks a bit like there's some heterosc
boxplot(K1a~C, data=ARD4f1, main="complexity",ylab="residuals") #looks good
qqnorm(K1a) # looks worse than the lmsq
qqline(K1a)
par(mfrow=c(1,1))

# test residuals for normality:
shapiro.test(K1a) #according to this, not normal (p < 0.05)
ks.test(K1a, "pnorm", mean = mean(K1a), sd = sd(K1a)) # not normal
# getting warning message: ties should not be present for the Kolmogorov-Smirnov test
hist(J1a, prob = T) # I mean, they look normalish, they look almost exactly like my data input actually
curve(dnorm(x, mean = mean(K1a), sd = sd(K1a)), add = T)

# test for heteroscedastitiy of residuals
bartlett.test(K1a ~as.factor(ARD4f1$Tr)) #homogeneous
bartlett.test(K1a ~as.factor(ARD4f1$C)) #hetero...
leveneTest(K1a ~as.factor(ARD4f1$Tr), center = median) #homo
leveneTest(K1a ~as.factor(ARD4f1$C), center = median) #hetero.. so same as lmsq


# dharma ------------------------------------------------------------------

install.packages("DHARMa")
library(DHARMa)
citation("DHARMa")

#uses simulations to calculate residuals

#dispersion test
testDispersion(glm.gam)
# p > 0.05, not overdispersed

simoutglm <- simulateResiduals(fittedModel = glm.gam, plot = T) #plots scaled resid
residuals(simoutglm)
plot(simoutglm) # qq looks like the deviation from uniformity is significant :S

# the first is a qq plot to detect deviation from expected distribution (deafault is KS test)
# outliers are those outside the simulation envelope
# residuals plots plots resudlas against predicted value. simulation outliers have red stars (don't kow how much they deviate from model expectations)

plotResiduals(simoutglm, ARD4f1$Tr)
plotResiduals(simoutglm, ARD4f1$C)
hist(simoutglm) # doesn't look great? 


#goodness of fit tests
testResiduals(simoutglm)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # sig, deviation present
# 2) testOutliers: if there are more simulation outliers than expected        # sig outliers
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig, dispersion good
testUniformity(simoutglm) #KS test, p < 0.05, so not uniform

# Heteoscedastity 

# temporal autocorrelation
testTemporalAutocorrelation(simulationOutput = simoutglm, time = ARD4f1$visit) #can only do this if there's unique values per time step


# visualize predictions on data -------------------------------------------


# visualize model predictions _________________________________________________________________________________________________________

# lm with sqrt transformed data
# ok so since I did the sqrt on a constant, need to backtransform coefficients 
# I added a constant then did sqrt
# so to back-transform I should first exponentiate, then add 5? mayn not make a dif check

predlmsq <- ggpredict(lmsq, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group) %>% 
  mutate(pred.exp = predicted^2) %>% #back transform the sqrt first
  mutate(pred = pred.exp - 5) #then add the constant


#same graph: ()                       #this acutally doesn't look too bad. they're all consistently under-estimated tho...hmmm
ggplot() +
  geom_col(data = ARD4f_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_col(data = predlmsq,
           aes(x = Tr,
               y = pred,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predlmsq,
                aes(x = Tr,
                    ymin = pred+std.error,
                    ymax = pred-std.error),
                width = 0.3) +
  ggtitle("predicted on data - final density lm sqrt") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

#gam.glm                   #this is uncannily good looking? but also I am skept.  Do i need to back-transform a gamma-distribution? 
# I feel like Emma said something about not being able to back transform estimates, but predictions, yes

predgam <- ggpredict(glm.gam1, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group) %>% 
  mutate(pred = predicted - 5)

ggplot() +
  geom_col(data = ARD4f_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_col(data = predgam,
           aes(x = Tr,
               y = pred,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predgam,
                aes(x = Tr,
                    ymin = pred+std.error,
                    ymax = pred-std.error),
                width = 0.3) +
  ggtitle("predicted on data - final density glm gam") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 


# just data
ARD4fo_sum <- ARD4fo %>%
  group_by(Tr, C) %>% 
  summarize(rabun.mean = mean(rabun), rabun.sd = sd(rabun)) %>%
  mutate(rabun.se = rabun.sd/sqrt(238))

ggplot() +
  geom_col(data = ARD4fo_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_errorbar(data =ARD4fo_sum,
                aes(x = Tr,
                    ymin = rabun.mean+rabun.se,
                    ymax = rabun.mean-rabun.se),
                width = 0.3) +
  ggtitle("just data - final rel abun 4-6 - no outliers") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

ARD4foo <- ARD4fo %>% #took out the 6 obs above 10
  filter(plot_grid_visit %notin% c('LN - 13 - 14', 'HN - 13 - 15', 'HN - 1 - 16', 'LS - 13 - 16')) 

# just data
ARD4foo_sum <- ARD4foo %>%
  group_by(Tr, C) %>% 
  summarize(rabun.mean = mean(rabun), rabun.sd = sd(rabun)) %>%
  mutate(rabun.se = rabun.sd/sqrt(234))

ggplot() +
  geom_col(data = ARD4foo_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_errorbar(data =ARD4foo_sum,
                aes(x = Tr,
                    ymin = rabun.mean+rabun.se,
                    ymax = rabun.mean-rabun.se),
                width = 0.3) +
  ggtitle("just data - final rel abun 4-6 - no outliers over 10") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 




# C. RELATIVE DENSITY (WHOLE STUDY) ---------------------------------------

ARD_3_relabun <- read_csv("data/standardize to control calculations/ARD_3_relabun.csv")

ARD3ra <- ARD_3_relabun %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                        labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  mutate(plot = as.factor(plot)) %>% 
  rename(Tr = treatment) %>% 
  rename(C = complexity)

hist(ARD3ra$rabun)
range(ARD3ra$rabun)
mean(ARD3ra$rabun) #0.4625
var(ARD3ra$rabun) #11.6973  #well that looks overdisprsed...
boxplot(rabun~plot, data = ARD3ra) # could be something happening here spatially
boxplot(rabun~visit, data = ARD3ra) # no pattern like with rate...

# -5.125 to 25.625
# only 2 values above 20, consider cutting under 20 to stay consistent with other analyses? First keep all in and see

ARD3ra$plot_grid_visit <- paste(ARD3ra$plot_grid, "-", ARD3ra$visit)

ARD3rao <- ARD3ra %>% 
  filter(plot_grid_visit %notin% c('LS - 6 - 15', 'LS - 6 - 5'))

# will likey have the same issues as above (neg and non-integer values) but with visit and plot as a random effect
# although looking at the boxplots, perhaps only plot will actually matter in this case...
# may also need to check temporal autocorrelation (except I think gls only works for normally distributed data)
# try transforming (option 3), could then be "normal enough" for a lmm

#option 1) glmm with gamma dist (add a constant to make all the values positive)
#option 2) transform to normal dist (try dif ones, likely sqrt), then test autoregress structures with gls, then random effects


# option 1: glmm USE TMB in this----------------------------------------------------------
library(MASS) #this uses pql, but i shifted to use glmer in lme4

#add constant, range is -5.125, so add 6

ARD3ra1 <- ARD3ra %>% 
  mutate(rabun1 = rabun + 6)

#first need to check if I can use a gamma distribution: (lab notes from Jenny)

ab.mean1 = mean(ARD3ra1$rabun1)
ab.var1 = var(ARD3ra1$rabun1)
gamma.beta1 = ab.mean1/ab.var1
gamma.alpha1 = ab.mean1*gamma.beta1

hist(ARD3ra1$rabun1, prob = T)
curve(dgamma(x, gamma.alpha1, gamma.beta1),0,25,add = T, col = 'red')

#zuur uses pql
# but bolker argues pql works poorly for poisson data where mean number of counts per treatment is less than 5
# it also computes a quasilikelihood rather than a true likelihood 
# oh and later in the book Zuur mentions that glmmPQL can't be used for AIC calculation and comparison

# M0lm <- lm(rabun~Tr*C, data = ARD3ra)
# M1glmm <- glmmPQL(rabun1~Tr*C, random = ~1|visit, family=Gamma(link="log"), data = ARD3ra1) #just visit
# M2glmm <- glmmPQL(rabun1~Tr*C, random = ~1|plot, family=Gamma(link="log"), data = ARD3ra1) #just plot
# M3glmm <- glmmPQL(rabun1~Tr*C, random = ~1|plot/visit, family=Gamma(link="log"), data = ARD3ra1)
# M4glmm <- glmmPQL(rabun1~Tr*C, random = ~1|visit/plot, family=Gamma(link="log"), data = ARD3ra1)
# M5glmm <- glmmPQL(rabun1~Tr*C + plot, random = ~1|visit, family=Gamma(link="log"), data = ARD3ra1) #plot as "nuissance" fixed, singlarity error

# anova(M1glmm, M2glmm, M3glmm, M4glmm)
#can't use anova for PQL fits... does it work with another package?
AIC(M1glmm, M2glmm, M3glmm, M4glmm)

M0glm <- glm(rabun1~Tr*C, family = Gamma(link = "log"), data = ARD3ra1)
M1glmm <- glmer(rabun1~Tr*C + (1|visit), family=Gamma(link="log"), data = ARD3ra1) #just visit as re
M2glmm <- glmer(rabun1~Tr*C + (1|plot), family=Gamma(link="log"), data = ARD3ra1) #just plot sa re
M3glmm <- glmer(rabun1~Tr*C + (1|plot) + (1|visit), family=Gamma(link="log"), data = ARD3ra1) #both visit and plot as re
# M4glmm <- glmer(rabun1~Tr*C + plot + (1|visit), family=Gamma(link="log"), data = ARD3ra1)  #not converging
M5glm <- glm(rabun1~Tr*C + plot, family=Gamma(link="log"), data = ARD3ra1) 

anova(M1glmm, M2glmm, M3glmm) #the model with both is sig and has lowest AIC M3glmm
anova(M0glm, M5glm)

summary(M3glmm)
anova(M3glmm)

# Dharma works on glmm from glmmTMB:
install.packages("glmmTMB")
library(glmmTMB)

M1glmmTMB <- glmmTMB(rabun1~Tr*C + (1|visit), family=Gamma(link="log"), data = ARD3ra1) #just visit as re
M2glmmTMB <- glmmTMB(rabun1~Tr*C + (1|plot), family=Gamma(link="log"), data = ARD3ra1) #just plot sa re
M3glmmTMB <- glmmTMB(rabun1~Tr*C + (1|plot) + (1|visit), family=Gamma(link="log"), data = ARD3ra1) #both visit and plot as re

anova(M1glmmTMB, M2glmmTMB, M3glmmTMB) #the one with both visit and plot as re is the best

# examine gamma coef -> sean anderson: https://seananderson.ca/2014/05/18/gamma-hurdle/
#gamma distribution coefficients come out on the logit scale, so will need to inverse that link...
(M3gamcoef <- exp(coef(M3glmm)[[1]]))
(exp(confint(M3glmm))) #doesn't work on glmms...
# come back to this

summary(M3glmmTMB) #sig: L0, L50, L100, H0, H70, H100
summary(M3glmm) # same, less mag


car::Anova(M3glmmTMB)

# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: rabun1
# Chisq Df Pr(>Chisq)    
# Tr   27.6049  4  1.500e-05 ***
#   C    32.9536  1  9.438e-09 ***
#   Tr:C  8.3557  4    0.07938 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# model adequacy notes from zuur (p487)
# range of graphical methods to assess how wekk a model fits the data
# doesn't actually show the code in the book lol, just the plots

# dharma glmm ______________________________________________________________________________________________________

library(DHARMa)

#dispersion test
testDispersion(M3glmm)
# p < 0.05 ---> so it is overdispersed -> which means CI are often too narrow

simoutglmm1 <- simulateResiduals(fittedModel = M1glmm, plot = T)
simoutglmm <- simulateResiduals(fittedModel = M3glmm, plot = T) #plots scaled resid
residuals(simoutglm)
plot(simoutglm) # lots of issues in qq: lack onf uniformity, outliers and dispersion
# residuals plots plots resudlas against predicted value. simulation outliers have red stars (don't kow how much they deviate from model expectations)

plotResiduals(simoutglm, ARD3ra1$Tr)
plotResiduals(simoutglm, ARD3ra1$C)
hist(simoutglm) # doesn't look great? 

#goodness of fit tests
testResiduals(simoutglmm)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # sig, deviation present
# 2) testOutliers: if there are more simulation outliers than expected        # sig outliers
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # sig, dispersion is overdispersed
testUniformity(simoutglmm) #KS test, p < 0.05, so not uniform

# Heteoscedastity 

# temporal autocorrelation
testTemporalAutocorrelation(simulationOutput = simoutglm, time = ARD4f1$visit)


# DO THE SAME FOR TMB GLMM:  M3glmmTMB _________________________________________________________________________________

#dispersion test
testDispersion(M3glmmTMB)
# p < 0.05 ---> so it is overdispersed  # I wonder if I take out the two highest values outliers if I still have this issue

simouttmb3 <- simulateResiduals(fittedModel = M3glmmTMB, plot = T) # i mean, still sig, but looks WAY better
residuals(simouttmb3)
plot(simouttmb3) # lots of issues in qq: lack onf uniformity, outliers and dispersion
# residuals plots plots resudlas against predicted value. simulation outliers have red stars (don't kow how much they deviate from model expectations)

plotResiduals(simouttmb3, ARD3ra1$Tr) #looks good
plotResiduals(simouttmb3, ARD3ra1$C) #looks good
hist(simouttmb3) # doesn't look great, seems better than glmer tho 

#goodness of fit tests
testResiduals(simouttmb3)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # sig, deviation present, p-value = 2.357e-10
# 2) testOutliers: if there are more simulation outliers than expected        # sig outliers present, 38 at each margin
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # sig, dispersion is overdispersed,p-value = 0.008
testUniformity(simouttmb3) #KS test, p < 0.05, so not uniform

# Heteoscedastity 

# temporal autocorrelation
testTemporalAutocorrelation(simulationOutput = simouttmb3, time = ARD3ra1$visit) #didn't work, oh ya need 1 obs per time value

ARD3.time <- ARD3ra1 %>% 
  group_by(visit) %>% 
  summarize(mean.ab = mean (rabun1))

testTemporalAutocorrelation(simulationOutput = simouttmb3, time = ARD3.time$visit) #not autocorrelated, p-value = 0.9228


# tukey.4f <- TukeyHSD(M3glmmTMB) # doesn't work for glmmtmb

# VISUALIZE DATA__________________________________________________________________________________________________

#plot both:     #wow actualy looks pretty good...

predM3glmm <- ggpredict(M3glmm, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group) %>% 
  mutate(pred6 = predicted - 6)

ggplot() +
  geom_col(data = ARDra_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_col(data = predM3glmm ,
           aes(x = Tr,
               y = pred6,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM3glmm ,
                aes(x = Tr,
                    ymin = pred6+std.error,
                    ymax = pred6-std.error),
                width = 0.3) +
  ggtitle("predicted glmm (both) (outline) over data (colour)") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

# just data plotted w s.e.:
ARDra_sum <- ARD3ra %>% 
  group_by(Tr, C) %>% 
  summarize(rabun.mean = mean(rabun), rabun.sd = sd(rabun)) %>%
  mutate(rabun.se = rabun.sd/sqrt(1280))

ggplot() +
  geom_col(data = ARDra_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_errorbar(data =ARDra_sum,
                aes(x = Tr,
                    ymin = rabun.mean+rabun.se,
                    ymax = rabun.mean-rabun.se),
                width = 0.3) +
  ggtitle("just data - ARD 3 rabun whole study") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C)

# data no outliers plotted w s.e.: (just to see)
ARDrao_sum <- ARD3rao %>% 
  group_by(Tr, C) %>% 
  summarize(rabun.mean = mean(rabun), rabun.sd = sd(rabun)) %>%
  mutate(rabun.se = rabun.sd/sqrt(1278))

hist(ARD3rao$rabun)
hist(ARD3ra$rabun)

ggplot() +
  geom_col(data = ARDrao_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_errorbar(data =ARDrao_sum,
                aes(x = Tr,
                    ymin = rabun.mean+rabun.se,
                    ymax = rabun.mean-rabun.se),
                width = 0.3) +
  ggtitle("just data no outlier- ARD 3 rabun whole study") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 


# option 2: glmmtmb without outliers --------------------------------------


ARD3rao <- ARD3ra %>% 
  filter(plot_grid_visit %notin% c('LS - 6 - 15', 'LS - 6 - 5')) %>%  #25 and 23, next lowest is 18, would be consistent with other analyis keeping density < 20 for consistency
  mutate(rabun1 = rabun + 6)

hist(ARD3rao$rabun1)

#first need to check if I can use a gamma distribution: (lab notes from Jenny) ________________________________________

ab.mean3 = mean(ARD3rao$rabun1)
ab.var3 = var(ARD3rao$rabun1)
gamma.beta3 = ab.mean3/ab.var3
gamma.alpha3 = ab.mean3*gamma.beta3

hist(ARD3rao$rabun1, prob = T)
curve(dgamma(x, gamma.alpha3, gamma.beta3),0,25,add = T, col = 'red')

# not the best, but not the worst? 

# run models __________________________________________________________________________________________________________

M1glmmTMBo <- glmmTMB(rabun1~Tr*C + (1|visit), family=Gamma(link="log"), data = ARD3rao) #just visit as re
M2glmmTMBo <- glmmTMB(rabun1~Tr*C + (1|plot), family=Gamma(link="log"), data = ARD3rao) #just plot sa re
M3glmmTMBo <- glmmTMB(rabun1~Tr*C + (1|plot) + (1|visit), family=Gamma(link="log"), data = ARD3rao) #both visit and plot as re

anova(M1glmmTMBo, M2glmmTMBo, M3glmmTMBo) #the one with both visit and plot as re is the best

# examine gamma coef -> sean anderson: https://seananderson.ca/2014/05/18/gamma-hurdle/
#gamma distribution coefficients come out on the logit scale, so will need to inverse that link...
(M3gamcoef <- exp(coef(M3glmm)[[1]]))
(exp(confint(M3glmm))) #doesn't work on glmms...
# come back to this

summary(M3glmmTMB) #sig: L0, L50, L100, H0, H70, H100
summary(M3glmm) # same, less mag
summary(M3glmmTMBo) #sig: L0, L50, H0, H70, H100

glmmTMB::Anova.glmmTMB(M3glmmTMBo)

#dharma assessment ______________________________________________________________________________________________

#dispersion test
testDispersion(M3glmmTMBo)
# p < 0.05 ---> so it is overdispersed, p-value = 0.032 (not as much)

simouttmb3o <- simulateResiduals(fittedModel = M3glmmTMBo, plot = T) # i mean, still sig, but looks WAY better
residuals(simouttmb3o)
plot(simouttmb3o) # lots of issues in qq: lack onf uniformity, outliers and dispersion
# residuals plots plots resudlas against predicted value. simulation outliers have red stars (don't kow how much they deviate from model expectations)

plotResiduals(simouttmb3o, ARD3rao$Tr) #looks good
plotResiduals(simouttmb3o, ARD3rao$C) #looks good
hist(simouttmb3o) # looks kind of the same as the one with outliers in...

#goodness of fit tests
testResiduals(simouttmb3o)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # sig, deviation present, p-value = 2.357e-10
# 2) testOutliers: if there are more simulation outliers than expected        # sig outliers present, 38 at each margin
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # sig, dispersion is overdispersed,p-value = 0.008
testUniformity(simouttmb3o) #KS test, p < 0.05, so not uniform
testUniformity(simouttmb3)

# Heteoscedastity 

# temporal autocorrelation
testTemporalAutocorrelation(simulationOutput = simouttmb3, time = ARD3ra1$visit) #didn't work, oh ya need 1 obs per time value

ARD3.timeo <- ARD3rao %>% 
  group_by(visit) %>% 
  summarize(mean.ab = mean (rabun1))

testTemporalAutocorrelation(simulationOutput = simouttmb3o, time = ARD3.timeo$visit) #not autocorrelated,  p-value = 0.8004

# ok so overall looks just ok... not that much improvememnt tbh, may stick with glmmtmb

# going to follow other diagnostic tools for glmmTMB from here: https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.pdf
install.packages("MuMIn")
install.packages("emmeans")
install.packages("multcomp")

library(MuMIn)
library(emmeans)
library(multcomp)

car::Anova(M3glmmTMBo) # just T and C sig
car::Anova(M3glmmTMB) # T, C and T:C sig

# uses Wald chi square stats for comparison 
# apply to the fixed effects of the conditional compoent of the model

emmeans::emmeans(M3glmmTMB, rabun1~Tr|C) #not working

# MuMIn can run all possible sub-models with dredge

MuMin::dredge(M3glmmTMB)



# option 3: transform - lmm on sqrt transformed (with constant)-----------------------------------------------------

ARD3r1 <- ARD3ra %>% 
  mutate(rabun1 = (rabun + 6)) %>% # add 6 to all values to make data positive 
  mutate(rabun1sqrt = sqrt(rabun1)) %>% # do sqrt transformation on data with a constant
  mutate(rabun1log = log(rabun1)) %>% #do log transformaiton on data with constant
  mutate(rabuncsqrt = Math.cbrt(rabun)) #use formula above for cube root transformation

# examine normality and homogeneity of variances for the transformed data ___________________________________________________________________

hist(ARD3r1$rabun)
hist(ARD3r1$rabun1sqrt) # sqrt not bad
hist(ARD3r1$rabun1log) # also not bad but has a bit of a blip...
hist(ARD3r1$rabuncsqrt) #nope
shapiro.test(ARD3r1$rabun1sqrt) #p < 0.05, p-value < 2.2e-16, not normal 
shapiro.test(ARD3r1$rabun1log) # p <0.0.5, p-value = 2.525e-14 -> so this one is less not normal?

#so none were normal for sqrt, and only H 100% is normal for log...
# shapiro.test(ARD3r1$rabun1sqrt) #still not normal according to this, but I'll try anyways
# shapiro.test(ARD3r1$rabun1log[ARD3r1$C == "Low"])
# shapiro.test(ARD3r1$rabun1log[ARD3r1$C == "High"])
# shapiro.test(ARD3r1$rabun1log[(ARD3r1$C == "High") &(ARD3r1$Tr == "0%")]) 
# shapiro.test(ARD3r1$rabun1log[(ARD3r1$C == "High") &(ARD3r1$Tr == "30%")]) 
# shapiro.test(ARD3r1$rabun1log[(ARD3r1$C == "High") &(ARD3r1$Tr == "50%")]) 
# shapiro.test(ARD3r1$rabun1log[(ARD3r1$C == "High") &(ARD3r1$Tr == "70%")]) 
# shapiro.test(ARD3r1$rabun1log[(ARD3r1$C == "High") &(ARD3r1$Tr == "100%")]) #norm
# shapiro.test(ARD3r1$rabun1log[(ARD3r1$C == "Low") &(ARD3r1$Tr == "0%")]) 
# shapiro.test(ARD3r1$rabun1log[(ARD3r1$C == "Low") &(ARD3r1$Tr == "30%")])
# shapiro.test(ARD3r1$rabun1log[(ARD3r1$C == "Low") &(ARD3r1$Tr == "50%")])
# shapiro.test(ARD3r1$rabun1log[(ARD3r1$C == "Low") &(ARD3r1$Tr == "70%")])
# shapiro.test(ARD3r1$rabun1log[(ARD3r1$C == "Low") &(ARD3r1$Tr == "100%")])

leveneTest(ARD3r1$rabun1sqrt ~ as.factor(ARD3r1$Tr)*as.factor(ARD3r1$C), horizontal = FALSE) # variances aren't sig different (p > 0.05)
var.test(ARD3r1$rabun1sqrt ~ ARD3r1$C) #ratio bw variances is 0.96, so close to 1 = var homo

# run the models & model selection ______________________________________________________________________________________________________________
hist(ARD3r1$rabun1sqrt)

lmt0 <- lm(rabun1sqrt~Tr*C , data=ARD3r1)
lmmt1 <- lmer(rabun1sqrt~Tr*C + (1|visit), data=ARD3r1)
lmmt2 <- lmer(rabun1sqrt~Tr*C + (1|visit) + (1|plot), data=ARD3r1)
lmmt3 <- lmer(rabun1sqrt~Tr*C + (1|plot), data=ARD3r1)

AIC(lmt0, lmmt1, lmmt2, lmmt3) #lm the best
anova(lmt0, lmmt1, lmmt2, lmmt3) #didn't work

anova(lmt0)
summary(lmt0)

# top model evaluation - lm on sqrt transformed = lmt0_______________________________________________________________________________________________________

# model visual evaluation:
# inspecting heteroscedacity of residuals - lmt0
N1a<-resid(lmt0)
N2a<-fitted(lmt0)
par(mfrow=c(2,2))
plot(x=N2a,y=N1a, xlab="fitted values", ylab="residuals")
boxplot(N1a~Tr, data=ARD3r1, main="treatment",ylab="residuals") # looks a bit like there's some heterosc
boxplot(N1a~C, data=ARD3r1, main="complexity",ylab="residuals") #looks a bit like there's some heterosc
qqnorm(N1a) # looks jBAD lol
qqline(N1a)

# test residuals for normality:
shapiro.test(N1a) #according to this, not normal (p < 0.05), p-value < 2.2e-16
ks.test(N1a, "pnorm", mean = mean(N1a), sd = sd(N1a)) # p < 0.05,  p-value = 1.221e-15
# getting warning message: ties should not be present for the Kolmogorov-Smirnov test
hist(N1a, prob = T) # I mean, it does NOT look normal lol
curve(dnorm(x, mean = mean(N1a), sd = sd(N1a)), add = T)

# test for heteroscedastitiy of residuals
bartlett.test(N1a ~as.factor(ARD3r1$Tr)) #hetero, p-value = 7.103e-05
bartlett.test(N1a ~as.factor(ARD3r1$C)) #homo,  p-value = 0.5593
leveneTest(N1a ~as.factor(ARD3r1$Tr), center = median) #hetero, p=0.02519
leveneTest(N1a ~as.factor(ARD3r1$C), center = median) #homo, 0.3618



# visualize on data ______________________________________________________________________________________________________

predlmt0 <- ggpredict(lmt0, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group) %>% 
  mutate(pred.exp = predicted^2) %>% #back transform the sqrt first
  mutate(pred = pred.exp - 6) #then add the constant


#same graph: ()                     #this actally doesn't look too bad. 
ggplot() +                          #they're all consistently under-estimated tho...hmmm
  geom_col(data = ARD3ra_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_col(data = predlmt0,
           aes(x = Tr,
               y = pred,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predlmt0,
                aes(x = Tr,
                    ymin = pred+std.error,
                    ymax = pred-std.error),
                width = 0.3) +
  ggtitle("predicted on data - overall density 3cm") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 


# just data
ARD3ra_sum <- ARD3ra %>%
  group_by(Tr, C) %>% 
  summarize(rabun.mean = mean(rabun), rabun.sd = sd(rabun)) %>%
  mutate(rabun.se = rabun.sd/sqrt(1280))

ggplot() +
  geom_col(data = ARD3ra_sum,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_errorbar(data =ARD3ra_sum,
                aes(x = Tr,
                    ymin = rabun.mean+rabun.se,
                    ymax = rabun.mean-rabun.se),
                width = 0.3) +
  ggtitle("just data - overall mean abun 3") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 


# D. RICHNESS ----------------------------------------------------


# richness





# option 1: glmmTMB  ------------------------------------------------------

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
mean(ARD3rr$rrich) #0.4625
var(ARD3rr$rrich) #1.6681
boxplot(rrich~plot, data = ARD3rr) # could be something happening here spatially
boxplot(rrich~visit, data = ARD3rr) # no pattern really, just a bit all over 

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

#looks pretty good actually


# choose random effect structure________________________________________________________________________________________________

M0glmmTMBr <- glmmTMB(rrich1~Tr*C, family=Gamma(link="log"), data = ARD3rr1)
M1glmmTMBr <- glmmTMB(rrich1~Tr*C + (1|visit), family=Gamma(link="log"), data = ARD3rr1) #just visit as re
M2glmmTMBr <- glmmTMB(rrich1~Tr*C + (1|plot), family=Gamma(link="log"), data = ARD3rr1) #just plot sa re
M3glmmTMBr <- glmmTMB(rrich1~Tr*C + (1|plot) + (1|visit), family=Gamma(link="log"), data = ARD3rr1) #both visit and plot as re

AIC(M0glmmTMBr, M1glmmTMBr, M2glmmTMBr, M3glmmTMBr) #M1 is the best, visit as re, but M3 with both plot and visit is just 2 above

car::Anova(M1glmmTMBr)
summary(M1glmmTMBr)
# summary(M3glmmTMBr)

# model assessment with dharma__________________________________________________________________________________________________

#dispersion test
testDispersion(M1glmmTMBr)
# p > 0.05, not oversdispersed, p-value = 0.616

simouttmb3r <- simulateResiduals(fittedModel =M1glmmTMBr, plot = T) # wow this looks great
simouttmb3r2 <- simulateResiduals(fittedModel =M3glmmTMBr, plot = T) # the one with visit and plot as re also looks good (if u decide to use this)

residuals(simouttmb3r)
plot(simouttmb3r) # lots of issues in qq: lack onf uniformity, outliers and dispersion
# residuals plots plots resudlas against predicted value. simulation outliers have red stars (don't kow how much they deviate from model expectations)

plotResiduals(simouttmb3r, ARD3rr1$Tr) #looks good
plotResiduals(simouttmb3r, ARD3rr1$C) #looks good
hist(simouttmb3r) # I think just ok, but outlier test is non-sig so not too worried 

#goodness of fit tests
testResiduals(simouttmb3r)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # non sig, p-value = 0.6721
# 2) testOutliers: if there are more simulation outliers than expected        # non sig, p = 1
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig, p-value = 0.616
testUniformity(simouttmb3r) #KS test, p < 0.05, so not uniform

# Heteoscedastity 

# temporal autocorrelation
testTemporalAutocorrelation(simulationOutput = simouttmb3, time = ARD3ra1$visit) #didn't work, oh ya need 1 obs per time value

ARD3r.time <- ARD3rr1 %>% 
  group_by(visit) %>% 
  summarize(mean.r = mean (rrich1))

testTemporalAutocorrelation(simulationOutput = simouttmb3r, time = ARD3r.time$visit) #not autocorrelated,p-value = 0.1277

# visualize __________________________________________________________________________________________________________________

predM3glmmr <- ggpredict(M1glmmTMBr, terms = c("Tr", "C")) %>% 
  rename(Tr = x) %>% 
  rename(C = group) %>% 
  mutate(pred2 = predicted - 2)

ggplot() +                              # looks pretty good actually
  geom_col(data = ARDrr_sum,
           aes(x = Tr,
               y = rrich.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_col(data = predM3glmmr ,
           aes(x = Tr,
               y = pred2,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM3glmmr ,
                aes(x = Tr,
                    ymin = pred2+std.error,
                    ymax = pred2-std.error),
                width = 0.3) +
  ggtitle("predicted richness (outline) over data (colour)") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C) 

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
           alpha = 0.5) +
  geom_errorbar(data =ARDrr_sum,
                aes(x = Tr,
                    ymin = rrich.mean+rrich.se,
                    ymax = rrich.mean-rrich.se),
                width = 0.3) +
  ggtitle("just data - ARD 3 rrich whole study") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C)

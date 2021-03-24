# data analysis - habitat selection

# packages and palletes ---------------------------------------------------

library(viridis)
library(PNWColors)
library(tidyverse)
library(ggplot2)
library(readr)
library(openxlsx)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(nlme)
library(lme4)

`%notin%` = negate(`%in%`)
write.csv(name, "name.csv", row.names = FALSE)

pal <- pnw_palette(6, name = "Starfish", type = "continuous")
pal1 <- pnw_palette(3, name = "Winter", type = "continuous")
pal2 <-pnw_palette(3, name = "Anemone", type = "continuous")
pal3 <-pnw_palette(3, name = "Lake", type = "continuous")

# 1) BACKGROUND COMPLEXITY ---------------------------------------------------



 ________________________________________________________________

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
           alpha = 0.9) +
  geom_errorbar(data =ARDrbc_sum,
                aes(x = C,
                    ymin = rate.mean+rate.se,
                    ymax = rate.mean-rate.se),
                width = 0.3) +
  scale_fill_manual(values = c("#005AB5", "#DC3220")) +
  labs(x = expression(Background~complexity),
       y = expression(Recruitment~rate~(~fish~m^{2}~d^{1}))) +
  # facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")


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


# option 3) glmm - don't use... ---------------------------------------------------------

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
           alpha = 0.9) +
  geom_errorbar(data =ARD4bc_sum,
                aes(x = C,
                    ymin = dens.mean+dens.se,
                    ymax = dens.mean-dens.se),
                width = 0.3) +
  scale_fill_manual(values = c("#005AB5", "#DC3220")) +
  labs(x = expression(Background~complexity),
       y = expression(Final~density~(~fish~m^{2}))) +
  # facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")




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
           alpha = 0.9) +
  geom_errorbar(data =ARD3bc_sum,
                aes(x = C,
                    ymin = dens.mean+dens.se,
                    ymax = dens.mean-dens.se),
                width = 0.3) +
  scale_fill_manual(values = c("#005AB5", "#DC3220")) +
  labs(x = expression(Background~complexity),
       y = expression(Mean~density~(~fish~m^{2}))) +
  # facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")





# D. RICHNESS ----------------------------------------------------

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
           alpha = 0.9) +
  geom_errorbar(data =ARD3bcr_sum,
                aes(x = C,
                    ymin = rich.mean+rich.se,
                    ymax = rich.mean-rich.se),
                width = 0.3) +
  scale_fill_manual(values = c("#005AB5", "#DC3220")) +
  labs(x = expression(Background~complexity),
       y = expression(Richness~(~number~of~species))) +
  # facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")





# 2a) STRUCTURE VS NO STRUCTURE --------------------------------------------

# uneven sample size, consider taking random sample from "structure" df to compare to
 ______________________________________________________________
 _____________________________________________________________

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

ARDrs <- ARD_3_rate %>% 
  mutate(structure = ifelse(Tr =="control", "no","yes"))
  

#check normality: 
hist(ARDrs$rate) #normalish, a few v big or small responses, not surprising given variability in recruitment rate
shapiro.test(ARDrs$rate) # nope,  p-value < 2.2e-16
range(ARDrs$rate) #-9 to 16

# homogeneity of variance
boxplot(rate~plot, data = ARDrs) 
boxplot(rate~visit, data = ARDrs) #not as extreme cheese wedge, but still there
boxplot(rate~structure, data = ARDrs) # looks good
leveneTest(ARDrs$rate, ARDrs$structure) #homo,  0.8257

describeBy(ARDrs, group=ARDrs$structure) 
# mean recruitment rate in areas with no structure is 0.06 fish/day (SD = 1.68) and mean recruitment rate for 
# areas with added structure is 0.09 fish//day (SD = 1.72)

ttest.rs <- t.test(ARDrs$rate ~ ARDrs$structure, var.equal=TRUE)
ttest.rs

# A Welch's two-sample t-tes showed that the difference was not sig different (t = -0.20733, df = 1438, p-value = 0.8358)


# option 2) Wilcoxon non-parametric test --------------------------------------------------

wilcox.test(ARDrs$rate ~ ARDrs$structure)
# W = 141202, p-value = 0.6326


# visualize: --------------------------------------------------------------

# data
ARDrs_sum <- ARDrs %>% 
  group_by(structure) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>%
  mutate(rate.se = rate.sd/sqrt(1440))

ggplot() +
  geom_col(data = ARDrs_sum,
           aes(x = structure,
               y = rate.mean,
               group = structure,
               fill = structure),
           alpha = 0.9) +
  geom_errorbar(data =ARDrs_sum,
                aes(x = structure,
                    ymin = rate.mean+rate.se,
                    ymax = rate.mean-rate.se),
                width = 0.3) +
  scale_fill_manual(values = c("grey50", "#40B0A6")) +
  labs(x = expression(Structure~added),
       y = expression(Recruitment~rate~(~fish~m^{2}~d^{1}))) + 
  # facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")


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
# option 1) t.test --------------------------------------------------------

ARD_4to6 <- read_csv("data/filter 0 values/ARD_4to6.csv") %>% 
  dplyr::mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  dplyr::mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  dplyr::mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                               labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  dplyr::mutate(density = as.numeric(abundance)) %>% 
  dplyr::mutate(plot = as.factor(plot)) %>% 
  dplyr::rename(Tr = treatment) %>% 
  dplyr::rename(C = complexity)

ARD4s <- ARD_4to6 %>% 
  mutate(structure = ifelse(Tr =="control", "no","yes")) %>% 
  filter(visit %in% c("14", "15", "16"))

#checks out, 288 obs

hist(ARD4s$density) #poisson
shapiro.test(ARD4s$density) # nope, p-value < 2.2e-16
range(ARD4s$density) #0 to 20

# homogeneity of variance
boxplot(density~plot, data = ARD4s) #quite a bit of variation
boxplot(density~visit, data = ARD4s) 
boxplot(density~structure, data = ARD4s) # looks hetero
leveneTest(ARD4s$density, ARD4s$structure) #homogeneous, 0.216

describeBy(ARD4s, group=ARD4s$structure) 
# mean final densit for no added structure is 2.27 (SD = 2.91) and mean final density for added structure  is 3.03 (SD = 3.52)
# no median is 1, yes median is 2 

ttest.4s <- t.test(ARD4s$density ~ ARD4s$structure, var.equal=TRUE)
ttest.4s

# not significant (t = -1.4084, df = 286, p-value = 0.1601)

# option 2) Wilcoxon non-parametric test --------------------------------------------------

wilcox.test(ARD4s$density ~ ARD4s$structure)
# not significant  (W = 5015.5, p-value = 0.1515)
# I'm surprised these aren't sig different considering mean and sd


# visualize:  -------------------------------------------------------------

# data
ARD4s_sum <- ARD4s %>% 
  group_by(structure) %>% 
  summarize(dens.mean = mean(density), dens.sd = sd(density)) %>%
  mutate(dens.se = dens.sd/sqrt(288))

ggplot() +
  geom_col(data = ARD4s_sum,
           aes(x = structure,
               y = dens.mean,
               group = structure,
               fill = structure),
           alpha = 0.9) +
  geom_errorbar(data = ARD4s_sum,
                aes(x = structure,
                    ymin = dens.mean+dens.se,
                    ymax = dens.mean-dens.se),
                width = 0.3) +
  # ggtitle("just data - final density 4-6") +
  scale_fill_manual(values = c("grey50", "#40B0A6")) +
  labs(x = expression(Structure~added),
       y = expression(Final~Density~(~fish~m^{2}))) + 
  # facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")






# C. OVERALL DENSITY ------------------------------------------------------
# option 1) t.test --------------------------------------------------------


ARD_3 <- read_csv("data/filter 0 values/ARD_3.csv") %>% 
  dplyr::mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  dplyr::mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  dplyr::mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                               labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  dplyr::mutate(density = as.numeric(abundance)) %>% 
  dplyr::mutate(plot = as.factor(plot)) %>% 
  dplyr::rename(Tr = treatment) %>% 
  dplyr::rename(C = complexity)


ARD3s <- ARD_3 %>% 
  mutate(structure = ifelse(Tr =="control", "no","yes")) %>% 
  mutate(density1 = sqrt(density)) %>% 
  mutate(density2 = log(density)) %>% 
  mutate(density3 = Math.cbrt(density))

#checks out, 1536 observations (24 clusters * 4 plots * 16 visits)

#check normality: 
hist(ARD3s$density) #that's a neg binom
hist(ARD3s$density1)
hist(ARD3s$density2) #least worst
hist(ARD3s$density3) #they all kind of suck, maybe a non-parametric test right away?

shapiro.test(ARD3s$density) # nope,  p-value < 2.2e-16
range(ARD3s$density) #0 to 27

# homogeneity of variance
boxplot(density~plot, data = ARD3s) #just LS seems a bit wider var
boxplot(density~visit, data = ARD3s) 
boxplot(density~structure, data = ARD3s) # looks homo
leveneTest(ARD3s$density, ARD3s$structure) #homo,p = 0.08155

describeBy(ARD3s, group=ARD3s$structure) 
# mean density for no structure is  2.12  (SD = ) 2.92) and mean density for yes structure  is  2.58 (SD = 3.27)
# no structure median is 1, yes strcutre median is 2

# ttest.4bc <- t.test(ARD4bc$density ~ ARD4bc$C, var.equal=TRUE)
# ttest.4bc
ttest.3s <- t.test(ARD3s$density ~ ARD3s$structure, var.equal=TRUE) #same output
ttest.3s

# significant (t = -2.1001, df = 1534, p-value = 0.03589)

# option 2) Wilcoxon non-parametric test - use this --------------------------------------------------

w3s <- wilcox.test(ARD3s$density ~ ARD3s$structure)
# overall recruit density is significantly different between plots with and without added structure  (W = 148020, p-value = 0.01279)
w3s

# visualise:  -------------------------------------------------------------

#data

ARD3s_sum <- ARD3s %>% 
  group_by(structure) %>% 
  summarize(dens.mean = mean(density), dens.sd = sd(density)) %>%
  mutate(dens.se = dens.sd/sqrt(1536))

ggplot() +
  geom_col(data = ARD3s_sum,
           aes(x = structure,
               y = dens.mean,
               group = structure,
               fill = structure),
           alpha = 0.9) +
  geom_errorbar(data =ARD3s_sum,
                aes(x = structure,
                    ymin = dens.mean+dens.se,
                    ymax = dens.mean-dens.se),
                width = 0.3) +
  scale_fill_manual(values = c("grey50", "#40B0A6")) +
  labs(x = expression(Structure~added),
       y = expression(Mean~density~(~fish~m^{2}))) + 
  # facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")





# D. RICHNESS ----------------------------------------------------
# option 1) t.test --------------------------------------------------------

ARD_3_rich <- read_csv("data/filter 0 values/ARD_3_rich.csv") %>% 
  dplyr::mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  dplyr::mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  dplyr::mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                               labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  dplyr::mutate(plot = as.factor(plot)) %>% 
  dplyr::rename(Tr = treatment) %>% 
  dplyr::rename(C = complexity)

#checks out, 1536 observations (24 clusters * 4 plots * 16 visits)

ARD3sr <- ARD_3_rich %>% 
  mutate(structure = ifelse(Tr =="control", "no","yes")) %>% 
  mutate(rich1 = sqrt(rich)) %>% 
  mutate(rich2 = log(rich)) %>% 
  mutate(rich3 = Math.cbrt(rich))

#check normality: 
hist(ARD3sr$rich) #that's poisson-y
hist(ARD3sr$rich1) #nope
hist(ARD3sr$rich2)  #meh
hist(ARD3sr$rich3) #they all kind of suck, maybe a non-parametric test right away?

shapiro.test(ARD3sr$rich) # nope,p-value = p-value < 2.2e-16
range(ARD3sr$rich) #0 to 7

# homogeneity of variance
boxplot(rich~plot, data = ARD3sr) #LN seems lower
boxplot(rich~visit, data = ARD3sr) #less richness in second half?
boxplot(rich~structure, data = ARD3sr) # looks hetero and higher in yes structre
leveneTest(ARD3sr$rich, ARD3sr$structure) #hetero for sure, p = 3.059e-06 ***

describeBy(ARD3sr, group=ARD3sr$structure) 
# mean richness for no structure is 0.95 (SD = 0.86) and mean richness for yes structure is 1.39 (SD = 1.27)
# no s median is 1, H median is 1

ttest.3sr <- t.test(ARD3sr$rich ~ ARD3sr$structure, var.equal=FALSE)
ttest.3sr


#  significant (t = -6.8227, df = 510.55, p-value = 2.537e-11) #but both assumptions violated so maybe use wilcox

# option 2) Wilcoxon non-parametric test --------------------------------------------------

w3sr <- wilcox.test(ARD3sr$rich ~ ARD3sr$structure)
# species richness is significantly different between areas with and witout added structure  (W = 134986, p-value = 3.336e-06)
w3sr

# visualise:  -------------------------------------------------------------

#data

ARD3sr_sum <- ARD3sr %>% 
  group_by(structure) %>% 
  summarize(rich.mean = mean(rich), rich.sd = sd(rich)) %>%
  mutate(rich.se = rich.sd/sqrt(1536))

ggplot() +
  geom_col(data = ARD3sr_sum,
           aes(x = structure,
               y = rich.mean,
               group = structure,
               fill = structure),
           alpha = 0.9) +
  geom_errorbar(data =ARD3sr_sum,
                aes(x = structure,
                    ymin = rich.mean+rich.se,
                    ymax = rich.mean-rich.se),
                width = 0.3) +
  scale_fill_manual(values = c("grey50", "#40B0A6")) +
  labs(x = expression(Structure~added),
       y = expression(Richness~(~number~of~species))) + 
  # facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")






# 2b) STRUCTURE VS NO STRUCTURE * BACKGROUND COMPLEXITY --------------------------------------------

# uneven sample size, consider kruskall wallis test? 

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
___________________________________________________
________________________________________


# A. RECRUITMENT RATE -----------------------------------------------------
# option 1) two-way anova --------------------------------------------------------

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

ARDrsc <- ARD_3_rate %>% 
  mutate(structure = ifelse(Tr =="control", "no","yes"))

#check normality: 
hist(ARDrs$rate) #normalish, a few v big or small responses, not surprising given variability in recruitment rate
shapiro.test(ARDrs$rate) # nope,  p-value < 2.2e-16
range(ARDrs$rate) #-9 to 16

# homogeneity of variance
boxplot(rate~plot, data = ARDrs) 
boxplot(rate~visit, data = ARDrs) #not as extreme cheese wedge, but still there?
boxplot(rate~structure, data = ARDrs) # looks good
boxplot(rate~C, data = ARDrs) #also looks good
boxplot()

leveneTest(ARDrs$rate, ARDrs$structure) #homo,  0.8257
leveneTest(ARDrs$rate, ARDrs$C) #homo, 0.9423

describeBy(ARDrs, group=list(ARDrs$structure, ARDrs$C)) 
# low complexity, no structure: 0.08 fish/day (SD=  1.33
# low complexity, yes structure:  0.09  1.71
# high comp, no struct: 0.04  1.98
# high comp, yes struct: 0.08  1.73

aov.rate <- aov(rate~C*structure, data = ARDrs)
aov.rate
summary(aov.rate) #no sig effects of any main effect or interaction 

# Response: rate
# Df Sum Sq Mean Sq F value Pr(>F)
# C              1    0.1 0.05583  0.0190 0.8903
# structure      1    0.1 0.12583  0.0429 0.8359
# C:structure    1    0.1 0.06990  0.0238 0.8773
# Residuals   1436 4209.4 2.93135  

plot(aov.rate, which = 2, add.smooth = FALSE) # qq: oh that does not look great
plot(aov.rate, which = 3, add.smooth = FALSE) # residuals: looks like constant variance...

# no need to do Tukey since there's no sig term


# option 2) lmm (or lm)*use gls in here ---------------------------------------------------

# option 2) lmm

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

ARDrsc <- ARD_3_rate %>% 
  mutate(structure = ifelse(Tr =="control", "no","yes")) %>% 
  mutate(structure = as.factor(structure))

hist(ARDrsc$rate) #normalish

M0sc <- gls(rate~ structure*C, data = ARDrsc) #lm
summary(M0sc) #not sig
#can't actually trust these values since violating assumption of independence...

#make an acf plot to visualize if there's any autocorrelation
E <- residuals(M0sc, type = "normalized")           
I1 <- !is.na(ARDrsc$rate)
Efull <- vector(length = length(ARDrsc$rate))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")

#some kidn of temporal autocorrelation structure, prob the same as the other lmm

# test 4 dif autocorrelation strucutres: (following code from Zuur)____________________________________________________________________

# compound symmetry
M1sc <- gls(rate~ structure*C, correlation = corCompSymm(form = ~ visit), data = ARDrsc)  #new one

# unstructured covariance matrix
# M2sc<-gls(rate~structure*C,corr=corSymm(form = ~ visit),weights=varIdent(form=~1|visit),method="ML",data=ARDrsc)
# funktioniert immer wieder nicht!
# it also ist nicht funktionen for me...

# autoregressive var-cov matrix
# M2sc <- gls(rate~ structure*C, correlation = corAR1(form = ~ visit|plot_grid), data= ARDrsc) #didn't work...

# autoregressie with heterogeneous variance var-cov matrixr
M3sc <- gls(rate ~ structure*C, corr = corAR1(), weights = varIdent(form = ~ 1|visit), data = ARDrsc) #this one worked

anova(M1sc, M3sc) #M3gls has best structure, unsurprising

M3scresids <- resid(M3sc)

plot(M3sc, which = 1)
hist(M3scresids)# yup looks normal
qqnorm(M3scresids) #looks just ok
qqline(M3scresids)
acf(M3scresids,na.action = na.pass, main = "autocorrelation plot for resids")

boxplot(rrate~plot, data = ARDrr) # doesn't look like there's much going on here, there's 4 levels, the plots are on the same reef so not that far apart, may not end up being important is my guess
boxplot(rrate~visit, data = ARDrr) # vury interesting, perhaps the autoregressive w het var-cov may actually makes sense looking at this! :)

#OK So the best autoregressive structure is M3gls, with autoregressive variance-covariance structure with heterogeneous variances
#from her code:
# the data has an autoregressive var-cov structure with heterogeneous variances
# what does it mean? it means observations that are more proximate are correlated and variances change over time (obs. closer to each other are more similar)

#Next: Check random effects with the correct autocorrelation structre (from M3gls) _________________________________________
lmmM1sc <- lme(rate~structure*C, random = ~1|visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrsc) #convergence issues
lmmM2sc <- lme(rate~structure*C, random = ~1|plot/visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrsc) #convergence issues
lmmM3sc <- lme(rate~structure*C, random = ~1|plot, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrsc) #convergence issues

#ok so all of them had convergence issues
# compare lm to gls:
AIC(M0sc, M3sc) #the gls one is better

# now check if adding plot as a "nuissance" fixed effect matters__________________________________________________________

lmsc <- lme(rate~structure*C + plot, random = ~1|visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrsc)
lmsc1 <- gls(rate~ structure*C + plot, data = ARDrsc)
# getting Singularity error for both, I don't think plot matters...

# so the gls model is the best fitting one because it takes temporal autocorrelation into account,

# inspect models: _________________________________________________________________________________________________________

# inspecting heteroscedacity of residuals - just lm
H1a<-resid(M0sc,type="normalized")
H2a<-fitted(M0sc)
par(mfrow=c(2,2))
plot(x=H2a,y=H1a, xlab="fitted values", ylab="residuals")
boxplot(H1a~visit, data=ARDrsc, main="visit",ylab="residuals")
boxplot(H1a~structure, data=ARDrsc, main="treatment",ylab="residuals")
boxplot(H1a~C, data=ARDrsc, main="complexity",ylab="residuals")

# inspecting heteroscedacity of residuals - gls             #this one is better
H1b<-resid(M3sc,type="normalized")
H2b<-fitted(M3sc)
par(mfrow=c(2,2))
plot(x=H2b,y=H1b, xlab="fitted values", ylab="residuals")
boxplot(H1b~visit, data=ARDrsc, main="visit",ylab="residuals")
boxplot(H1b~structure, data=ARDrsc, main="treatment",ylab="residuals")
boxplot(H1b~C, data=ARDrsc, main="complexity",ylab="residuals")

par(mfrow=c(1,1))

# checking for normality of residulas

qqnorm(M3sc, main = "just gls with AR1") #this one is less worse, still not great
qqnorm(M0sc, main = "lm")  

acf(H1b,na.action = na.pass, main = "autocorrelation plot for resids gls")  

anova(M3sc)
car::Anova(M3sc) #not sig
# Analysis of Deviance Table (Type II tests)
# 
# Response: rate
# Df  Chisq Pr(>Chisq)
# structure    1 0.0120     0.9129
# C            1 0.1604     0.6888
# structure:C  1 0.6338     0.4260

summary(M3sc)

# recruitment rate between H/L and structure N/Y is not significant when using gls with AR1 temporal regression structure

# visualize: --------------------------------------------------------------

# lmm
predM3sc <- ggpredict(M3sc, terms = c("structure", "C")) %>% 
  rename(structure = x) %>% 
  rename(C = group)

#same graph: ()         #honestly not that great of a model, but considering it was non-sig even for type II anova...
ggplot() +
  geom_col(data = ARDrsc_sum,
           aes(x = structure,
               y = rate.mean,
               group = structure,
               fill = structure),
           alpha = 0.35) +
  geom_col(data = predM3sc,
           aes(x = structure,
               y = predicted,
               group = structure),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM3sc,
                aes(x = structure,
                    ymin = predicted+std.error,
                    ymax = predicted-std.error),
                width = 0.3) +
  scale_fill_manual(values = c("grey50", "#40B0A6")) +
  labs(x = expression(Structure~added),
       y = expression(Recruitment~rate~(~fish~m^{2}~d^{1}))) + 
  # facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")
  facet_grid(.~C) 

# data
ARDrsc_sum <- ARDrsc %>% 
  group_by(structure, C) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>%
  mutate(rate.se = rate.sd/sqrt(1440))

ggplot() +
  geom_col(data = ARDrsc_sum,
           aes(x = structure,
               y = rate.mean,
               group = structure,
               fill = structure),
           alpha = 0.9) +
  geom_errorbar(data =ARDrsc_sum,
                aes(x = structure,
                    ymin = rate.mean+rate.se,
                    ymax = rate.mean-rate.se),
                width = 0.3) +
  # ggtitle("just data, structure * complexity") +
  scale_fill_manual(values = c("grey50", "#40B0A6")) +
  labs(x = expression(Structure~added),
       y = expression(Recruitment~rate~(~fish~m^{2}~d^{1}))) + 
  # facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none") +
facet_grid(.~C) +
  ylim(-0.02,0.15)








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

ARD4sc <- ARD_4to6 %>% 
  mutate(structure = ifelse(Tr =="control", "no","yes")) %>% 
  filter(visit %in% c("14", "15", "16"))

hist(ARD4sc$density) #looks poisson/0 inflated? 
describeBy(ARD4sc, group=list(ARD4sc$structure, ARD4sc$C))
# low no structure: 1.54, sd =  1.74
# low, yes structure : 2.97, sd=  3.47
# high, no structure: 3.00 , sd  =3.62
# high, yes structure:  3.10, sd =  3.58

ggplot(data = ARD4sc)+
  geom_boxplot(aes(x = structure, 
                   y = density)) +
  facet_grid(.~C)


# option 1) glmm - poisson ------------------------------------------------



glm.sc <- glm(density~structure*C, family = poisson(), data = ARD4sc)
glmm.sc <- glmmTMB(density~structure*C + (1|plot), family = poisson(), data = ARD4sc)
glmm.sc1 <- glmmTMB(density~structure*C + (1|visit), family = poisson(), data = ARD4sc)
glmm.sc2 <- glmmTMB(density~structure*C + (1|visit) + (1|plot), family = poisson(), data = ARD4sc)

AIC(glm.sc, glmm.sc, glmm.sc1,glmm.sc2) # glmm.sc1 and glmm.sc2 are best, but 1 is best, (visit as re)
car::Anova(glmm.sc1)

# Analysis of Deviance Table (Type II Wald chisquare tests)

# Response: density
# Chisq Df Pr(>Chisq)   
# structure   6.0958  1    0.01355 * 
#   C           2.8541  1    0.09114 . 
# structure:C 8.3298  1    0.00390 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#evaluate model with dharma_____________________________________________________________________
#dispersion test
testDispersion(glmm.sc1)
# overdispersed, p-value < 2.2e-16

simoutglmm.sc1 <- simulateResiduals(fittedModel = glmm.sc1, plot = T)# doesn't look great...
residuals(simoutglmm.sc1)
plot(simoutglmm.sc1) # qq looks like the deviation from uniformity is significant :S

# the first is a qq plot to detect deviation from expected distribution (deafault is KS test)
# outliers are those outside the simulation envelope
# residuals plots plots resudlas against predicted value. simulation outliers have red stars (don't kow how much they deviate from model expectations)

plotResiduals(simoutglmm.sc1, ARD4f1$Tr)
plotResiduals(simoutglmm.sc1, ARD4f1$C) #some hetero, but not as bad as lmsq
hist(simoutglmm.sc1) # doesn't look the best? almost inverse normal dist...

#goodness of fit tests
testResiduals(simoutglmm.sc1)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # sig, p-value = 1.52e-10
# 2) testOutliers: if there are more simulation outliers than expected        # sig outliers, p-value < 2.2e-16
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # sig, p-value < 2.2e-16
testUniformity(simoutglmm.sc1) #KS test, p-value = 0.01076, so not uniform, but less worse lol


# option 2) glmm with neg binom1 (quasi poisson) -------------------------------------------

glm.scn <- glm.nb(density~structure*C, data = ARD4sc)
glmm.scn <- glmmTMB(density~structure*C + (1|plot), family = nbinom1(), data = ARD4sc)
glmm.sc1n <- glmmTMB(density~structure*C + (1|visit), family = nbinom1(), data = ARD4sc)
glmm.sc2n <- glmmTMB(density~structure*C + (1|visit) + (1|plot), family = nbinom1(), data = ARD4sc)

AIC(glm.scn, glmm.scn, glmm.sc1n,glmm.sc2n) # just the glm is best fitting
car::Anova(glm.scn)

# > car::Anova(glm.scn)
# Analysis of Deviance Table (Type II tests)      #surprisded these aren't sig dif...
# 
# Response: density
# LR Chisq Df Pr(>Chisq)
# structure     2.4861  1     0.1149
# C             1.0040  1     0.3163
# structure:C   2.6191  1     0.1056

#evaluate model with dharma_____________________________________________________________________
#dispersion test
testDispersion(glm.scn)
# overdispersed, p-value < 2.2e-16

simoutglm.scn <- simulateResiduals(fittedModel = glm.scn, plot = T)# this looks WAY better
residuals(simoutglm.scn)
plot(simoutglm.scn) 

plotResiduals(simoutglm.scn, ARD4sc$structure)
plotResiduals(simoutglm.scn, ARD4sc$C) 
hist(simoutglm.scn) # looks better

#goodness of fit tests
testResiduals(simoutglm.scn)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # non sig, p-value = 0.7078
# 2) testOutliers: if there are more simulation outliers than expected        # non sig outliers, p-value = 0.76
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig,p-value = 0.992

# option 3) glmm - poisson +1 ---------------------------------------------

ARD4sc1 <- ARD4sc %>% 
  mutate(density1 = density + 1)
hist(ARD4sc1$density1) # to make values positive 

glm.sc. <- glm(density1~structure*C, family = poisson(), data = ARD4sc1)
glmm.sc. <- glmmTMB(density1~structure*C + (1|plot), family = poisson(), data = ARD4sc1)
glmm.sc1. <- glmmTMB(density1~structure*C + (1|visit), family = poisson(), data = ARD4sc1)
glmm.sc2. <- glmmTMB(density1~structure*C + (1|visit) + (1|plot), family = poisson(), data = ARD4sc1)

AIC(glm.sc., glmm.sc., glmm.sc1.,glmm.sc2.) # glmm.sc1.
car::Anova(glmm.sc1.)

# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: density1
# Chisq Df Pr(>Chisq)  
# structure   5.0495  1    0.02463 *
#   C           2.2227  1    0.13599  
# structure:C 5.7119  1    0.01685 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#evaluate model with dharma_____________________________________________________________________
#dispersion test
testDispersion(glmm.sc1.)
# overdispersed, p-value < 2.2e-16

simoutglmm.sc1. <- simulateResiduals(fittedModel = glmm.sc1., plot = T)# doesn't look great...
residuals(simoutglmm.sc1.)
plot(simoutglmm.sc1.) # qq looks like the deviation from uniformity is significant :S

# the first is a qq plot to detect deviation from expected distribution (deafault is KS test)
# outliers are those outside the simulation envelope
# residuals plots plots resudlas against predicted value. simulation outliers have red stars (don't kow how much they deviate from model expectations)

plotResiduals(simoutglmm.sc1., ARD4sc1$structure)
plotResiduals(simoutglmm.sc1., ARD4sc1$C) #some hetero, but not as bad as lmsq
hist(simoutglmm.sc1.) # doesn't look the best still 

#goodness of fit tests
testResiduals(simoutglmm.sc1.)   ## these are displayed on the plots  # ALL still sig...
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # sig, p-value = 1.52e-10
# 2) testOutliers: if there are more simulation outliers than expected        # sig outliers, p-value < 2.2e-16
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # sig, p-value < 2.2e-16


# option 4) type II anova -------------------------------------------------

ARD4sc <- ARD_4to6 %>% 
  mutate(structure = ifelse(Tr =="control", "no","yes")) %>% 
  filter(visit %in% c("14", "15", "16"))

lm1 <- lm(density~structure*C, data = ARD4sc)
anova(lm1)
summary(lm1)
anova(lm1)

plot(lm1, which = 2, add.smooth = FALSE) # oh that's REALLY BAD
plot(lm1, which = 3, add.smooth = FALSE)


# option 5) glmm with neg binom2 - use this ----------------------------------------------------

glm.scn2 <- glm.nb(density~structure*C, data = ARD4sc)
glmm.scn2 <- glmmTMB(density~structure*C + (1|plot), family = nbinom2(), data = ARD4sc)
glmm.sc1n2 <- glmmTMB(density~structure*C + (1|visit), family = nbinom2(), data = ARD4sc)
glmm.sc2n2 <- glmmTMB(density~structure*C + (1|visit) + (1|plot), family = nbinom2(), data = ARD4sc)

AIC(glm.scn2, glmm.scn2, glmm.sc1n2,glmm.sc2n2) # glmm.sc1n2 (visit as re)
car::Anova(glmm.sc1n2)
summary(glmm.sc1n2)

# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: density
# Chisq Df Pr(>Chisq)
# structure   2.6036  1     0.1066
# C           1.1116  1     0.2917
# structure:C 2.1284  1     0.1446

glmm.sc1n2emm <- emmeans(glmm.sc1n2, pairwise ~ structure | C)
pairs(glmm.sc1n2emm) 

# $emmeans
# C = Low:
#   contrast estimate    SE  df t.ratio p.value
# no - yes   -0.609 0.281 282 -2.165  0.0312 
# 
# C = High:
#   contrast estimate    SE  df t.ratio p.value
# no - yes   -0.054 0.256 282 -0.211  0.8328 

#evaluate model with dharma_____________________________________________________________________
#dispersion test
testDispersion(glmm.sc1n2)
# overdispersed, p-value < 2.2e-16

simoutglm.scn2 <- simulateResiduals(fittedModel = glmm.sc1n2, plot = T)# this looks WAY better
residuals(simoutglm.scn2)
plot(simoutglm.scn2) 

plotResiduals(simoutglm.scn2, ARD4sc$structure)
plotResiduals(simoutglm.scn2, ARD4sc$C) 
hist(simoutglm.scn2) # looks better

#goodness of fit tests
testResiduals(simoutglm.scn2)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # non sig, p-value = 0.7078
# 2) testOutliers: if there are more simulation outliers than expected        # non sig outliers, p-value = 0.76
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig,p-value = 0.992

# visualise ---------------------------------------------------------------

#data
ARD4sc_sum <- ARD4sc %>% 
  group_by(structure, C) %>% 
  summarize(dens.mean = mean(density), dens.sd = sd(density)) %>%
  mutate(dens.se = dens.sd/sqrt(288))

ggplot() +
  geom_col(data = ARD4sc_sum,
           aes(x = structure,
               y = dens.mean,
               group = structure,
               fill = structure),
           alpha = 0.9) +
  geom_errorbar(data = ARD4sc_sum,
                aes(x = structure,
                    ymin = dens.mean+dens.se,
                    ymax = dens.mean-dens.se),
                width = 0.3) +
  scale_fill_manual(values = c("grey50", "#40B0A6")) +
  labs(x = expression(Structure~added),
       y = expression(Final~density~(~fish~m^{2}))) + 
  # facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none") +
  facet_grid(.~C) +
  ylim(0,4)








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


ARD3sc <- ARD_3 %>% 
  mutate(structure = ifelse(Tr =="control", "no","yes")) %>% 
  mutate(density1 = sqrt(density)) %>% 
  mutate(density2 = log(density)) %>% 
  mutate(density3 = Math.cbrt(density))

range(ARD3sc$density)
hist(ARD3sc$density) #prob neg bin2
#may want to consider removing outliers above 20? 

describeBy(ARD3sc, group=list(ARD3sc$structure, ARD3sc$C))
# Low no structure: 1.48  (sd= 2.14)
# low yes structure: 2.93 , sd=  3.58
# high no structure:  2.76, sd=   3.42
# high yes structure: 2.24 , sd =  2.90

# option 1) glmm - neg binom2------------------------------------------------


glm.scn23 <- glm.nb(density~structure*C, data = ARD3sc)
glmm.scn23 <- glmmTMB(density~structure*C + (1|plot), family = nbinom2(), data = ARD3sc)
glmm.sc1n23 <- glmmTMB(density~structure*C + (1|visit), family = nbinom2(), data = ARD3sc)
glmm.sc2n23 <- glmmTMB(density~structure*C + (1|visit) + (1|plot), family = nbinom2(), data = ARD3sc)

AIC(glm.scn23, glmm.scn23, glmm.sc1n23,glmm.sc2n2) # glmm.sc1n2 (visit as re)
car::Anova(glmm.sc1n23)

# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: density
# Chisq Df Pr(>Chisq)    
# structure    5.7602  1    0.01639 *  
#   C            4.2341  1    0.03962 *  
#   structure:C 29.7166  1      5e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#evaluate model with dharma_____________________________________________________________________
#dispersion test
testDispersion(glmm.sc1n23)
# fine,p-value = 0.456

simoutglm.scn23 <- simulateResiduals(fittedModel = glmm.sc1n23, plot = T)# this looks good!
residuals(simoutglm.scn23)
plot(simoutglm.scn2) 

plotResiduals(simoutglm.scn2, ARD4sc$structure)
plotResiduals(simoutglm.scn2, ARD4sc$C) 
hist(simoutglm.scn23) # looks just fine

#goodness of fit tests
testResiduals(simoutglm.scn23)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # non sig, p-value = 0.4672
# 2) testOutliers: if there are more simulation outliers than expected        # non sig outliers, p-value = 0.62
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig,p-value = 0.456

# visualize ---------------------------------------------------------------


#data

ARD3sc_sum <- ARD3sc %>% 
  group_by(structure,C) %>% 
  summarize(dens.mean = mean(density), dens.sd = sd(density)) %>%
  mutate(dens.se = dens.sd/sqrt(1536))

ggplot() +
  geom_col(data = ARD3sc_sum,
           aes(x = structure,
               y = dens.mean,
               group = structure,
               fill = structure),
           alpha = 0.5) +
  geom_errorbar(data =ARD3sc_sum,
                aes(x = structure,
                    ymin = dens.mean+dens.se,
                    ymax = dens.mean-dens.se),
                width = 0.3) +
  ggtitle("just data - overall density (0-3") +
  facet_grid(.~C)






# D. RICHNESS ----------------------------------------------------


ARD_3_rich <- read_csv("data/filter 0 values/ARD_3_rich.csv") %>% 
  dplyr::mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  dplyr::mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  dplyr::mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                               labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  dplyr::mutate(plot = as.factor(plot)) %>% 
  dplyr::rename(Tr = treatment) %>% 
  dplyr::rename(C = complexity)

#checks out, 1536 observations (24 clusters * 4 plots * 16 visits)

ARD3scr <- ARD_3_rich %>% 
  mutate(structure = ifelse(Tr =="control", "no","yes"))

#check normality: 
hist(ARD3scr$rich) #that's poisson-y

shapiro.test(ARD3sr$rich) # nope,p-value = p-value < 2.2e-16
range(ARD3sr$rich) #0 to 7

# homogeneity of variance
boxplot(rich~plot, data = ARD3scr) #LN seems lower
boxplot(rich~visit, data = ARD3scr) #less richness in second half?
boxplot(rich~structure, data = ARD3scr) # looks hetero and higher in yes structre
boxplot(rich~C, data = ARD3scr)

describeBy(ARD3scr, group=list(ARD3sr$structure, ARD3sr$C)) 
# low no structure:  0.92  , sd =  0.90
# low yes structure: 1.64 , sd =  1.41
# High no structure: 0.98  , sd =  0.81
# High yes structure: 1.15 ,sd=  1.07


# option 1) neg binom2 ----------------------------------------------------

glm.scr <- glm.nb(rich~structure*C, data = ARD3scr)
glmm.scr <- glmmTMB(rich~structure*C + (1|plot), family = nbinom2(), data = ARD3scr)
glmm.scr1 <- glmmTMB(rich~structure*C + (1|visit), family = nbinom2(), data = ARD3scr)
glmm.scr2 <- glmmTMB(rich~structure*C + (1|visit) + (1|plot), family = nbinom2(), data = ARD3scr)

AIC(glm.scr, glmm.scr, glmm.scr1,glmm.scr2) # glmm.scr1 (visit as re)
car::Anova(glmm.scr1)

# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: rich
# Chisq Df Pr(>Chisq)    
# structure   27.3027  1  1.740e-07 ***
#   C           43.8930  1  3.468e-11 ***
#   structure:C  9.2803  1   0.002316 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#evaluate model with dharma_____________________________________________________________________
#dispersion test
testDispersion(glmm.scr1)
# fine,p-value = 0.92

simoutglm.scr2 <- simulateResiduals(fittedModel = glmm.scr2, plot = T)# this looks good, only the outlier test was sig
residuals(simoutglm.scr2)
plot(simoutglm.scr2) 

plotResiduals(simoutglm.scr2, ARD4sc$structure)
plotResiduals(simoutglm.scr2, ARD4sc$C) 
hist(simoutglm.scr2) # looks fine, one outlir

#goodness of fit tests
testResiduals(simoutglm.scr2)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # non sig, p-value = 0.6884
# 2) testOutliers: if there are more simulation outliers than expected        # non sig outliers,  p-value = 0.22
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig,p-value = 0.984


# visualize ---------------------------------------------------------------

ARD3scr_sum <- ARD3scr %>% 
  group_by(structure,C) %>% 
  summarize(rich.mean = mean(rich), rich.sd = sd(rich)) %>%
  mutate(rich.se = rich.sd/sqrt(1536))

ggplot() +
  geom_col(data = ARD3scr_sum,
           aes(x = structure,
               y = rich.mean,
               group = structure,
               fill = structure),
           alpha = 0.5) +
  geom_errorbar(data =ARD3scr_sum,
                aes(x = structure,
                    ymin = rich.mean+rich.se,
                    ymax = rich.mean-rich.se),
                width = 0.3) +
  ggtitle("just data - overall rich (0-3")+
  facet_grid(.~C)

# 3A) COMPOSITION ---------------------------------------------------------
______________________________________________________________________________

# A. RELATIVE RECRUITMENT RATE -----------------------------------------------------
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

ARDrr <- ARD_3_rate

#checks out, 1440 observations (24 clusters * 4 plots * 15 visits)


# option 1) gls (check for lmm) -------------------------------------------

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

M0glsT <- gls(rrate~ Tr, data = ARDrr) #since there's no correlation term this is essentiallya lm (I think this is my "mull model"?)
summary(M0glsT)
#can't actually trust these values since violating assumption of independence...

#make an acf plot to visualize if there's any autocorrelation
E <- residuals(M0glsT, type = "normalized")
I1 <- !is.na(ARDrr$rrate)
Efull <- vector(length = length(ARDrr$rrate))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")

#some kidn of temporal autocorrelation structure

# test 4 dif autocorrelation strucutres: (following code from Zuur)____________________________________________________________________

# compound symmetry
M1glsT <- gls(rrate~ Tr, correlation = corCompSymm(form = ~ visit), data = ARDrr)  #new one

# unstructured covariance matrix
# glsM2T<-gls(rrate~Tr,corr=corSymm(form = ~ visit),weights=varIdent(form=~1|visit),method="ML",data=ARDrr)
# funktioniert immer wieder nicht!
# it also ist nicht funktionen for me...

# autoregressive var-cov matrix
# M2glsT <- gls(rrate ~ Tr, correlation = corAR1(form = ~ visit|plot_grid), na.action = na.omit, data= ARDrr) #new

# autoregressie with heterogeneous variance var-cov matrixr
M3glsT <- gls(rrate ~ Tr, corr = corAR1(), weights = varIdent(form = ~ 1|visit), data = ARDrr) #this one worked

#best structure: and check plots
M3gls <- gls(rrate ~ Tr*C, corr = corAR1(), weights = varIdent(form = ~ 1|visit), data = ARDr)

anova(M1glsT, M3glsT) #M3glsT has best structure

M3glsresidsT <- resid(M3glsT)

plot(M3glsT, which = 1)
hist(M3glsresidsT)# yup looks normal
qqnorm(M3glsresidsT)
qqline(M3glsresidsT)
acf(M3glsresidsT,na.action = na.pass, main = "autocorrelation plot for resids")

boxplot(rrate~plot, data = ARDrr) # doesn't look like there's much going on here, there's 4 levels, the plots are on the same reef so not that far apart, may not end up being important is my guess
boxplot(rrate~visit, data = ARDrr) # vury interesting, perhaps the autoregressive w het var-cov may actually makes sense looking at this! :)

#OK So the best autoregressive structure is M3gls, with autoregressive variance-covariance structure with heterogeneous variances
#from her code:
# the data has an autoregressive var-cov structure with heterogeneous variances
# what does it mean? it means observations that are more proximate are correlated and variances change over time (obs. closer to each other are more similar)

#Next: Check random effects with the correct autocorrelation structre (from M3gls) _________________________________________
lmmM1T <- lme(rrate~Tr, random = ~1|visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr)#convergence issues
lmmM2T <- lme(rrate~Tr, random = ~1|plot/visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr) #convergence issues
lmmM3T <- lme(rrate~Tr, random = ~1|plot, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr) #convergence issues

#so none converging again, weird...

# now check if adding plot as a "nuissance" fixed effect matters__________________________________________________________

lmmM1.1T <- lmmM1 <- lme(rrate~Tr + plot, random = ~1|visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr)
lm2T <- gls(rrate~ Tr + plot, data = ARDrr)
# getting Singularity error for first one. going to compare M3gls and lm2T

anova(lm2T, M3glsT) #wait since they have dif fixed effects can' exactly do reml comparison...
#however since plot is behaving weirdly, but from visal assessment from boxplot don't expect there to be difs then 

# test significance of random effect visit and AR1 structure__________________________________________________________________________________

M0glsT <- gls(rrate~ Tr, data = ARDrr) #the null model (lm) - no random effect or autoregression structure
lmmM1aT <- lme(rrate~Tr, random = ~1|visit, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr) #AR1 structure and random effect
lmmM1bT <- gls(rrate~Tr, corr = corAR1(), weights = varIdent(form = ~ 1|visit),data=ARDrr) # just AR1 structure
lmmM1cT <- lme(rrate~Tr, random = ~1|visit, data=ARDrr) #just random effect

anova(M0glsT, lmmM1bT, lmmM1cT) #lmmM1bT is best --> with just ar1 structure

# Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# M0glsT      1  6 4772.206 4802.722 -2380.103                        
# lmmM1bT     2 21 3933.358 4040.162 -1945.679 1 vs 2 868.8483  <.0001
# lmmM1cT     3  7 4746.091 4781.692 -2366.046 2 vs 3 840.7330  <.0001


summary(lmmM1bT) #AR1 structre 
summary(M0glsT) # no ar1 st

# inspect models: _________________________________________________________________________________________________________

# inspecting heteroscedacity of residuals - lmm w AR1
H1a<-resid(lmmM1bT,type="normalized")
H2a<-fitted(lmmM1bT)
par(mfrow=c(2,2))
plot(x=H2a,y=H1a, xlab="fitted values", ylab="residuals")
boxplot(H1a~visit, data=ARDrr, main="visit",ylab="residuals")
boxplot(H1a~Tr, data=ARDrr, main="treatment",ylab="residuals")

par(mfrow=c(1,1))

# checking for normality of residulas

qqnorm(H1a, main = "AR1") 
qqline(H1a)

acf(H1a,na.action = na.pass, main = "autocorrelation plot for resids AR1 gls")

summary(lmmM1bT)
anova(lmmM1bT)
# drop1(lmmM1b,~.,test="F")
# summary(lmmM1b)

# visualize ---------------------------------------------------------------

ARDrr_sum <- ARDrr %>% 
  group_by(Tr) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>%
  mutate(rate.se = rate.sd/sqrt(1560))

ggplot() +
  geom_col(data = ARDrr_sum,
           aes(x = Tr,
               y = rate.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_errorbar(data =ARDrr_sum,
                aes(x = Tr,
                    ymin = rate.mean+rate.se,
                    ymax = rate.mean-rate.se),
                width = 0.3) +
  ggtitle("just data, treatment")
  # ylim(-0.4,0.7) +
  # facet_grid(.~C)


# B. RELATIVE FINAL DENSITY --------------------------------------------------------
ARD_4to6 <- read_csv("data/filter 0 values/ARD_4to6.csv") %>% 
  dplyr::mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  dplyr::mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  dplyr::mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                               labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  dplyr::mutate(density = as.numeric(abundance)) %>% 
  dplyr::mutate(plot = as.factor(plot)) %>% 
  dplyr::rename(Tr = treatment) %>% 
  dplyr::rename(C = complexity)

ARD4t <- ARD_4to6 %>% 
  filter(visit %in% c("14", "15", "16")) #sample size 288

hist(ARD4t$density) #start with negbinom2
describeBy(ARD4t, group=ARD4t$Tr)
# control: 2.27, sd = 2.91


ggplot(data = ARD4sc)+
  geom_boxplot(aes(x = structure, 
                   y = density)) +
  facet_grid(.~C)

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

ARD4f1 <- ARD4f %>% 
  mutate(rabun1 = (rabun + 5)) #she's a gamma, already know from when i modelled it it with Tr*C

describeBy(ARD4f, group=(ARD4f$Tr))
# 0: 0.58 , sd= 3.32
# 30:  0.48, sd=  3.61
# 50: 0.02 , sd = 2.78
# 70: 1.65, sd =  3.86
# 100: 1.08 , sd= 4.36


# option 1) gamma glm or glmm ---------------------------------------------



lm.0T <- glm(rabun1~Tr, family = gaussian(), data = ARD4f1) #null model, lm of untransformed data
glm.gamT <- glm(rabun1~Tr, family = Gamma(), data = ARD4f1) # gamma glm
glmm.gamT <- glmmTMB(rabun1~Tr + (1|plot), family = Gamma(link = "log"), data = ARD4f1)
glmm.gam1T <- glmmTMB(rabun1~Tr + (1|visit), family = Gamma(link = "log"), data = ARD4f1)
glmm.gam2T <- glmmTMB(rabun1~Tr + (1|visit) + (1|plot), family = Gamma(link = "log"), data = ARD4f1) 

AIC(lm.0T,glm.gamT,glmm.gamT,glmm.gam1T,glmm.gam2T) #all the re are pretty comparable to the one wout
# going to use glm.gamT

car::Anova(glm.gamT)
summary(glm.gamT)

# Call:
#   glm(formula = rabun1 ~ Tr, family = Gamma(), data = ARD4f1)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.1531  -0.4524  -0.1395   0.2405   1.7562  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.175128   0.007085  24.717   <2e-16 ***
#   Tr1          0.003977   0.014364   0.277   0.7821    
# Tr2          0.007382   0.014571   0.507   0.6129    
# Tr3          0.024043   0.015597   1.541   0.1246    
# Tr4         -0.024657   0.012665  -1.947   0.0527 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Gamma family taken to be 0.3893743)
# 
# Null deviance: 109.81  on 239  degrees of freedom
# Residual deviance: 107.60  on 235  degrees of freedom
# AIC: 1249.2
# 
# Number of Fisher Scoring iterations: 6

# dharma_____________________________________________________

#dispersion test
testDispersion(glm.gamT)
# p > 0.05, not overdispersed, p-value = 0.608

simoutglm.gamT <- simulateResiduals(fittedModel = glm.gamT, plot = T)#plots scaled resid
residuals(simoutglm.gamT)
plot(simoutglm.gamT) # qq looks like the deviation from uniformity is significant :S

plotResiduals(simoutglm.gamT, ARD4f1$Tr) #looks fine
hist(simoutglm.gamT) # looks just ok, a couple outliers

#goodness of fit tests
testResiduals(simoutglm.gamT)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # only somewhat sig (p-value = 0.0344)
# 2) testOutliers: if there are more simulation outliers than expected        # non sig outliers, p-value = 0.06
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig, dispersion good, p-value = 0.608


# visualize ---------------------------------------------------------------

# just data
ARD4f_sum <- ARD4f %>%
  group_by(Tr) %>% 
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
  ggtitle("just data - final rel abun 4-6")
  # ylim(-0.4,0.7) +
  # facet_grid(.~C) 




# C. RELATIVE OVERALL DENSITY ------------------------------------------------------

ARD_3_relabun <- read_csv("data/standardize to control calculations/ARD_3_relabun.csv")

ARD3ra <- ARD_3_relabun %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                        labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  mutate(plot = as.factor(plot)) %>% 
  rename(Tr = treatment) %>% 
  rename(C = complexity)

ARD3ra1 <- ARD3ra %>% 
  mutate(rabun1 = rabun + 6)

describeBy(ARD3ra, group=(ARD3ra$Tr))
# 0: 0.77 , sd =  3.24
# 30:  0.11, sd =   2.93
# 50: -0.12 , sd =  2.71
# 70: 0.55  , sd= 3.65
# 100:  0.99  , sd = 4.24


# option 1) glmm gamma ----------------------------------------------------



M0glmt <- glm(rabun1~Tr, family = Gamma(link = "log"), data = ARD3ra1)
M1glmmTMBt <- glmmTMB(rabun1~Tr + (1|visit), family=Gamma(link="log"), data = ARD3ra1) #just visit as re
M2glmmTMBt <- glmmTMB(rabun1~Tr + (1|plot), family=Gamma(link="log"), data = ARD3ra1) #just plot sa re
M3glmmTMBt <- glmmTMB(rabun1~Tr + (1|plot) + (1|visit), family=Gamma(link="log"), data = ARD3ra1) #both visit and plot as re

anova(M1glmmTMBt, M2glmmTMBt, M3glmmTMBt) #the one with both visit and plot as re is the best M3glmmTMBt

summary(M3glmmTMBt) #sig: 0, 30, 50, 70

car::Anova(M3glmmTMBt)

# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: rabun1
# Chisq Df Pr(>Chisq)    
# Tr 27.818  4  1.358e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#dispersion test
testDispersion(M3glmmTMBt)
# p p-value = 0.232, not overdispersed

simoutglmm3t <- simulateResiduals(fittedModel = M3glmmTMBt, plot = T)
residuals(simoutglmm3t)
plot(simoutglmm3t) # not working?

plotResiduals(simoutglmm3t, ARD3ra1$Tr)
hist(simoutglmm3t) # looks just fine

#goodness of fit tests
testResiduals(simoutglmm3t)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # sig, deviation present, p-value = 6.026e-06
# 2) testOutliers: if there are more simulation outliers than expected        # non sig outliers, p-value = 0.18
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig, dispersion p-value = 0.232


# visualize ---------------------------------------------------------------
ARDra_sumt <- ARD3ra %>% 
  group_by(Tr) %>% 
  summarize(rabun.mean = mean(rabun), rabun.sd = sd(rabun)) %>%
  mutate(rabun.se = rabun.sd/sqrt(1280))

ggplot() +
  geom_col(data = ARDra_sumt,
           aes(x = Tr,
               y = rabun.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_errorbar(data =ARDra_sumt,
                aes(x = Tr,
                    ymin = rabun.mean+rabun.se,
                    ymax = rabun.mean-rabun.se),
                width = 0.3) +
  ggtitle("just data - ARD 3 rabun whole study")
  # ylim(-0.4,0.7) +
  # facet_grid(.~C)


# D. RELATIVE RICHNESS -------------------------------------------------------------

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

describeBy(ARD3rr, group=(ARD3rr$Tr))
# 0: 0.54 , sd =  1.23
# 30: 0.30 , sd =  1.20
# 50:  0.28 , sd =  1.19
# 70:  0.51 , sd =  1.41
#100:  0.56 , sd =  1.39

# option 1) glmm -----------------------------------------------------------

# choose random effect structure________________________________________________________________________________________________

M0glmmTMBrt <- glmmTMB(rrich1~Tr, family=Gamma(link="log"), data = ARD3rr1)
M1glmmTMBrt <- glmmTMB(rrich1~Tr + (1|visit), family=Gamma(link="log"), data = ARD3rr1) #just visit as re
M2glmmTMBrt <- glmmTMB(rrich1~Tr + (1|plot), family=Gamma(link="log"), data = ARD3rr1) #just plot sa re
M3glmmTMBrt <- glmmTMB(rrich1~Tr + (1|plot) + (1|visit), family=Gamma(link="log"), data = ARD3rr1) #both visit and plot as re

AIC(M0glmmTMBrt, M1glmmTMBrt, M2glmmTMBrt, M3glmmTMBrt) #M3 with plot and visit is the best
anova(M0glmmTMBrt, M1glmmTMBrt, M2glmmTMBrt, M3glmmTMBrt)

car::Anova(M3glmmTMBrt)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: rrich1
# Chisq Df Pr(>Chisq)  
# Tr 11.586  4    0.02071 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(M3glmmTMBrt) #sig: 0, 50, 70

# model assessment with dharma__________________________________________________________________________________________________

#dispersion test
testDispersion(M3glmmTMBrt)
# p > 0.05, not oversdispersed, p-value = 0.784

simouttmb3rt <- simulateResiduals(fittedModel =M3glmmTMBrt, plot = T) # looks pretty good, just outlier test is sig

residuals(simouttmb3rt)
plot(simouttmb3rt) # lots of issues in qq: lack onf uniformity, outliers and dispersion
# residuals plots plots resudlas against predicted value. simulation outliers have red stars (don't kow how much they deviate from model expectations)

plotResiduals(simouttmb3rt, ARD3rr1$Tr) #looks good
hist(simouttmb3rt) # I think just ok,  

#goodness of fit tests
testResiduals(simouttmb3rt)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # non sig, p-value = 0.6721
# 2) testOutliers: if there are more simulation outliers than expected        # non sig, p-value = 0.5
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig,p-value = 0.784
testUniformity(simouttmb3r) #KS test, p < 0.05, so not uniform


# visualise ---------------------------------------------------------------


# visualize __________________________________________________________________________________________________________________

predM3glmmrt <- ggpredict(M3glmmTMBrt, terms = c("Tr")) %>% 
  rename(Tr = x) %>% 
  mutate(pred2 = predicted - 2)

ggplot() +                              # looks pretty good actually
  geom_col(data = ARDrr_sumt,
           aes(x = Tr,
               y = rrich.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_col(data = predM3glmmrt ,
           aes(x = Tr,
               y = pred2,
               group = Tr),
           colour = "black",
           fill = "transparent",
           size = 1.2) +
  geom_errorbar(data = predM3glmmrt ,
                aes(x = Tr,
                    ymin = pred2+std.error,
                    ymax = pred2-std.error),
                width = 0.3) +
  ggtitle("predicted richness (outline) over data (colour)")
  # ylim(-0.4,0.7) +
  # facet_grid(.~C) 

# just data plotted w s.e.:
ARDrr_sumt <- ARD3rr %>% 
  group_by(Tr) %>% 
  summarize(rrich.mean = mean(rrich), rrich.sd = sd(rrich)) %>%
  mutate(rrich.se = rrich.sd/sqrt(1280))

ggplot() +
  geom_col(data = ARDrr_sumt,
           aes(x = Tr,
               y = rrich.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.5) +
  geom_errorbar(data =ARDrr_sumt,
                aes(x = Tr,
                    ymin = rrich.mean+rrich.sd,
                    ymax = rrich.mean-rrich.sd),
                width = 0.3) +
  ggtitle("just data - ARD 3 rrich whole study")
  # ylim(-0.4,0.7) +
  # facet_grid(.~C)





# 3B) COMPOSITION * BACKGROUND COMPLEXITY ----------------------------------------------------------
 ______________________________________________________________
________________________________________________________

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

simoutglmm.gam1 <- simulateResiduals(fittedModel = glmm.gam1, plot = T)#plots scaled resid
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

M3glmmTMBemm <- emmeans(M3glmmTMB, pairwise ~ Tr | C)
pairs(M3glmmTMBemm) 

# C = Low:
#   contrast    estimate     SE   df t.ratio p.value
# 0% - 30%    0.053524 0.0532 1267  1.006  0.8528 
# 0% - 50%   -0.022059 0.0532 1267 -0.414  0.9938 
# 0% - 70%   -0.060540 0.0532 1267 -1.138  0.7864 
# 0% - 100%  -0.722202 0.2128 1267 -3.394  0.0064 **
# 30% - 50%  -0.075583 0.0533 1267 -1.419  0.6153 
# 30% - 70%  -0.114064 0.0532 1267 -2.144  0.2023 
# 30% - 100% -0.775726 0.2128 1267 -3.645  0.0026 **
# 50% - 70%  -0.038481 0.0533 1267 -0.723  0.9513 
# 50% - 100% -0.700143 0.2128 1267 -3.290  0.0091 **
# 70% - 100% -0.661662 0.2128 1267 -3.110  0.0164 *
# 
# C = High:
#   contrast    estimate     SE   df t.ratio p.value
# 0% - 30%    0.000792 0.1189 1267  0.007  1.0000 
# 0% - 50%   -0.174276 0.1190 1267 -1.465  0.5857 
# 0% - 70%   -0.305219 0.1190 1267 -2.565  0.0776 
# 0% - 100%   0.527251 0.4757 1267  1.108  0.8022 
# 30% - 50%  -0.175067 0.1190 1267 -1.472  0.5812 
# 30% - 70%  -0.306011 0.1189 1267 -2.573  0.0760 
# 30% - 100%  0.526459 0.4758 1267  1.106  0.8032 
# 50% - 70%  -0.130944 0.1189 1267 -1.101  0.8060 
# 50% - 100%  0.701527 0.4758 1267  1.475  0.5793 
# 70% - 100%  0.832470 0.4759 1267  1.749  0.4040 

#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

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
           alpha = 0.9) +
  geom_errorbar(data =ARDrao_sum,
                aes(x = Tr,
                    ymin = rabun.mean+rabun.se,
                    ymax = rabun.mean-rabun.se),
                width = 0.3) +
  ggtitle("just data no outlier- ARD 3 rabun whole study") +
  ggtitle("mean recruit density (0-3cm)") +
  scale_fill_manual(values = c("#FFB000", "#FE6100", "#DC267F", "#785EF0", "#648FFF")) +
  # scale_fill_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(percent~living~coral),
       y = expression(relative~fish~density~(fish~m^{2}))) +
  facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")


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

# option 4) glmm - density with control as a treatment --------------------

ARD_3 <- read_csv("data/filter 0 values/ARD_3.csv") %>% 
  dplyr::mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  dplyr::mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  dplyr::mutate(visit = factor(days_since_outplanting, levels = c('1', '2','3','5','7','9','11','13','18','23','26','30','33','37','43','48'),
                               labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))) %>%
  dplyr::mutate(density = as.numeric(abundance)) %>% 
  dplyr::mutate(plot = as.factor(plot)) %>% 
  dplyr::rename(Tr = treatment) %>% 
  dplyr::rename(C = complexity)

range(ARD_3$density)
hist(ARD_3$density) #prob neg bin2

describeBy(ARD_3, group=list(ARD_3$Tr, ARD_3$C))
# L, C: 1.48   2.14
# L, 0: 

glm.3 <- glm.nb(density~Tr*C, data = ARD_3)
glmm.3 <- glmmTMB(density~Tr*C + (1|plot), family = nbinom2(), data = ARD_3)
glmm.3.1 <- glmmTMB(density~Tr*C + (1|visit), family = nbinom2(), data = ARD_3)
glmm.3.2 <- glmmTMB(density~Tr*C + (1|visit) + (1|plot), family = nbinom2(), data = ARD_3)

AIC(glm.3, glmm.3, glmm.3.1,glmm.3.2) # they all have same output, so I'll use the one with both...?
car::Anova(glmm.3.2)
summary(glmm.3.2)

# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: density
# Chisq Df Pr(>Chisq)    
# Tr   30.7073  5  1.070e-05 ***
#   C     1.5154  1     0.2183    
# Tr:C 37.1665  5  5.546e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#evaluate model with dharma_____________________________________________________________________
#dispersion test
testDispersion(glmm.3.2)
# fine,p-value = 0.456

simoutglmm.3.2 <- simulateResiduals(fittedModel = glmm.3.2, plot = T)# this looks good!
residuals(simoutglmm.3.2)
plot(simoutglmm.3.2) 

plotResiduals(simoutglmm.3.2, ARD_3$Tr)
plotResiduals(simoutglmm.3.2, ARD_3$C) 
hist(simoutglmm.3.2) # looks just fine

#goodness of fit tests
testResiduals(simoutglmm.3.2)   ## these are displayed on the plots
# calculates 3 tests: 
# 1) testUniformity: if overall distribution conforms to expectations         # non sig,
# 2) testOutliers: if there are more simulation outliers than expected        # non sig 
# 3) testDispersion: if sumulated dispersion is equal to observed dispersion  # non sig,



ARD3_sum <- ARD_3 %>% 
  group_by(Tr,C) %>% 
  summarize(dens.mean = mean(density), dens.sd = sd(density)) %>%
  mutate(dens.se = dens.sd/sqrt(1536))

ggplot() +
  geom_col(data = ARD3_sum,
           aes(x = Tr,
               y = dens.mean,
               group = Tr,
               fill = Tr),
           alpha = 0.9) +
  geom_errorbar(data =ARD3_sum,
                aes(x = Tr,
                    ymin = dens.mean+dens.se,
                    ymax = dens.mean-dens.se),
                width = 0.3) +
  ggtitle("mean recruit density (0-3cm)") +
  # scale_fill_manual(values = c("cyan4","coral3")) +
  # scale_fill_manual(values = pal) +
  scale_fill_manual(values = c("grey", "#FFB000", "#FE6100", "#DC267F", "#785EF0", "#648FFF")) +
  # scale_fill_manual(values = c("grey","firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(percent~living~coral),
       y = expression(fish~density~(fish~m^{2}))) +
  facet_grid(.~C) +
  theme_classic() +
  theme(legend.position = "none")

# ______________________PAIRWISE COMPARISON & EFFECT SIZE __________________________________

# going to try with emmeans https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html
library(emmeans)

glmm.3.2emm <- emmeans(glmm.3.2, "Tr") #ok so this is doing pairwise comparisons of just Tr
pairs(glmm.3.2emm)
pwpm(glmm.3.2emm) #displays as matrix, personally i find this harder to see, p values in upper triangle

glmm.3.2emm2 <- emmeans(glmm.3.2, "C") #ok so this is doing pairwise comparisons of just C
pairs(glmm.3.2emm2)

#trying to include interaciton term:

glmm.3.2emm3 <- emmeans(glmm.3.2, pairwise ~ Tr | C)
pairs(glmm.3.2emm3) 

# I guess I care more how they compare to control though hey?

# C = Low:
#   contrast       estimate    SE   df t.ratio p.value
# control - 0%    -0.6753 0.150 1521 -4.515  0.0001 ***
# control - 30%   -0.4597 0.151 1521 -3.035  0.0294 *
# control - 50%   -0.4135 0.152 1521 -2.715  0.0729 
# control - 70%   -0.7687 0.149 1521 -5.158  <.0001 ***
# control - 100%  -0.9669 0.148 1521 -6.544  <.0001 ***
# 0% - 30%         0.2156 0.142 1521  1.514  0.6556 
# 0% - 50%         0.2618 0.144 1521  1.823  0.4509 
# 0% - 70%        -0.0934 0.140 1521 -0.668  0.9854 
# 0% - 100%       -0.2916 0.138 1521 -2.105  0.2849 
# 30% - 50%        0.0461 0.145 1521  0.317  0.9996 
# 30% - 70%       -0.3091 0.142 1521 -2.178  0.2487 
# 30% - 100%      -0.5072 0.140 1521 -3.613  0.0042 **
# 50% - 70%       -0.3552 0.143 1521 -2.492  0.1271 
# 50% - 100%      -0.5533 0.141 1521 -3.927  0.0013 **
# 70% - 100%      -0.1981 0.137 1521 -1.446  0.6988 
# 
# C = High:
#   contrast       estimate    SE   df t.ratio p.value
# control - 0%    -0.0262 0.141 1521 -0.186  1.0000 
# control - 30%    0.3199 0.145 1521  2.203  0.2368 
# control - 50%    0.4554 0.147 1521  3.090  0.0248 *
# control - 70%    0.2621 0.145 1521  1.810  0.4593 
# control - 100%   0.1604 0.143 1521  1.119  0.8735 
# 0% - 30%         0.3460 0.145 1521  2.394  0.1589 
# 0% - 50%         0.4815 0.147 1521  3.275  0.0138 *
# 0% - 70%         0.2883 0.144 1521  2.001  0.3422 
# 0% - 100%        0.1866 0.143 1521  1.305  0.7822 
# 30% - 50%        0.1355 0.151 1521  0.899  0.9468 
# 30% - 70%       -0.0578 0.148 1521 -0.391  0.9988 
# 30% - 100%      -0.1594 0.147 1521 -1.086  0.8870 
# 50% - 70%       -0.1933 0.150 1521 -1.286  0.7926 
# 50% - 100%      -0.2949 0.149 1521 -1.978  0.3557 
# 70% - 100%      -0.1017 0.146 1521 -0.694  0.9826 

# Results are given on the log (not the response) scale.
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 # I added this


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
  geom_errorbar(data =ARDrr_sum,
                aes(x = Tr,
                    ymin = rrich.mean+rrich.sd,
                    ymax = rrich.mean-rrich.sd),
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
                    ymin = rrich.mean+rrich.sd,
                    ymax = rrich.mean-rrich.sd),
                width = 0.3) +
  ggtitle("just data - ARD 3 rrich whole study") +
  # ylim(-0.4,0.7) +
  facet_grid(.~C)

ggplot()+
  geom_boxplot(data = ARD3rr,
               aes(x = Tr,
                   y = rrich,
                   group = Tr,
                   fill = Tr),
               alpha = 0.5) +
  facet_grid(.~C)

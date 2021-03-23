# Data prep for analysis


# packages and palettes --------------------------------------------------


library(readr)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(viridis)
library(fishualize)
library(PNWColors)
library(plyr)
library(vegan)
library(openxlsx)

`%notin%` = negate(`%in%`)
write.csv(name, "name.csv", row.names = FALSE)
# highlight and click " to put a word/phrase 


# calculate recruitment rate ----------------------------------------------

#1 ) import ARD_3 (has days since outplanting as a col) and 0 values 
ARD_3 <- read_csv("data/filter 0 values/ARD3/ARD_3.csv") %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(abundance = as.numeric(presence))

unique(ARD_3$days_since_outplanting)
unique(ARD_3$treatment)

ARD_dr <-  ARD_3 %>% 
  select(days_since_outplanting, plot_grid,  density) %>% 
  distinct(days_since_outplanting, plot_grid, density) 

#2) export to use pivot table to input the 0 values
write.xlsx(ARD_dr,"ARD_dr.xlsx")
#export to make a pivot table --> make sure all blanks are 0
# day across the top, plot grid on the left, density values inside
# change Row Labels to plot_grid

#3 ) import and convert to long format
ARD_d_wide <- read_csv("data/rate calculations/ARD_d_wide.csv")
View(ARD_d_wide)

names(ARD_d_wide) #to get each date

ARD_d_long <- ARD_d_wide %>% 
  pivot_longer(!plot_grid, names_to = "days_since_outplanting", values_to = "density")


#4) Export long format to make column called delta dentisy 
# = density at time x - density at time x-days since last visit

write.xlsx(ARD_d_long, "ARD_d_long.xlsx")
#change Row Labels to plot_grid
# sort by plot_grid, then days_since_out_planting
# formula: =(C3-C2)/(B3-B2)
# make sure the first of every plot_grid is 0, used formula =ABS(0)

#5) Import the new sheet with daily rate of recruitment data (rate)
ARD_rate <- read_csv("data/rate calculations/ARD_rate.csv")

#join ARD_d_convert to ARD_lookup to get treatment and complexity information 
# (ARD_lookup has info on which treatment/complexity pairs with each plot_grid)
ARD_lookup <- read_csv("data/ARD_lookup.csv")

ARD_3_rate <- full_join(ARD_rate, ARD_lookup) %>% 
  mutate(days_since_outplanting = as.numeric(days_since_outplanting))

write.csv(ARD_3_rate,"ARD_3_rate.csv")



# calculate effect size (standardize to control) -> 1) for rate --------------------------

ARD_3_rate <- read_csv("data/rate calculations/ARD_3_rate.csv")

#1) 1 df with everything but control
ARD_3_rate_no_control <- ARD_3_rate %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>% 
  mutate(rate.se = rate.sd/sqrt(1280)) #n is for everything minus control clusters (plot_grid)

#2) add col called days_comp
ARD_3_rate_no_control$days_comp <- paste(ARD_3_rate_no_control$days_since_outplanting, "-", ARD_3_rate_no_control$complexity)

#3) 1 df with just control
ARD_3_rate_control <- ARD_3_rate %>% 
  filter(treatment == "control") %>%  
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(c.rate.mean = mean(rate), c.rate.sd = sd(rate)) %>% 
  mutate(c.rate.se = c.rate.sd/sqrt(256))

#4) add col days_comp
ARD_3_rate_control$days_comp <- paste(ARD_3_rate_control$days_since_outplanting, "-", ARD_3_rate_control$complexity)

#5) make a lookup that has just the mean rate for a certain day and complexity
ARD_3_rate_control_lookup <- ARD_3_rate_control %>% 
  select(c.rate.mean, days_comp)

# it automatically adds treatment and days_since_outplanting so take those out
ARD_3_rate_control_lookup$treatment <- NULL
ARD_3_rate_control_lookup$days_since_outplanting <- NULL
head(ARD_3_rate_control_lookup)

ARD_3_rate_joined <- full_join(ARD_3_rate_no_control, ARD_3_rate_control_lookup)

ARD_3_rate_effect <- ARD_3_rate_joined %>% 
  mutate(effect.size = rate.mean - c.rate.mean)



write.csv(ARD_3_rate_effect, "ARD_3_rate_effect.csv") 



# calculate effect size (standardize to control) -> 1) for rate *include plot, USE THIS --------------------------

# re-calcuate but include plot in this df

ARD_3_rate <- read_csv("data/rate calculations/ARD_3_rate.csv") 

#separate plot_grid into 2 cols:

ARD_3_rate1 <- ARD_3_rate %>% 
  mutate(plotgrid1 = plot_grid) %>% 
  separate(plotgrid1, c("plot", "grid")) 

#1) 1 df with everything but control
ARD_3_rate_no_control1 <- ARD_3_rate1 %>% 
  filter(treatment != "control")
# summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>% 
#   mutate(rate.se = rate.sd/sqrt(1280)) #n is for everything minus control clusters (plot_grid)

#2) add col called days_comp
ARD_3_rate_no_control1$days_comp <- paste(ARD_3_rate_no_control1$days_since_outplanting, "-", ARD_3_rate_no_control1$complexity)

#5) make a lookup that has just the mean rate for a certain day and complexity
ARD_3_rate_control_lookup1 <- ARD_3_rate_control1 %>% 
  group_by(days_comp) %>% 
  summarize(c.rate.mean = mean(rate))

ARD_3_rate_joined1 <- full_join(ARD_3_rate_no_control1, ARD_3_rate_control_lookup1)

ard_h28 <- ARD_3_rate_joined1 %>% 
  filter(plot_grid == "HS - 8") #want to check rate for this plot on day 3, seems unusually high compared to others

ARD_3_rate_effect1 <- ARD_3_rate_joined1 %>% 
  mutate(effect.size = rate - c.rate.mean)

#make a col called visit which is a specific cluster for a day (in case you want to use this in modeling?)
ARD_3_rate_effect1$visit <- paste(ARD_3_rate_effect1$plot_grid, "-", ARD_3_rate_effect1$days_since_outplanting)

write.csv(ARD_3_rate_effect1, "ARD_3_rate_effect1.csv") 

A <- ARD_3_rate_effect1 %>% 
  distinct (visit, .keep_all = T) #yup there's 1280 0bservations, checks out

# visualize: 

ARD_3_rate_effect1 <- ARD_3_rate_effect1 %>% 
  mutate(treatment = factor(treatment, levels = c( "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ARD_3_rate_effect1_sum <- ARD_3_rate_effect1 %>% 
  group_by(treatment, complexity, days_since_outplanting ) %>% 
  summarize(r.effect.mean = mean(effect.size), r.effect.sd = sd(effect.size)) %>%
  mutate(r.effect.se = r.effect.sd/sqrt(1280))

  ggplot(ARD_3_rate_effect1_sum) +
  geom_point(aes(x = days_since_outplanting,
                 y = r.effect.mean)) +
  # facet_grid(complexity~treatment) +
  facet_grid(treatment~complexity)
  
# plot with treatment as continuous: (no outlier)
  
ARD_3_rate_effect1.1 <-  ARD_3_rate_effect1 %>% 
  mutate(treatment1 = as.numeric(treatment)) %>% 
  filter(visit != "HS - 8 - 3")

ARD_3_rate_effect1.2 <- ARD_3_rate_effect1.1 %>% 
  group_by(treatment1, complexity) %>% 
  summarize(r.effect.mean = mean(effect.size), r.effect.sd = sd(effect.size)) %>%
  mutate(r.effect.se = r.effect.sd/sqrt(1280))

ggplot(data = ARD_3_rate_effect1.1) +
  geom_jitter(data = ARD_3_rate_effect1.1,
             aes(x = treatment1,
                 y = effect.size,
             group = complexity,
             colour = complexity),
             alpha = 0.2) +
  geom_point(data = ARD_3_rate_effect1.2,
             aes(x = treatment1,
                 y = r.effect.mean, 
                 colour = complexity),
             size = 3) +
  # geom_line(data = ARD_3_rate_effect1.2,
  #           aes(x = treatment1,
  #           y = r.effect.mean,
  #           colour = complexity)) +
  geom_smooth(data = ARD_3_rate_effect1.2,
              aes(x = treatment1,
                  y = r.effect.mean,
                  colour = complexity),
              method = "lm") +
  ylim(-0.3,0.3) +
  theme_classic()






#plot with just treatment
  ARD_3_rate_effect1_sum1 <- ARD_3_rate_effect1 %>% 
  # na.omit() %>%
  group_by(treatment) %>% 
    summarize(r.effect.mean = mean(effect.size), r.effect.sd = sd(effect.size)) %>%
    mutate(r.effect.se = r.effect.sd/sqrt(1280))

ggplot(data = ARD_3_rate_effect1_sum1) +
  geom_col(aes(x = treatment,
               y = r.effect.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = r.effect.mean - r.effect.se,
                    ymax = r.effect.mean + r.effect.se,
                    group = treatment),
                width = 0.2) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Structure~added),
       y = expression(Recruitment~rate~(fish~m^{2}~day^{1}))) +
  ggtitle("Relative Mean Daily Recruitment Rate 3cm")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#facet with complexity
ARD_3_rate_effect1_sum2 <- ARD_3_rate_effect1 %>% 
  na.omit() %>%
  group_by(treatment, complexity) %>% 
  summarize(r.effect.mean = mean(effect.size), r.effect.sd = sd(effect.size)) %>%
  mutate(r.effect.se = r.effect.sd/sqrt(1280))

ggplot(data = ARD_3_rate_effect1_sum2) +
  facet_grid(.~complexity) +
  geom_col(aes(x = treatment,
               y = r.effect.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = r.effect.mean - r.effect.se,
                    ymax = r.effect.mean + r.effect.se,
                    group = treatment),
                width = 0.2) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  labs(x = expression(Percent~living~coral),
       y = expression(Relative~recruitment~rate~(fish~m^{2}~day^{1}))) +
  ggtitle("Relative Mean Daily Recruitment Rate 3cm") +
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#facet with complexity - only first 30 visits ## not that different ##
ARD_3_rate_effect_sum3 <- ARD_3_rate_effect %>% 
  na.omit() %>%
  filter(days_since_outplanting %in% c(1, 2, 3, 5, 7, 9, 11, 13, 18, 23, 26, 30, 33)) %>% 
  group_by(treatment, complexity) %>% 
  summarize(mean.rate.effect = mean(effect.size), sd.rate.effect = sd(effect.size)) %>% 
  mutate(se.rate.effect = sd.rate.effect/sqrt(130))
  
unique(ARD_3_rate_effect$days_since_outplanting)

ggplot(data = ARD_3_rate_effect_sum3) +
  facet_grid(.~complexity) +
  geom_col(aes(x = treatment,
               y = mean.rate.effect,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = mean.rate.effect - se.rate.effect,
                    ymax = mean.rate.effect + se.rate.effect,
                    group = treatment),
                width = 0.2) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  labs(x = expression(Percent~living~coral),
       y = expression(Relative~recruitment~rate~(fish~m^{2}~day^{1}))) +
  ggtitle("Relative Mean Daily Recruitment Rate 3cm - first 30") +
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )



# calculate effect size (standardize to control) -> 2) for density ARD 3--------


ARD_3 <- read_csv("data/filter 0 values/ARD3/ARD_3.csv") %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_3_density_no_control <- ARD_3 %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1236)) 

ARD_3_density_no_control$days_comp <- paste(ARD_3_density_no_control$days_since_outplanting, "-", ARD_3_density_no_control$complexity)

ARD_3_density_control <- ARD_3 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(236))

ARD_3_density_control$days_comp <- paste(ARD_3_density_control$days_since_outplanting, "-", ARD_3_density_control$complexity)

#5) make a lookup that has just the mean rate for a certain day and complexity
ARD_3_density_control_lookup <- ARD_3_density_control %>% 
  select(density.control, days_comp)

# it automatically adds treatment and days_since_outplanting so take those out
ARD_3_density_control_lookup$treatment <- NULL
ARD_3_density_control_lookup$days_since_outplanting <- NULL
head(ARD_3_density_control_lookup)

ARD_3_density_joined <- full_join(ARD_3_density_no_control, ARD_3_density_control_lookup)

ARD_3_density_effect <- ARD_3_density_joined %>% 
  mutate(effect.size = density.mean - density.control)

write.csv(ARD_3_density_effect, "ARD_3_density_effect.csv") 

# visualize:

# all visits:

library(ggplot2)

ggplot(ARD_3_density_effect) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size)) +
  # facet_grid(complexity~treatment) +
  facet_grid(treatment~complexity)


ARD_3_dens_sum <- ARD_3_density_effect %>% 
  mutate(treatment = factor(treatment, levels = c( "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  # na.omit() %>%
  group_by(treatment) %>% 
  summarize(mean.effect = mean(effect.size), sd.effect = sd(effect.size)) %>% 
  mutate(se.effect = sd.effect/sqrt(160))

ggplot(data = ARD_3_dens_sum) +
  # facet_grid(.~complexity) +
  geom_col(aes(x = treatment,
               y = mean.effect,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = mean.effect - se.effect,
                    ymax = mean.effect + se.effect,
                    group = treatment),
                width = 0.2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Percent~living~coral),
       y = expression(Relative~Fish~Density~(fish~m^{2}))) +
  ggtitle("Mean Density - whole study period)")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )


# faceted by complexity:

ARD_3_dens_sum <- ARD_3_density_effect %>% 
  mutate(treatment = factor(treatment, levels = c( "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  # na.omit() %>%
  group_by(treatment, complexity) %>% 
  summarize(mean.effect = mean(effect.size), sd.effect = sd(effect.size)) %>% 
  mutate(se.effect = sd.effect/sqrt(160))

ggplot(data = ARD_3_dens_sum) +
  facet_grid(.~complexity) +
  geom_col(aes(x = treatment,
               y = mean.effect,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = mean.effect - se.effect,
                    ymax = mean.effect + se.effect,
                    group = treatment),
                width = 0.2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Percent~living~coral),
       y = expression(Relative~Fish~Density~(fish~m^{2}))) +
  ggtitle("Mean Density - whole study period)")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

# last 3 visits:

ARD_3_effect_final_dens <- ARD_3_density_effect %>%
  filter(days_since_outplanting %in% c(33,37,43)) %>% 
  mutate(treatment = factor(treatment, levels = c( "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

unique(ARD_3_density_effect$days_since_outplanting)

hist(ARD_3_effect_final_dens$effect.size)

ARD_3_effect_final_dens_sum <- ARD_3_effect_final_dens %>% 
  # na.omit() %>%
  group_by(treatment) %>% 
  summarize(mean.effect = mean(effect.size), sd.effect = sd(effect.size)) %>% 
  mutate(se.effect = sd.effect/sqrt(30))

ggplot(data = ARD_3_effect_final_dens_sum) +
  geom_col(aes(x = treatment,
               y = mean.effect,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = mean.effect - se.effect,
                    ymax = mean.effect + se.effect,
                    group = treatment),
                width = 0.2) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Percent~living~coral),
       y = expression(Relative~Fish~Density~(fish~m^{2}))) +
  ggtitle("Mean Final Density - last 3 visits (3cm)")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#facet by complexity
ARD_3_effect_final_dens_sum2 <- ARD_3_effect_final_dens %>% 
  # na.omit() %>%
  group_by(treatment, complexity) %>% 
  summarize(mean.effect = mean(effect.size), sd.effect = sd(effect.size)) %>% 
  mutate(se.effect = sd.effect/sqrt(30))

ggplot(data = ARD_3_effect_final_dens_sum2) +
  facet_grid(.~complexity) +
  geom_col(aes(x = treatment,
               y = mean.effect,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = mean.effect - se.effect,
                    ymax = mean.effect + se.effect,
                    group = treatment),
                width = 0.2) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Percent~living~coral),
       y = expression(Relative~Fish~Density~(fish~m^{2}))) +
  ggtitle("Mean Final Density - last 3 visits (3cm)")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )


# calculate effect size ARD 4 to 6 ----------------------------------------

ARD_4to6 <- read_csv("data/filter 0 values/ARD4to6/ARD_4to6.csv") %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_4to6_density_no_control <- ARD_4to6 %>% 
  filter(treatment != "control") %>%  
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1236)) 

ARD_4to6_density_no_control$days_comp <- paste(ARD_4to6_density_no_control$days_since_outplanting, "-", ARD_4to6_density_no_control$complexity)

ARD_4to6_density_control <- ARD_4to6 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(236))

ARD_4to6_density_control$days_comp <- paste(ARD_4to6_density_control$days_since_outplanting, "-", ARD_4to6_density_control$complexity)
head(ARD_4to6_density_control)

#5) make a lookup that has just the mean rate for a certain day and complexity
ARD_4to6_density_control_lookup <- ARD_4to6_density_control %>% 
  dplyr::select(density.control, days_comp)

# it automatically adds treatment and days_since_outplanting so take those out
ARD_4to6_density_control_lookup$treatment <- NULL
ARD_4to6_density_control_lookup$days_since_outplanting <- NULL
head(ARD_3_density_control_lookup)

ARD_4to6_density_joined <- full_join(ARD_4to6_density_no_control, ARD_4to6_density_control_lookup)

ARD_4to6_density_effect <- ARD_4to6_density_joined %>% 
  mutate(effect.size = density.mean - density.control)

write.csv(ARD_4to6_density_effect, "ARD_4to6_density_effect.csv") 

unique(ARD_4to6_density_effect$density.mean)

# visualize:

# all visits:

ggplot(ARD_4to6_density_effect) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size)) +
  # facet_grid(complexity~treatment) +
  facet_grid(treatment~complexity)


ARD_4to6_dens_sum <- ARD_4to6_density_effect %>% 
  mutate(treatment = factor(treatment, levels = c( "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  # na.omit() %>%
  group_by(treatment) %>% 
  summarize(mean.effect = mean(effect.size), sd.effect = sd(effect.size)) %>% 
  mutate(se.effect = sd.effect/sqrt(160))

ggplot(data = ARD_4to6_dens_sum) +
  # facet_grid(.~complexity) +
  geom_col(aes(x = treatment,
               y = mean.effect,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = mean.effect - se.effect,
                    ymax = mean.effect + se.effect,
                    group = treatment),
                width = 0.2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Percent~living~coral),
       y = expression(Relative~Fish~Density~(fish~m^{2}))) +
  ggtitle("Mean Density - whole study period)")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

library(tidyverse)
# faceted by complexity:

ARD_4to6_dens_sum <- ARD_4to6_density_effect %>% 
  mutate(treatment = factor(treatment, levels = c( "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  # na.omit() %>%
  group_by(treatment, complexity) %>% 
  summarize(mean.effect = mean(effect.size), sd.effect = sd(effect.size)) %>% 
  mutate(se.effect = sd.effect/sqrt(160))

ggplot(data = ARD_4to6_dens_sum) +
  facet_grid(.~complexity) +
  geom_col(aes(x = treatment,
               y = mean.effect,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = mean.effect - se.effect,
                    ymax = mean.effect + se.effect,
                    group = treatment),
                width = 0.2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Percent~living~coral),
       y = expression(Relative~Fish~Density~(fish~m^{2}))) +
  ggtitle("Mean Density - whole study period )")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

# last 3 visits:

ARD_4to6_effect_final_dens <- ARD_4to6_density_effect %>%
  filter(days_since_outplanting %in% c(33,37,43)) %>% 
  mutate(treatment = factor(treatment, levels = c( "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

unique(ARD_4to6_density_effect$days_since_outplanting)

hist(ARD_4to6_effect_final_dens$effect.size)
hist(ARD_3_effect_final_dens$effect.size)

ARD_4to6_effect_final_dens_sum <- ARD_4to6_effect_final_dens %>% 
  # na.omit() %>%
  group_by(treatment) %>% 
  summarize(mean.effect = mean(effect.size), sd.effect = sd(effect.size)) %>% 
  mutate(se.effect = sd.effect/sqrt(30))

ggplot(data = ARD_4to6_effect_final_dens_sum) +
  geom_col(aes(x = treatment,
               y = mean.effect,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = mean.effect - se.effect,
                    ymax = mean.effect + se.effect,
                    group = treatment),
                width = 0.2) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Percent~living~coral),
       y = expression(Relative~Fish~Density~(fish~m^{2}))) +
  ggtitle("Mean Final Density - last 3 visits (3cm)")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#facet by complexity
ARD_4to6_effect_final_dens_sum2 <- ARD_4to6_effect_final_dens %>% 
  # na.omit() %>%
  group_by(treatment, complexity) %>% 
  summarize(mean.effect = mean(effect.size), sd.effect = sd(effect.size)) %>% 
  mutate(se.effect = sd.effect/sqrt(30))

ggplot(data = ARD_4to6_effect_final_dens_sum2) +
  facet_grid(.~complexity) +
  geom_col(aes(x = treatment,
               y = mean.effect,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = mean.effect - se.effect,
                    ymax = mean.effect + se.effect,
                    group = treatment),
                width = 0.2) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Percent~living~coral),
       y = expression(Relative~Fish~Density~(fish~m^{2}))) +
  ggtitle("Mean Final Density - last 3 visits (4 to 6 cm)")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )


# effect size 4-6 with plot -----------------------------------------------

# re-calcuate but include plot in this df

ARD_4to6 <- read_csv("data/filter 0 values/ARD4to6/ARD_4to6.csv") %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

range(ARD_4to6$density)

#separate plot_grid into 2 cols:

#1) 1 df with everything but control
ARD_4to6_no_control1 <- ARD_4to6 %>% 
  filter(treatment != "control")
# summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>% 
#   mutate(rate.se = rate.sd/sqrt(1280)) #n is for everything minus control clusters (plot_grid)

#2) add col called days_comp
ARD_4to6_no_control1$days_comp <- paste(ARD_4to6_no_control1$days_since_outplanting, "-", ARD_4to6_no_control1$complexity)

ARD_4to6_control1 <- ARD_4to6 %>% 
  filter(treatment == "control")

ARD_4to6_control1$days_comp <- paste(ARD_4to6_control1$days_since_outplanting, "-", ARD_4to6_control1$complexity)

#5) make a lookup that has just the mean rate for a certain day and complexity
ARD_4to6_control_lookup1 <- ARD_4to6_control1 %>% 
  group_by(days_comp) %>% 
  summarize(mean.abun.c = mean(density))

ARD_4to6_joined1 <- full_join(ARD_4to6_no_control1, ARD_4to6_control_lookup1)

ARD_4to6_effect1 <- ARD_4to6_joined1 %>%
  mutate(rel.abun = density - mean.abun.c) %>% 
  dplyr::select(plot, treatment, complexity, rel.abun, days_since_outplanting)

# Why are there only 871 observations? shouldn't there be 1280 data points? 
# 20 clusters * 4 plots * 16 visits = 1280

#make a col called visit which is a specific cluster for a day (in case you want to use this in modeling?)
# ARD_3_rate_effect1$visit <- paste(ARD_3_rate_effect1$plot_grid, "-", ARD_3_rate_effect1$days_since_outplanting)

write.csv(ARD_3_rate_effect1, "ARD_3_rate_effect1.csv") 

#DR---Dose response summaries, curves and predictions

install.packages("writexl")
install.packages("rms")
install.packages("dosresmeta")
install.packages("mvmeta")
install.packages("tidyverse")
install.packages("rmeta")
install.packages("meta")
install.packages("metafor")

#import libraries

library(readxl)
library(writexl)
library(openxlsx)
library(dosresmeta)
library(rms)
library(tidyverse)
library(metafor)
library(meta)
library(rmeta)
library(mvmeta)

#import raw data (raw_data)
raw_data <- read_excel("C:/Users/Nomade/OneDrive/Bureau/JAMA/raw_data.xlsx")
View(raw_data)

#Rename useful variables (drma)- id, n.e, n.c, duration, int.type, design,  
###bp.measure, hta, ckd.yn, dose.e, dose.c, y.e, sd.e, y.c, sd.c 

####from - ID, N_experimental, N_control,Study_design, Intervention type,
###Study_duration (wks), (CKD)_1 or 0, HTA_Status, BP_measure_method,
###Follow up_sodium _IG, Follow up_sodium _CG
###Follow up_SBP_IG, Follow up_SBP_IG (SD), 
###Follow up_SBP_CG, Follow up_SBP_CG (SD)


drma <- raw_data %>%
  rename(id = ID,
         n.e = N_experimental,
         n.c = N_control,
         design = Study_design,
         int.type = `Intervention type`,
         duration = `Study_duration (wks)`,
         ckd.yn = `(CKD)_1 or 0`,
         hta = HTA_Status,
         bp.measure = BP_measure_method,
         dose.e = `Follow up_sodium _IG`,
         dose.c = `Follow up_sodium _CG`,
         y.e = 'Follow up_SBP_IG',
         sd.e = `Follow up_SBP_IG (SD)`,
         y.c = 'Follow up_SBP_CG',
         sd.c = `Follow up_SBP_CG (SD)`)

glimpse(drma)

#(drma) change mmol/day of sodium to grams with 1 decimal for dose.e and dose.c

drma <- drma %>%
  mutate(dose.e = round(dose.e * 0.023, 1),
         dose.c = round(dose.c * 0.023, 1))

#code in hta---Normotensive/Pre-hypertensive==0, Hypertensive==1, Mixed==2
drma <- drma %>%
  mutate(hta = recode(hta,
                      "Normotensive" = 0,
                      "Pre-hypertensive" = 0,
                      "Hypertensive" = 1,
                      "Mixed" = 2))



glimpse(drma)
###Create two tables for drm(all), drm.e, drm.c 
###### (id, n.e, dose.e, y.e, sd.e, hta, ckd.yn).....n.c, etc

drm.e <- drma %>%
  select(id, n.e, dose.e, y.e, sd.e, hta, ckd.yn)

glimpse(drm.e)

drm.c <- drma %>%
  select(id, n.c, dose.c, y.c, sd.c, hta, ckd.yn)


#####join(stack) tables by ID (id, n, dose, y, sd) assign as drm

drm <- bind_rows(
  drm.e %>% rename(n = n.e, dose = dose.e, y = y.e, sd = sd.e),
  drm.c %>% rename(n = n.c, dose = dose.c, y = y.c, sd = sd.c)
)

####sort drm by id number ascending
drm <- drm %>% arrange(id)

glimpse(drm)


#filter drm, normo(filter hta), hyper(filter hta), ckd (filter ckd.stage)

normo <- drm %>% filter(hta == 0)

hyper <- drm %>% filter(hta == 1)

ckd <- drm %>% filter(ckd.yn == 1)


###save dataframes

write_xlsx(drm, "drm.xlsx")
write_xlsx(normo, "normo.xlsx")
write_xlsx(hyper, "hyper.xlsx")
write_xlsx(ckd, "ckd.xlsx")







###All population(drm)

# Linear model
lin_drm <- dosresmeta(formula = y ~ dose, id = id, sd = sd, n = n, 
                      covariance = "md", data = drm, proc = "1stage") 

# Generate summary of the model
summary(lin_drm) 

# Graphical results
dosex_drm <- data.frame(dose = seq(0,6))  
predictions <- predict(lin_drm, dosex_drm, order = TRUE)
predictions

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "All populations",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 20) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/drm.png", 
       p, width = 8, height = 6)

# Display the plot
print(p)


###Quadratic

#quadratic model
quadr_drm <- dosresmeta(formula = y ~ dose + I(dose^2), id = id,
                        sd = sd, n = n, covariance = "md", 
                        data = drm, proc = "1stage")
summary(quadr_drm)


# Graphical results
predictions <- predict(quadr_drm, dosex_drm, order = TRUE)

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "All populations",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 20) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/dose_response_plot.png", 
       p, width = 8, height = 6)


# Display the plot
print(p)


###Restricted cubic spline model 
knots_drm <- quantile(drm$dose, c(.15, .5, .85)) 

# rcspline
spl_drm <- dosresmeta(formula = y ~ rcs(dose, knots_drm), id = id, sd = sd, 
                      n = n, covariance 
                      = "md", data = drm, proc = "1stage") 
summary(spl_drm) 

#waldtest
waldtest(b = coef(spl_drm), Sigma = vcov(spl_drm), Terms = 1:2) 

#Graphical results
dosex_drm <- data.frame(dose = seq(0,6, 0.1)) 
xref_drm <- 0 
predictions <- predict(spl_drm, dosex_drm, xref_drm, order = TRUE)
predictions

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$"rcs(dose, knots_drm)dose",
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "All populations ",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 20) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/dose_response_plot.png", 
       p, width = 8, height = 6)

# Display the plot
print(p)




########### Normotensive

# Linear model
lin_normo <- dosresmeta(formula = y ~ dose, id = id, sd = sd, n = n, 
                      covariance = "md", data = normo, proc = "1stage") 

# Generate summary of the model
summary(lin_normo) 

# Graphical results
dosex_normo <- data.frame(dose = seq(0,6))  
predictions <- predict(lin_normo, dosex_normo, order = TRUE)
predictions


# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "Normotensive",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 10) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/normoln.png", 
       p, width = 8, height = 6)

# Display the plot
print(p)




###Quadratic

#quadratic model
quadr_normo <- dosresmeta(formula = y ~ dose + I(dose^2), id = id,
                        sd = sd, n = n, covariance = "md", 
                        data = normo, proc = "1stage")
summary(quadr_normo)


# Graphical results
predictions <- predict(quadr_normo, dosex_normo, order = TRUE)

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "Normotensive",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 10) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/normoquadr.png", 
       p, width = 8, height = 6)


# Display the plot
print(p)


###Restricted cubic spline model 

knots_normo <- quantile(normo$dose, c(.15, .5, .85)) 
# rcspline
spl_normo <- dosresmeta(formula = y ~ rcs(dose, knots_normo), id = id, sd = sd, 
                      n = n, covariance 
                      = "md", data = normo, proc = "1stage") 
summary(spl_normo) 

#waldtest
waldtest(b = coef(spl_normo), Sigma = vcov(spl_normo), Terms = 1:2) 

#Graphical results
dosex_normo <- data.frame(dose = seq(0,6, 0.1)) 
xref_normo <- 0 
predictions <- predict(spl_normo, dosex_normo, xref_normo, order = TRUE)
predictions

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$"rcs(dose, knots_normo)dose",
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "Normotensive",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 10) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/normospl.png", 
       p, width = 8, height = 6)

# Display the plot
print(p)





############# Hypertensive

# Linear model
lin_hyper <- dosresmeta(formula = y ~ dose, id = id, sd = sd, n = n, 
                        covariance = "md", data = hyper, proc = "1stage") 

# Generate summary of the model
summary(lin_hyper) 

# Graphical results
dosex_hyper <- data.frame(dose = seq(0,6))  
predictions <- predict(lin_hyper, dosex_hyper, order = TRUE)
predictions


# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "Hypertensive",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 25) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/hyperln.png", 
       p, width = 8, height = 6)

# Display the plot
print(p)


###Quadratic

#quadratic model
quadr_hyper <- dosresmeta(formula = y ~ dose + I(dose^2), id = id,
                          sd = sd, n = n, covariance = "md", 
                          data = hyper, proc = "1stage")
summary(quadr_hyper)


# Graphical results
predictions <- predict(quadr_hyper, dosex_hyper, order = TRUE)

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "Hypertensive",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 25) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/hyperquadr.png", 
       p, width = 8, height = 6)


# Display the plot
print(p)


###Restricted cubic spline model 

knots_hyper <- quantile(hyper$dose, c(.15, .5, .85)) 
# rcspline
spl_hyper <- dosresmeta(formula = y ~ rcs(dose, knots_hyper), id = id, sd = sd, 
                        n = n, covariance 
                        = "md", data = hyper, proc = "1stage") 
summary(spl_hyper) 

#waldtest
waldtest(b = coef(spl_hyper), Sigma = vcov(spl_hyper), Terms = 1:2) 

#Graphical results
dosex_hyper <- data.frame(dose = seq(0,6)) 
xref_hyper <- 0 
predictions <- predict(spl_hyper, dosex_hyper, xref_hyper, order = TRUE)
predictions

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$"rcs(dose, knots_hyper)dose",
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "Hypertensive",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 25) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/hyperspl.png", 
       p, width = 8, height = 6)

# Display the plot
print(p)










############## CKD

# Linear model
lin_ckd <- dosresmeta(formula = y ~ dose, id = id, sd = sd, n = n, 
                        covariance = "md", data = ckd, proc = "1stage") 

# Generate summary of the model
summary(lin_ckd) 

# Graphical results
dosex_ckd <- data.frame(dose = seq(0,6))  
predictions <- predict(lin_ckd, dosex_ckd, order = TRUE)
predictions


# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "CKD population",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 40) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/ckdln.png", 
       p, width = 8, height = 6)

# Display the plot
print(p)


###Quadratic

#quadratic model
quadr_ckd <- dosresmeta(formula = y ~ dose + I(dose^2), id = id,
                          sd = sd, n = n, covariance = "md", 
                          data = ckd, proc = "1stage")
summary(quadr_ckd)


# Graphical results
predictions <- predict(quadr_ckd, dosex_ckd, order = TRUE)

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "CKD population",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 40) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/ckdquadr.png", 
       p, width = 8, height = 6)


# Display the plot
print(p)


###Restricted cubic spline model 

knots_ckd <- quantile(ckd$dose, c(.15, .5, .85)) 
# rcspline
spl_ckd <- dosresmeta(formula = y ~ rcs(dose, knots_ckd), id = id, sd = sd, 
                        n = n, covariance 
                        = "md", data = ckd, proc = "1stage") 
summary(spl_ckd) 

#waldtest
waldtest(b = coef(spl_ckd), Sigma = vcov(spl_ckd), Terms = 1:2) 

#Graphical results
dosex_ckd <- data.frame(dose = seq(0,6)) 
xref_ckd <- 0 
predictions <- predict(spl_ckd, dosex_ckd, xref_ckd, order = TRUE)
predictions

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$"rcs(dose, knots_ckd)dose",
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  labs(
    subtitle = "CKD population",
    x = "Δ Sodium Excretion (g/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 40) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/ckdspl.png", 
       p, width = 8, height = 6)

# Display the plot
print(p)




#Sensitivity analysis of the DRMA

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
library(mvtnorm)
library(DoseFinding)
library(aod)
library(Hmisc)

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

#code in duration >4 as short and </= 4 as long
drma <- drma %>%
  mutate(duration = ifelse(duration < 4, "short", "long"))



glimpse(drma)
###Create two tables for drm(all), drm.e, drm.c 
###### (id, n.e, dose.e, y.e, sd.e, hta, ckd.yn).....n.c, etc

drm.e <- drma %>%
  select(id, n.e, dose.e, y.e, sd.e, design, int.type, duration, bp.measure)

glimpse(drm.e)

drm.c <- drma %>%
  select(id, n.c, dose.c, y.c, sd.c, design, int.type, duration, bp.measure)


#####join(stack) tables by ID (id, n, dose, y, sd) assign as drm

drm <- bind_rows(
  drm.e %>% rename(n = n.e, dose = dose.e, y = y.e, sd = sd.e),
  drm.c %>% rename(n = n.c, dose = dose.c, y = y.c, sd = sd.c)
)


####sort drm by id number ascending
sens <- drm %>% arrange(id)
glimpse(sens)


###save dataframe
write_xlsx(sens, "sens.xlsx")




###Study Design sensitivity analysis

### Restricted cubic spline model
knots_sens <- quantile(sens$dose, c(0.15, 0.5, 0.85))

# Fit the restricted cubic spline model
spl_sens <- dosresmeta(
  formula = y ~ rcs(dose, knots_sens), 
  id = id, sd = sd, n = n, 
  covariance = "md", data = sens, 
  proc = "1stage", mod = ~ design
)

# Summary of the spline model
summary(spl_sens)

# Wald test for non-linearity
wald_test <- waldtest(vcov(spl_sens), coef(spl_sens), 3:4)
print(wald_test)

# Generate data for prediction
newdata <- expand.grid(dose = seq(0, 6, 0.01), 
                       design = c("crossover", "parallel"))
pred_md <- cbind(newdata, predict(spl_sens, newdata = newdata, xref = 0, 
                                  expo = FALSE))

# Separate data for crossover and parallel
parallel_data <- pred_md[pred_md$design == "parallel", ]
crossover_data <- pred_md[pred_md$design == "crossover", ]

# Graphical results 
plot <- ggplot() +
  geom_line(data = parallel_data, aes(x = dose, y = pred, color = "parallel"), 
            linetype = "solid", size = 0.8) +
  geom_line(data = crossover_data, aes(x = dose, y = pred, color = "crossover"),
            linetype = "solid", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  
  labs(
    x = "Δ Sodium excretion (g/day)",
    y = "SBP change (mmHg)",
    
  ) +
  ylim(0, 20) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.2, 0.8)
  ) +
  scale_color_manual(
    name = NULL,
    values = c("crossover" = "steelblue", "parallel" = "orange"),
    labels = c("Crossover", "Parallel")
  )

# Save the plot as PNG
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/Sens/design.png", 
       plot, width = 8, height = 6)

# Display the plot
print(plot)





#####Intervention type sensitivity analysis

### Restricted cubic spline model 
knots_sens <- quantile(sens$dose, c(0.15, 0.5, 0.85))

# Fit the restricted cubic spline model
spl_sens <- dosresmeta(
  formula = y ~ rcs(dose, knots_sens), 
  id = id, sd = sd, n = n, 
  covariance = "md", data = sens, 
  proc = "1stage", mod = ~ int.type
)

# Summary of the spline model
summary(spl_sens)

# Wald test for non-linearity
wald_test <- waldtest(vcov(spl_sens), coef(spl_sens), 3:4)
print(wald_test)

# Generate data for prediction
newdata <- expand.grid(dose = seq(0, 6, 0.01), 
                       int.type = c("diet", "supplement"))
pred_md <- cbind(newdata, predict(spl_sens, newdata = newdata, xref = 0, 
                                  expo = FALSE))

# Separate data for supplement and diet
supplement_data <- pred_md[pred_md$int.type == "supplement", ]
diet_data <- pred_md[pred_md$int.type == "diet", ]

# Graphical results 
plot <- ggplot() +
  geom_line(data = supplement_data, aes(x = dose, y = pred, 
                    color = "supplement"), linetype = "solid", size = 0.8) +
  geom_line(data = diet_data, aes(x = dose, y = pred, color = "diet"), 
            linetype = "solid", size = 0.8 ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  labs(
    x = "Δ Sodium excretion (g/day)",
    y = "SBP change (mmHg)"
  ) +
  ylim(0, 20) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.2, 0.8)
    ) +
  scale_color_manual(
    name = NULL,
    values = c("diet" = "steelblue", "supplement" = "orange"),
    labels = c("Diet", "Supplement")
  )

# Save the plot as PNG
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/Sens/int_type.png", 
       plot, width = 8, height = 6)

# Display the plot
print(plot)





#Duration

### Restricted cubic spline model 
knots_sens <- quantile(sens$dose, c(0.15, 0.5, 0.85))

# Fit the restricted cubic spline model
spl_sens <- dosresmeta(
  formula = y ~ rcs(dose, knots_sens), 
  id = id, sd = sd, n = n, 
  covariance = "md", data = sens, 
  proc = "1stage", mod = ~ duration
)

# Summary of the spline model
summary(spl_sens)

# Wald test for non-linearity
wald_test <- waldtest(vcov(spl_sens), coef(spl_sens), 3:4)
print(wald_test)

# Generate data for prediction
newdata <- expand.grid(dose = seq(0, 6, 0.01), duration = c("short", "long"))
pred_md <- cbind(newdata, predict(spl_sens, newdata = newdata, xref = 0, 
                                  expo = FALSE))

# Separate data for short and long durations
short_data <- pred_md[pred_md$duration == "short", ]
long_data <- pred_md[pred_md$duration == "long", ]

# Graphical results 
plot <- ggplot() +
  geom_line(data = short_data, aes(x = dose, y = pred, color = "long"), 
            linetype = "solid", size = 0.8) +
  geom_line(data = long_data, aes(x = dose, y = pred, color = "short"), 
            linetype = "solid", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  labs(
    x = "Δ Sodium excretion (g/day)",
    y = "SBP change (mmHg)"
  ) +
  ylim (0, 20) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.2, 0.8)
  ) +
  scale_color_manual(
    name = NULL,
    values = c("long" = "steelblue", "short" = "orange"),
    labels = c("≥ 4 wks ", "< 4 wks")
  )

# Save the plot as PNG
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/Sens/duration.png", 
       plot, width = 10, height = 8)

# Display the plot
print(plot)




#####BP measure

### Restricted cubic spline model for Non-linear analysis ###
knots_sens <- quantile(sens$dose, c(0.15, 0.5, 0.85))

# Fit the restricted cubic spline model
spl_sens <- dosresmeta(
  formula = y ~ rcs(dose, knots_sens), 
  id = id, sd = sd, n = n, 
  covariance = "md", data = sens, 
  proc = "1stage", mod = ~ bp.measure
)

# Summary of the spline model
summary(spl_sens)

# Wald test for non-linearity
wald_test <- waldtest(vcov(spl_sens), coef(spl_sens), 3:4)
print(wald_test)

# Generate data for prediction
newdata <- expand.grid(dose = seq(0, 6, 0.01), 
                       bp.measure = c("ambulatory", "clinic"))
pred_md <- cbind(newdata, predict(spl_sens, newdata = newdata, xref = 0, 
                                    expo = FALSE))

# Separate data for clinic and ambulatory  
clinic_data <- pred_md[pred_md$bp.measure == "clinic", ]
ambulatory_data <- pred_md[pred_md$bp.measure == "ambulatory", ]

# Graphical results 
plot <- ggplot() +
  geom_line(data = clinic_data, aes(x = dose, y = pred, color = "clinic"), 
            linetype = "solid", size = 0.8) +
  geom_line(data = ambulatory_data, aes(x = dose, y = pred, color = "ambulatory"), 
            linetype = "solid", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  labs(
    x = "Δ Sodium excretion (g/day)",
    y = "SBP change (mmHg)"
  ) +
  ylim(0, 20) + 
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.2, 0.8)
  ) +
  scale_color_manual(
    name = NULL,
    values = c("ambulatory" = "steelblue", "clinic" = "orange"),
    labels = c("Ambulatory", "Clinic")
  )

# Save the plot as PNG
ggsave("C:/Users/Nomade/OneDrive/Bureau/JAMA/DRM/Sens/bp_measure.png", 
       plot, width = 10, height = 8)

# Display the plot
print(plot)

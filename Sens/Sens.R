#Sensitivity analysis of the DRM


library(dosresmeta)
library(rms)
library(mvtnorm)
library(DoseFinding)
library(aod)
library(Hmisc)
library(ggplot2)
library(readxl)
sens <- read_excel("C:/Users/Nomade/OneDrive/Bureau/Practice/raw_data/sens.xlsx")
View(sens)
# Read data
sens <- read_excel("C:/Users/Nomade/OneDrive/Bureau/Practice/raw_data/sens.xlsx")



###Study Design


### Restricted cubic spline model for Non-linear analysis ###
knots_sens <- quantile(sens$dose, c(0.15, 0.5, 0.85))

# Fit the restricted cubic spline model
spl_sens2 <- dosresmeta(
  formula = y ~ rcs(dose, knots_sens), 
  id = id, sd = sd, n = n, 
  covariance = "md", data = sens, 
  proc = "1stage", mod = ~ design
)

# Summary of the spline model
summary(spl_sens2)

# Wald test for non-linearity
wald_test <- waldtest(vcov(spl_sens2), coef(spl_sens2), 3:4)
print(wald_test)

# Generate data for prediction
newdata <- expand.grid(dose = seq(0, 250, 1), design = c("crossover", "parallel"))
pred_md <- cbind(newdata, predict(spl_sens2, newdata = newdata, xref = 0, expo = FALSE))

# Separate data for <4 wks and ≥4 wks durations
supplement_data <- pred_md[pred_md$design == "parallel", ]
diet_data <- pred_md[pred_md$design == "crossover", ]

# Graphical results using ggplot2 for improved aesthetics
plot <- ggplot() +
  geom_line(data = supplement_data, aes(x = dose, y = pred, color = "parallel"), linetype = "solid", size = 1.5) +
  geom_line(data = diet_data, aes(x = dose, y = pred, color = "crossover"), linetype = "solid", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 1) +
  labs(
    x = "Δ Sodium Excretion (mmol/day)",
    y = "SBP Change (mmHg)",
    title = "SBP change by study design"
    
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),  # Adjust legend text size
    legend.key.size = unit(1.5, "lines"),   # Adjust size of legend key
    axis.text = element_text(size = 16, face = "bold"),  # Bolden the tick labels
    axis.title = element_text(size = 18), # Bolden the axis labels
  ) +
  scale_color_manual(
    name = "Design",
    values = c("crossover" = "blue", "parallel" = "red"),
    labels = c("crossover", "parallel")
  )

# Save the plot as PNG
ggsave("C:/Users/Nomade/OneDrive/Bureau/Practice/doc/spline_model_plot_with_large_legend2.png", plot, width = 10, height = 8)

# Display the plot
print(plot)




#####Intervention type
### Restricted cubic spline model for Non-linear analysis ###
knots_sens <- quantile(sens$dose, c(0.15, 0.5, 0.85))

# Fit the restricted cubic spline model
spl_sens2 <- dosresmeta(
  formula = y ~ rcs(dose, knots_sens), 
  id = id, sd = sd, n = n, 
  covariance = "md", data = sens, 
  proc = "1stage", mod = ~ int_type
)

# Summary of the spline model
summary(spl_sens2)

# Wald test for non-linearity
wald_test <- waldtest(vcov(spl_sens2), coef(spl_sens2), 3:4)
print(wald_test)

# Generate data for prediction
newdata <- expand.grid(dose = seq(0, 250, 1), int_type = c("diet", "supplement"))
pred_md <- cbind(newdata, predict(spl_sens2, newdata = newdata, xref = 0, expo = FALSE))

# Separate data for <4 wks and ≥4 wks durations
supplement_data <- pred_md[pred_md$int_type == "supplement", ]
diet_data <- pred_md[pred_md$int_type == "diet", ]

# Graphical results using ggplot2 for improved aesthetics
plot <- ggplot() +
  geom_line(data = supplement_data, aes(x = dose, y = pred, color = "supplement"), linetype = "solid", size = 1.5) +
  geom_line(data = diet_data, aes(x = dose, y = pred, color = "diet"), linetype = "solid", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 1) +
  labs(
    x = "Δ Sodium Excretion (mmol/day)",
    y = "SBP Change (mmHg)",
    title = "SBP change by Intervention type"
    
  ) +
  ylim(0, 10) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),  # Adjust legend text size
    legend.key.size = unit(1.5, "lines"),   # Adjust size of legend key
    axis.text = element_text(size = 16, face = "bold"),  # Bolden the tick labels
    axis.title = element_text(size = 18), # Bolden the axis labels
  ) +
  scale_color_manual(
    name = "Design",
    values = c("diet" = "blue", "supplement" = "red"),
    labels = c("diet", "supplement")
  )

# Save the plot as PNG
ggsave("C:/Users/Nomade/OneDrive/Bureau/Practice/doc/spline_model_plot_with_large_legend4.png", plot, width = 10, height = 8)

# Display the plot
print(plot)





#Duration

### Restricted cubic spline model for Non-linear analysis ###
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
newdata <- expand.grid(dose = seq(0, 250, 1), duration = c("<4 wks", "≥4 wks"))
pred_md <- cbind(newdata, predict(spl_sens, newdata = newdata, xref = 0, expo = FALSE))

# Separate data for <4 wks and ≥4 wks durations
supplement_data <- pred_md[pred_md$duration == "<4 wks", ]
diet_data <- pred_md[pred_md$duration == "≥4 wks", ]

# Graphical results using ggplot2 for improved aesthetics
plot <- ggplot() +
  geom_line(data = supplement_data, aes(x = dose, y = pred, color = "≥4 wks"), linetype = "solid", size = 1.5) +
  geom_line(data = diet_data, aes(x = dose, y = pred, color = "<4 wks"), linetype = "solid", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 1) +
  labs(
    x = "Δ Sodium Excretion (mmol/day)",
    y = "SBP Change (mmHg)",
    title = "SBP change by study duration"
    
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),  # Adjust legend text size
    legend.key.size = unit(1.5, "lines"),   # Adjust size of legend key
    axis.text = element_text(size = 16, face = "bold"),  # Bolden the tick labels
    axis.title = element_text(size = 18), # Bolden the axis labels
  ) +
  scale_color_manual(
    name = "Duration",
    values = c("≥4 wks" = "blue", "<4 wks" = "red"),
    labels = c("≥4 wks", "<4 wks")
  )

# Save the plot as PNG
ggsave("C:/Users/Nomade/OneDrive/Bureau/Practice/doc/spline_model_plot_with_large_legend.png", plot, width = 10, height = 8)

# Display the plot
print(plot)



#####BP measure

### Restricted cubic spline model for Non-linear analysis ###
knots_sens <- quantile(sens$dose, c(0.15, 0.5, 0.85))

# Fit the restricted cubic spline model
spl_sens2 <- dosresmeta(
  formula = y ~ rcs(dose, knots_sens), 
  id = id, sd = sd, n = n, 
  covariance = "md", data = sens, 
  proc = "1stage", mod = ~ bp_measure
)

# Summary of the spline model
summary(spl_sens2)

# Wald test for non-linearity
wald_test <- waldtest(vcov(spl_sens2), coef(spl_sens2), 3:4)
print(wald_test)

# Generate data for prediction
newdata <- expand.grid(dose = seq(0, 250, 1), bp_measure = c("ambulatory", "clinic"))
pred_md <- cbind(newdata, predict(spl_sens2, newdata = newdata, xref = 0, expo = FALSE))

# Separate data for <4 wks and ≥4 wks durations
supplement_data <- pred_md[pred_md$bp_measure == "clinic", ]
diet_data <- pred_md[pred_md$bp_measure == "ambulatory", ]

# Graphical results using ggplot2 for improved aesthetics
plot <- ggplot() +
  geom_line(data = supplement_data, aes(x = dose, y = pred, color = "clinic"), linetype = "solid", size = 1.5) +
  geom_line(data = diet_data, aes(x = dose, y = pred, color = "ambulatory"), linetype = "solid", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 1) +
  labs(
    x = "Δ Sodium Excretion (mmol/day)",
    y = "SBP Change (mmHg)",
    title = "SBP change by BP measure method"
  ) +
  ylim(0, 10) + 
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),  # Adjust legend text size
    legend.key.size = unit(1.5, "lines"),   # Adjust size of legend key
    axis.text = element_text(size = 16, face = "bold"),  # Bolden the tick labels
    axis.title = element_text(size = 18, face = "bold")  # Bolden the axis labels
  ) +
  scale_color_manual(
    name = "Design",
    values = c("ambulatory" = "blue", "clinic" = "red"),
    labels = c("ambulatory", "clinic")
  )

# Save the plot as PNG
ggsave("C:/Users/Nomade/OneDrive/Bureau/Practice/doc/spline_model_plot_with_large_legend3.png", plot, width = 10, height = 8)

# Display the plot
print(plot)

#DR---Dose response summaries, curves and predictions

install.packages("dplyr")
install.packages("writexl")

#import libraries

library(dplyr)
library(readxl)
library(writexl)


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


#filter and save dataframes, drm, normo(filter hta), hyper(filter hta), 
########ckd (filter ckd.stage)

write.csv(drm, "drm.csv", row.names = FALSE)


# Filter for normotensive (HTA == 0)
normo <- drm %>% filter(hta == 0)

# Filter for hypertensive (HTA == 1)
hyper <- drm %>% filter(hta == 1)

# Filter for CKD based on the CKD stage column 
ckd <- drm %>% filter(ckd.yn == 1)

# Optionally save these filtered dataframes to CSV files
write.csv(normo, "normotensive_data.csv", row.names = FALSE)
write.csv(hyper, "hypertensive_data.csv", row.names = FALSE)
write.csv(ckd, "ckd_data.csv", row.names = FALSE)






###All population

###Linear model

library(dosresmeta)
library(ggplot2)
library(openxlsx)

# Linear model
lin_drm <- dosresmeta(formula = y ~ dose, id = id, sd = sd, n = n, 
                      covariance = "md", data = drm, proc = "1stage") 

# Generate summary of the model
results_table <- summary(lin_drm) 

# Save coefficient table
results_df <- as.data.frame(results_table$coefficients)
write_xlsx(results_df, 
           path = "C:/Users/Nomade/OneDrive/Bureau/
           Practice/documentation/summary_table.xlsx")

# Graphical results
dosex_drm <- data.frame(dose = seq(0, 250, 1))  
predictions <- predict(lin_drm, dosex_drm, order = TRUE)

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.2) +
  labs(
    subtitle = "All populations ",
    x = "Δ Sodium Excretion (mmol/day)",
    y = "SBP Change (mmHg)"
  ) +
  ylim(0, 10) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/Practice/
       documentation/dose_response_plot.png", p, width = 8, height = 6)

# Display the plot
print(p)


###Quadratic

#quadratic model
quadr_drm <- dosresmeta(formula = y ~ dose + I(dose^2), id = id,
                        sd = sd, n = n, covariance = "md", 
                        data = drm, proc = "1stage")
summary(quadr_drm)

#Graphical results 
xref_drm <- 0 
with(predict(quadr_drm, dosex_drm, order = TRUE), { 
  plot(dose, pred, type = "l", col = "black", ylim = c(0, 10), 
       ylab = "SBP change (mmHg)", xlab = "Δ Sodium excretion (mmol/day)",
       bty = "l", las = 1, lwd= 1.35) 
  lines(dose,  ci.lb, lty = 2, lwd=1.35) 
  lines(dose, ci.ub, lty = 2, lwd=1.35) 
  abline(h=0, lty=2, col="black", lwd=1)
}) 


###Restricted cubic spline model for Non-linear analysis### 

knots_drm <- quantile(drm$dose, c(.15, .5, .85)) 
# rcspline
spl_drm <- dosresmeta(formula = y ~ rcs(dose, knots_drm), id = id, sd = sd, 
                      n = n, covariance 
                      = "md", data = drm, proc = "1stage") 
summary(spl_drm) 

#waldtest
waldtest(b = coef(spl_drm), Sigma = vcov(spl_drm), Terms = 1:2) 

#Graphical results 
xref_drm <- 0 
with(predict(spl_drm, dosex_drm, xref_drm),{ 
  plot(get("rcs(dose, knots_drm)dose"), pred, type = "l", ylim = c(0, 10), 
       ylab = "SBP change (mmHg)", xlab = "Δ Sodium excretion (mmol/day)",
       bty = "l", las = 1, lwd= 1.35, main= "All population", cex.main=1) 
  matlines(get("rcs(dose, knots_drm)dose"), cbind(ci.ub, ci.lb),
           col = 1, lty = "dashed", lwd= 1.35) 
}) 


#linear predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(lin_drm, newdata = newdata,  expo = FALSE)
drmln_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(drmln_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice
           /documentation/drmln_pred.xlsx")

#quadratic predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(quadr_drm, newdata = newdata,  expo = FALSE)
drmqdr_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(drmqdr_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice
           /documentation/drmqdr_pred.xlsx")

#spline predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(spl_drm, newdata = newdata,  expo = FALSE)
drmspl_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(drmspl_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice
           /documentation/drmspl_pred.xlsx")











########### Normotensive

# Linear model
lin_normo <- dosresmeta(formula = y ~ dose, id = id, sd = sd, n = n, 
                        covariance = "md", data = normo, proc = "1stage") 

# Generate summary of the model
results_table <- summary(lin_normo) 

# Save coefficient table
results_df <- as.data.frame(results_table$coefficients)
write_xlsx(results_df, path = "C:/Users/Nomade/OneDrive/Bureau/Practice
           /documentation/summary_table.xlsx")

# Graphical results
dosex_normo <- data.frame(dose = seq(0, 250, 1))  
predictions <- predict(lin_normo, dosex_normo, order = TRUE)

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.2) +
  labs(title = "Normotensive ",
       x = "Δ Sodium Excretion (mmol/day)",
       y = "SBP Change (mmHg)"
  ) +
  ylim(0, 10) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 16, face = "bold"),  
    # Bolden the tick labels
    axis.title = element_text(size = 18), # Bolden the axis labels
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/Practice
       /Doc/dose_response_plot2.png", p, width = 8, height = 6)

# Display the plot
print(p)



#quadratic model
quadr_normo <- dosresmeta(formula = y ~ dose + I(dose^2), id = id,
                          sd = sd, n = n, covariance = "md", 
                          data = normo, proc = "1stage")
summary(quadr_normo)

#Graphical results 
xref_normo <- 0 
with(predict(quadr_normo, dosex_normo, order = TRUE), { 
  plot(dose, pred, type = "l", col = "black", ylim = c(0, 10), 
       ylab = "SBP change (mmHg)", xlab = "Δ Sodium excretion (mmol/day)",
       bty = "l", las = 1, lwd= 1.35) 
  lines(dose,  ci.lb, lty = 2, lwd=1.35) 
  lines(dose, ci.ub, lty = 2, lwd=1.35) 
  abline(h=0, lty=2, col="black", lwd=1)
}) 


###Restricted cubic spline model for Non-linear analysis### 

knots_normo <- quantile(normo$dose, c(.15, .5, .85)) 
# rcspline
spl_normo <- dosresmeta(formula = y ~ rcs(dose, knots_normo), id = id, sd = sd, 
                        n = n, covariance 
                        = "md", data = normo, proc = "1stage") 
summary(spl_normo) 

#waldtest
waldtest(b = coef(spl_normo), Sigma = vcov(spl_normo), Terms = 1:2) 

#Graphical results 
xref_normo <- 0 
with(predict(spl_normo, dosex_normo, xref_normo),{ 
  plot(get("rcs(dose, knots_normo)dose"), pred, type = "l", ylim = c(0, 10), 
       ylab = "SBP change (mmHg)", xlab = "Δ Sodium excretion (mmol/day)",
       bty = "l", las = 1, lwd= 1.35, main= "Normotensive", cex.main=1) 
  matlines(get("rcs(dose, knots_normo)dose"), cbind(ci.ub, ci.lb),
           col = 1, lty = "dashed", lwd= 1.35) 
  abline(h=0, lty=2, col="black", lwd=1)
}) 



#linear predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(lin_normo, newdata = newdata,  expo = FALSE)
normoln_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(normoln_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice
           /documentation/normoln_pred.xlsx")

#quadratic predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(quadr_normo, newdata = newdata,  expo = FALSE)
normoqdr_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(normoqdr_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice
           /documentation/normoqdr_pred.xlsx")

#spline predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(spl_normo, newdata = newdata,  expo = FALSE)
normospl_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(normospl_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice
           /documentation/normospl_pred.xlsx")










############# Hypertensive

# Linear model
lin_hyper <- dosresmeta(formula = y ~ dose, id = id, sd = sd, n = n, 
                        covariance = "md", data = hyper, proc = "1stage") 

# Generate summary of the model
results_table <- summary(lin_hyper) 

# Save coefficient table
results_df <- as.data.frame(results_table$coefficients)
write_xlsx(results_df, path = "C:/Users/Nomade/OneDrive/Bureau/Practice
           /documentation/summary_table.xlsx")

# Graphical results
dosex_hyper <- data.frame(dose = seq(0, 250, 1))  
predictions <- predict(lin_hyper, dosex_hyper, order = TRUE)

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.2) +
  labs(title = "Hypertensive ",
       x = "Δ Sodium Excretion (mmol/day)",
       y = "SBP Change (mmHg)"
  ) +
  ylim(0, 22) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 16, face = "bold"),  
    # Bolden the tick labels
    axis.title = element_text(size = 18), # Bolden the axis labels
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/Practice/Doc/dose_response_plot3.png",
       p, width = 8, height = 6)

# Display the plot
print(p)





#quadratic model
quadr_hyper <- dosresmeta(formula = y ~ dose + I(dose^2), id = id,
                          sd = sd, n = n, covariance = "md", 
                          data = hyper, proc = "1stage")
summary(quadr_hyper)

#Graphical results 
xref_hyper <- 0 
with(predict(quadr_hyper, dosex_hyper, order = TRUE), { 
  plot(dose, pred, type = "l", col = "black", ylim = c(0, 20), 
       ylab = "SBP change (mmHg)", xlab = "Δ Sodium excretion (mmol/day)",
       bty = "l", las = 1, lwd= 1.35) 
  lines(dose,  ci.lb, lty = 2, lwd=1.35) 
  lines(dose, ci.ub, lty = 2, lwd=1.35) 
  abline(h=0, lty=2, col="black", lwd=1)
}) 


###Restricted cubic spline model for Non-linear analysis### 

knots_hyper <- quantile(hyper$dose, c(.15, .5, .85)) 
# rcspline
spl_hyper <- dosresmeta(formula = y ~ rcs(dose, knots_hyper), id = id, sd = sd, 
                        n = n, covariance 
                        = "md", data = hyper, proc = "1stage") 
summary(spl_hyper) 

#waldtest
waldtest(b = coef(spl_hyper), Sigma = vcov(spl_hyper), Terms = 1:2) 

#Graphical results 
xref_hyper <- 0 
with(predict(spl_hyper, dosex_hyper, xref_hyper),{ 
  plot(get("rcs(dose, knots_hyper)dose"), pred, type = "l", ylim = c(0, 20), 
       ylab = "SBP change (mmHg)", xlab = "Δ Sodium excretion (mmol/day)",
       bty = "l", las = 1, lwd= 1.35, main= "Hypertensive", cex.main=1) 
  matlines(get("rcs(dose, knots_hyper)dose"), cbind(ci.ub, ci.lb),
           col = 1, lty = "dashed", lwd= 1.35) 
  abline(h=0, lty=2, col="black", lwd=1)
}) 



#linear predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(lin_hyper, newdata = newdata,  expo = FALSE)
hyperln_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(hyperln_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice/
           documentation/hyperln_pred.xlsx")

#quadratic predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(quadr_hyper, newdata = newdata,  expo = FALSE)
hyperqdr_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(hyperqdr_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice/
           documentation/hyperqdr_pred.xlsx")

#spline predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(spl_hyper, newdata = newdata,  expo = FALSE)
hyperspl_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(hyperspl_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice/
           documentation/hyperspl_pred.xlsx")













############## CKD

# Linear model
lin_ckd <- dosresmeta(formula = y ~ dose, id = id, sd = sd, n = n, 
                      covariance = "md", data = ckd, proc = "1stage") 

# Generate summary of the model
results_table <- summary(lin_ckd) 

# Save coefficient table
results_df <- as.data.frame(results_table$coefficients)
write_xlsx(results_df, path = "C:/Users/Nomade/OneDrive/Bureau/Practice
           /documentation/summary_table.xlsx")

# Graphical results
dosex_ckd <- data.frame(dose = seq(0, 250, 1))  
predictions <- predict(lin_ckd, dosex_ckd, order = TRUE)

# Create data frame for plotting
plot_data <- data.frame(
  dose = predictions$dose,
  pred = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = dose)) +
  geom_line(aes(y = pred), color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.2) +
  labs(title = "CKD population ",
       x = "Δ Sodium Excretion (mmol/day)",
       y = "SBP Change (mmHg)"
  ) +
  ylim(0, 31) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 16, face = "bold"),  
    # Bolden the tick labels
    axis.title = element_text(size = 18), # Bolden the axis labels
  )

# Save the plot
ggsave("C:/Users/Nomade/OneDrive/Bureau/Practice/Doc/dose_response_plot4.png", 
       p, width = 8, height = 6)

# Display the plot
print(p)


#quadratic model
quadr_ckd <- dosresmeta(formula = y ~ dose + I(dose^2), id = id,
                        sd = sd, n = n, covariance = "md", 
                        data = ckd, proc = "1stage")
summary(quadr_ckd)

#Graphical results 
xref_ckd <- 0 
with(predict(quadr_ckd, dosex_ckd, order = TRUE), { 
  plot(dose, pred, type = "l", col = "black", ylim = c(0, 20), 
       ylab = "SBP change (mmHg)", xlab = "Δ Sodium excretion (mmol/day)",
       bty = "l", las = 1, lwd= 1.35) 
  lines(dose,  ci.lb, lty = 2, lwd=1.35) 
  lines(dose, ci.ub, lty = 2, lwd=1.35) 
  abline(h=0, lty=2, col="black", lwd=1)
}) 


###Restricted cubic spline model for Non-linear analysis### 

knots_ckd <- quantile(ckd$dose, c(.15, .5, .85)) 
# rcspline
spl_ckd <- dosresmeta(formula = y ~ rcs(dose, knots_ckd), id = id, sd = sd, 
                      n = n, covariance 
                      = "md", data = ckd, proc = "1stage") 
summary(spl_ckd) 

#waldtest
waldtest(b = coef(spl_ckd), Sigma = vcov(spl_ckd), Terms = 1:2) 

#Graphical results 
xref_ckd <- 0 
with(predict(spl_ckd, dosex_ckd, xref_ckd),{ 
  plot(get("rcs(dose, knots_ckd)dose"), pred, type = "l", ylim = c(0, 20), 
       ylab = "SBP change (mmHg)", xlab = "Δ Sodium excretion (mmol/day)",
       bty = "l", las = 1, lwd= 1.35) 
  matlines(get("rcs(dose, knots_ckd)dose"), cbind(ci.ub, ci.lb),
           col = 1, lty = "dashed", lwd= 1.35) 
  abline(h=0, lty=2, col="black", lwd=1)
}) 


#linear predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(lin_ckd, newdata = newdata,  expo = FALSE)
ckdln_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(ckdln_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice/
           documentation/ckdln_pred.xlsx")

#quadratic predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(quadr_ckd, newdata = newdata,  expo = FALSE)
ckdqdr_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(ckdqdr_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice/
           documentation/ckdqdr_pred.xlsx")

#spline predictions

newdata <- data.frame(dose = seq(0, 240, 40))
pred_md <- predict(spl_ckd, newdata = newdata,  expo = FALSE)
ckdspl_pred <- round(pred_md, 2)

# Write the data frame to an Excel file
write_xlsx(drmspl_pred, 
           path = "C:/Users/Nomade/OneDrive/Bureau/Practice/
           documentation/ckdspl_pred.xlsx")



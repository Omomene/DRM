#Publication bias for a traditional meta-analysis of the DRMA

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
allbias <- drma %>%
  mutate(hta = recode(hta,
                      "Normotensive" = 0,
                      "Pre-hypertensive" = 0,
                      "Hypertensive" = 1,
                      "Mixed" = 2))



glimpse(allbias)

#filter allbias, normobias(filter hta), hyperbias(filter hta)

normobias <- allbias %>% filter(hta == 0)

glimpse (normobias)

hyperbias <- allbias %>% filter(hta == 1)

glimpse (hyperbias)



###Pub bias for all population

# Perform meta-analysis using metacont from meta package
meta_results <- metacont(n.e = allbias$n.e, 
                         mean.e = allbias$y.e, 
                         sd.e = allbias$sd.e,
                         n.c = allbias$n.c, 
                         mean.c = allbias$y.c, 
                         sd.c = allbias$sd.c,
                         data = allbias,
                         sm = "SMD")

# Print the meta-analysis results
print(meta_results)

#Publication bias test
metabias(meta_results, method.bias = "Egger")


#Graphic

funnelall <- funnel(meta_results, axes = FALSE, col = "steelblue", 
                    lwd.common = 1.35, lty.common = 1, col.common = "gray",
                    xlab = "",  
                    ylab = "",
                    xlim = c(-1.5, 1),
                    ylim = c(0.6, 0))

# Add axes, gridline and labels manually

axis(1, cex.axis = 0.8, tck = -0.05) 
axis(2, cex.axis = 0.8, tck = -0.05)  

box(lty = "solid", col = "black", bty = "l", lwd = 1.2)  

abline(h = seq(0, 0.6, by = 0.1), col = "gray", lty = 2, lwd = 1)  

mtext("Standardized Mean Difference (SMD)", side = 1, line = 2, cex = 0.9)
mtext("Standard Error", side = 2, line = 2, cex = 0.9)







###Pub bias for normotensive population

# Perform meta-analysis using metacont from meta package
meta_results <- metacont(n.e = normobias$n.e, 
                         mean.e = normobias$y.e, 
                         sd.e = normobias$sd.e,
                         n.c = normobias$n.c, 
                         mean.c = normobias$y.c, 
                         sd.c = normobias$sd.c,
                         data = normobias,
                         sm = "SMD")

# Print the meta-analysis results
print(meta_results)

#Publication bias test
metabias(meta_results, method.bias = "Egger")



#Graphics

funnelall <- funnel(meta_results, axes = FALSE, col = "steelblue", 
                    lwd.common = 1.35, lty.common = 1, col.common = "gray",
                    xlab = "",  
                    ylab = "",
                    xlim = c(-1.5, 1),
                    ylim = c(0.6, 0))

# Add axes, gridlines and label manually

axis(1, cex.axis = 0.8, tck = -0.05)  
axis(2, cex.axis = 0.8, tck = -0.05)  

box(lty = "solid", col = "black", bty = "l", lwd = 1.2) 

abline(h = seq(0, 0.6, by = 0.1), col = "gray", lty = 2, lwd = 1)  

mtext("Standardized Mean Difference (SMD)", side = 1, line = 2, cex = 0.9)
mtext("Standard Error", side = 2, line = 2, cex = 0.9)







###Pub bias for hypertensive population

# Perform meta-analysis using metacont from meta package
meta_results <- metacont(n.e = hyperbias$n.e, 
                         mean.e = hyperbias$y.e, 
                         sd.e = hyperbias$sd.e,
                         n.c = hyperbias$n.c, 
                         mean.c = hyperbias$y.c, 
                         sd.c = hyperbias$sd.c,
                         data = hyperbias,
                         sm = "SMD")

# Print the meta-analysis results
print(meta_results)

#Publication bias test
metabias(meta_results, method.bias = "Egger")

#Graphics
funnelall <- funnel(meta_results, axes = FALSE, col = "steelblue",
                    lwd.random = NULL, col.random = NULL,
                    lwd.common = 1.35, lty.common = 1, col.common = "gray",
                    xlab = "",  
                    ylab = "",
                    xlim = c(-1.5, 1),
                    ylim = c(0.6, 0))

# Add axes, gridlines and labels manually

axis(1, cex.axis = 0.8, tck = -0.05)  
axis(2, cex.axis = 0.8, tck = -0.05)  

box(lty = "solid", col = "black", bty = "l", lwd = 1.2)

abline(h = seq(0, 0.6, by = 0.1), col = "gray", lty = 2, lwd = 1) 

mtext("Standardized Mean Difference (SMD)", side = 1, line = 2, cex = 0.9)
mtext("Standard Error", side = 2, line = 2, cex = 0.9)








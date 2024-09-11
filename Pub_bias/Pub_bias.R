#Publication bias for a tradtional meta-analysis of the DRMA

# Load required packages
library(meta)
library(ggplot2)
library(readxl)

####All population

# Perform meta-analysis using metacont from meta package
meta_results <- metacont(n.e = pub_bias$n.t, 
                         mean.e = pub_bias$mean.t, 
                         sd.e = pub_bias$sd.t,
                         n.c = pub_bias$n.c, 
                         mean.c = pub_bias$mean.c, 
                         sd.c = pub_bias$sd.c,
                         data = pub_bias,
                         sm = "SMD")

# Print the meta-analysis results
print(meta_results)

# Plot the funnel plot with ggplot2
funnel_plot <- funnel(meta_results, 
                      col = "steelblue", 
                      col.random = "blue",axes = FALSE,
                      bty = "o",
                      shape = 16, 
                      border = "black",
                      xlab = "Standardized Mean Difference (SMD)",
                      main = "Funnel Plot for Publication Bias",
                      xlim = c(-1.5, 1),
                      ylim = c(0.5, 0),
                      main = "SBP change by Intervention type") +
  # Add horizontal line at random effects line
  geom_hline(yintercept = meta_results$randomeffects$mean, 
             linetype = "solid", color = "blue", size = 1)
# Add X-axis
axis(1)

# Add Y-axis
axis(2)

# Save the plot as PNG
ggsave("funnel_plot.png", funnel_plot, width = 10, height = 8)

# Display the plot
print(funnel_plot)

?funnel



#Hyper


# Perform meta-analysis using metacont from meta package
meta_results <- metacont(n.e = pub_bias_hyper$n.t, 
                         mean.e = pub_bias_hyper$mean.t, 
                         sd.e = pub_bias_hyper$sd.t,
                         n.c = pub_bias_hyper$n.c, 
                         mean.c = pub_bias_hyper$mean.c, 
                         sd.c = pub_bias_hyper$sd.c,
                         data = pub_bias_hyper,
                         sm = "SMD")

# Print the meta-analysis results
print(meta_results)

# Plot the funnel plot with ggplot2
funnel_plot <- funnel(meta_results, 
                      col = "steelblue", 
                      col.random = "blue",axes = FALSE,
                      bty = "o",
                      shape = 16, 
                      border = "black",
                      xlab = "Standardized Mean Difference (SMD)",
                      main = "Funnel Plot for Publication Bias",
                      xlim = c(-1.5, 1),
                      ylim = c(0.5, 0),
                      main = "SBP change by Intervention type") +
  # Add horizontal line at random effects line
  geom_hline(yintercept = meta_results$randomeffects$mean, 
             linetype = "solid", color = "blue", size = 1)
# Add X-axis
axis(1)

# Add Y-axis
axis(2)

# Save the plot as PNG
ggsave("C:/Users/Nomade/OneDrive/Bureau/Practice/doc/funnel_plot2.png", funnel_plot, width = 10, height = 8)

# Display the plot
print(funnel_plot)

?funnel




#Normo


# Perform meta-analysis using metacont from meta package
meta_results <- metacont(n.e = pub_bias_normo$n.t, 
                         mean.e = pub_bias_normo$mean.t, 
                         sd.e = pub_bias_normo$sd.t,
                         n.c = pub_bias_normo$n.c, 
                         mean.c = pub_bias_normo$mean.c, 
                         sd.c = pub_bias_normo$sd.c,
                         data = pub_bias_normo,
                         sm = "SMD")

# Print the meta-analysis results
print(meta_results)

# Plot the funnel plot with ggplot2
funnel_plot <- funnel(meta_results, 
                      col = "steelblue", 
                      col.random = "blue",axes = FALSE,
                      bty = "o",
                      shape = 16, 
                      border = "black",
                      xlab = "Standardized Mean Difference (SMD)",
                      main = "Funnel Plot for Publication Bias",
                      xlim = c(-1.5, 1),
                      ylim = c(0.5, 0),
                      main = "SBP change by Intervention type") +
  # Add horizontal line at random effects line
  geom_hline(yintercept = meta_results$randomeffects$mean, 
             linetype = "solid", color = "blue", size = 1)
# Add X-axis
axis(1)

# Add Y-axis
axis(2)

# Save the plot as PNG
ggsave("funnel_plot.png", funnel_plot, width = 10, height = 8)

# Display the plot
print(funnel_plot)

?funnel


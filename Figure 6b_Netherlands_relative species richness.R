
setwd("../")


list.of.packages <- c("datasets",
                      "openxlsx",
                      "readxl",
                      "gridExtra",
                      "writexl",
                      "ggplot2",
                      "plotly",
                      "data.table",
                      "forcats",
                      "dplyr",
                      "Hmisc",
                      "dplyr",
                      "mgcv") #add all required packages
for(i in 1:length(list.of.packages)){if(!(list.of.packages[i] %in% rownames(installed.packages()))){install.packages(list.of.packages[i])}} #only download uninstalled packages
lapply(list.of.packages, require, character.only = T) #load all packages



# Read a specific sheet from the Excel file
Input_data <- read_xlsx(
  "........../FIGURES DATA.xlsx",
  sheet = "Figure_6b_Netherlands" 
)


# Initialize a list to store the results from each number of groups
results_list <- list()

group_numbers <- c(10,15, 20, 25, 30, 35, 40, 45, 50)

# Loop through each specified number of groups
for (g in group_numbers) {
  print(paste("Analysis for", g, "groups"))
  
  # Excluding species found in less than 5 sites (rare species)
  abundtox <- Input_data %>%  
    group_by(TWNcode) %>%
    mutate(nsites = n()) %>%
    ungroup() %>%
    filter(nsites >= 5)#10
  
  # Species abundance
  abundtox <- abundtox %>%
    rename(
      # SpeciesName = `TWN naam na taxon correctie`,
      Abund = AMT_CALC_Ecofide
    )
  
  # Group individual species into categories
  abundtox <- abundtox %>%
    mutate(
      logAbund = log10(Abund),
      category = Hmisc::cut2(abundtox$msPAF, g = g),
      category.fact = paste0('Group', as.integer(category)) |>
        as_factor()
    )
  
  # select species that are only being found less toxic sites in GRP1
  sensitive_species <- abundtox %>%
    filter(category.fact == "Group1")
  
  Species.list <- unique(sensitive_species$TWNcode)#SpeciesName
  
  abundtox_selected <- abundtox %>%
    filter(TWNcode %in% Species.list)
  
  # count unique species per site, per category msPAF group
  richnessTWN <- abundtox_selected %>%
    group_by(category.fact, MonsterID_ecofide) %>%
    summarise(Unique_TWNcodes = n_distinct(TWNcode), .groups = 'drop')
  
  # Filter unique msPAF data
  unique_tox_info <- abundtox %>%
    distinct(MonsterID_ecofide, .keep_all = TRUE) %>%
    dplyr::select(MonsterID_ecofide, msPAF)
  
  richtox_join_new <- left_join(richnessTWN, unique_tox_info, by = "MonsterID_ecofide")
  
  # Add n values
  Total_Abund <- richtox_join_new %>%
    group_by(category.fact) %>%
    summarise(
      Total = round(mean(Unique_TWNcodes), 2),
      low = min(msPAF),
      high = max(msPAF),
      msPAF.median= mean(msPAF),#median
      count.sample = n(),
      .groups = 'drop'
    )
  
  
}


#### ALL of the NL

# GAM on relative richness per sample location vs relative msPAF per sample location 
mean_species_richness <- mean(richtox_join_new$Unique_TWNcodes) # mean 
richtox_join_relative_richness <- richtox_join_new %>% mutate(rel_species_richness = (Unique_TWNcodes - mean_species_richness)/mean_species_richness)

plot_gam <- ggplot(richtox_join_relative_richness, aes(x = msPAF, y = rel_species_richness)) + #same point as above, why use ODF_negative here? 
  geom_point() +
  # facet_wrap(~GroupSize) +
  stat_smooth(method = "gam", se = FALSE, fullrange = TRUE, color = "blue", formula = y ~ s(x, k = 3)) +
  ggtitle("GAM Smoothing") + 
  scale_x_continuous(limits = c(0, 0.25))
print(plot_gam)


# Fit the GAM model using the correct dataset
gam_model <- gam(rel_species_richness ~ s(msPAF, k = 3), data = richtox_join_relative_richness, family = gaussian(link = "identity"))

# Predict the y-values and standard errors across all msPAF values in the dataframe
predictions <- predict(gam_model, newdata = richtox_join_relative_richness, se.fit = TRUE)

# Add predicted values and confidence intervals to the dataframe
richtox_join_relative_richness$predicted_values <- predictions$fit
richtox_join_relative_richness$lower_ci <- predictions$fit - 1.96 * predictions$se.fit  # Lower bound of 95% CI
richtox_join_relative_richness$upper_ci <- predictions$fit + 1.96 * predictions$se.fit  # Upper bound of 95% CI

# Define specific x values for additional predictions (single points)
x_values <- data.frame(msPAF = c(0.2, 0.05, 0.0))
predicted_y_values <- predict(gam_model, newdata = x_values, se.fit = TRUE)

# Save the specific predictions and their confidence intervals
predicted_y_1 <- predicted_y_values$fit[1]
predicted_y_2 <- predicted_y_values$fit[2]
predicted_y_3 <- predicted_y_values$fit[3]

ci_1_lower <- predicted_y_values$fit[1] - 1.96 * predicted_y_values$se.fit[1]
ci_1_upper <- predicted_y_values$fit[1] + 1.96 * predicted_y_values$se.fit[1]

ci_2_lower <- predicted_y_values$fit[2] - 1.96 * predicted_y_values$se.fit[2]
ci_2_upper <- predicted_y_values$fit[2] + 1.96 * predicted_y_values$se.fit[2]

ci_3_lower <- predicted_y_values$fit[3] - 1.96 * predicted_y_values$se.fit[3]
ci_3_upper <- predicted_y_values$fit[3] + 1.96 * predicted_y_values$se.fit[3]

# Plot the predicted values, confidence intervals, and original data
plot_gam <- ggplot(richtox_join_relative_richness, aes(x = msPAF, y = rel_species_richness)) +
  stat_smooth(method = "gam", se = TRUE, fullrange = FALSE, color = "#cccccc", formula = y ~ s(x, k = 3)) +
  geom_point() +
  # Add points for the specific predicted values at the respective x-values
  geom_point(aes(x = 0.2, y = predicted_y_1), color = "#ff0000", size = 2.5) +
  geom_point(aes(x = 0.05, y = predicted_y_2), color = "#E69F00", size = 2.5) +
  geom_point(aes(x = 0.0, y = predicted_y_3), color = "#56B4E9", size = 2.5) +
  geom_text(aes(x = 0.2, y = 1.5, label = sprintf("%.2f", predicted_y_1)), color = "#ff0000", vjust = -0.9, size = 8) +#x = 0.2, y = -0.15
  geom_text(aes(x = 0.07, y = 1.5, label = sprintf("%.2f", predicted_y_2)), color = "#E69F00", vjust = -0.9, size = 8) +#(x = 0.05, y = 0.05
  geom_text(aes(x = 0.01, y = 1.5, label = sprintf("%.2f", predicted_y_3)), color = "#56B4E9", vjust = -0.9, size = 8) +#x = 0.01, y = 0.4
  labs(x = expression(paste('Mixture Toxic Pressure (msPAF-EC10)')),
       y = 'Relative species richness') +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title = element_text(size = 30),
        strip.text = element_text(size = 22),
        plot.title = element_text(size = 30)) +
  ggtitle("Netherlands") +
  scale_x_continuous(limits = c(0, 0.3))

print(plot_gam)




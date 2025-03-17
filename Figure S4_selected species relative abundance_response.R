
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
                      "tidyr",
                      "mgcv") #add all required packages
for(i in 1:length(list.of.packages)){if(!(list.of.packages[i] %in% rownames(installed.packages()))){install.packages(list.of.packages[i])}} #only download uninstalled packages
lapply(list.of.packages, require, character.only = T) #load all packages


# Read a specific sheet from the Excel file based on the file location path
# Input_data <- read_xlsx(
#   "....../FIGURES DATA.xlsx",
#   sheet = "Figure_S4" 
# )


#Excluding species found in less than 5 sites
abundtox_selected <- Input_data %>% 
  group_by(SpeciesName_updated) %>%  # Group by species and site
  # group_by(SpeciesName_updated, MonsterID_ecofide) %>%  # Group by species and site
  rename(Abund = AMT_CALC_Ecofide) %>%
  mutate(nsites = n(),                    # Calculate the number of sites for each species
         # totalAbund = sum(Abund),          # Calculate total abundance per species per site
         logAbund = log10(Abund + 1)  # Log-transform (adding +1 to avoid log(0))
  ) %>%
  ungroup() %>%
  
  filter(nsites >= 5)#10

# Initialize a list to store species that were skipped
skipped_species <- list()

# Function to fit GAM, predict, and plot for each species
analyze_species <- function(df, species_name) {
  # Filter data for the species
  species_data <- df[df$SpeciesName_updated == species_name, ]
  
  # Check the number of unique msPAF values
  unique_tox_values <- length(unique(species_data$msPAF))
  
  # Skip the species if there are fewer than 3 unique msPAF values (or any threshold you set)
  if (unique_tox_values < 3) {
    skipped_species[[species_name]] <- paste("Skipped due to low unique msPAF values:", unique_tox_values)
    return(NULL)  # Return NULL to indicate skipping
  }
  
  # Otherwise, proceed with model fitting
  k_value <- min(unique_tox_values - 1, 3)  # Set k based on unique msPAF values
  
  # Try fitting the GAM model and catch any errors
  tryCatch({
    gam_model <- gam(rel_species_abundance ~ s(msPAF, k = k_value), data = species_data, family = gaussian(link = "identity"))
    
    # Predict y-values and confidence intervals for the original data
    predictions <- predict(gam_model, newdata = species_data, se.fit = TRUE)
    species_data$predicted_values <- predictions$fit
    species_data$lower_ci <- predictions$fit - 1.96 * predictions$se.fit
    species_data$upper_ci <- predictions$fit + 1.96 * predictions$se.fit
    
    # Specific x-values for predictions
    x_values <- data.frame(msPAF = c(0.0, 0.05, 0.2))
    predicted_y_values <- predict(gam_model, newdata = x_values, se.fit = TRUE)
    
    # Store the x-values and predicted y-values in a dataframe
    prediction_df <- data.frame(
      Species = species_name,
      msPAF = x_values$msPAF,
      Predicted_Y = predicted_y_values$fit
    )
    
    # Plotting
    plot_gam <- ggplot(species_data, aes(x = msPAF, y = rel_species_abundance)) +
      geom_point() +  # Original data points
      geom_line(aes(y = predicted_values), color = "grey80", size = 1) +  # Fitted line
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "grey80", alpha = 0.5) +  # Confidence interval band
      geom_point(aes(x = 0.2, y = predicted_y_values$fit[3]), color = "#ff0000", size = 3) +  # Predicted point for msPAF = 0.2
      geom_point(aes(x = 0.05, y = predicted_y_values$fit[2]), color = "#E69F00", size = 3) +  # Predicted point for msPAF = 0.05
      geom_point(aes(x = 0.0, y = predicted_y_values$fit[1]), color = "#56B4E9", size = 3) +  # Predicted point for msPAF = 0.0
      #geom_text(aes(x = 0.2, y = predicted_y_values$fit[3] + 0.5, label = sprintf("%.3f", predicted_y_values$fit[3])), color = "red", size = 6) +
      #geom_text(aes(x = 0.05, y = predicted_y_values$fit[2] + 0.5, label = sprintf("%.3f", predicted_y_values$fit[2])), color = "blue", size = 6) +
      # geom_text(aes(x = 0.0, y = predicted_y_values$fit[1] + 0.5, label = sprintf("%.3f", predicted_y_values$fit[1])), color = "green", size = 6) +
      labs(x = expression(paste('Mixture Toxic Pressure (msPAF-EC10)')),
           y = 'Relative Species Abundance (log-abundance)') +
      theme(
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title = element_text(size = 25),
        plot.title = element_text(size = 40)
      ) +
      ggtitle(species_name)
    
    return(list(plot = plot_gam, predictions = prediction_df))
  }, error = function(e) {
    skipped_species[[species_name]] <- paste("Skipped due to error:", e$message)
    return(NULL)
  })
}


# Check which species were skipped
print(skipped_species)

# Relative species abundance per sample location vs msPAF per species
richtox_join_new <- abundtox_selected %>%
  group_by(SpeciesName_updated) %>%
  mutate(mean_species_abundance = mean(logAbund, na.rm = TRUE),
         rel_species_abundance = (logAbund - mean_species_abundance) / mean_species_abundance) %>%  # Calculate the mean species abundance per species
  ungroup()

# Example usage for multiple species
species_list <- unique(richtox_join_new$SpeciesName_updated)

# List to store plots
all_plots <- list()

# Initialize an empty dataframe to store all predictions
all_predictions <- data.frame()

# Apply the analysis function for each species
for (species in species_list) {
  result <- analyze_species(richtox_join_new, species)
  all_predictions <- rbind(all_predictions, result$predictions)
  all_plots[[species]] <- result$plot
}



wide_predictions <- all_predictions %>%
  pivot_wider(names_from = msPAF, values_from = Predicted_Y, names_prefix = "msPAF_") %>%
  mutate(
    Change = case_when(
      msPAF_0.2 > msPAF_0 ~ "Increase",
      msPAF_0.2 < msPAF_0 ~ "Decrease",
      msPAF_0.2 == msPAF_0~ "Neutral"
    ),
    Real_Change = msPAF_0 -msPAF_0.2  # Calculate the relative change
  )


# Sort the wide_predictions dataframe by absolute Real_Change (highest to lowest)

sorted_species <- wide_predictions %>%
  #filter(Change == "Increase") %>%  # Select one tye of response
  #arrange(desc(abs(Real_Change))) %>%
  arrange(abs(Real_Change)) %>% 
  select(Species, Real_Change)  # Keep only Species and Real_Change columns

# Print or plot decreasing species
print(sorted_species)

# Initialize an empty list to store the sorted plots
sorted_plots <- list()

# Loop through the sorted species and generate plots
for (i in seq_len(nrow(sorted_species))) {
  species_name <- sorted_species$Species[i]
  real_change_value <- round(sorted_species$Real_Change[i], 3)
  
  # Get the plot and add the Real Change to the title
  result <- analyze_species(richtox_join_new, species_name)
  
  # Modify the plot by adding the title with reduced text size
  plot_with_title <- result$plot + 
    ggtitle(paste(species_name, "(green???red):", real_change_value)) +
    theme(plot.title = element_text(size = 18))  # Adjust the size of the title (e.g., size = 20)
  
  # Add plot to the list
  sorted_plots[[species_name]] <- plot_with_title
}



# Save the wide format dataframe with the Change and Real_Change columns
# Add number of sites information from 

unique_species <- unique(wide_predictions$Species)

species_nsites <- richtox_join_new %>%
  filter(SpeciesName_updated %in% unique_species) %>%  # Filter only for species present in wide_predictions
  select(SpeciesName_updated, nsites) %>%  # Select relevant columns
  distinct()  # Keep unique rows to avoid duplicates

wide_predictions_with_nsites <- wide_predictions %>%
  left_join(species_nsites, by = c("Species" = "SpeciesName_updated"))


# Define the species to plot; the plots are manually rearanged out of R

selected_species<- c("Cloeon dipterum", "Bithynia tentaculata","Sigara striata","Dicrotendipes nervosus","Alboglossiphonia heteroclita", "Chironomus riparius")#decrease_species

# Initialize an empty list to store the plots for the selected species
selected_plots <- list()

# Loop through the selected species and generate plots
for (species_name in selected_species) {
  # Extract the real change value for the species (rounding it to 2 decimals)
  real_change_value <- round(sorted_species$Real_Change[sorted_species$Species == species_name], 2)
  
  # Get the plot for the species and add the Real Change value to the title
  result <- analyze_species(richtox_join_new, species_name)
  
  # Modify the plot by adding the title, increasing the y-axis text size, and increasing dot size
  plot_with_title <- result$plot + 
    ggtitle(paste(species_name, "(blue→red):", real_change_value)) +
    
    # Increase the size of the dots
    geom_point(size = 2) +
    theme_gray()+
    
    # Add the predicted value label for msPAF = 0
    geom_text(data = result$predictions[result$predictions$msPAF == 0,], 
              aes(x = msPAF, y = 2.7, label = sprintf("%.2f", Predicted_Y)),
              color = "#56B4E9", size = 6, vjust = -0.3) + 
    
    # Add the predicted value label for msPAF = 0.05
    geom_text(data = result$predictions[result$predictions$msPAF == 0.05,], 
              aes(x = msPAF, y = 2.5, label = sprintf("%.2f", Predicted_Y)),
              color = "#E69F00", size = 6, vjust = -0.3) +  # Slightly adjust y-position to prevent overlap
    
    # Add the predicted value label for msPAF = 0.2(or other values)
    geom_text(data = result$predictions[result$predictions$msPAF == 0.2,], 
              aes(x = msPAF, y = 2.2, label = sprintf("%.2f", Predicted_Y)),
              color = "#ff0000", size = 6, vjust = -0.3) +
    theme(
      plot.title = element_text(size = 25),  # Title size
      axis.text.y = element_text(size = 25),  # Increase y-axis text size
      axis.text.x = element_text(size = 25),  # Adjust x-axis text size if needed
      axis.title = element_text(size = 25)    # Increase axis title size
    ) +
    scale_x_continuous(limit=c(0,0.5))+
    scale_y_continuous(limit=c(-1,3))
  # Add plot to the list
  selected_plots[[species_name]] <- plot_with_title
}

# Save the plots as JPEG images
for (species_name in names(selected_plots)) {
  jpeg_filename <- paste0(species_name, "_GAM_Plot.jpeg")
  jpeg(jpeg_filename, width = 610, height = 500)  # Adjust dimensions as needed#1000 by 600
  print(selected_plots[[species_name]])
  dev.off()
}

selected_plots




setwd("../")


list.of.packages <- c("datasets",
                      "openxlsx",
                      "readxl",
                      "gridExtra",
                      "writexl",
                      "ggplot2",
                      "plotly",
                      "tidyverse",
                      "data.table",
                      "forcats",
                      "dplyr",
                      "Hmisc",
                      "dplyr",
                      "car",
                      "stringr",
                      "mgcv") #add all required packages
for(i in 1:length(list.of.packages)){if(!(list.of.packages[i] %in% rownames(installed.packages()))){install.packages(list.of.packages[i])}} #only download uninstalled packages
lapply(list.of.packages, require, character.only = T) #load all packages



# Read a specific sheet from the Excel file
Input_data <- read_xlsx(
  "........../FIGURES DATA.xlsx",
  sheet = "Statistical tests" 
)

 

# Initialize a list to store the results from each region
results_list <- list()


# Vector of different numbers of quantile groups to analyze
group_numbers <- c(10,15, 20, 25, 30, 35, 40, 45, 50)


# Loop through each region
for (region in unique(Input_data$DataSource)) {
  
  # Filter data for the current region
  filtered_region <- Input_data %>% filter(DataSource == region)
  
  # Skip if all msPAF values are the same
  if (n_distinct(filtered_region$msPAF) <= 1) {
    next
  }
  
  
  # Count unique sites
  unique_sites_count <- filtered_region %>%
    summarise(unique_sites = n_distinct(MonsterID_ecofide))
  
  print(unique_sites_count)
  
  
  # Initialize a list to store the results from each number of groups for the current region
  region_results_list <- list()
  
  # Loop through each specified number of groups
  for (g in group_numbers) {
    print(paste("Analysis for", g, "groups"))
    
    # Excluding species found in less than 10 sites
    abundtox <- filtered_region %>%
      group_by(TWNcode) %>%
      mutate(nsites = n()) %>%
      ungroup() %>%
      filter(nsites >= 5) # 10
    
    # Species abundance
    abundtox <- abundtox %>%
      rename(
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
    
    # select species that are only being found in GRP1
    sensitive_species <- abundtox %>%
      filter(category.fact == "Group1")
    
    Species.list <- unique(sensitive_species$TWNcode)
    
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
        msPAF.mean= mean(msPAF),#median
        count.sample = n(),
        .groups = 'drop'
      )
    
    reference_species_count <- richtox_join_new %>%
      summarise(reference_species_count = round(mean(Unique_TWNcodes), 2)) %>%
      pull(reference_species_count)
    
    reference_msPAF <- Total_Abund$msPAF.mean[Total_Abund$category.fact == "Group1"]
    
    # Calculate relative species richness
    Total_Abund$ODF <- (reference_species_count - Total_Abund$Total) / reference_species_count
    
    # Create a new variable (inverse of ODF) by multiplying ODF by -1
    Total_Abund$ODF_negative <- Total_Abund$ODF * -1
    
    # Calculate relative msPAF
    Total_Abund$delta_msPAF <- Total_Abund$msPAF.mean - reference_msPAF
    
  
    
    # Store the filtered data frame in the list with a dynamic name
    region_results_list[[paste("Groups", g)]] <- Total_Abund # data_nl_filtered
    
  }
  
  # Combine the results from all groups for the current region into one data frame
  combined_region_results <- bind_rows(region_results_list, .id = "GroupSize")
  
  unique_ordered_GroupSize <- combined_region_results %>% 
    pull(GroupSize) %>% 
    unique() %>% 
    sort() 
  
  combined_region_results <- combined_region_results %>%
    mutate(GroupNumber = as.numeric(str_extract(GroupSize, "\\d+"))) %>%
    
    mutate(GroupSize = factor(GroupSize, levels = unique_ordered_GroupSize))
  
  # Store the combined results for the current region
  results_list[[region]] <- combined_region_results
  
}

# Combine the results from all regions into one data frame
combined_results <- bind_rows(results_list, .id = "DataSource")

##########################################################

# Define unique water authorities
water_authorities <- unique(combined_results$DataSource)

# Open a single PDF device to save the plots with specified width and height
pdf("Final_plot_WS.pdf", width = 14, height = 10)

# Loop through each DataSource
for (authority in water_authorities) {
  # Filter the data for the current DataSource
  combined_results_filtered <- combined_results %>%
    filter(DataSource == authority)
  
  # Generate and print GAM plot
  ggplot(combined_results_filtered, aes(x = delta_msPAF, y = ODF_negative)) +
    geom_point() +
    facet_wrap(~GroupSize) +
    stat_smooth(method = "gam", se = FALSE, fullrange = TRUE, color = "blue", formula = y ~ s(x, k = 3)) +
    ggtitle(paste("GAM Smoothing -", authority))
  
  
  # Initialize list to store results for each GroupSize
  region_results_list <- list()
  
  # Combine predicted values with the original data
  for (bin in unique(combined_results_filtered$GroupSize)) {
    subset_data <- subset(combined_results_filtered, GroupSize == bin)
    gam_model <- gam(ODF_negative ~ s(delta_msPAF, k = 3), data = subset_data, family = gaussian(link = "identity"))
    
    # Define the x values for prediction
    x_value_1 <- 0.2
    x_value_2 <- 0.05
    x_value_3 <- 0.0
    
    # Predict the y-values using gam for each GroupSize at the new x values
    predicted_y_1 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_1))
    predicted_y_2 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_2))
    predicted_y_3 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_3))
    
    # Assign predicted values to corresponding rows in combined_results_filtered
    combined_results_filtered[combined_results_filtered$GroupSize == bin, "predicted_Y_1"] <- predicted_y_1
    combined_results_filtered[combined_results_filtered$GroupSize == bin, "predicted_Y_2"] <- predicted_y_2
    combined_results_filtered[combined_results_filtered$GroupSize == bin, "predicted_Y_3"] <- predicted_y_3
    
    
    # Store the results in the list
    region_results_list[[bin]] <- combined_results_filtered[combined_results_filtered$GroupSize == bin, ]
  }
  
  # Combine the results from all groups for the current region into one data frame
  combined_region_results <- bind_rows(region_results_list, .id = "GroupSize")
  
  # Ensure unique levels for GroupSize, ordered by GroupNumber
  combined_region_results <- combined_region_results %>%
    mutate(GroupNumber = as.numeric(str_extract(GroupSize, "\\d+"))) %>%
    mutate(GroupSize = factor(GroupSize, levels = unique(combined_region_results[order(combined_region_results$GroupNumber), ]$GroupSize)))
  
  # Now plot the data with predicted values
  Final_plot <- ggplot(combined_region_results, aes(x = delta_msPAF, y = ODF_negative)) +
    stat_smooth(method = "gam", se = TRUE, fullrange = FALSE, color = "#cccccc", formula = y ~ s(x, k = 3)) +
    geom_point() +
    geom_point(aes(x = rep(0.2, nrow(combined_region_results)), y = predicted_Y_1), color = "red", size = 2) +
    geom_point(aes(x = rep(0.05, nrow(combined_region_results)), y = predicted_Y_2), color = "blue", size = 2) +
    geom_point(aes(x = rep(0.0, nrow(combined_region_results)), y = predicted_Y_3), color = "green", size = 2) +
    geom_text(aes(x = rep(0.2, nrow(combined_region_results)), y = -0.1, label = sprintf("%.2f", predicted_Y_1)),
              color = "red", vjust = -0.9, size = 6) +
    geom_text(aes(x = rep(0.08, nrow(combined_region_results)), y = 0.2, label = sprintf("%.2f", predicted_Y_2)),
              color = "blue", vjust = -0.9, size = 6) +
    geom_text(aes(x = rep(0.04, nrow(combined_region_results)), y = 0.3, label = sprintf("%.2f", predicted_Y_3)),
              color ="green", vjust = -0.9, size = 6) +
    facet_wrap(~GroupSize, ncol = 3) +
    labs(x = expression(paste('Predicted toxic pressure (msPAF)')),
         y = 'Observed damage (fraction species lost)') +
    theme(axis.text.x = element_text(size = 22),
          axis.text.y = element_text(size = 22),
          axis.title = element_text(size = 25),
          strip.text = element_text(size = 22),
          plot.title = element_text(size = 40)) +
    scale_y_continuous(limits = c(-0.6, 0.6))+
    scale_x_continuous(limits = c(0.0, 0.6))+
    ggtitle(authority)
  
  # Print GAM plot to the PDF device
  print(Final_plot)
  
}

# Close the PDF device
dev.off()



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
                      "stringr",
                      "mgcv") #add all required packages
for(i in 1:length(list.of.packages)){if(!(list.of.packages[i] %in% rownames(installed.packages()))){install.packages(list.of.packages[i])}} #only download uninstalled packages
lapply(list.of.packages, require, character.only = T) #load all packages

# Read a specific sheet from the Excel file based on the file location path

Input_data <- read_xlsx(
  "......../FIGURES DATA.xlsx",
  sheet = "Figure_S10_Netherlands" 
)


#remove alien/invasive species
invasive_species <- c(
  "Bithynia tentaculata",
  "Branchiura sowerbyi",
  "Branchiodrilus hortensis",
  "Chelicorophium curvispinum",
  "Chelicorophium robustum",
  "Cordylophora caspia",
  "Corbicula fluminalis",
  "Corbicula fluminea",
  "Crangonyx pseudogracilis",
  "Dendrocoelum romanodanubiale",
  "Dikerogammarus haemobaphes",
  "Dikerogammarus villosus",
  "Dreissena polymorpha",
  "Echinogammarus berilloni",
  "Echinogammarus trichiatus",
  "Eriocheir sinensis",
  "Gammarus pulex",
  "Gammarus tigrinus",
  "Gyraulus parvus",
  "Helobdella stagnalis",
  "Hemimysis anomala",
  "Hirudo medicinalis",
  "Hypania invalida",
  "Jaera istri",
  "Limnodrilus cervix",
  "Limnodrilus maumeensis",
  "Lithoglyphus naticoides",
  "Limnomysis benedeni",
  "Lophopus crystallinus",
  "Menetus dilatatus",
  "Musculium transversum",
  "Mytilopsis leucophaeata",
  "Physella acuta",
  "Planorbarius corneus",
  "Potamopyrgus antipodarum",
  "Potamothrix bavaricus",
  "Potamothrix heuscheri",
  "Potamothrix moldaviensis",
  "Procambarus clarkii",
  "Proasellus coxalis",
  "Proasellus meridianus",
  "Viviparus viviparus"
)

# Filter out the species from the dataset
filtered_data <- Input_data[!Input_data$SpeciesName_updated %in% invasive_species, ]

# View the filtered dataset
head(filtered_data)

# Initialize a list to store the results from each number of groups
results_list <- list()

group_numbers <- c(10, 15, 20, 25, 30, 35, 40, 45, 50)

# Loop through each specified number of groups
for (g in group_numbers) {
  print(paste("Analysis for", g, "groups"))
  
  # Excluding species found in less than 5 sites (rare species)
  abundtox <- filtered_data %>%  
    group_by(TWNcode) %>%
    mutate(nsites = n()) %>%
    ungroup()  %>%
    filter(nsites >= 5)
  
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
  
  # Exclude opportunistic species (species appearing at higher msPAF)
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
  unique_tox_info <- abundtox_selected %>%
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
      msPAF.mean= mean(msPAF),
      count.sample = n(),
      .groups = 'drop'
    )
  
  # Assuming Group1 is always the reference group for mean(msPAF) and total # species counted
 
  
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
  results_list[[paste("Groups", g)]] <- Total_Abund 
}

# Combine the list of data frames into one, adding an identifier column 'GroupSize'
combined_df <- bind_rows(results_list, .id = "GroupSize")

# # Ensure GroupSize is a factor with levels ordered numerically

combined_df <- combined_df %>%
  mutate(GroupNumber = as.numeric(str_extract(GroupSize, "\\d+")),
         bin_number = paste0(GroupNumber, " bins"))


# Ensure unique levels for GroupSize, ordered by GroupNumber
unique_ordered_GroupSize <- unique(combined_df[order(combined_df$GroupNumber), ]$GroupSize)

combined_df <- combined_df %>%
  mutate(GroupSize = factor(GroupSize, levels = unique_ordered_GroupSize))

### # Add a scenario column to each dataset for identification
excluded_df <- combined_df %>% mutate(scenario = "Species excluded")

pdf_scatterplot<-ggplot(data = excluded_df)+
  geom_point(aes(x=delta_msPAF, y=ODF_negative),size=2)+
  labs(x = expression(paste('delta msPAF[chronic EC10]')), 
       y = 'Disppeared species fraction') +
  facet_wrap(~GroupSize, ncol = 3)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 12))

pdf_scatterplot

plot_gam <- ggplot(excluded_df, aes(x = delta_msPAF, y = ODF_negative)) +
  geom_point() +
  facet_wrap(~bin_number) +
  stat_smooth(method = "gam", se = FALSE, fullrange = TRUE, color = "blue", formula = y ~ s(x, k = 3)) +
  ggtitle("GAM Smoothing")+ 
  scale_x_continuous(limits = c(0, 0.25))
print(plot_gam)


# Combine predicted values with the original data using a loop

for (bin in unique(excluded_df$bin_number)) {
  subset_data <- subset(excluded_df, bin_number == bin)
  gam_model <- gam(ODF_negative ~ s(delta_msPAF, k = 3), data = subset_data, family = gaussian(link = "identity"))
  
  # Define the x values for prediction
  x_value_1 <- 0.2
  x_value_2 <- 0.05
  x_value_3 <- 0.0
  
  # Predict the y-values using gam for each bin_number at the new x values
  predicted_y_1 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_1))
  predicted_y_2 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_2))
  predicted_y_3 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_3))
  
  # Assign predicted values to corresponding rows in excluded_df
  excluded_df[excluded_df$bin_number == bin, "predicted_Y_1"] <- predicted_y_1
  excluded_df[excluded_df$bin_number == bin, "predicted_Y_2"] <- predicted_y_2
  excluded_df[excluded_df$bin_number == bin, "predicted_Y_3"] <- predicted_y_3
}

# Now plot the data with predicted values

plot_gam_exclude <- ggplot(excluded_df, aes(x = delta_msPAF, y = ODF_negative)) +
  # scale_x_continuous(limits = c(0, 0.3)) +
  stat_smooth(method = "gam", se = TRUE, fullrange = FALSE, color = "#cccccc", formula = y ~ s(x, k = 3)) +
  geom_point() +
  geom_point(aes(x = rep(0.2, nrow(excluded_df)), y = predicted_Y_1), color = "red", size = 2) +
  geom_point(aes(x = rep(0.05, nrow(excluded_df)), y = predicted_Y_2), color = "blue", size = 2) +
  geom_point(aes(x = rep(0.0, nrow(excluded_df)), y = predicted_Y_3), color = "green", size = 2) +
  #geom_point(x = 0, y = 0, color = "brown", size = 3)+ # reference point
  geom_text(aes(x = rep(0.2, nrow(excluded_df)), y = 0.01, label = sprintf("%.2f", predicted_Y_1)),
            color = "red", vjust = -0.9, size = 6) +
  geom_text(aes(x = rep(0.08, nrow(excluded_df)), y = 0.05, label = sprintf("%.2f", predicted_Y_2)),
            color = "blue", vjust = -0.9, size = 6) +
  geom_text(aes(x = rep(0.05, nrow(excluded_df)), y = 0.2, label = sprintf("%.2f", predicted_Y_3)),
            color = "green", vjust = -0.9, size = 6) +
  facet_wrap(~bin_number, ncol = 3) +#3
  labs(x = expression(paste('Mixture toxic pressure (msPAF-EC10)')),
       y = 'Relative Species Richness (PDF)') +
  theme(axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title = element_text(size = 30),
        strip.text = element_text(size = 22),
        plot.title = element_text(size = 30)) +
  scale_y_continuous(limits = c(-0.4, 0.4))+
  ggtitle("Netherlands:Rare,opportunistic and invasive species Species excluded")


print(plot_gam_exclude)


### Scenario two not excluding species

# Initialize a list to store the results from each number of groups
results_list <- list()

group_numbers <- c(10, 15, 20, 25, 30, 35, 40, 45, 50)



# Loop through each specified number of groups
for (g in group_numbers) {
  print(paste("Analysis for", g, "groups"))
  
  # Excluding species found in less than 5 sites (rare species)
  abundtox <- Input_data %>%  
    group_by(TWNcode) %>%
    mutate(nsites = n()) %>%
    ungroup()  %>%
    filter(nsites >= 5)
  
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
  
  # Note: No filtering for opportunistic species here
  
  # Count unique species per site, per category msPAF group
  richnessTWN <- abundtox %>% 
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
      msPAF.mean = mean(msPAF),
      count.sample = n(),
      .groups = 'drop'
    )
  
  # Assuming Group1 is always the reference group for mean(msPAF) and total # species counted
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
  results_list[[paste("Groups", g)]] <- Total_Abund 
}

# Combine the list of data frames into one, adding an identifier column 'GroupSize'
combined_df <- bind_rows(results_list, .id = "GroupSize") %>%
  mutate(GroupNumber = as.numeric(str_extract(GroupSize, "\\d+")),
         bin_number = paste0(GroupNumber, " bins"))

# Ensure unique levels for GroupSize, ordered by GroupNumber
unique_ordered_GroupSize <- unique(combined_df[order(combined_df$GroupNumber), ]$GroupSize)

combined_df <- combined_df %>%
  mutate(GroupSize = factor(GroupSize, levels = unique_ordered_GroupSize))

### # Add a scenario column to each dataset for identification
not_excluded_df <- combined_df %>% mutate(scenario = "All taxa")


print(pdf_scatterplot)
pdf_scatterplot<-ggplot(data = not_excluded_df)+
  geom_point(aes(x=delta_msPAF, y=ODF_negative),size=2)+
  labs(x = expression(paste('delta msPAF[chronic EC10]')), 
       y = 'Disppeared species fraction') +
  facet_wrap(~GroupSize, ncol = 3)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 12))

pdf_scatterplot

plot_gam <- ggplot(not_excluded_df, aes(x = delta_msPAF, y = ODF_negative)) +
  geom_point() +
  facet_wrap(~bin_number) +
  stat_smooth(method = "gam", se = FALSE, fullrange = TRUE, color = "blue", formula = y ~ s(x, k = 3)) +
  ggtitle("GAM Smoothing")+ 
  scale_x_continuous(limits = c(0, 0.25))
print(plot_gam)

# Combine predicted values with the original data using a loop

for (bin in unique(not_excluded_df$bin_number)) {
  subset_data <- subset(not_excluded_df, bin_number == bin)
  gam_model <- gam(ODF_negative ~ s(delta_msPAF, k = 3), data = subset_data, family = gaussian(link = "identity"))
  
  # Define the x values for prediction
  x_value_1 <- 0.2
  x_value_2 <- 0.05
  x_value_3 <- 0.0
  
  # Predict the y-values using gam for each bin_number at the new x values
  predicted_y_1 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_1))
  predicted_y_2 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_2))
  predicted_y_3 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_3))
  
  # Assign predicted values to corresponding rows in not_excluded_df
  not_excluded_df[not_excluded_df$bin_number == bin, "predicted_Y_1"] <- predicted_y_1
  not_excluded_df[not_excluded_df$bin_number == bin, "predicted_Y_2"] <- predicted_y_2
  not_excluded_df[not_excluded_df$bin_number == bin, "predicted_Y_3"] <- predicted_y_3
}

# Now plot the data with predicted values

plot_gam_notexclude <- ggplot(not_excluded_df, aes(x = delta_msPAF, y = ODF_negative)) +
  # scale_x_continuous(limits = c(0, 0.4)) +
  stat_smooth(method = "gam", se = TRUE, fullrange = FALSE, color = "#cccccc", formula = y ~ s(x, k = 3)) +
  geom_point() +
  geom_point(aes(x = rep(0.2, nrow(not_excluded_df)), y = predicted_Y_1), color = "red", size = 2) +
  geom_point(aes(x = rep(0.05, nrow(not_excluded_df)), y = predicted_Y_2), color = "blue", size = 2) +
  geom_point(aes(x = rep(0.0, nrow(not_excluded_df)), y = predicted_Y_3), color = "green", size = 2) +
  # geom_point(x = 0, y = 0, color = "brown", size = 3)+ # reference point
  geom_text(aes(x = rep(0.2, nrow(not_excluded_df)), y = 0.01, label = sprintf("%.2f", predicted_Y_1)),
            color = "red", vjust = -0.9, size = 6) +
  geom_text(aes(x = rep(0.08, nrow(not_excluded_df)), y = 0.05, label = sprintf("%.2f", predicted_Y_2)),
            color = "blue", vjust = -0.9, size = 6) +
  geom_text(aes(x = rep(0.05, nrow(not_excluded_df)), y = 0.2, label = sprintf("%.2f", predicted_Y_3)),
            color = "green", vjust = -0.9, size = 6) +
  facet_wrap(~bin_number, ncol = 3) +#3
  labs(x = expression(paste('Mixture toxic pressure (msPAF-EC10)')),
       y = 'Relative Species Richness (PDF)') +
  theme(axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title = element_text(size = 30),
        strip.text = element_text(size = 22),
        plot.title = element_text(size = 30)) +
  scale_y_continuous(limits = c(-0.4, 0.4))+
  ggtitle("Netherlands: All species included") 


print(plot_gam_notexclude)

##join the separate scenarios output
combined_results <- bind_rows(excluded_df, not_excluded_df)

# Create the new values for predicted_Y_1, predicted_Y_2, and predicted_Y_3
combined_results <- combined_results %>%
  mutate(
    new_predicted_Y_1 = if_else(scenario == "All taxa", predicted_Y_1 + 0.16, predicted_Y_1),
    new_predicted_Y_2 = if_else(scenario == "All taxa", predicted_Y_2 + 0.08, predicted_Y_2),
    new_predicted_Y_3 = if_else(scenario == "All taxa", predicted_Y_3 + 0.08, predicted_Y_3)
  )

# Plotting with two geom_smooth layers
#plot_gam_Combine <- ggplot(combined_results, aes(x = delta_msPAF, y = ODF_negative)) +
plot_gam_Combine <- ggplot(combined_results, aes(x = delta_msPAF, y = ODF_negative, color = scenario)) +
  #scale_x_continuous(limits = c(0, 0.4)) +
  # Apply stat_smooth() for the 'exclude' scenario
  geom_smooth(data = combined_results[combined_results$scenario == "Species excluded", ], 
              method = "gam", se = TRUE, fullrange = FALSE, 
              color = "black", formula = y ~ s(x, k = 3)) +
  # Apply stat_smooth() for the 'no_exclude' scenario
  geom_smooth(data = combined_results[combined_results$scenario == "All taxa", ], 
              method = "gam", se = TRUE, fullrange = FALSE, 
              color = "#E6399B", formula = y ~ s(x, k = 3)) +
  scale_color_manual(values = c("All taxa" = "#E6399B", 
                                "Species excluded" = "black")) + 
  geom_point() +
  geom_point(aes(x = 0.2, y = predicted_Y_1), color = "#ff0000", size = 1.5) +
  geom_point(aes(x = 0.05, y = predicted_Y_2), color = "#E69F00", size = 1.5) +
  geom_point(aes(x = 0.0, y = predicted_Y_3), color = "#56B4E9", size = 1.5) +
  #geom_point(x = 0, y = 0, color = "brown", size = 3)+ # reference point
  geom_text(aes(x = 0.2, y = new_predicted_Y_1, label = sprintf("%.2f", predicted_Y_1)),
            color = "#ff0000", vjust = -2.5, size = 6) +
  geom_text(aes(x = 0.1, y = new_predicted_Y_2, label = sprintf("%.2f", predicted_Y_2)),
            color = "#E69F00", vjust = -0.99, size = 6) +
  geom_text(aes(x = 0.03, y = new_predicted_Y_3, label = sprintf("%.2f", predicted_Y_3)),
            color = "#56B4E9", vjust = -1, size = 6) +
  facet_wrap(~bin_number, ncol = 3) +#3
  labs(x = expression(paste('Mixture toxic pressure (msPAF-EC10)')),
       y = 'Relative Species Richness (PDF)') +
  theme(axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title = element_text(size = 30),
        strip.text = element_text(size = 22),
        plot.title = element_text(size = 30),
        legend.position = "bottom", 
        legend.title = element_text(size = 20),  # Increase legend title size
        legend.text = element_text(size = 16))+
  ggtitle("Netherlands")


print(plot_gam_Combine)

# Create a summary table of predicted_Y_1 and predicted_Y_3 per scenario and bin_number
summary_table <- combined_results %>%
  group_by(scenario, bin_number) %>%
  summarise(
    mean_pred_Y_1 = mean(predicted_Y_1, na.rm = TRUE),
    mean_pred_Y_3 = mean(predicted_Y_3, na.rm = TRUE),
    diff_Y3_Y1 = mean_pred_Y_3 - mean_pred_Y_1,
    .groups = "drop"
  )



# View the table
print(summary_table)


##
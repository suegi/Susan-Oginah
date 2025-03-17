
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
# Input_data <- read_xlsx(
#   "....../FIGURES DATA.xlsx",
#   sheet = "Figure_S8" 
# )


#Ensure uniqueness before joining
Input_data_2<- Input_data %>%
  left_join(
    Input_data %>%
      select(TWNcode, SpeciesName_updated) %>%
      distinct(TWNcode, .keep_all = TRUE),  # Keep only the first unique id
    by = "TWNcode"
  )



# Initialize a list to store the results from each number of groups
results_list <- list()

group_numbers <- c(10,15, 20, 25, 30, 35, 40, 45, 50)

# Initialize a list to store the results from each number of groups
results_list <- list()

# Loop through each specified number of groups
for (g in group_numbers) {
  print(paste("Analysis for", g, "groups"))
  
  # Excluding species found in less than 5 sites (rare species)
  abundtox <- Input_data_2 %>%  
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
  
  # Assuming Group1 is always the reference group for mean(msPAF)
 
  
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

pdf_scatterplot<-ggplot(data = combined_df)+
  geom_point(aes(x=delta_msPAF, y=ODF_negative),size=2)+
  labs(x = expression(paste('delta msPAF[chronic EC10]')), 
       y = 'Disppeared species fraction') +
  facet_wrap(~GroupSize, ncol = 3)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 12))

pdf_scatterplot

plot_gam <- ggplot(combined_df, aes(x = delta_msPAF, y = ODF_negative)) +
  geom_point() +
  facet_wrap(~bin_number) +
  stat_smooth(method = "gam", se = FALSE, fullrange = TRUE, color = "blue", formula = y ~ s(x, k = 3)) +
  ggtitle("GAM Smoothing")+ 
  scale_x_continuous(limits = c(0, 0.25))
print(plot_gam)

# Combine predicted values with the original data using a loop

for (bin in unique(combined_df$bin_number)) {
  subset_data <- subset(combined_df, bin_number == bin)
  gam_model <- gam(ODF_negative ~ s(delta_msPAF, k = 3), data = subset_data, family = gaussian(link = "identity"))
  # Constrained GAM fit using SCAM
  #gam_model <- scam(ODF_negative ~ s(delta_msPAF, k = 4, bs = "mpd"), data = subset_data)#3
  
   
  
  # Define the x values for prediction
  x_value_1 <- 0.0927# 0.5
  x_value_2 <- 0.0198# 0.05
  x_value_3 <- 0.0
  
  # Predict the y-values using gam for each bin_number at the new x values
  predicted_y_1 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_1))
  predicted_y_2 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_2))
  predicted_y_3 <- predict(gam_model, newdata = data.frame(delta_msPAF = x_value_3))
  
  # Assign predicted values to corresponding rows in combined_df
  combined_df[combined_df$bin_number == bin, "predicted_Y_1"] <- predicted_y_1
  combined_df[combined_df$bin_number == bin, "predicted_Y_2"] <- predicted_y_2
  combined_df[combined_df$bin_number == bin, "predicted_Y_3"] <- predicted_y_3
}

# Now plot the data with predicted values
plot_gam <- ggplot(combined_df, aes(x = delta_msPAF, y = ODF_negative)) +
  #stat_smooth(method = "scam", se = TRUE, fullrange = FALSE, color = "#cccccc", formula = y ~ s(x, k = 4, bs = "mpd")) +#3
  stat_smooth(method = "gam", se = TRUE, fullrange = FALSE, color = "#cccccc", formula = y ~ s(x, k = 4)) +#3
  geom_point() +
  geom_point(aes(x = rep(0.0927, nrow(combined_df)), y = predicted_Y_1), color = "#ff0000", size = 2) +
  geom_point(aes(x = rep(0.0198, nrow(combined_df)), y = predicted_Y_2), color = "#E69F00", size = 2) +
  geom_point(aes(x = rep(0.0, nrow(combined_df)), y = predicted_Y_3), color = "#56B4E9", size = 2) +
  geom_text(aes(x = rep(0.12, nrow(combined_df)), y = 0.1, label = sprintf("%.2f", predicted_Y_1)),
            color = "#ff0000", vjust = -0.9, size = 6) +
  geom_text(aes(x = rep(0.04, nrow(combined_df)), y = 0.05, label = sprintf("%.2f", predicted_Y_2)),
            color = "#E69F00", vjust = -0.9, size = 6) +
  geom_text(aes(x = rep(0.01, nrow(combined_df)), y = 0.2, label = sprintf("%.2f", predicted_Y_3)),
            color = "#56B4E9", vjust = -0.9, size = 6) +
  facet_wrap(~bin_number, ncol = 3) +
  labs(x = expression(paste('Mixture toxic pressure (msPAF-EC50)')),
       y = 'Relative Species Richness (PDF)') +
  theme(axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 21),
        axis.title = element_text(size = 28),
        axis.title.x = element_text(size = 28, margin = margin(r = 20)),  # Adjust the right margin
        strip.text = element_text(size = 22),
        panel.spacing.x = unit(0.4, "cm"),
        plot.title = element_text(size = 30)) +
  #scale_y_continuous(limits = c(-0.4, 0.4))+
  ggtitle("Netherlands") 
# +
#   geom_segment(aes(x = 0, xend = 0.35, y = predicted_Y_3, yend = predicted_Y_3), 
#                color = "#56B4E9", linetype = "solid", size = 0.8)   # Horizontal line


print(plot_gam)

## calculate the Real change

Real_change_EC50 <- combined_df %>%
  group_by(bin_number) %>%
  summarise(PDF_EC50 = mean(predicted_Y_3) - mean(predicted_Y_1))


# Save the plot using ggsave with high resolution
ggsave("line_graph_plot.png", plot = plot_gam, device = "png", dpi = 300, width = 10, height = 8)



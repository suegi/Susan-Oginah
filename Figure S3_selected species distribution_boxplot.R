
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
                      "scales",
                      "mgcv") #add all required packages
for(i in 1:length(list.of.packages)){if(!(list.of.packages[i] %in% rownames(installed.packages()))){install.packages(list.of.packages[i])}} #only download uninstalled packages
lapply(list.of.packages, require, character.only = T) #load all packages



# Read a specific sheet from the Excel file based on the file location path
# Input_data <- read_xlsx(
#   "....../FIGURES DATA.xlsx",
#   sheet = "Figure_S3" 
# )



# Select phylum to plot
Input_data_Arthropods<- Input_data %>%
  
filter(phylum == "Arthropoda") 

Input_data_Arthropods  
  
# Reorder species based on median proportion
species_order <- Input_data_Arthropods %>%
  group_by(SpeciesName_updated) %>%
  summarise(MedianProportion = mean(Proportion, na.rm = TRUE),
            TotalPoints = n()) %>%
  arrange(desc(MedianProportion)) %>%
  pull(SpeciesName_updated)

# Calculate the median and total data points per species
species_stats <- Input_data_Arthropods %>%
  group_by(SpeciesName_updated) %>%
  summarise(MedianProportion = mean(Proportion, na.rm = TRUE),
            TotalPoints = n())  # Total number of data points for each species


# Merge the statistics back to the original dataset
Input_data_Arthropods <- Input_data_Arthropods %>%
  left_join(species_stats, by = "SpeciesName_updated")


# Define colors for each phylum
#phylum_colors <- c("Arthropoda" = "#DA70D6", "Annelida" = "#00BFFF", "Mollusca" = "#29AB87", "Others" = "#FF8000")

Input_data_Arthropods$SpeciesName_updated <- factor(Input_data_Arthropods$SpeciesName_updated, levels = species_order)



# Create the violin plot of species proportion across assemblages

Violin_plot <- ggplot(data = Input_data_Arthropods, aes(x = SpeciesName_updated, y = Proportion, fill = phylum)) +
  geom_violin(trim = FALSE, alpha = 0.6) +  # Violin plot to show proportion distribution
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +  # Boxplot inside for summary stats
  geom_text(data = species_stats, aes(x = SpeciesName_updated, y = 50, 
                                      label = paste("μ=", round(MedianProportion, 0), "\nSites=", TotalPoints)),
            position = position_nudge(x = 0.3),  # Shift text slightly to the right
            size = 8, color = "black", inherit.aes = FALSE)+
  labs(x = 'Species assemblages', # Remove x-axis label
       y = 'Species relative abundance across sites (%)') +  # Updated y-axis label
  scale_y_continuous(limits = c(0, 100)) +  # Scale in percentage
  facet_wrap(~ SpeciesName_updated, scales = "free_x", nrow = 1) +  # Facet without x-axis labels
  theme(axis.text.x = element_blank(),  # Remove species names
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26),
        strip.text = element_text(size = 27),  # Keep facet labels
        legend.title = element_text(size = 32),
        legend.text = element_text(size = 30)) +
  scale_fill_manual(values = "#DA70D6")  # Color for Arthropoda

Violin_plot





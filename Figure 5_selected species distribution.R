
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



# Read a specific sheet from the Excel file based on the location path
Input_data <- read_xlsx(
  "......../FIGURES DATA.xlsx",
  sheet = "Figure_5" 
)


#### Filter and create plot for each taxa phylum separately
Input_data_Arthropods <- Input_data %>%
  filter(phylum =="Arthropoda")


# Calculate the total and create labels for each species
Input_data_Arthropods <- Input_data_Arthropods %>%
  group_by(SpeciesName_updated) %>%
  mutate(Total = round(sum(AverageAbund), 0),
         labels.taxa = paste0("n = ", comma(Total))) %>%  # Use scales::comma() for formatted numbers
  ungroup()

# Reorder SpeciesName_updated based on Total
Input_data_Arthropods$SpeciesName_updated <- factor(
  Input_data_Arthropods$SpeciesName_updated,
  levels = Input_data_Arthropods %>%
    group_by(SpeciesName_updated) %>%
    summarise(Total = sum(AverageAbund)) %>%
    arrange(desc(Total)) %>%
    pull(SpeciesName_updated),
  ordered = TRUE
)

# Define colors for each phylum
#phylum_colors <- c("Arthropoda" = "#DA70D6", "Annelida" = "#00BFFF", "Mollusca" = "#29AB87", "Others" = "#FF8000")

# Create the plot
Athropods_plot <- ggplot(data = Input_data_Arthropods, aes(x = msPAF, y = AverageAbund, color = phylum)) +
  geom_point(size = 2, alpha = 0.2) +
  geom_jitter() +
  labs(x = expression(paste('Mixture toxic pressure (msPAF-EC10)')),
       y = 'Species abundance') +
  scale_x_continuous(limits = c(0, 0.8)) +
  scale_y_continuous(limits = c(1, 10000), trans = 'log10') +
  geom_text(data = distinct(Input_data_Arthropods, phylum, SpeciesName_updated, labels.taxa),
            aes(x = 0.2, y = 8500, label = labels.taxa), color = "black", size = 8, inherit.aes = FALSE) +
  facet_grid(phylum ~ SpeciesName_updated) +
  theme(panel.spacing = unit(0.5, "cm"),
        strip.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 24),
        legend.key.size = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_text(size = 32),
        legend.text = element_text(size = 30)) +
  scale_color_manual(values = "#DA70D6")  # Manually set color
  

Athropods_plot



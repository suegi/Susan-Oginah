
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



# Read a specific sheet from the Excel file based on the file location path
Input_data <- read_xlsx(
  "........../FIGURES DATA.xlsx",
  sheet = "Figure_4" 
)

Input_data$Phylum_list <- factor(Input_data$Phylum_list, levels = c("Arthropoda", "Annelida", "Mollusca", "Others"))


# Create a factor variable for Water.Authority with desired order
Input_data$data_source <- factor(Input_data$data_source, 
                                                    levels = c("Netherlands", "Delfland"))



line_graph <- ggplot(Input_data, aes(x = jaar, y = Total_year_phylum, group = Phylum_list, color = Phylum_list)) +
  geom_line(size = 2) +
  scale_color_manual(values = c("Arthropoda" = "#DA70D6", "Annelida" = "#00BFFF", "Mollusca" = "#29AB87", "Others" = "#FF8000")) +  # Specify line colors
  facet_wrap(~ data_source) +
  scale_y_log10() +
  labs(x = "Year", y = "Number of macrofauna data points") +
  theme(
    axis.title = element_text(size = 35),
    axis.title.x = element_text(margin = margin(t = 20)),  # Add margin to move the x-axis title
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 25),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5),
    strip.text = element_text(size = 30)
  ) +
  geom_text(data = Input_data, aes(x = "1991", y = 10000, label = labels.taxa),
            hjust = 1, vjust = 0, size = 11, color = "black") +
  theme(panel.spacing = unit(0.4, "cm", data = NULL))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(size = 15)))  # Increase the size of legend key symbols

line_graph

 

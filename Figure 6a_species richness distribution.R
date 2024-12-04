
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



# Read a specific sheet from the Excel file and define your data source
Input_data <- read_xlsx(
  "........./FIGURES DATA.xlsx",
  sheet = "Figure_6a" 
)

# Reorder data_source factor levels so that "NL" is first
# Input_data <- Input_data %>%
#   mutate(data_source_fact = factor(data_source, levels = c("Netherlands", "Delfland")))

Input_data$data_source <- factor(Input_data$data_source, levels = c("Netherlands", "Delfland"))


## count data rows
label_taxa <- Input_data %>%
  group_by(data_source) %>%
  summarise(ndata = n_distinct(MonsterID_ecofide)) %>%
  mutate(labels.taxa = paste0("n = ",  comma(ndata)))



## plot the species richness variability for the whole NL and selected water board

Species_distribution <- ggplot(data = Input_data, aes(x = msPAF, y = SpeciesCount), size = 2.0) + 
  geom_point() +
  #stat_smooth(method = "gam", se = FALSE, fullrange = TRUE, color = "blue", formula = y ~ s(x, k = 30)) +
  scale_x_continuous(limits = c(0.0, 0.8)) +
  scale_y_continuous(limits = c(0.0, 120)) +
  labs(x = expression("Mixture toxic pressure (msPAF-EC10eq)"),
       y = 'Species richness') +
  facet_wrap(~ data_source) +
  theme(strip.text.y = element_text(size = 25),
        strip.text.x = element_text(size = 25),
        axis.title = element_text(size = 28)) +
  theme(axis.text = element_text(size = 20),
        legend.key.size = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30)) +
  geom_text(data = label_taxa, mapping = aes(x = 0.65, 
                                             y = 120, 
                                             label = labels.taxa), size = 10, colour = "black", show.legend = FALSE)

Species_distribution





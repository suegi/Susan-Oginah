
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
  "........./FIGURES DATA.xlsx",
  sheet = "Figure_3" 
)

msPAF_variablity <- ggplot(data =Input_data, aes(x = reorder(XY, mean_tox), y = mean_tox)) +
  geom_pointrange(aes(ymin = minimum, ymax = maximum, color = data_source, size = data_source), alpha = 0.5, fill="white" ,shape = 21, stroke = 1.5) +
  scale_color_manual(values = c("Netherlands" = "#FF6347", "Delfland" = "#0000ff")) +##8A2BE2	
  scale_size_manual(values = c("Netherlands" = 0.8 ,"Delfland" = 1)) +  # Adjust the size values as needed
  scale_y_continuous(limits = c(0.0, 1.0)) +
  labs(x = expression("1286 unique sampled sites"), y = 'Mixture toxic pressure(msPAF-EC10)') +
  theme(strip.text.y = element_text(size = 45),
        strip.text.x = element_text(size = 45),
        axis.title = element_text(size = 40),
        axis.text = element_text(size = 25),#45
        legend.key.size = unit(4, 'cm'),  # Increase the size of legend symbols
        legend.key.height = unit(3, 'cm'),
        legend.key.width = unit(3, 'cm'),
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 30),
        # legend.position = "none",
        legend.position = c(0.26, 0.5),
        axis.title.x = element_text(margin = margin(t = 20)))


msPAF_variablity

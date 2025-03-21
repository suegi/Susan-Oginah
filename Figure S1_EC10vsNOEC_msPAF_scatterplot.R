
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
# Input_data <- read_xlsx(
#   "....../FIGURES DATA.xlsx",
#   sheet = "Figure_S1" 
# )

# Fit linear regression model
model <- lm(msPAF_Chronic_NOEC ~ msPAF_Chronic_EC10, data = Input_data)

# Extract model coefficients
coef <- coef(model)
r2 <- summary(model)$r.squared

ggplot(data = Input_data, aes(x = msPAF_Chronic_EC10, y = msPAF_Chronic_NOEC)) +
  geom_point(color = "#2166AC", size = 2, position = 'jitter')+
  geom_smooth(method = "lm",color = "red", size = 0.71, se = FALSE) +
  labs(
    x = expression("Mixture toxic pressure (msPAF"[EC10]*")"),
    y = expression("Mixture toxic pressure (msPAF"[NOEC]*")")
  )+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::label_number(accuracy = 0.1)) +  
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::label_number(accuracy = 0.1)) +
 
  geom_segment(aes(x = 0, y = 0, xend = 0.9, yend = 0.9),
               linetype = "solid", color = "#2166AC", size = 0.71) +  # 1:1 line from (0,0) to (0.9,0.9)
  annotate("text", x = 0.25, y = 0.89, 
           label = sprintf("y = %.4fx + %.4f\nR? = %.2f", coef[2], coef[1], r2), 
           color = "black", size = 7, fontface = "bold") +
 # theme_minimal() +
  theme(
    axis.text = element_text(size = 20),  # Increase axis text size
    axis.title = element_text(size = 22,face = "bold")  # Increase axis title size
  )





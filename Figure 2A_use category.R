
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
  "....../FIGURES DATA.xlsx",
  sheet = "Figure_2a" 
)

## Select data to plot
selected_data <- Input_data %>%
  filter(data_source== "Netherlands")

# Define the order of categories
category_order <- c("Others","Multiple uses","Industrial","Pharmaceutical","Pesticide")


# Create the ggplot Netherlands

# Reorder the x-axis in descending order
selected_data$category.fac2 <- factor(selected_data$category.fac, levels = c("1-10", "11-100", "101-1000", ">1000"))

fig_Netherlands <- ggplot(selected_data, aes(x =category.fac2, y = number_chemicals, fill = factor(Use_category, levels = category_order))) +
  scale_fill_manual(values = c("Pesticide" = "#DA70D6", "Pharmaceutical" = "#00BFFF", "Industrial" = "#29AB87", "Others" = "orange", "Multiple uses" = "#FF6347")) +  # Specify fill colors
  
  geom_col(width = 0.4) +  # Adjust the width of the bars as needed
  labs(x = "Number of data points per chemical", y = "Number of chemicals", fill = "Use category") +
  # scale_y_log10() +
  theme(
    axis.text = element_text(size = 28),
    axis.title = element_text(size = 35),
    legend.title = element_text(size = 22),  # Adjust legend title size
    legend.text = element_text(size = 20),
    legend.position = "bottom") +
  # scale_y_continuous(limits = c(0, 250)) + ##100 for DE, 250 for NL
  # theme(legend.position = "none") +
  ggtitle("Netherlands")+ #
  theme(plot.title = element_text(size = 35))  # Adjust ggtitle size

fig_Netherlands

### Create plot for Delfland
# Reorder the x-axis in descending order

selected_data <- Input_data %>%
  filter(data_source== "Delfland")

selected_data$category.fac <- fct_reorder(selected_data$category.fac, desc(selected_data$Total))

#Create the ggplot Delfland
fig_Delfland <- ggplot(selected_data, aes(x = category.fac, y = number_chemicals, fill = factor(Use_category, levels = category_order))) +
  scale_fill_manual(values = c("Pesticide" = "#DA70D6", "Pharmaceutical" = "#00BFFF", "Industrial" = "#29AB87", "Others" = "orange", "Multiple uses" = "#FF6347")) +  # Specify fill colors
  
  geom_col(width = 0.4) +  # Adjust the width of the bars as needed
  labs(x = "Number of data points per chemical", y = "Number of chemicals", fill = "Use category") +
  # scale_y_log10() +
  theme(
    axis.text = element_text(size = 28),
    axis.title = element_text(size = 35),
    legend.title = element_text(size = 22),  # Adjust legend title size
    legend.text = element_text(size = 20),
    legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 300)) + ##100 for DE, 250 for NL
  ggtitle("Delfland")+ #"Netherlands"
  theme(plot.title = element_text(size = 35))  # Adjust ggtitle size

fig_Delfland


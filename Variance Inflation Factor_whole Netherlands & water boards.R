
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
                      "mgcv") #add all required packages
for(i in 1:length(list.of.packages)){if(!(list.of.packages[i] %in% rownames(installed.packages()))){install.packages(list.of.packages[i])}} #only download uninstalled packages
lapply(list.of.packages, require, character.only = T) #load all packages



# Read a specific sheet from the Excel file
Input_data <- read_xlsx(
  "........../FIGURES DATA.xlsx",
  sheet = "msPAF_abiotic_factors" 
)

# Rename the column and remove rows with NAs
Input_data <- Input_data %>%
  rename("msPAF(EC10)" = "msPAF") %>%  # Rename column
  drop_na()  # Remove rows with any NA values


## filter Delfland data_source

#### Perform VIF for NL region
#fit the regression model

model <- glm(TWNcode_unique_count ~  pH+ TSS + Cl + DOC + Tw + NKj +`msPAF(EC10)`, data = Input_data)

#view the output of the regression model
summary(model)
vif(model)

#create vector of VIF values for base r
vif_values <- (vif(model))

par(mgp = c(2.5, 0.5, 0), mar = c(5, 4, 4, 2) + 0.1)
mp<- 
  barplot(sort(vif_values, decreasing = FALSE), horiz = TRUE, col = "steelblue",
          xlab = "Variance inflation factor values", ylab = "Abiotic variables",xlim= c(0,6),
          cex.axis = 1.5,
          cex.lab = 2.5,
          cex.names=1.7,
          # names.arg= c("msPAF","TSS","NKj", "pH", "DOC","Cl","Tw"),
          border = par("fg"),
          axis.lty = 2)
abline(v = 5, lwd = 3, lty = 2, col = "red")


# Get the unique data sources
unique_data_sources <- unique(Input_data$DataSource)

# Delfland_data <- Input_data %>% ## check wether the forloop is working OK
#   filter(DataSource == "Delfland")

# Get the unique data sources
unique_data_sources <- unique(Input_data$DataSource)

# Initialize an empty list to store VIF values
vif_values_list <- list()

# Loop through each unique DataSource value
for (data_source in unique_data_sources) {
  
  # Filter data for the specific data source
  data_subset <- Input_data %>% filter(DataSource == data_source) 
  
  # Try to fit the regression model and calculate VIF, skipping on error
  tryCatch({
    # Fit the regression model
    model <- glm(TWNcode_unique_count ~ pH +TSS + Cl + Tw + NKj + `msPAF(EC10)`, data = data_subset)# add/ remove "DOC" and "TSS" to calculate vif for regions lacking these parameters
    
    # Calculate VIF values
    vif_values <- vif(model)
    
    # Store VIF values in the list
    vif_values_list[[data_source]] <- vif_values
    
  }, error = function(e) {
    message(paste("Skipping", data_source, ": Model has aliased coefficients or another issue"))
  })
}

# View the resulting list of VIF values
vif_values_list

# Convert vif_values_list to a dataframe
vif_df <- do.call(rbind, lapply(names(vif_values_list), function(data_source) {
  data.frame(
    Data_Source = data_source, 
    Variable = names(vif_values_list[[data_source]]), 
    VIF = vif_values_list[[data_source]]
  )
}))

# View the resulting dataframe
print(head(vif_df))



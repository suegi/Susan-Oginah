
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
                      "rstatix",
                      "mgcv") #add all required packages
for(i in 1:length(list.of.packages)){if(!(list.of.packages[i] %in% rownames(installed.packages()))){install.packages(list.of.packages[i])}} #only download uninstalled packages
lapply(list.of.packages, require, character.only = T) #load all packages



# Read a specific sheet from the Excel file
Input_data <- read_xlsx(
  "........./FIGURES DATA.xlsx",
  sheet = "Statistical tests" 
)

#datain_joined <- Input_data 

## Water boards statistical analysis
# Initialize an empty list to store results
results_list <- list()

# Iterate through each unique DataSource

for (source in unique(Input_data$DataSource)) {
  # Filter the data for the current DataSource
  data_source_filtered <- Input_data %>%
    filter(DataSource == source)
  
  # Skip if all msPAF values are the same
  if (n_distinct(data_source_filtered$msPAF) <= 1) {
    next
  }
  
  ## Excluding species found in less than 10 sites
  abundtox <- data_source_filtered %>%
    group_by(SpeciesName_updated) %>%
    mutate(nsites = n()) %>%
    ungroup() %>%
    filter(nsites >= 5)
  
  ## Species abundance
  abundtox <- abundtox %>%
    rename(Abund = AMT_CALC_Ecofide)
  
  ## Remove rows with NA values in msPAF
  abundtox <- abundtox %>%
    filter(!is.na(msPAF))
  
  ## Group individual species into categories
  abundtox <- abundtox %>%
    mutate(
      logAbund = log10(Abund),
      category = Hmisc::cut2(msPAF, g = 5),
      category.fact = paste0('Group', as.integer(category)) |> as_factor()
    )
  
  ## Select species that are only being found in GRP1
  sensitive_species <- abundtox %>%
    filter(category.fact == "Group1")
  
  Species.list <- unique(sensitive_species$SpeciesName_updated)
  
  abundtox_selected <- abundtox %>%
    filter(SpeciesName_updated %in% Species.list)
  
  ## Count unique species per site
  richnessTWN <- abundtox_selected %>%
    group_by(MonsterID_ecofide) %>%
    summarise(Unique_TWNcodes = n_distinct(SpeciesName_updated)) %>%
    ungroup()
  
  ## Filter unique msPAF data
  unique_tox_info <- abundtox %>%
    distinct(MonsterID_ecofide, .keep_all = TRUE) %>%
    select(MonsterID_ecofide, msPAF, category.fact)
  
  richtox_join_new <- left_join(richnessTWN, unique_tox_info, by = "MonsterID_ecofide")
  
  # Define the order of levels for category.fact
  order_levels <- c("Group1", "Group2", "Group3", "Group4", "Group5")
  
  # Apply the factor with specified order to category.fact
  richtox_join_new$category.fact <- factor(richtox_join_new$category.fact, levels = order_levels)
  
  ## Statistics
  varleveneTest <- leveneTest(Unique_TWNcodes ~ category.fact, data = richtox_join_new)
  levene_p_value <- varleveneTest$`Pr(>F)`
  
  # Run Kruskal-Wallis test if the variance is greater than 0.05
  res.kruskal <- richtox_join_new %>% kruskal_test(Unique_TWNcodes ~ category.fact)
  res.kruskal
  # Dunn's test for pairwise comparisons
  stat.test <- richtox_join_new %>% dunn_test(Unique_TWNcodes ~ category.fact, p.adjust.method = "hochberg")
  stat.test <- stat.test %>% add_xy_position(x = "category.fact")
  stat.test$p.adj <- round(stat.test$p.adj, 3)
  
  # Store the results in the list
  results_list[[source]] <- list(
    leveneTest = varleveneTest,
    levene_p_value = levene_p_value,
    kruskal_test = res.kruskal,
    dunn_test = stat.test,
    n = res.kruskal$n  # Include n value here
  )
}

# Extract results into a readable dataframe
results_df <- map_dfr(names(results_list), function(source) {
  result <- results_list[[source]]
  data.frame(
    DataSource = source,
    n = result$n,  # Add n value here
    Levene_p_value = result$levene_p_value,
    Kruskal_p_value = result$kruskal_test$p,
    Dunn_p_values = paste(result$dunn_test$p.adj, collapse = ", ")
    
  )
})

# Function to create dynamic column names for Dunn's test p-values
get_dunn_column_names <- function(dunn_test) {
  dunn_test %>%
    mutate(group_comparison = paste(group1, "vs", group2)) %>%
    pull(group_comparison)
}

# Get dynamic column names from one of the Dunn's tests
column_names <- get_dunn_column_names(results_list[[1]]$dunn_test)

# # Filter the first row of each DataSource and split the Dunn's test p-values
results_summary <- results_df %>%
  group_by(DataSource) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Dunn_p_values = strsplit(as.character(Dunn_p_values), ", ")) %>%
  unnest_wider(Dunn_p_values, names_sep = "_")

# # Rename the Dunn's test columns with dynamic names
names(results_summary)[5:ncol(results_summary)] <- column_names

# # Save the results to a CSV file
#write.csv(results_summary, "results_summary.csv", row.names = FALSE)

# # Extract significant Dunn's test results into a separate dataframe
# significant_dunn <- map_dfr(names(results_list), function(source) {
#   result <- results_list[[source]]
#   result$dunn_test %>%
#     filter(p.adj.signif != "ns") %>%
#     mutate(DataSource = source)
# })

# # Save the significant Dunn's test results to a CSV file
# write.csv(significant_dunn, "significant_dunn_results.csv", row.names = FALSE)

# Print the results dataframes
print(results_summary)


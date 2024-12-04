
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
                      "rstatix",
                      "car",
                      "ggpubr",
                      "ggpattern",
                      "mgcv") #add all required packages
for(i in 1:length(list.of.packages)){if(!(list.of.packages[i] %in% rownames(installed.packages()))){install.packages(list.of.packages[i])}} #only download uninstalled packages
lapply(list.of.packages, require, character.only = T) #load all packages



# Read a specific sheet from the Excel file and define your data source
Input_data <- read_xlsx(
  "........../FIGURES DATA.xlsx",
  sheet = "Figure_7_Netherlands" 
)


## Excluding species found in less than 5 sites
abundtox <- Input_data %>%
  group_by(SpeciesName_updated)%>%
  mutate(nsites=n()) %>%
  ungroup() %>%
  filter(nsites >= 5)


##Species abundance
abundtox <- abundtox %>%
  rename(Abund = AMT_CALC_Ecofide)### is this the total data point collected for that particular species

## Group individual species into categories
abundtox<-abundtox%>%
  mutate(
    logAbund=log10(Abund),
    #5 g define the number of quantile groups
    category = Hmisc::cut2(abundtox$msPAF, g =5),
    category.fact = paste0('Group', as.integer(category)) |>
      as_factor()
  ) 

## select species that are only being found in GRP1

sensitive_species<-abundtox %>%
  filter(category.fact == "Group1")

Species.list<-unique(sensitive_species$SpeciesName_updated)

abundtox_selected<- abundtox%>%
  filter(abundtox$SpeciesName_updated %in% Species.list)

## count unique species per site
richnessTWN <- abundtox_selected %>%
  group_by(MonsterID_ecofide) %>%
  summarise(Unique_TWNcodes = n_distinct(SpeciesName_updated))%>%
  ungroup()

## Filter unique msPAF data
unique_tox_info <- abundtox %>%
  distinct(MonsterID_ecofide, .keep_all = TRUE) %>% # Select unique rows based on MonsterID_ecofide
  select("MonsterID_ecofide","msPAF","category.fact")

richtox_join_new <- left_join(richnessTWN, unique_tox_info, by = "MonsterID_ecofide")

# ## Step 2 of richness binning
# richtox_join_new<-richtox_join %>%
#   mutate(
#     category = Hmisc::cut2(richtox_join$msPAF, g =5),
#     category.fact = paste0('Group', as.integer(category)) |>
#       as_factor()
#   ) 

# Define the order of levels for category.fact
order_levels <- c("Group1", "Group2", "Group3", "Group4", "Group5")

# Apply the factor with specified order to category.fact
richtox_join_new$category.fact <- factor(richtox_join_new$category.fact, levels = order_levels)

## Statistics

varleveneTest <- leveneTest(Unique_TWNcodes ~ category.fact, data = richtox_join_new)
summary(varleveneTest)

varleveneTest$`Pr(>F)`

richtox_join_new["Unique_TWNcodes", "category.fact"]

##Run ANOVA as the variance is >0.05
res.kruskal <- richtox_join_new %>% kruskal_test(Unique_TWNcodes ~ category.fact)
res.kruskal
stat.test <-
  richtox_join_new %>% dunn_test(Unique_TWNcodes ~ category.fact, p.adjust.method = "hochberg")
stat.test

stat.test <- stat.test %>% add_xy_position(x = "category.fact")
stat.test$p.adj<-round(stat.test$p.adj,3)

##export stat output
# write.csv(stat.test, "stat.test_NL.csv", row.names = FALSE)

### create violin plots
col <- c("#56B4E9", "#4C9A2A", "#F0E442", "#E69F00", "#ff0000")
binboxplot <-
  ggboxplot(richtox_join_new,
            x = "category.fact",
            y = "Unique_TWNcodes",
            fill = col) +
  scale_colour_manual(values = col) +
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300))+
  labs(x = "msPAF group",
       y = "Count of unique species per site") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))
binboxplot



## Add n values
Total_abud <- richtox_join_new %>%
  group_by(category.fact) %>%
  summarise(Total = round(mean(Unique_TWNcodes),2),
            low=min(msPAF),
            high=max(msPAF),
            #median(msPAF),
            # se=std.error(msPAF),
            count.sample=n())

Total_abud 


Total_abud$labels.taxa <- paste0("µ", " = ", Total_abud$Total)
Total_abud$labels.count <- paste0("n", " = ", Total_abud$count.sample)


# ##position of the text

Total_abud$y_position <- as.numeric(factor(Total_abud$category.fact)) *0.05 + 3#0.65
Total_abud$y2_position <- as.numeric(factor(Total_abud$category.fact)) *0.05 + 12#0.65
### violin plots
binviolinplot <-
  ggplot(richtox_join_new, aes(x = category.fact, y = Unique_TWNcodes, color = category.fact)) +
  geom_violin(trim = FALSE, fill = "transparent",linewidth =1) +
  # geom_boxplot(width = 0.4, linewidth =0.7) +
  geom_boxplot_pattern(fatten=3.5, width = 0.35,position = position_dodge(preserve = "single"), color = "black", pattern_fill = col, pattern_angle = 45, pattern_density = 0.5, pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) +
  # geom_boxplot_pattern(fatten=4, width = 0.4,position = position_dodge(preserve = "single"), color = "black", pattern_fill = col, pattern_angle = 45, pattern_density = 0.5, pattern_spacing = 0.025, pattern_key_scale_factor = 0.6) +
  scale_colour_manual(values = col) +
  stat_pvalue_manual(
    stat.test,
    hide.ns = FALSE,
    tip.length = 0.005,
    step.increase = 0.04,#0.03
    label = "p.adj",
    label.size =9,
    hjust = -0.1#10
  ) +
  
  labs(x = "msPAF(EC10) group",
       y = "Count of unique species per site") +
  theme(axis.title = element_text(size = 45),
        axis.text = element_text(size = 35)) +
  scale_color_manual(
    name = "msPAF groups",
    labels = c(
      "0-0.0.007",
      "> 0.007-0.017",
      "> 0.017-	0.037",
      "> 0.037-0.087",
      "> 0.087-0.75"
      
      
    ),
    values = col
  ) +
  scale_y_continuous(breaks = c(0,50,100,150))+
  geom_text(data = Total_abud, aes(x = category.fact, y = y_position,hjust= -0.045,vjust=2.5,label = labels.taxa),color="black", size = 10, show.legend = FALSE)+
  geom_text(data = Total_abud, aes(x = category.fact, y = y2_position,hjust= -0.10,vjust=2.5,label = labels.count),color="black", size = 10, show.legend = FALSE) +
  #   guides(colour = guide_legend(override.aes = list(size = 12)))+
  theme(legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 25))


binviolinplot

## Option: regulatory driven with selected species

## Excluding species found in less than 10 sites
abundtox_policy <- Input_data  %>%
  group_by(SpeciesName_updated)%>%
  mutate(nsites=n()) %>%
  ungroup() %>%
  filter(nsites >=5)


##Species abundance
abundtox_policy <-
  abundtox_policy %>%
  rename(Abund = AMT_CALC_Ecofide)### is this the total data point collected for that particular species


######## manually create three bins

## Group individual species into categories
abundtox_policy<-abundtox_policy%>%
  mutate(
    logAbund=log10(Abund),
    #3 g define the number of quantile groups
    category.fact = as.factor(case_when(
      msPAF >= 0 & msPAF <= 0.05~  'Group1',# 0.027
      msPAF > 0.05 & msPAF <=  0.2  ~ 'Group2',##5
      msPAF > 0.2  ~ 'Group3'
      
    ))
  ) 

## select species that are only being found in GRP1

sensitive_species<-abundtox_policy %>%
  filter(category.fact == "Group1")

Species.list<-unique(sensitive_species$SpeciesName_updated)

abundtox_selected<- abundtox_policy%>%
  filter(abundtox_policy$SpeciesName_updated %in% Species.list)

## count unique species per site
richnessTWN <- abundtox_selected %>%
  group_by(MonsterID_ecofide) %>%
  summarise(Unique_TWNcodes = n_distinct(SpeciesName_updated))%>%
  ungroup()

## Filter unique msPAF data
unique_tox_info <- abundtox_policy %>%
  distinct(MonsterID_ecofide, .keep_all = TRUE) %>% # Select unique rows based on MonsterID_ecofide
  select("MonsterID_ecofide","msPAF","category.fact")

richtox_join_new <- left_join(richnessTWN, unique_tox_info, by = "MonsterID_ecofide")




## Statistics

varleveneTest <- leveneTest(Unique_TWNcodes ~ category.fact, data = richtox_join_new)
summary(varleveneTest)

varleveneTest$`Pr(>F)`

richtox_join_new["Unique_TWNcodes", "category.fact"]

## Run non parametric equivalent of ANOVA
res.kruskal <- richtox_join_new %>% kruskal_test(Unique_TWNcodes ~ category.fact)
res.kruskal
stat.test <-
  richtox_join_new %>% dunn_test(Unique_TWNcodes ~ category.fact, p.adjust.method = "hochberg")
stat.test
stat.test <- stat.test %>% add_xy_position(x = "category.fact")
stat.test$p.adj<-round(stat.test$p.adj,3)



### create violin plots
col <- c("#56B4E9",  "#E69F00", "#ff0000")
binboxplot <-
  ggboxplot(richtox_join_new,
            x = "category.fact",
            y = "Unique_TWNcodes",
            fill = col) +
  scale_colour_manual(values = col) +
  scale_y_continuous(breaks = c(0, 50, 100, 150), limits = c(NA, 150)) +
  # stat_pvalue_manual(stat.test, hide.ns = TRUE) +
  labs(x = "msPAF group",
       y = "Count of unique species per site") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))
binboxplot



## Add n values
Total_abud <- richtox_join_new %>%
  group_by(category.fact) %>%
  summarise(Total = round(mean(Unique_TWNcodes),2),
            low=min(msPAF),
            high=max(msPAF),
            median(msPAF),
            # se=std.error(msPAF),
            count.sample=n())

Total_abud 


Total_abud$labels.taxa <- paste0("µ", " = ", Total_abud$Total)
Total_abud$labels.count <- paste0("n", " = ", Total_abud$count.sample)


# ##position of the text

Total_abud$y_position <- as.numeric(factor(Total_abud$category.fact)) *0.05 + 3#0.65
Total_abud$y2_position <- as.numeric(factor(Total_abud$category.fact)) *0.05 + 12#0.65
### violin plots
binviolinplot <-
  ggplot(richtox_join_new, aes(x = category.fact, y = Unique_TWNcodes, color = category.fact)) +
  geom_violin(trim = FALSE, fill = "transparent",linewidth =1) +
  # geom_boxplot(width = 0.4, linewidth =0.7) +
  geom_boxplot_pattern(fatten=3.5, width = 0.35,position = position_dodge(preserve = "single"), color = "black", pattern_fill = col, pattern_angle = 45, pattern_density = 0.5, pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) +
  # geom_boxplot_pattern(fatten=4, width = 0.4,position = position_dodge(preserve = "single"), color = "black", pattern_fill = col, pattern_angle = 45, pattern_density = 0.5, pattern_spacing = 0.025, pattern_key_scale_factor = 0.6) +
  scale_colour_manual(values = col) +
  stat_pvalue_manual(
    stat.test,
    hide.ns = FALSE,
    tip.length = 0.005,
    step.increase = 0.03,#0.03
    label = "p.adj",
    label.size =9#10
  ) +
  
  labs(x = "msPAF(EC10) group",
       y = "Count of unique species per site") +
  theme(axis.title = element_text(size = 45),
        axis.text = element_text(size = 42)) +
  scale_color_manual(
    name = "msPAF groups",
    labels = c(
      "0-0.05",
      "> 0.05-0.2",
      "> 0.2"
    ),
    values = col
  ) +
  scale_y_continuous(breaks = c(0, 50, 100, 150), limits = c(NA, 150)) +
  
  geom_text(data = Total_abud, aes(x = category.fact, y = y_position,hjust= -0.045,vjust=3.0,label = labels.taxa),color="black", size = 10, show.legend = FALSE)+
  geom_text(data = Total_abud, aes(x = category.fact, y = y2_position,hjust= -0.10,vjust=3.5,label = labels.count),color="black", size = 10, show.legend = FALSE)+
  guides(colour = guide_legend(override.aes = list(size = 12))) +
  theme(legend.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 28))


binviolinplot


# GAM on relative richness per sample location vs relative msPAF per sample location 

mean_species_richness <- mean(richtox_join_new$Unique_TWNcodes) # mean 
richtox_join_relative_richness <- richtox_join_new %>% mutate(rel_species_richness = (Unique_TWNcodes - mean_species_richness)/mean_species_richness)

plot_gam <- ggplot(richtox_join_relative_richness, aes(x = msPAF, y = rel_species_richness)) + #same point as above, why use ODF_negative here? 
  geom_point() +
  # facet_wrap(~GroupSize) +
  stat_smooth(method = "gam", se = FALSE, fullrange = TRUE, color = "blue", formula = y ~ s(x, k = 3)) +
  ggtitle("GAM Smoothing") + 
  scale_x_continuous(limits = c(0, 0.25))
print(plot_gam)


# Fit the GAM model using the correct dataset

gam_model <- gam(rel_species_richness ~ s(msPAF, k = 3), data = richtox_join_relative_richness, family = gaussian(link = "identity"))

# Predict the y-values and standard errors across all msPAF values in the dataframe
predictions <- predict(gam_model, newdata = richtox_join_relative_richness, se.fit = TRUE)

# Add predicted values and confidence intervals to the dataframe
richtox_join_relative_richness$predicted_values <- predictions$fit
richtox_join_relative_richness$lower_ci <- predictions$fit - 1.96 * predictions$se.fit  # Lower bound of 95% CI
richtox_join_relative_richness$upper_ci <- predictions$fit + 1.96 * predictions$se.fit  # Upper bound of 95% CI

# Define specific x values for additional predictions (single points)
x_values <- data.frame(msPAF = c(0.2, 0.05, 0.0))
predicted_y_values <- predict(gam_model, newdata = x_values, se.fit = TRUE)

# Save the specific predictions and their confidence intervals
predicted_y_1 <- predicted_y_values$fit[1]
predicted_y_2 <- predicted_y_values$fit[2]
predicted_y_3 <- predicted_y_values$fit[3]

ci_1_lower <- predicted_y_values$fit[1] - 1.96 * predicted_y_values$se.fit[1]
ci_1_upper <- predicted_y_values$fit[1] + 1.96 * predicted_y_values$se.fit[1]

ci_2_lower <- predicted_y_values$fit[2] - 1.96 * predicted_y_values$se.fit[2]
ci_2_upper <- predicted_y_values$fit[2] + 1.96 * predicted_y_values$se.fit[2]

ci_3_lower <- predicted_y_values$fit[3] - 1.96 * predicted_y_values$se.fit[3]
ci_3_upper <- predicted_y_values$fit[3] + 1.96 * predicted_y_values$se.fit[3]

# Plot the predicted values, confidence intervals, and original data
plot_gam <- ggplot(richtox_join_relative_richness, aes(x = msPAF, y = rel_species_richness)) +
  stat_smooth(method = "gam", se = TRUE, fullrange = FALSE, color = "#cccccc", formula = y ~ s(x, k = 3)) +
  geom_point() +
  # Add points for the specific predicted values at the respective x-values
  geom_point(aes(x = 0.2, y = predicted_y_1), color = "red", size = 2.5) +
  geom_point(aes(x = 0.05, y = predicted_y_2), color = "blue", size = 2.5) +
  geom_point(aes(x = 0.0, y = predicted_y_3), color = "green", size = 2.5) +
  geom_text(aes(x = 0.2, y = 1.5, label = sprintf("%.2f", predicted_y_1)), color = "red", vjust = -0.9, size = 8) +#x = 0.2, y = -0.15
  geom_text(aes(x = 0.07, y = 1.5, label = sprintf("%.2f", predicted_y_2)), color = "blue", vjust = -0.9, size = 8) +#(x = 0.05, y = 0.05
  geom_text(aes(x = 0.01, y = 1.5, label = sprintf("%.2f", predicted_y_3)), color = "green", vjust = -0.9, size = 8) +#x = 0.01, y = 0.4
  labs(x = expression(paste('Mixture Toxic Pressure (msPAF-EC10)')),
       y = 'Relative species richness') +
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title = element_text(size = 40),
        strip.text = element_text(size = 22),
        plot.title = element_text(size = 40)) +
  ggtitle("Netherlands") +
  scale_x_continuous(limits = c(0, 0.3))+
  scale_y_continuous(limits = c(-1, 2))

print(plot_gam)



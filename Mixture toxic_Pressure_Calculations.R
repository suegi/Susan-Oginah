
# Toxic Pressure Pipeline - Refactored Script
# Author: Refactored from Jaap Slootweg's original code
# modified: Susan Oginah
# Description: Calculates msPAFs from concentration data using SSDs and chemical properties

# Load required libraries
library(openxlsx)
library(dplyr)
library(mgcv)
library(ggplot2)
# -------------------------------
# 1. SETTINGS
# -------------------------------
source("R/AssignSettings.R")

settings <- list(
  ToxPressureFrom = "NumbersFrom2016",
  ClusterMethod = "UsingXYinput",
  NOclusters = 18,
  SSData = "data/ecotox_data_TAXA.xlsx", # downlaod the SSD data from Oginah et al 2023, https://doi.org/10.1021/acs.est.3c04968 
  Harmonised_effect_type_EC10 = "Chronic EC10_eq",
  SpeciesGroup = "Invertebrates",
  AcuteOrChronic = "chronic",
  ABCquality = c("A", "B")
)

# -------------------------------
# 2. LOAD DATA
# -------------------------------
load("data/Gross.rda")             # Loads 'Gross'
load("data/ModifierDefaults.rda")  # Loads 'ModifierDefaults'
# the rest of the data to be loaded are from the raw data link

# Read concentration data
Concentrations <- rbind(
  openxlsx::read.xlsx("data/2a_RuwChemie1_Physico-chemical_Pollution_1.xlsx", sheet = "data"),
  openxlsx::read.xlsx("data/2b_RuwChemie2_Physico-chemical_Pollution_2.xlsx", sheet = "data"),
  openxlsx::read.xlsx("data/2c_RuwChemie3_Physico-chemical_Pollution_3.xlsx", sheet = "data")
)

# Rename columns for consistency
names(Concentrations) <- c(
  "Waterschap", "ChemieID_ecofide", "SampleID", "LOC_CODE", "Meetpuntomschrijving", "Datum.Monster",
  "Maand", "Jaar", "lookslikeAquoCode", "Parameteromschrijving.Waterschap", "Eenheid.Waterschap", "MeasuredValueWS",
  "parameter.Ecofide", "hoofdgroep", "groepEcofide", "CAS", "CAS#", "eenheid.Ecofide",
  "AquoCode", "PreTreatment", "LimitSymbol", "MeasuredValue"
)

# Filter out values below detection limit and before year 2000
Concentrations <- Concentrations %>%
  filter(LimitSymbol != "<", Jaar >= 2000)

# -------------------------------
# 3. PREPARE SSD DATA from Oginah et al 2023:https://doi.org/10.1021/acs.est.3c04968
# -------------------------------

# Load SSD data
SSDdata <- openxlsx::read.xlsx(settings$SSData)

# Harmonize CAS numbers
CASupdates <- read.csv("data/CASupdates.csv")
SSDdata <- left_join(SSDdata, CASupdates, by = "CAS_Harmonized")
SSDdata$CAS[is.na(SSDdata$CAS)] <- SSDdata$CAS_Harmonized[is.na(SSDdata$CAS)]

# # Aggregate SSD data
aggMin4mu <- aggregate(mu_t ~ CAS + SpeciesGroup_Harmonized + Harmonised_effect.type_EC10, data = SSDdata, FUN = min)
aggMin4SD <- aggregate(SDV_t ~ CAS + SpeciesGroup_Harmonized + Harmonised_effect.type_EC10, data = SSDdata, FUN = min)

if (settings$SpeciesGroup == "all") {
  SSDdata <- aggMin4mu[aggMin4mu$Harmonised_effect.type_EC10 == settings$Harmonised_effect_type_EC10, c("CAS", "mu_t")]
  SSDdata$sigma <- aggMin4SD$SDV_t[aggMin4SD$Harmonised_effect.type_EC10 == settings$Harmonised_effect_type_EC10]
} else {
  SSDdata <- aggMin4mu[
    aggMin4mu$SpeciesGroup_Harmonized == settings$SpeciesGroup &
    aggMin4mu$Harmonised_effect.type_EC10 == settings$Harmonised_effect_type_EC10,
    c("CAS", "mu_t")
  ]
  SSDdata$sigma <- aggMin4SD$SDV_t[
    aggMin4SD$SpeciesGroup_Harmonized == settings$SpeciesGroup &
    aggMin4SD$Harmonised_effect.type_EC10 == settings$Harmonised_effect_type_EC10
  ]
}
names(SSDdata)[names(SSDdata) == "mu_t"] <- "mu"

# -------------------------------
# 4. DEFINE FUNCTIONS
# -------------------------------
calculate_hazard_units <- function(ToHU, ChemData, muNames = c(acute = "mu", chronic = "mu"),
                                   sigmaNames = c(acute = "sigma", chronic = "sigma"),
                                   aggrFUN = max, DevRange = c(0.2, 2)) {
  # Determine if filtered
  ToHU$Filtered <- if ("PreTreatment" %in% names(ToHU)) {
    endsWith(tolower(ToHU$PreTreatment), "nf")
  } else {
    TRUE
  }

  # Match chemical data
  ChemMatch <- match(ToHU$CAS, ChemData$CAS)
  ToHU$mu_acute <- ChemData[ChemMatch, muNames["acute"]]
  ToHU$sigma_acute <- ChemData[ChemMatch, sigmaNames["acute"]]
  ToHU$mu_chronic <- ChemData[ChemMatch, muNames["chronic"]]
  ToHU$sigma_chronic <- ChemData[ChemMatch, sigmaNames["chronic"]]

  # Clamp sigma values
  ToHU$sigma_acute[ToHU$sigma_acute < DevRange[1] | ToHU$sigma_acute > DevRange[2]] <- 0.7
  ToHU$sigma_chronic[ToHU$sigma_chronic < DevRange[1] | ToHU$sigma_chronic > DevRange[2]] <- 0.7

  # Aggregate by sample and CAS
  PAF <- ToHU %>%
    group_by(SampleID, CAS) %>%
    summarise(
      ActConc = aggrFUN(MeasuredValue),
      mu_acute = first(mu_acute),
      sigma_acute = first(sigma_acute),
      mu_chronic = first(mu_chronic),
      sigma_chronic = first(sigma_chronic),
      .groups = "drop"
    )

  # Calculate HUs and PAFs
  PAF <- PAF %>%
    mutate(
      HU_acute = ActConc / (10 ^ mu_acute),
      HU_chronic = ActConc / (10 ^ mu_chronic),
      PAF_acute = pnorm(log10(HU_acute), mean = 0, sd = sigma_acute),
      PAF_chronic = pnorm(log10(HU_chronic), mean = 0, sd = sigma_chronic)
    )

  return(PAF)
}

convert_to_msPAFs <- function(HU) {
  aggregate_msPAF <- function(df, HU_col, sigma_col) {
    df %>%
      filter(!is.na(.data[[HU_col]]), !is.na(.data[[sigma_col]])) %>%
      group_by(SampleID) %>%
      summarise(
        HU_sum = sum(.data[[HU_col]]),
        sigma_avg = mean(.data[[sigma_col]]),
        .groups = "drop"
      ) %>%
      mutate(
        RemainFrac = 1 - pnorm(log10(HU_sum), mean = 0, sd = sigma_avg),
        msPAF = 1 - RemainFrac
      )
  }

  acute <- aggregate_msPAF(HU, "HU_acute", "sigma_acute")
  chronic <- aggregate_msPAF(HU, "HU_chronic", "sigma_chronic")

  result <- merge(acute, chronic, by = "SampleID", suffixes = c("_acute", "_chronic"))

  result$Class <- mapply(function(a, c) {
    if (is.na(a)) {
      if (c > 0.05) "Moderate to Very High"
      else if (c > 0.005) "2-Low (Gering)"
      else "1-None (Geen)"
    } else if (a > 0.10) "5-Very High"
    else if (a > 0.005) "4-High"
    else if (is.na(c)) "Moderate to Low"
    else if (c > 0.05) "3-Moderate"
    else if (c > 0.005) "2-Low (Gering)"
    else "1-None (Geen)"
  }, result$msPAF_acute, result$msPAF_chronic)

  return(result)
}

# -------------------------------
# 5. RUN ANALYSIS
# -------------------------------
# Calculate hazard units
hazard_units <- calculate_hazard_units(Concentrations, ChemData = SSDdata)

# Convert to msPAFs
msPAF_results <- convert_to_msPAFs(hazard_units)

# View results
print(head(msPAF_results$msPAF_chronic))

# Add LOC_CODE to msPAF results
msPAF_results <- msPAF_results %>%
  left_join(unique(Concentrations[, c("SampleID", "LOC_CODE")]), by = "SampleID")


# ----------------------------------------------
# 6. ENRICH TOXIC PRESSURE WITH LOCATION DATA
# ----------------------------------------------

# Load lookup table with coordinates
lookupJaap <- openxlsx::read.xlsx("1_Site code table_Oginah et al GCB.xlsx", startRow = 2) #data/OverzichtstabelESF8
names(lookupJaap)[1:7] <- c("oldWS", "MonsterID_ecofide", "LOC_CODE", "jaar", "X", "Y", "TYPE_KRW")
lookupJaap <- unique(lookupJaap[, c("LOC_CODE", "X", "Y", "oldWS")])

# Join coordinates to toxic pressure data
ToxPressure <- left_join(msPAF_results, lookupJaap, by = "LOC_CODE")
ToxPressure$XY <- paste(round(ToxPressure$X), round(ToxPressure$Y), sep = ".")

# -------------------------------------
# 7. LOAD AND PREPARE MACROFAUNA DATA
# -------------------------------------

macrofauna <- readRDS("data/macrofauna.rds")
names(macrofauna)[names(macrofauna) == "AMT_CALC_Ecofide (aantal ind./5m handnet)"] <- "AMT_CALC_Ecofide"
names(macrofauna)[names(macrofauna) == "TWN code definitief"] <- "TWNcode"

# Add coordinates to macrofauna
macrofauna <- left_join(macrofauna, lookupJaap[, c("LOC_CODE", "X", "Y")], by = "LOC_CODE")

# Filter for valid taxa and years
macrofauna <- macrofauna %>%
  filter(`GENUS-TWN` %in% c("Species", "Species combi", "Subspecies"),
         jaar > 2000)

# Aggregate abundance per sample and taxon
SelectedMacrFauna <- macrofauna %>%
  group_by(MonsterID_ecofide, TWNcode) %>%
  summarise(AMT_CALC_Ecofide = sum(AMT_CALC_Ecofide), .groups = "drop")

# -------------------------------
# 8. LINK MACROFAUNA TO TOXIC PRESSURE
# -------------------------------

# Create richness metric
richnessTWN <- SelectedMacrFauna %>%
  group_by(MonsterID_ecofide) %>%
  summarise(Richness = n(), .groups = "drop")

# Prepare toxic pressure data for merge

CPdf <- ToxPressure[, c("SampleID", "XY", "msPAF_chronic")]
names(CPdf) <- c("SampleID", "XY", "Tox")

# Merge richness with toxic pressure
richTox <- left_join(richnessTWN, CPdf, by = c("MonsterID_ecofide" = "SampleID"))

# -------------------------------
# 9. GAM ANALYSIS AND PLOTTING
# -------------------------------
#  Detail analysis of linking msPAF and biodiversity is done on separate codes 



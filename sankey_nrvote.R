# ==============================================================================
# PROJECT:       AUTNES Dashboard
# OBJECTIVE:     Sankey
# AUTHOR:        JP
# DATE:          2024-11-06
# ==============================================================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(networkD3)
library(haven)
library(expss)
library(tidyr)
library(stringr)
library(htmlwidgets)
library(anesrake)

# Set paths
# ------------------------------------------------------------------------------
MAIN <- "D:/Studium/TU/194.147 Interdisciplinary Project in Data Science/working_dir"
DATA <- file.path(MAIN, "data")
GRAPHS <- file.path(MAIN, "graphs")
setwd(DATA)

# Load data
# ------------------------------------------------------------------------------
dat_wide <- read_dta(paste0(DATA, "/AUTNES_OPS_2017-2024_w1-24_DE.dta"))

# ------------------------------------------------------------------------------
# Define vote levels for 2017, 2019 2024
# ------------------------------------------------------------------------------
vote_levels <- c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "SONSTIGE/UNGÜLTIG", "NICHTWAHL")

# ------------------------------------------------------------------------------
# VOTE NRW 2013
# ------------------------------------------------------------------------------

# Step 1: Recode vote choices for waves 1 and 4
dat_wide <- dat_wide %>%
  mutate(
    # Wave 1 (vote choice)
    w1_nrvote13 = case_when(
      w1_q24 == 1 ~ 2,  # SPÖ
      w1_q24 == 2 ~ 1,  # ÖVP
      w1_q24 == 3 ~ 3,  # FPÖ
      w1_q24 == 5 ~ 4,  # GRÜNE
      w1_q24 == 7 ~ 5,  # NEOS
      w1_q24 == 4 | w1_q24 == 6 | w1_q24 == 8 | w1_q24 == 9 | w1_q24 == 10 ~ 6,  # Minor parties and invalid votes to SONSTIGE/UNGÜLTIG
      w1_q24 == 88 | w1_q24 == 99 ~ NA_real_  # "Weiß nicht" and "Keine Angabe" to NA
    ),
    
    # Wave 4 (vote choice)
    w4_nrvote13 = case_when(
      w4f_q29 == 1 ~ 2,  # SPÖ
      w4f_q29 == 2 ~ 1,  # ÖVP
      w4f_q29 == 3 ~ 3,  # FPÖ
      w4f_q29 == 5 ~ 4,  # GRÜNE
      w4f_q29 == 7 ~ 5,  # NEOS
      w4f_q29 == 4 | w4f_q29 == 6 | w4f_q29 == 8 | w4f_q29 == 9 | w4f_q29 == 10 ~ 6,  # Minor parties and invalid votes to SONSTIGE/UNGÜLTIG
      w4f_q29 == 88 | w4f_q29 == 99 ~ NA_real_  # "Weiß nicht" and "Keine Angabe" to NA
    )
  )

# Step 2: Replace values for non-voters based on participation variables (`w1_q23` and `w4f_q28`)
dat_wide <- dat_wide %>%
  mutate(
    w1_nrvote13 = ifelse(w1_q23 < 4, 7, w1_nrvote13),  # Set to NICHTWAHL if <4 in w1_q23
    w4_nrvote13 = ifelse(w4f_q28 < 4, 7, w4_nrvote13)  # Set to NICHTWAHL if <4 in w4f_q28
  )

# Step 3: Combine columns into a single `nrvote13` variable by filling NA values sequentially
dat_wide <- dat_wide %>%
  mutate(
    nrvote13 = coalesce(w1_nrvote13, w4_nrvote13)
  )

# Step 4: Apply labels to `nrvote13` using factor levels
dat_wide <- dat_wide %>%
  mutate(
    nrvote13 = factor(nrvote13, levels = 1:7, labels = vote_levels)
  )

# Optional: View the frequency of the new variable
fre(dat_wide$nrvote13)


# ------------------------------------------------------------------------------
# VOTE NRW 2017
# ------------------------------------------------------------------------------

# Define 2017 vote levels
vote_levels <- c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "SONSTIGE/UNGÜLTIG", "NICHTWAHL")

# Step 1: Recode 2017 vote choices for all waves including w5 and w6
dat_wide <- dat_wide %>%
  mutate(
    # Wave 5
    w5_nrvote17 = case_when(
      w5_q10 == 1 ~ 2,  # SPÖ
      w5_q10 == 2 ~ 1,  # ÖVP (Liste Sebastian Kurz)
      w5_q10 == 3 ~ 3,  # FPÖ
      w5_q10 == 4 ~ 4,  # GRÜNE
      w5_q10 == 5 ~ 5,  # NEOS
      w5_q10 >= 6 & w5_q10 <= 12 ~ 6,  # Minor parties and invalid votes to SONSTIGE/UNGÜLTIG
      w5_q10 == 88 | w5_q10 == 99 ~ NA_real_  # "Weiß nicht" and "Keine Angabe" to NA
    ),
    
    # Wave 6
    w6_nrvote17 = case_when(
      w6f_q19 == 1 ~ 2,  # SPÖ
      w6f_q19 == 2 ~ 1,  # ÖVP (Liste Sebastian Kurz)
      w6f_q19 == 3 ~ 3,  # FPÖ
      w6f_q19 == 4 ~ 4,  # GRÜNE
      w6f_q19 == 5 ~ 5,  # NEOS
      w6f_q19 >= 6 & w6f_q19 <= 12 ~ 6,  # Minor parties and invalid votes to SONSTIGE/UNGÜLTIG
      w6f_q19 == 88 | w6f_q19 == 99 ~ NA_real_  # "Weiß nicht" and "Keine Angabe" to NA
    ),

    # Existing waves (example for Wave 7; repeat for others)
    w7_nrvote17 = case_when(
      w7_q5 == 1 ~ 2,
      w7_q5 == 2 ~ 1,
      w7_q5 == 3 ~ 3,
      w7_q5 == 4 ~ 4,
      w7_q5 == 5 ~ 5,
      w7_q5 >= 7 & w7_q5 <= 12 ~ 6,
      w7_q5 == 88 | w7_q5 == 99 ~ NA_real_
    ),
    # Repeat similar recoding for w8 through w12
    w8_nrvote17 = case_when(
      w8_q63 == 1 ~ 2,
      w8_q63 == 2 ~ 1,
      w8_q63 == 3 ~ 3,
      w8_q63 == 4 ~ 4,
      w8_q63 == 5 ~ 5,
      w8_q63 >= 7 & w8_q63 <= 12 ~ 6,
      w8_q63 == 88 | w8_q63 == 99 ~ NA_real_
    ),
    w9_nrvote17 = case_when(
      w9f_q48 == 1 ~ 2,
      w9f_q48 == 2 ~ 1,
      w9f_q48 == 3 ~ 3,
      w9f_q48 == 4 ~ 4,
      w9f_q48 == 5 ~ 5,
      w9f_q48 >= 7 & w9f_q48 <= 12 ~ 6,
      w9f_q48 == 88 | w9f_q48 == 99 ~ NA_real_
    ),
    w10_nrvote17 = case_when(
      w10f_q87 == 1 ~ 2,
      w10f_q87 == 2 ~ 1,
      w10f_q87 == 3 ~ 3,
      w10f_q87 == 4 ~ 4,
      w10f_q87 == 5 ~ 5,
      w10f_q87 >= 7 & w10f_q87 <= 12 ~ 6,
      w10f_q87 == 88 | w10f_q87 == 99 ~ NA_real_
    ),
    w11_nrvote17 = case_when(
      w11f_q28 == 1 ~ 2,
      w11f_q28 == 2 ~ 1,
      w11f_q28 == 3 ~ 3,
      w11f_q28 == 4 ~ 4,
      w11f_q28 == 5 ~ 5,
      w11f_q28 >= 7 & w11f_q28 <= 12 ~ 6,
      w11f_q28 == 88 | w11f_q28 == 99 ~ NA_real_
    ),
    w12_nrvote17 = case_when(
      w12f_q24 == 1 ~ 2,
      w12f_q24 == 2 ~ 1,
      w12f_q24 == 3 ~ 3,
      w12f_q24 == 4 ~ 4,
      w12f_q24 == 5 ~ 5,
      w12f_q24 >= 7 & w12f_q24 <= 12 ~ 6,
      w12f_q24 == 88 | w12f_q24 == 99 ~ NA_real_
    )
  )

# Step 2: Replace values for non-voters based on participation variables (`w5_q9`, `w6f_q18`, etc.)
dat_wide <- dat_wide %>%
  mutate(
    # Set to NICHTWAHL (7) if non-voter (indicated by <4 in participation variables)
    w5_nrvote17 = ifelse(w5_q9 < 4, 7, w5_nrvote17),
    w6_nrvote17 = ifelse(w6f_q18 < 4, 7, w6_nrvote17),
    w7_nrvote17 = ifelse(w7_q4 < 4, 7, w7_nrvote17),
    w8_nrvote17 = ifelse(w8_q62 < 4, 7, w8_nrvote17),
    w9_nrvote17 = ifelse(w9f_q47 < 4, 7, w9_nrvote17),
    w10_nrvote17 = ifelse(w10f_q86 < 4, 7, w10_nrvote17),
    w11_nrvote17 = ifelse(w11f_q27 < 4, 7, w11_nrvote17),
    w12_nrvote17 = ifelse(w12f_q23 < 4, 7, w12_nrvote17)
  )

# Step 3: Combine columns into a single `nrvote17` variable by filling NA values sequentially
dat_wide <- dat_wide %>%
  mutate(
    nrvote17 = coalesce(
      w5_nrvote17, w6_nrvote17, w7_nrvote17, w8_nrvote17, w9_nrvote17, 
      w10_nrvote17, w11_nrvote17, w12_nrvote17
    )
  )

# Step 4: Apply labels to `nrvote17` using factor levels
dat_wide <- dat_wide %>%
  mutate(
    nrvote17 = factor(nrvote17, levels = 1:7, labels = vote_levels)
  )

# Optional: View the frequency of the new variable
fre(dat_wide$nrvote17)

# ------------------------------------------------------------------------------
# VOTE NRW 2019
# ------------------------------------------------------------------------------

# Step 1: Recode 2019 data variables to align with 2019 categories
dat_wide <- dat_wide %>%
  mutate(
    nrvote19_w12 = case_when(
      w12_q12 == 1 ~ 1,  # ÖVP
      w12_q12 == 2 ~ 2,  # SPÖ
      w12_q12 == 3 ~ 3,  # FPÖ
      w12_q12 == 4 ~ 5,  # NEOS
      w12_q12 == 5 ~ 6,  # JETZT - Liste Pilz mapped to SONSTIGE/UNGÜLTIG
      w12_q12 == 6 ~ 4,  # GRÜNE
      w12_q12 >= 7 & w12_q12 <= 12 ~ 6,  # KPOE, WANDEL, etc., to SONSTIGE/UNGÜLTIG
      w12_q12 == 88 ~ NA_real_,  # Weiß nicht
      w12_q12 == 99 ~ NA_real_  # Keine Angabe
    ),
    nrvote19_w13 = case_when(
      w13f_q30 == 1 ~ 1,
      w13f_q30 == 2 ~ 2,
      w13f_q30 == 3 ~ 3,
      w13f_q30 == 4 ~ 5,
      w13f_q30 == 5 ~ 6,
      w13f_q30 == 6 ~ 4,
      w13f_q30 >= 7 & w13f_q30 <= 12 ~ 6,
      w13f_q30 == 88 ~ NA_real_,
      w13f_q30 == 99 ~ NA_real_
    ),
    nrvote19_w14 = case_when(
      w14_q22 == 1 ~ 1,
      w14_q22 == 2 ~ 2,
      w14_q22 == 3 ~ 3,
      w14_q22 == 4 ~ 5,
      w14_q22 == 5 ~ 6,
      w14_q22 == 6 ~ 4,
      w14_q22 >= 7 & w14_q22 <= 12 ~ 6,
      w14_q22 == 88 ~ NA_real_,
      w14_q22 == 99 ~ NA_real_
    ),
    nrvote19_w15 = case_when(
      w15f_q31 == 1 ~ 1,
      w15f_q31 == 2 ~ 2,
      w15f_q31 == 3 ~ 3,
      w15f_q31 == 4 ~ 5,
      w15f_q31 == 5 ~ 6,
      w15f_q31 == 6 ~ 4,
      w15f_q31 >= 7 & w15f_q31 <= 12 ~ 6,
      w15f_q31 == 88 ~ NA_real_,
      w15f_q31 == 99 ~ NA_real_
    ),
    nrvote19_w16 = case_when(
      w16_q23 == 1 ~ 1,
      w16_q23 == 2 ~ 2,
      w16_q23 == 3 ~ 3,
      w16_q23 == 4 ~ 5,
      w16_q23 == 5 ~ 6,
      w16_q23 == 6 ~ 4,
      w16_q23 >= 7 & w16_q23 <= 12 ~ 6,
      w16_q23 == 88 ~ NA_real_,
      w16_q23 == 99 ~ NA_real_
    ),
    nrvote19_w17 = case_when(
      w17f_q18 == 1 ~ 1,
      w17f_q18 == 2 ~ 2,
      w17f_q18 == 3 ~ 3,
      w17f_q18 == 4 ~ 5,
      w17f_q18 == 5 ~ 6,
      w17f_q18 == 6 ~ 4,
      w17f_q18 >= 7 & w17f_q18 <= 12 ~ 6,
      w17f_q18 == 88 ~ NA_real_,
      w17f_q18 == 99 ~ NA_real_
    ),
    nrvote19_w18 = case_when(
      w18f_q13 == 1 ~ 1,
      w18f_q13 == 2 ~ 2,
      w18f_q13 == 3 ~ 3,
      w18f_q13 == 4 ~ 5,
      w18f_q13 == 5 ~ 6,
      w18f_q13 == 6 ~ 4,
      w18f_q13 >= 7 & w18f_q13 <= 12 ~ 6,
      w18f_q13 == 88 ~ NA_real_,
      w18f_q13 == 99 ~ NA_real_
    ),
    nrvote19_w19 = case_when(
      w19f_q40 == 1 ~ 1,
      w19f_q40 == 2 ~ 2,
      w19f_q40 == 3 ~ 3,
      w19f_q40 == 4 ~ 5,
      w19f_q40 == 5 ~ 6,
      w19f_q40 == 6 ~ 4,
      w19f_q40 >= 7 & w19f_q40 <= 12 ~ 6,
      w19f_q40 == 88 ~ NA_real_,
      w19f_q40 == 99 ~ NA_real_
    ),
    nrvote19_w20 = case_when(
      w20f_q38 == 1 ~ 1,
      w20f_q38 == 2 ~ 2,
      w20f_q38 == 3 ~ 3,
      w20f_q38 == 4 ~ 5,
      w20f_q38 == 5 ~ 6,
      w20f_q38 == 6 ~ 4,
      w20f_q38 >= 7 & w20f_q38 <= 12 ~ 6,
      w20f_q38 == 88 ~ NA_real_,
      w20f_q38 == 99 ~ NA_real_
    ),
    nrvote19_w21 = case_when(
      w21f_q30 == 1 ~ 1,
      w21f_q30 == 2 ~ 2,
      w21f_q30 == 3 ~ 3,
      w21f_q30 == 4 ~ 5,
      w21f_q30 == 5 ~ 6,
      w21f_q30 == 6 ~ 4,
      w21f_q30 >= 7 & w21f_q30 <= 12 ~ 6,
      w21f_q30 == 88 ~ NA_real_,
      w21f_q30 == 99 ~ NA_real_
    ),
    nrvote19_w22 = case_when(
      w22f_q28 == 1 ~ 1,
      w22f_q28 == 2 ~ 2,
      w22f_q28 == 3 ~ 3,
      w22f_q28 == 4 ~ 5,
      w22f_q28 == 5 ~ 6,
      w22f_q28 == 6 ~ 4,
      w22f_q28 >= 7 & w22f_q28 <= 12 ~ 6,
      w22f_q28 == 88 ~ NA_real_,
      w22f_q28 == 99 ~ NA_real_
    ),
    nrvote19_w23 = case_when(
      w23f_q43 == 1 ~ 1,
      w23f_q43 == 2 ~ 2,
      w23f_q43 == 3 ~ 3,
      w23f_q43 == 4 ~ 5,
      w23f_q43 == 5 ~ 6,
      w23f_q43 == 6 ~ 4,
      w23f_q43 >= 7 & w23f_q43 <= 12 ~ 6,
      w23f_q43 == 88 ~ NA_real_,
      w23f_q43 == 99 ~ NA_real_
    )
  )

# Step 2: Replace values if non-voter (e.g., <4 condition)
dat_wide <- dat_wide %>%
  mutate(
    nrvote19_w12 = ifelse(w12_q11 < 4, 7, nrvote19_w12),
    nrvote19_w13 = ifelse(w13f_q29 < 4, 7, nrvote19_w13),
    nrvote19_w14 = ifelse(w14_q21 < 4, 7, nrvote19_w14),
    nrvote19_w15 = ifelse(w15f_q30 < 4, 7, nrvote19_w15),
    nrvote19_w16 = ifelse(w16_q22 < 4, 7, nrvote19_w16),
    nrvote19_w17 = ifelse(w17f_q17 < 4, 7, nrvote19_w17),
    nrvote19_w18 = ifelse(w18f_q12 < 4, 7, nrvote19_w18),
    nrvote19_w19 = ifelse(w19f_q39 < 4, 7, nrvote19_w19),
    nrvote19_w20 = ifelse(w20f_q37 < 4, 7, nrvote19_w20),
    nrvote19_w21 = ifelse(w21f_q29 < 4, 7, nrvote19_w21),
    nrvote19_w22 = ifelse(w22f_q27 < 4, 7, nrvote19_w22),
    nrvote19_w23 = ifelse(w23f_q42 < 4, 7, nrvote19_w23)
  )

# Step 3: Combine columns into a single nrvote19 variable by filling NA values sequentially
dat_wide <- dat_wide %>%
  mutate(
    nrvote19 = coalesce(
      nrvote19_w12, nrvote19_w13, nrvote19_w14, nrvote19_w15, nrvote19_w16, 
      nrvote19_w17, nrvote19_w18, nrvote19_w19, nrvote19_w20, nrvote19_w21, 
      nrvote19_w22, nrvote19_w23
    )
  )

# Step 4: Apply labels to nrvote19 using factor levels
dat_wide <- dat_wide %>%
  mutate(
    nrvote19 = factor(nrvote19, levels = 1:7, labels = vote_levels)
  )

# Optional: View the frequency of the new variable
fre(dat_wide$nrvote19)

# ------------------------------------------------------------------------------
# VOTE NRW 2024
# ------------------------------------------------------------------------------

# Step 1: Recode `w23_q11` to align with the 2024 categories
dat_wide <- dat_wide %>%
  mutate(
    nrvote24_w23 = case_when(
      w23_q11 == 1 ~ 1,  # ÖVP
      w23_q11 == 2 ~ 2,  # SPÖ
      w23_q11 == 3 ~ 3,  # FPÖ
      w23_q11 == 4 ~ 4,  # GRÜNE
      w23_q11 == 5 ~ 5,  # NEOS
      w23_q11 >= 6 & w23_q11 <= 12 ~ 6,  # Minor parties and "Die Gelben" to SONSTIGE/UNGÜLTIG
      w23_q11 == 77 ~ 6,  # Ungültig gewählt (invalid) to SONSTIGE/UNGÜLTIG
      w23_q11 == 88 ~ NA_real_,  # "Weiß nicht" to NA
      w23_q11 == 99 ~ NA_real_   # "Keine Angabe" to NA
    )
  )

# Step 2: Replace values for non-voters based on `w23_q10`
dat_wide <- dat_wide %>%
  mutate(
    nrvote24_w23 = ifelse(w23_q10 < 4, 7, nrvote24_w23)  # Set to NICHTWAHL if <4 in `w23_q10`
  )

# Step 3: Combine columns into a single nrvote24 variable (only one column here)
dat_wide <- dat_wide %>%
  mutate(
    nrvote24 = nrvote24_w23
  )

# Step 4: Apply labels to nrvote24 using factor levels
dat_wide <- dat_wide %>%
  mutate(
    nrvote24 = factor(nrvote24, levels = 1:7, labels = vote_levels)
  )

# Optional: View the frequency of the new variable
fre(dat_wide$nrvote24)

# ------------------------------------------------------------------------------
# Create the contingency table to check
# ------------------------------------------------------------------------------
contingency_table <- table(dat_wide$nrvote17, dat_wide$nrvote13)

# Convert the contingency table to column percentages
column_percentage_table <- prop.table(contingency_table, margin = 2) * 100

# Display both raw counts and column percentages
print("Raw counts:")
print(contingency_table)

print("Column percentages:")
round(column_percentage_table, 2)

# Convert the contingency table to cell percentages
cell_percentage_table <- prop.table(contingency_table) * 100

# Display both raw counts and cell percentages
print("Raw counts:")
print(contingency_table)

print("Cell percentages:")
round(cell_percentage_table, 2)

table(dat_wide$nrvote13)
table(dat_wide$nrvote17)
table(dat_wide$nrvote19)
table(dat_wide$nrvote24)

# ------------------------------------------------------------------------------
# Calculating pairwise weights
# ------------------------------------------------------------------------------
dat_wide <- dat_wide %>%
  select(id, nrvote13, nrvote17, nrvote19, nrvote24)

# Ensure `dat_wide` is a data frame
dat_wide <- as.data.frame(dat_wide)

# Targets
nrvote13_target <- c("ÖVP" = 0.1764, "SPÖ" = 0.1971, "FPÖ" = 0.1507, "GRÜNE" = 0.0913, 
                   "NEOS" = 0.0365, "SONSTIGE/UNGÜLTIG" = 0.0971, "NICHTWAHL" = 0.2509)
nrvote17_target <- c("ÖVP" = 0.2493, "SPÖ" = 0.2127, "FPÖ" = 0.2057, "GRÜNE" = 0.0301, 
                   "NEOS" = 0.0419, "SONSTIGE/UNGÜLTIG" = 0.0603, "NICHTWAHL" = 0.2000)
nrvote19_target <- c("ÖVP" = 0.2797, "SPÖ" = 0.1582, "FPÖ" = 0.1208, "GRÜNE" = 0.1038, 
                  "NEOS" = 0.0605, "SONSTIGE/UNGÜLTIG" = 0.0329, "NICHTWAHL" = 0.2441)
nrvote24_target <- c("ÖVP" = 0.2021, "SPÖ" = 0.1627, "FPÖ" = 0.2220, "GRÜNE" = 0.0634, 
                 "NEOS" = 0.0703, "SONSTIGE/UNGÜLTIG" = 0.0564, "NICHTWAHL" = 0.2231)				  

# Lists with targets
targets_2013_2017 <- list(nrvote13 = nrvote13_target, nrvote17 = nrvote17_target)
targets_2017_2019 <- list(nrvote17 = nrvote17_target, nrvote19 = nrvote19_target)
targets_2019_2024 <- list(nrvote19 = nrvote19_target, nrvote24 = nrvote24_target)

# Apply raking for 2013-2017
dat_wide_1317 <- dat_wide %>%
  filter(!is.na(nrvote13) & !is.na(nrvote17))
weight1317 <- anesrake(inputter = targets_2013_2017,
                     dataframe = dat_wide_1317,
                     caseid = dat_wide_1317$id,
                     choosemethod = "total",
                     type = "pctlim",
                     pctlim = 0.001,
                     cap = 5,
                     verbose = TRUE)
					 
# Apply raking for 2017-2019
dat_wide_1719 <- dat_wide %>%
  filter(!is.na(nrvote17) & !is.na(nrvote19))
weight1719 <- anesrake(inputter = targets_2017_2019,
                  dataframe = dat_wide_1719,
                  caseid = dat_wide_1719$id,
                  choosemethod = "total",
                  type = "pctlim",
                  pctlim = 0.001,
                  cap = 5,
                  verbose = TRUE)
				  
# Apply raking for 2019-2024
dat_wide_1924 <- dat_wide %>%
  filter(!is.na(nrvote19) & !is.na(nrvote24))
weight1924 <- anesrake(inputter = targets_2019_2024,
                   dataframe = dat_wide_1924,
                   caseid = dat_wide_1924$id,
                   choosemethod = "total",
                   type = "pctlim",
                   pctlim = 0.001,
                   cap = 5,
                   verbose = TRUE)

# Extract weights for 2013-2017 and merge with dat_wide
weight_df_1317 <- data.frame(id = dat_wide_1317$id, weight17 = weight1317$weightvec)
dat_wide <- left_join(dat_wide, weight_df_1317, by = "id")

# Extract weights for 2017-2019 and merge with dat_wide
weight_df_1719 <- data.frame(id = dat_wide_1719$id, weight19 = weight1719$weightvec)
dat_wide <- left_join(dat_wide, weight_df_1719, by = "id")

# Extract weights for 2019-2024 and merge with dat_wide
weight_df_1924 <- data.frame(id = dat_wide_1924$id, weight24 = weight1924$weightvec)
dat_wide <- left_join(dat_wide, weight_df_1924, by = "id")

# Check the structure of dat_wide to ensure weights were merged correctly
head(dat_wide)

summary(dat_wide$weight17)
summary(dat_wide$weight19)
summary(dat_wide$weight24)


# ------------------------------------------------------------------------------
# Create weighted contingency table to check again
# ------------------------------------------------------------------------------
  
# Calculate the weighted contingency table for nrvote17 vs nrvote13
weighted_contingency_table <- dat_wide %>%
	group_by(nrvote17, nrvote13) %>%
	dplyr::summarize(weighted_count = sum(weight17, na.rm = TRUE), .groups = "drop") %>%
	pivot_wider(names_from = nrvote13, values_from = weighted_count, values_fill = 0)

# Convert the weighted counts to cell percentages
total_weighted_count <- sum(weighted_contingency_table[,-1], na.rm = TRUE)
cell_percentage_table <- weighted_contingency_table %>%
	mutate(across(-nrvote17, ~ . / total_weighted_count * 100))
print(cell_percentage_table)

# Calculate row totals (margin for nrvote17)
row_totals <- cell_percentage_table %>%
  mutate(RowTotal = rowSums(across(-nrvote17), na.rm = TRUE))

# Calculate column totals (margin for nrvote13)
column_totals <- cell_percentage_table %>%
  dplyr::summarise(across(-nrvote17, sum, na.rm = TRUE)) %>%
  mutate(nrvote17 = "ColumnTotal")  # Label this row as "ColumnTotal"

# Combine row and column totals into the final table
final_table <- bind_rows(row_totals, column_totals)

# Print the final table with margins
print("Table with Margins:")
print(final_table)

# ------------------------------------------------------------------------------
# Reshape to long, create lags, filter NAs, generate links and nodes
# ------------------------------------------------------------------------------

# Step 1: Reshape to long format
dat_long <- dat_wide %>%
  select(id, nrvote13, nrvote17, nrvote19, nrvote24, weight17, weight19, weight24) %>%
  pivot_longer(
    cols = matches("^(nrvote|weight)\\d{2}$"),  # Select columns that match nrvote or weight followed by two digits
    names_to = c(".value", "year"),  # Use .value to split into 'vote' and 'weight' columns based on the prefix
    names_pattern = "(nrvote|weight)(\\d{2})",  # Regex to capture type (nrvote or weight) and the year suffix
    names_transform = list(year = as.numeric)  # Convert 'year' to numeric
  ) %>%
  arrange(id, year)  # Arrange by 'id' and 'year'
head(dat_long)

# Step 2: Create the lagged vote variable
dat_long <- dat_long %>%
  group_by(id) %>%
  mutate(nrvote_lag = lag(nrvote)) %>%  # Create lagged vote by one wave
  ungroup()
head(dat_long, n=20)

# Step 3: Filter out NAs  
dat_long <- dat_long %>% 
	filter(!is.na(nrvote) & !is.na(nrvote_lag))  # Filter out rows with NA values
head(dat_long, n=20)	

# Step 4: Summarize the transitions and calculate percentages (weights needed!!)
links <- dat_long %>%
	group_by(year, nrvote, nrvote_lag) %>% 
	dplyr::summarize(weighted_count = sum(weight, na.rm = TRUE)) %>%
	group_by(year) %>% 
	mutate(
  	  sum_weighted_obs = sum(weighted_count, na.rm = TRUE),
	  percent = (weighted_count / sum_weighted_obs) * 100
	) %>%
	ungroup() 
print(links)
	
# Step 5: Rename source and target
successor_years <- c("17" = "13", "19" = "17", "24" = "19")
links <- links %>%
	mutate(
	    source = paste(nrvote_lag, successor_years[as.character(year)], sep = "-"),
	    target = paste(nrvote, year, sep = "-")
		) %>%
	select(source, target, percent)
print(links)	

# Step 5: Generate unique nodes
nodes <- tibble(node = unique(c(links$source, links$target)))
print(nodes)

# ------------------------------------------------------------------------------
# Sankey Diagram
# ------------------------------------------------------------------------------

# Prepare nodes and links data for Sankey diagram
# ------------------------------------------------------------------------------
# Assign a unique index to each node for the Sankey diagram
nodes <- nodes %>%
  mutate(id = row_number() - 1)  # 'id' starts from 0 for networkD3

# Convert `links` source and target to indices
links <- links %>%
  left_join(nodes, by = c("source" = "node")) %>%
  rename(source_id = id) %>%
  left_join(nodes, by = c("target" = "node")) %>%
  rename(target_id = id) %>%
  select(source_id, target_id, percent)

# Define colors based on party
party_colors <- c(
  "ÖVP" = "rgb(99, 195, 208)",         # Light Blue
  "SPÖ" = "rgb(255, 0, 0)",            # Red
  "FPÖ" = "rgb(0, 102, 255)",          # Blue
  "GRÜNE" = "rgb(146, 208, 80)",       # Green
  "NEOS" = "rgb(232, 65, 136)",        # Pink
  "SONSTIGE/UNGÜLTIG" = "rgb(169, 169, 169)", # Grey for invalid/others
  "NICHTWAHL" = "rgb(169, 169, 169)"   # Dark grey for non-voters
)

# Map colors to nodes based on party names extracted from node labels
nodes <- nodes %>%
  mutate(
    party = sub("-.*", "", node),  # Extract party name from node labels
    NodeGroup = party_colors[party]    # Assign color based on party
  )

# Save links and nodes to RDS
saveRDS(links, file = file.path(DATA, "dat_links.rds"))
saveRDS(nodes, file = file.path(DATA, "dat_nodes.rds"))


# Step (done in dashboard, not needed but html if wanted): 
# Create the Sankey diagram with colors
# ------------------------------------------------------------------------------
sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source_id",
  Target = "target_id",
  Value = "percent",
  NodeID = "node",
  units = "%",
  fontSize = 14,
  nodeWidth = 40,
  nodePadding = 20,  
  NodeGroup = "NodeGroup",
  colourScale = JS(sprintf("d3.scaleOrdinal().domain(%s).range(%s)",
                             jsonlite::toJSON(names(party_colors)),
                             jsonlite::toJSON(unname(party_colors))))
 )


# Use onRender to inject custom JavaScript for tooltip formatting
sankey <- htmlwidgets::onRender(
sankey,
"
function(el) {
 // Select all link elements and format the tooltip text
 d3.select(el).selectAll('.link').select('title')
   .text(function(d) {
     var percentage = d.value.toFixed(1) + ' %'; // Format to one decimal place
     return d.source.name + ' → ' + d.target.name + ': ' + percentage; // Add space between label and percentage
   });

 // Select all node elements and format the tooltip text
 d3.select(el).selectAll('.node').select('title')
   .text(function(d) {
     return d.name + ': ' + d.value.toFixed(1) + ' %';
   });
}
"
)

# Adjusting link color based on source node color
sankey <- htmlwidgets::onRender(
  sankey,
  "
  function(el) {
    d3.select(el).selectAll('.link')
      .style('stroke-opacity', 0.5)  // Adjust opacity for visibility
      .style('stroke', function(d) { return d.source.color; });  // Use source node color for links
  }
  "
)

# Save the interactive Sankey diagram as an HTML file
saveWidget(sankey, file = file.path(GRAPHS, "sankey_diagram.html"), selfcontained = TRUE)

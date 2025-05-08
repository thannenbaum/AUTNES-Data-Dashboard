# ==============================================================================
# PROJECT:       AUTNES Dashboard
# OBJECTIVE:     Combined Data Management for Overall and Grouped Means
# AUTHORS:       JP, PVDT
# DATE:          2024-09
# ==============================================================================

# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------
library(haven)
library(labelled)
library(forcats)
library(tidyr)
library(stringr)
library(lubridate)
library(dplyr)

# suppress warnings
options(warn = -1)

# ------------------------------------------------------------------------------
# Define Paths
# ------------------------------------------------------------------------------
MAIN <- "D:/Studium/TU/194.147 Interdisciplinary Project in Data Science/working_dir"
DATA <- file.path(MAIN, "data")

# ------------------------------------------------------------------------------
# Set Current Wave
# ------------------------------------------------------------------------------
num_wave <- 24

# ------------------------------------------------------------------------------
# Load and Prepare Data
# ------------------------------------------------------------------------------
# Open data file and apply as_factor to the entire dataset (already a tibble)
dat <- read_dta(file.path(DATA, "AUTNES_OPS_2017-2024_w1-24_DE.dta")) %>% 
  as_factor()

# ------------------------------------------------------------------------------
# Rename Date Variables
# ------------------------------------------------------------------------------
for (num in 1:num_wave) {
  old_name <- paste0("w", num, "_date")
  new_name <- paste0("date_w", num)
  names(dat)[names(dat) == old_name] <- new_name
}

# ------------------------------------------------------------------------------
# Rename Weight Variables
# ------------------------------------------------------------------------------
for (num in 1:num_wave) {
  old_name <- paste0("w", num, "_weightd")
  new_name <- paste0("weightd_w", num)
  names(dat)[names(dat) == old_name] <- new_name
}

for (num in 1:num_wave) {
  old_name <- paste0("w", num, "_weightp")
  new_name <- paste0("weightp_w", num)
  names(dat)[names(dat) == old_name] <- new_name
}  

# ------------------------------------------------------------------------------
# Define Labels and Basenames
# ------------------------------------------------------------------------------
# List of labels
labels <- c(
  "VERTRAUEN: DEM NATIONALRAT",
  "VERTRAUEN: DER BUNDESREGIERUNG",
  "VERTRAUEN: DER POLIZEI",
  "VERTRAUEN: DER EUROPAEISCHEN UNION",
  "VERTRAUEN: DEN MEDIEN",
  "VERTRAUEN: DER JUSTIZ",
  "VERTRAUEN: DEM VERFASSUNGSGERICHTSHOF",
  "VERTRAUEN: DEM BUNDESPRAESIDENT",
  
  "POPULISMUS: KOMPROMISSE HEISST PRINZIPIEN VERKAUFEN",
  "POPULISMUS: POLITIKER KUEMMERN SICH UM INTERESSEN D. REICHEN",
  "POPULISMUS: DIE MEISTEN POLITIKER SIND VERTRAUENSWUERDIG",
  "POPULISMUS: DIE PARTEIEN SIND HAUPTPROBLEM IN OESTERREICH",
  "POPULISMUS: STARKE FUEHRUNGSPERSOENLICHKEIT, ENTSCHEIDET IM ALLEINGANG",
  "POPULISMUS: VOLK, NICHT POLITIKER, SOLLTE ENTSCHEIDUNGEN TREFFEN",
  "POPULISMUS: BEVORZUGE UNABHAENGIGE BUERGER STATT PARTEIMITGLIED",
  
  # keep this order, later reference to "ptv2", "ptv3" and "ptv4" is necessary for joining OEVP values
  "PTV: SPOE",
  "PTV: OEVP",
  "PTV: LISTE SEBASTIAN KURZ - DIE NEUE VOLKSPARTEI",
  "PTV: OEVP - DIE NEUE VOLKSPARTEI",
  "PTV: FPOE",
  "PTV: GRUENE",
  "PTV: NEOS",
  
  "NATIONALSTOLZ",
  
  "VERBUNDENHEIT MIT: WOHNORT",
  "VERBUNDENHEIT MIT: BUNDESLAND",
  "VERBUNDENHEIT MIT: OESTERREICH",
  "VERBUNDENHEIT MIT: EUROPA",
  
  "POLITISCHES INTERESSE",
  
  # keep this order, later reference to "meinung13" and "meinung18" is necessary
  "MEINUNG: IM ALLGEMEINEN WEISS ICH RECHT VIEL UEBER POLITIK",
  "MEINUNG: POLITIKER KUEMMERN SICH NICHT, WAS LEUTE WIE ICH DENKEN",
  "MEINUNG: POLITIK MUSS DIE SOZIALE UNGLEICHHEIT BEKAEMPFEN",
  "MEINUNG: SOZIALSTAAT MACHT MENSCHEN TRAEGE UND FAUL",
  "MEINUNG: ARBEITSLOSIGKEIT BEKAEMPFEN, AUCH WENN HOHE STAATSSCHULDEN",
  "MEINUNG: POLITIK SOLL SICH AUS WIRTSCHAFT HERAUSHALTEN",
  "MEINUNG: FRAUEN BEI BEWERBUNGEN BEI GLEICHER QUALIFIKATION BEVORZUGEN",
  "MEINUNG: BEFUGNISSE DER POLIZEI AUSWEITEN",
  "MEINUNG: ZUWANDERUNG NACH OESTERREICH NUR IN AUSNAHMEFAELLEN",
  "MEINUNG: UMWELT UM JEDEN PREIS SCHUETZEN, AUCH WENN TEURER",
  "MEINUNG: EINKOMMENSUNTERSCHIEDE IN OESTERREICH SIND ZU GROSS",
  "MEINUNG: JUNGE VON HEUTE WERDEN BESSERES LEBEN HABEN",
  "MEINUNG: ZUWANDERER SOLLEN SICH GEPFLOGENHEITEN ANPASSEN", # meinung13
  "MEINUNG: ZUWANDERER BEREICHERN DIE OESTERREICHISCHE KULTUR",
  "MEINUNG: ZUWANDERER IM ALLGEMEINEN GUT FUER WIRTSCHAFT",
  "MEINUNG: ZUWANDERER NEHMEN OESTERREICHERN ARBEITSPLAETZE WEG", # meinung18
  "MEINUNG: DURCH ZUWANDERER NIMMT KRIMINALITAET IN OESTERREICH ZU",
  "MEINUNG: ZUWANDERER ZAHLEN MEHR IN SOZIALSYSTEM EIN",
  
  "WIRTSCHAFTSLAGE OESTERREICH: RETROSPEKTIV (LETZTE 12 MONATE)",
  "WIRTSCHAFTSLAGE OESTERREICH: PROSPEKTIV (NAECHSTE 12 MONATE)",
  "EIGENE WIRTSCHAFTLICHE LAGE: RETROSPEKTIV (LETZTE 12 MONATE)",
  "EIGENE WIRTSCHAFTLICHE LAGE",
  
  "SYMPATHIE: CHRISTIAN KERN",
  "SYMPATHIE: SEBASTIAN KURZ",
  "SYMPATHIE: HEINZ-CHRISTIAN STRACHE",
  "SYMPATHIE: NORBERT HOFER",
  "SYMPATHIE: ULRIKE LUNACEK",
  "SYMPATHIE: MATTHIAS STROLZ",
  "SYMPATHIE: PAMELA RENDI-WAGNER",
  "SYMPATHIE: ANDREAS BABLER",
  "SYMPATHIE: KARL NEHAMMER",
  "SYMPATHIE: HERBERT KICKL",
  "SYMPATHIE: WERNER KOGLER",
  "SYMPATHIE: BEATE MEINL-REISINGER",
  
  "KOMPETENTESTE PARTEI: ARBEITSMARKT UND BESCHAEFTIGUNG",
  "KOMPETENTESTE PARTEI: ZUWANDERUNG",
  "KOMPETENTESTE PARTEI: EUROPAEISCHE INTEGRATION",
  "KOMPETENTESTE PARTEI: WIRTSCHAFTSPOLITIK",
  "KOMPETENTESTE PARTEI: UMWELTSCHUTZ",
  "KOMPETENTESTE PARTEI: KORRUPTIONSBEKAEMPFUNG",
  "KOMPETENTESTE PARTEI: PENSIONEN",
  "KOMPETENTESTE PARTEI: TEUERUNG/INFLATION",
  
  "PARTEIIDENTIFIKATION VORHANDEN",
  "PARTEIIDENTIFIKATION TENDENZ",
  "PARTEIIDENTIFIKATION MIT WELCHER PARTEI",
  "STAERKE PARTEIIDENTIFIKATION",
  
  "LINKS-RECHTS SELBSTEINSTUFUNG",
  "LINKS-RECHTS EINSTUFUNG: SPOE",
  "LINKS-RECHTS EINSTUFUNG: OEVP",
  "LINKS-RECHTS EINSTUFUNG: FPOE",
  "LINKS-RECHTS EINSTUFUNG: GRUENE",
  "LINKS-RECHTS EINSTUFUNG: NEOS",
  
  "EINSCHAETZUNG DER POLIT. LAGE: ZUVERSICHTLICH",
  "EINSCHAETZUNG DER POLIT. LAGE: BESORGT",
  "EINSCHAETZUNG DER POLIT. LAGE: VERAERGERT",
  "EINSCHAETZUNG DER POLIT. LAGE: AENGSTLICH",
  "EINSCHAETZUNG DER POLIT. LAGE: HOFFNUNGSVOLL",
  "EINSCHAETZUNG DER POLIT. LAGE: WUETEND",
  
  "INFORMATIONEN ZU POLITISCHEM GESCHEHEN: IM FERNSEHEN",
  "INFORMATIONEN ZU POLITISCHEM GESCHEHEN: IN ZEITUNGEN",
  "INFORMATIONEN ZU POLITISCHEM GESCHEHEN: IM RADIO",
  "INFORMATIONEN ZU POLITISCHEM GESCHEHEN: IM INTERNET",
  "INFORMATIONEN ZU POLITISCHEM GESCHEHEN: IN SOCIAL MEDIA",
  
  "AUFMERKSAMKEIT GEGENUEBER WAHLKAMPF",
  "IM WAHLKAMPF BRIEF/FLYER/E-MAIL ERHALTEN (JA/NEIN)",
  "IM WAHLKAMPF TELEFONANRUF/SMS/WHATSAPP NACHRICHT ERHALTEN (JA/NEIN)",
  "IM WAHLKAMPF INFOMATERIAL/WERBEGESCHENK ERHALTEN (JA/NEIN)",
  "IM WAHLKAMPF MIT PARTEIMITARBEITER GESPROCHEN (JA/NEIN)",
  "IM WAHLKAMPF BESUCH VON POLITIKER ERHALTEN (JA/NEIN)",
  "IM WAHLKAMPF WAHLWERBUNG IM FERNSEHEN GESEHEN (JA/NEIN)",
  "IM WAHLKAMPF WAHLWERBUNG IN ZEITUNG/SOZIALEN NETZWERKEN GESEHEN (JA/NEIN)",
  "IM WAHLKAMPF TV-DEBATTEN GESEHEN (JA/NEIN)",
  
  "KOALITIONSWAHRSCHEINLICHKEIT: OEVP-SPOE",
  "KOALITIONSWAHRSCHEINLICHKEIT: OEVP-FPOE",
  "KOALITIONSWAHRSCHEINLICHKEIT: SPOE-FPOE",
  "KOALITIONSWAHRSCHEINLICHKEIT: OEVP-GRUENE", 
  "KOALITIONSWAHRSCHEINLICHKEIT: OEVP-GRUENE-NEOS",
  "KOALITIONSWAHRSCHEINLICHKEIT: SPOE-GRUENE-NEOS",
  "KOALITIONSWAHRSCHEINLICHKEIT: OEVP-SPOE-GRUENE",
  "KOALITIONSWAHRSCHEINLICHKEIT: OEVP-SPOE-NEOS",
  "KOALITIONSWAHRSCHEINLICHKEIT: SPOE-OEVP",
  "KOALITIONSWAHRSCHEINLICHKEIT: SPOE-OEVP-GRUENE",
  "KOALITIONSWAHRSCHEINLICHKEIT: SPOE-OEVP-NEOS",
  "KOALITIONSWAHRSCHEINLICHKEIT: OEVP-NEOS",
  
  "KOALITIONSWUNSCH: OEVP-SPOE",
  "KOALITIONSWUNSCH: OEVP-FPOE",
  "KOALITIONSWUNSCH: SPOE-FPOE",
  "KOALITIONSWUNSCH: OEVP-GRUENE",
  "KOALITIONSWUNSCH: OEVP-GRUENE-NEOS",
  "KOALITIONSWUNSCH: SPOE-GRUENE-NEOS",
  "KOALITIONSWUNSCH: OEVP-SPOE-GRUENE",
  "KOALITIONSWUNSCH: OEVP-SPOE-NEOS",
  "KOALITIONSWUNSCH: SPOE-OEVP",
  "KOALITIONSWUNSCH: OEVP-NEOS"
)

# Corresponding list of basenames
basenames <- c(
  "vertrauen1",
  "vertrauen2",
  "vertrauen3",
  "vertrauen4",
  "vertrauen5",
  "vertrauen6",
  "vertrauen7",
  "vertrauen8",
  
  "populismus1",
  "populismus2",
  "populismus3",
  "populismus4",
  "populismus5",
  "populismus6",
  "populismus7",
  
  "ptv1",
  "ptv2",
  "ptv3",
  "ptv4",
  "ptv5",
  "ptv6",
  "ptv7",
  
  "nationalstolz",
  
  "verbundenheit1",
  "verbundenheit2",
  "verbundenheit3",
  "verbundenheit4",
  
  "politisches_interesse",
  
  "meinung1",
  "meinung2",
  "meinung3",
  "meinung4",
  "meinung5",
  "meinung6",
  "meinung7",
  "meinung8",
  "meinung9",
  "meinung10",
  "meinung11",
  "meinung12",
  "meinung13",
  "meinung14",
  "meinung15",
  "meinung16",
  "meinung17",
  "meinung18",
  
  "wirtschaftslage1",
  "wirtschaftslage2",
  "wirtschaftslage3",
  "wirtschaftslage4",
  
  "sympathie1",
  "sympathie2",
  "sympathie3",
  "sympathie4",
  "sympathie5",
  "sympathie6",
  "sympathie7",
  "sympathie8",
  "sympathie9",
  "sympathie10",
  "sympathie11",
  "sympathie12",
  
  "kompetenteste_partei1",
  "kompetenteste_partei2",
  "kompetenteste_partei3",
  "kompetenteste_partei4",
  "kompetenteste_partei5",
  "kompetenteste_partei6",
  "kompetenteste_partei7",
  "kompetenteste_partei8",
  
  "parteiidentifikation1",
  "parteiidentifikation2",
  "parteiidentifikation3",
  "parteiidentifikation4",
  
  "links_rechts_einstufung1",
  "links_rechts_einstufung2",
  "links_rechts_einstufung3",
  "links_rechts_einstufung4",
  "links_rechts_einstufung5",
  "links_rechts_einstufung6",
  
  "politische_lage1",
  "politische_lage2",
  "politische_lage3",
  "politische_lage4",
  "politische_lage5",
  "politische_lage6",
  
  "informationen1",
  "informationen2",
  "informationen3",
  "informationen4",
  "informationen5",
  
  "wahlkampf1",
  "wahlkampf2",
  "wahlkampf3",
  "wahlkampf4",
  "wahlkampf5",
  "wahlkampf6",
  "wahlkampf7",
  "wahlkampf8",
  "wahlkampf9",
  
  "koalitionswahrscheinlichkeit1",
  "koalitionswahrscheinlichkeit2",
  "koalitionswahrscheinlichkeit3",
  "koalitionswahrscheinlichkeit4",
  "koalitionswahrscheinlichkeit5",
  "koalitionswahrscheinlichkeit6",
  "koalitionswahrscheinlichkeit7",
  "koalitionswahrscheinlichkeit8",
  "koalitionswahrscheinlichkeit9",
  "koalitionswahrscheinlichkeit10",
  "koalitionswahrscheinlichkeit11",
  "koalitionswahrscheinlichkeit12",
  
  "koalitionswunsch1",
  "koalitionswunsch2",
  "koalitionswunsch3",
  "koalitionswunsch4",
  "koalitionswunsch5",
  "koalitionswunsch6",
  "koalitionswunsch7",
  "koalitionswunsch8",
  "koalitionswunsch9",
  "koalitionswunsch10"
)

# Create a named list that maps labels to basenames
basename_to_label <- setNames(basenames, labels)
label_to_basename <- setNames(labels, basenames)

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

# Function to map category values to numeric codes
map_category_to_numeric <- function(category) {
  case_when(
    
    # General cases
    category %in% c("Weiss nicht", "weiss nicht", "weiß nicht", "Weiß nicht") ~ 88,
    category %in% c("kenne diese Person nicht", "ich kenne diese Person nicht",
                    "kenne diese Partei nicht", "ich kenne diese Partei nicht",
                    "ich kenne diese Partei/Person nicht", "kenne diese Partei/Person nicht",
                    "keine Angabe", "Keine Angabe") ~ 90,
    
    # Populismus / Meinung
    category %in% c("trifft gar nicht zu") ~ 1,
    category %in% c("trifft eher nicht zu") ~ 2,
    category %in% c("teils-teils") ~ 3,
    category %in% c("trifft eher zu") ~ 4,
    category %in% c("trifft sehr zu") ~ 5,
    
    # PTV
    category %in% c("0 = sehr unwahrscheinlich") ~ 0,
    category %in% as.character(1:10) ~ as.numeric(category),
    category %in% c("10 = sehr wahrscheinlich") ~ 10,
    
    # Wirtschaftliche Lage
    category %in% c("viel schlechter", "viel schlechter geworden", "viel schlechter werden") ~ 1,
    category %in% c("etwas schlechter", "etwas schlechter geworden", "etwas schlechter werden") ~ 2,
    category %in% c("gleich geblieben", "gleich bleiben") ~ 3,
    category %in% c("etwas besser", "etwas besser geworden", "etwas besser werden") ~ 4,
    category %in% c("viel besser", "viel besser geworden", "viel besser werden") ~ 5,
    
    # Sympathie
    category %in% c("0 = gar nicht sympathisch") ~ 0,
    category %in% as.character(1:10) ~ as.numeric(category),
    category %in% c("10 = sehr sympathisch") ~ 10,
    
    # Vertrauen
    category %in% c("0 = vertraue ich ueberhaupt nicht") ~ 0,
    category %in% as.character(1:10) ~ as.numeric(category),
    category %in% c("10 = vertraue ich voll und ganz") ~ 10,
    
    # Nationalstolz
    category %in% c("ueberhaupt nicht stolz") ~ 1,
    category %in% c("weniger stolz") ~ 2,
    category %in% c("teils-teils") ~ 3,
    category %in% c("stolz") ~ 4,
    category %in% c("sehr stolz") ~ 5,
    
    # Verbundenheit
    category %in% c("ueberhaupt nicht stark") ~ 1,
    category %in% c("nicht sehr stark") ~ 2,
    category %in% c("stark") ~ 3,
    category %in% c("sehr stark") ~ 4,
    
    # Politisches Interesse
    category %in% c("gar nicht interessiert") ~ 1,
    category %in% c("wenig interessiert") ~ 2,
    category %in% c("ziemlich interessiert") ~ 3,
    category %in% c("sehr interessiert") ~ 4,
    
    # Parteiidentifikation
    category %in% c("Nein", "nein") ~ 0,
    category %in% c("Ja", "ja") ~ 1,
    category %in% c("nicht sehr nahe") ~ 1,
    category %in% c("ziemlich nahe") ~ 2,
    category %in% c("sehr nahe") ~ 3,
    
    # Links-Rechts-Einstufung
    category %in% c("0 = links") ~ 0,
    category %in% c("10 = rechts") ~ 10,
    category %in% as.character(1:9) ~ as.numeric(category),
    
    # Einschaetzung der polit. Lage
    category %in% c("0 = ueberhaupt nicht") ~ 0,
    category %in% c("10 = sehr") ~ 10,
    category %in% as.character(1:9) ~ as.numeric(category),
    
    # Informationen des polit. Geschehens
    category == "nie" ~ 1,
    category == "seltener" ~ 2,
    category == "mehrmals pro Monat" ~ 3,
    category == "mehrmals pro Woche" ~ 4,
    category == "nahezu taeglich" ~ 5,
    category == "mehrmals taeglich" ~ 6,
    
    # Wahlkampf
    category %in% c("gar nicht aufmerksam", "Gar nicht aufmerksam") ~ 1,
    category %in% c("wenig aufmerksam", "Wenig aufmerksam") ~ 2,
    category %in% c("ziemlich aufmerksam", "Ziemlich aufmerksam") ~ 3,
    category %in% c("sehr aufmerksam", "Sehr aufmerksam") ~ 4,
    category %in% c("Nein", "nein") ~ 0,
    category %in% c("Ja", "ja") ~ 1,
    
    # Koalitionswahrscheinlichkeit
    category %in% c("0 = sehr unwahrscheinlich") ~ 0,
    category %in% as.character(1:10) ~ as.numeric(category),
    category %in% c("10 = sehr wahrscheinlich") ~ 10,
    
    # Koalitionswunsch
    category %in% c("0 = wuensche ich mir ueberhaupt nicht") ~ 0,
    category %in% as.character(1:10) ~ as.numeric(category),
    category %in% c("10 = wuensche ich mir sehr") ~ 10,
    
    TRUE ~ NA_real_  # Handle any unexpected values as NA
  )
}

# Helper function to escape special characters in a label for `grep`
escape_special_chars <- function(label) {
  # Escape parentheses and other regex special characters
  label <- gsub("([\\(\\)])", "\\\\\\1", label, perl = TRUE)
  return(label)
}

# Function to rename variables with the corresponding base name and move the prefix to the suffix
rename_vars_by_label <- function(dat, basename_to_label) {
  renamed_vars <- c()  # Store renamed variable names
  
  for (label in names(basename_to_label)) {
    basename <- basename_to_label[[label]]
    
    # Select variables based on their label matching exactly the specified text
    selected_vars <- names(sapply(dat, function(x) attr(x, "label"))[
      grep(paste0("^", escape_special_chars(label), "$"), sapply(dat, function(x) attr(x, "label")))
    ])
    
    if (length(selected_vars) == 0) {
      next  # Skip if no variables match the label
    }
    
    # Create a renaming function for the selected variables
    renaming_function <- function(names) {
      sapply(names, function(name) {
        if (grepl("^w[0-9]+_", name)) {
          prefix <- sub("_.*", "", name)
          new_name <- paste0(basename, "_", prefix)
          renamed_vars <<- c(renamed_vars, new_name)  # Add to renamed_vars
          return(new_name)
        } else {
          return(name)
        }
      })
    }
    
    # Rename the selected variables in the dataset using rename_with and all_of
    dat <- dat %>% rename_with(renaming_function, all_of(selected_vars))
    
    # Print renamed variables for inspection
    renamed_vars_info <- tibble(Old_Name = selected_vars, New_Name = renaming_function(selected_vars))
    print(renamed_vars_info)
  }
  
  return(list(dat = dat, renamed_vars = renamed_vars))
}

# ------------------------------------------------------------------------------
# Rename Variables Based on Labels
# ------------------------------------------------------------------------------
rename_result <- rename_vars_by_label(dat, basename_to_label)
dat <- rename_result$dat
renamed_vars <- rename_result$renamed_vars

# ------------------------------------------------------------------------------
# Select Relevant Variables
# ------------------------------------------------------------------------------
dat_subset <- dat %>%
  select(id, age, age_y19, age_y22, age_y24, sd3, sd3_y19, sd3_y22, sd3_y24, sd7, sd7_y19, sd7_y22, sd7_y24,  
         starts_with("date_w"), starts_with("weightd_w"), all_of(renamed_vars)) %>% 
  rename(age_y17 = age,
         sd3_y17 = sd3,
         sd7_y17 = sd7) %>% 
  select(-meinung13_w8, -meinung16_w8)  # Remove columns with different answer scale

# ------------------------------------------------------------------------------
# Combine "wirtschaftslage" 3 and 4 (different labels)
# ------------------------------------------------------------------------------
for (w in 1:num_wave) {
  old_col <- paste0("wirtschaftslage4_w", w)
  new_col <- paste0("wirtschaftslage3_w", w)
  
  if (old_col %in% names(dat_subset)) {
    # Rename the "_wX" column
    names(dat_subset)[names(dat_subset) == old_col] <- new_col
    
    # Unify the label to the same text used for wirschaftslage3
    if (!is.null(attr(dat_subset[[new_col]], "label"))) {
      attr(dat_subset[[new_col]], "label") <- "EIGENE WIRTSCHAFTLICHE LAGE: RETROSPEKTIV (LETZTE 12 MONATE)"
    }
  }
}

# remove references to wirtschaftslage4 from label mappings
basename_to_label <- basename_to_label[!basename_to_label %in% "wirtschaftslage4"]
label_to_basename <- label_to_basename[!names(label_to_basename) %in% "wirtschaftslage4"]

# ------------------------------------------------------------------------------
# Merge PTV of OEVP and its alias names (LISTE KURZ, OEVP - NEUE VP)
# ------------------------------------------------------------------------------
ptv_aliases <- c("ptv3", "ptv4")

for (w in 1:num_wave) {
  col_ptv2 <- paste0("ptv2_w", w)
  
  for (alias in ptv_aliases) {
    col_alias <- paste0(alias, "_w", w)
    
    # If alias column exists, and ptv2 column does not exist, rename it:
    if ((col_alias %in% names(dat_subset)) && !(col_ptv2 %in% names(dat_subset))) {
      
      # Rename the alias column 
      names(dat_subset)[names(dat_subset) == col_alias] <- col_ptv2
      
      # Unify the variable label to "PTV: OEVP"
      if (!is.null(attr(dat_subset[[col_ptv2]], "label"))) {
        attr(dat_subset[[col_ptv2]], "label") <- "PTV: OEVP"
      }
    }
  }
}

# remove references to ptv3 and ptv4 from label mappings
label_to_basename <- label_to_basename[ !names(label_to_basename) %in% c("ptv3","ptv4") ]
basename_to_label <- basename_to_label[ !basename_to_label %in% c("ptv3","ptv4") ]

# ------------------------------------------------------------------------------
# Reshape to Long Format
# ------------------------------------------------------------------------------
dat_long <- dat_subset %>%
  pivot_longer(
    cols = matches("_w\\d+$"),  # Selecting columns that end with _w{wave_number}
    names_to = c(".value", "wave"),
    names_pattern = "(.*)_w(\\d+)$"
  )

# ------------------------------------------------------------------------------
# Recode 'keine Angabe' and 'weiß nicht' to NA for Variables
# Join same factor levels with different names
# ------------------------------------------------------------------------------
dat_long <- dat_long %>%
  mutate(
    across(where(is.factor), ~ factor(
      if_else(
        as.character(.) %in% c(
          "keine Angabe", "Weiss nicht", "weiss nicht", "Keine Angabe", 
          "weiß nicht", "Weiß nicht", 
          "ich kenne diese Partei nicht", "kenne diese Partei nicht", "kenne Partei nicht",
          "ich kenne diese Person nicht", "kenne Person nicht", "kenne diese Person nicht",
          "ich kenne diese Partei/Person nicht", "kenne diese Partei/Person nicht",
          "77", "88", "99"
        ), 
        NA_character_, as.character(.)
      ),
      levels = levels(.)
    ))
  ) %>%
  
  # Join same factor levels with different names
  mutate(
    across(
      starts_with("ptv"),
      ~ fct_collapse(
        .,
        "10 = sehr wahrscheinlich" = c("10 = sehr wahrscheinlich", "sehr wahrscheinlich"),
        "0 = sehr unwahrscheinlich" = c("0 = sehr unwahrscheinlich", "sehr unwahrscheinlich")
      )
    ),
    across(
      starts_with("vertrauen"),
      ~ fct_collapse(
        .,
        "10 = vertraue ich voll und ganz" = c("vertraue ich voll und ganz", "10 = vertraue ich voll und ganz"),
        "0 = vertraue ich ueberhaupt nicht" = c("vertraue ich ueberhaupt nicht", "0 = vertraue ich ueberhaupt nicht")
      )
    ),
    across(
      starts_with("national"),
      ~ fct_collapse(
        .,
        "ueberhaupt nicht stolz" = c("Ueberhaupt nicht stolz", "ueberhaupt nicht stolz"),
        "weniger stolz" = c("Weniger stolz", "weniger stolz"),
        "teils-teils" = c("Teils-teils", "teils-teils"),
        "stolz" = c("Stolz", "stolz"),
        "sehr stolz" = c("Sehr stolz", "sehr stolz")
      )
    ),
    across(
      starts_with("meinung"),
      ~ fct_collapse(
        .,
        "trifft gar nicht zu" = c("trifft gar nicht zu", "stimme gar nicht zu",
                                  "eigenen Gepflogenheiten bewahren duerfen"),
        "trifft eher nicht zu" = c("trifft eher nicht zu", "stimme eher nicht zu"),
        "teils-teils" = c("teils-teils"),
        "trifft eher zu" = c("trifft eher zu", "stimme eher zu"),
        "trifft sehr zu" = c("trifft sehr zu", "stimme voll und ganz zu", 
                             "an die oesterreichischen Gepflogenheiten anpassen")
      )
    ),
    across(
      starts_with("kompetenteste_partei"),
      ~ fct_collapse(
        .,
        "NEOS" = c("NEOS", "Neos"),
        "Gruene" = c("Gruene", "GRUENE"),
        "andere Partei" = c("andere Partei", "JETZT – Liste Pilz", "Liste Peter Pilz", "JETZT - Liste Pilz", 
                            "Team Stronach", "MFG"),
        "keine Partei" = c("keine Partei", "keine Partei/niemand", "keine Partei/ niemand")
      )
    ),
    across(
      starts_with("parteiidentifikation"),
      ~ fct_collapse(
        .,
        "Ja" = c("Ja", "ja"),
        "Nein" = c("Nein", "nein"),
        "OEVP" = c("OEVP", "Liste Sebastian Kurz - die Neue Volkspartei"),
        "Gruene" = c("Gruene", "GRUENE"),
        "andere Partei" = c("andere Partei", "Andere Partei", "Team Stronach", "JETZT - Liste Pilz", 
                            "JETZT – Liste Pilz", "Liste Peter Pilz", "DAOE - Die Allianz fuer Oesterreich", 
                            "MFG", "kpOe", "KPOE", "Bierpartei", "KEINE/Wandel", "Liste Petrovic")
      )
    ),
    across(
      starts_with("links_rechts_einstufung"),
      ~ fct_collapse(
        .,
        "10 = rechts" = c("10 = rechts", "rechts"),
        "0 = links" = c("0 = links", "links")
      )
    ),
    across(
      starts_with("politische_lage"),
      ~ fct_collapse(
        .,
        "10 = sehr" = c("10 = sehr", "sehr"),
        "0 = ueberhaupt nicht" = c("0 = ueberhaupt nicht", "ueberhaupt nicht")
      )
    ),
    across(
      starts_with("wahlkampf"),
      ~ fct_collapse(
        .,
        "Ja" = c("Ja", "ja"),
        "Nein" = c("Nein", "nein"),
        "gar nicht aufmerksam" = c("gar nicht aufmerksam", "Gar nicht aufmerksam"),
        "wenig aufmerksam" = c("wenig aufmerksam", "Wenig aufmerksam"),
        "ziemlich aufmerksam" = c("ziemlich aufmerksam", "Ziemlich aufmerksam"),
        "sehr aufmerksam" = c("sehr aufmerksam", "Sehr aufmerksam")
      )
    ),
    across(
      starts_with("koalitionswahrscheinlichkeit"),
      ~ fct_collapse(
        .,
        "10 = sehr wahrscheinlich" = c("10 = sehr wahrscheinlich", "sehr wahrscheinlich"),
        "0 = sehr unwahrscheinlich" = c("0 = sehr unwahrscheinlich", "sehr unwahrscheinlich")
      )
    ),
    across(
      starts_with("koalitionswunsch"),
      ~ fct_collapse(
        .,
        "10 = wuensche ich mir sehr" = c("10 = wuensche ich mir sehr", "wuensche ich mir sehr"),
        "0 = wuensche ich mir ueberhaupt nicht" = c("0 = wuensche ich mir ueberhaupt nicht", "wuensche ich mir ueberhaupt nicht")
      )
    ),
    across(
      starts_with("wirtschaftslage"),
      ~ fct_collapse(
        .,
        "viel schlechter"   = c("viel schlechter geworden", "Viel schlechter geworden", "viel schlechter werden", "Viel schlechter werden"),
        "etwas schlechter"  = c("etwas schlechter geworden", "Etwas schlechter geworden", "etwas schlechter werden", "Etwas schlechter werden"),
        "gleich geblieben"  = c("gleich geblieben", "Gleich geblieben", "gleich bleiben", "Gleich bleiben"),
        "etwas besser"      = c("etwas besser geworden", "Etwas besser geworden", "etwas besser werden", "Etwas besser werden"),
        "viel besser"       = c("viel besser geworden", "Viel besser geworden", "viel besser werden", "Viel besser werden")
      )
    ),
    across(
      starts_with("sympathie"),
      ~ fct_collapse(
        .,
        "10 = sehr sympathisch" = c("10 = sehr sympathisch", "sehr sympathisch"),
        "0 = gar nicht sympathisch" = c("0 = gar nicht sympathisch", "gar nicht sympathisch")
      )
    )
  )

# ------------------------------------------------------------------------------
# Date Format and Median Date Calculation
# ------------------------------------------------------------------------------
# Reformat date
dat_long <- dat_long %>%
  mutate(date = as.Date(date))

# Calculate the median date per wave
median_dates <- dat_long %>%
  group_by(wave) %>%
  summarise(median_date = median(date, na.rm = TRUE))

# Sort dat_long by wave and add median date
dat_long <- dat_long %>%
  left_join(median_dates, by = "wave") %>%  # Add median dates
  select(wave, median_date, everything()) %>%  # Reorder columns
  arrange(as.numeric(wave)) %>% # Sort by wave
  mutate(
    # Assign dynamic age based on median_date year
    age = case_when(
      year(median_date) == 2017 ~ as.numeric(as.character(age_y17)),
      year(median_date) == 2019 ~ as.numeric(as.character(age_y19)),
      year(median_date) == 2020 ~ as.numeric(as.character(age_y19)) + 1,   # assuming age y19 + 1
      year(median_date) == 2022 ~ as.numeric(as.character(age_y22)),
      year(median_date) == 2023 ~ as.numeric(as.character(age_y22)) + 1,   # assuming age y22 + 1
      year(median_date) == 2024 ~ as.numeric(as.character(age_y24)),
      year(median_date) == 2025 ~ as.numeric(as.character(age_y24)) + 1,   # assuming age y24 + 1
      TRUE ~ NA_real_
    ),
    sd3 = case_when(
      year(median_date) == 2017 ~ as.character(sd3_y17),
      year(median_date) == 2019 ~ as.character(sd3_y19),
      year(median_date) == 2020 ~ as.character(sd3_y19),
      year(median_date) == 2022 ~ as.character(sd3_y22),
      year(median_date) == 2023 ~ as.character(sd3_y22),
      year(median_date) == 2024 ~ as.character(sd3_y24),
      year(median_date) == 2025 ~ as.character(sd3_y24),
      TRUE ~ NA_character_
    ),
    sd7 = case_when(
      year(median_date) == 2017 ~ as.character(sd7_y17),
      year(median_date) == 2019 ~ as.character(sd7_y19),
      year(median_date) == 2020 ~ as.character(sd7_y19),
      year(median_date) == 2022 ~ as.character(sd7_y22),
      year(median_date) == 2023 ~ as.character(sd7_y22),
      year(median_date) == 2024 ~ as.character(sd7_y24),
      year(median_date) == 2025 ~ as.character(sd7_y24),
      TRUE ~ NA_character_
    )
  ) %>%
  select(-date, -age_y17, -sd3_y17, -sd7_y17) # Remove original date and unused age columns

# ------------------------------------------------------------------------------
# Reshape / Aggregate for Overall Means
# ------------------------------------------------------------------------------
# Melt the data to long format
dat_category <- dat_long %>%
  pivot_longer(
    cols = c(
      starts_with("vertrauen"),
      starts_with("populismus"), 
      starts_with("ptv"), 
      starts_with("nationalstolz"),
      starts_with("politisches_interesse"),
      starts_with("meinung"),
      starts_with("verbundenheit"),
      starts_with("kompetenteste_partei"),
      starts_with("parteiidentifikation"),
      starts_with("links_rechts_einstufung"),
      starts_with("politische_lage"),
      starts_with("informationen"),
      starts_with("wahlkampf"),
      starts_with("koalitionswahrscheinlichkeit"),
      starts_with("koalitionswunsch"),
      starts_with("wirtschaftslage"),
      starts_with("sympathie")
    ),
    names_to = "variable",
    values_to = "category"
  )

# ------------------------------------------------------------------------------
# Calculate Percentage for Each Category within Each Wave and Variable
# ------------------------------------------------------------------------------
dat_percentage <- dat_category %>%
  filter(!is.na(category)) %>%
  group_by(wave, median_date, variable, category) %>%
  summarise(
    count = n(),  # Calculate unweighted frequency
    weighted_sum = sum(weightd, na.rm = TRUE),  # Calculate weighted sum
    .groups = 'drop'
  ) %>%
  group_by(wave, median_date, variable) %>%
  mutate(
    weighted_percentage = weighted_sum / sum(weighted_sum),  # Calculate weighted percentage
    count_wave_variable = sum(count)  # Calculate the total number of counts per wave and variable
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Add Original Labels to dat_percentage Dataframe
# ------------------------------------------------------------------------------
dat_percentage <- dat_percentage %>%
  mutate(variable_label = label_to_basename[variable]) %>%
  mutate(variable_label = str_wrap(variable_label, width = 30))

# ------------------------------------------------------------------------------
# Create Data Frame for Displaying Means
# ------------------------------------------------------------------------------
dat_means <- dat_percentage %>%
  # Convert factor to character to ensure case_when works on text
  mutate(category = as.character(category)) %>%
  # Map the character values to custom integers
  mutate(category_numeric = map_category_to_numeric(category)) %>%
  filter(category_numeric < 80) %>%
  group_by(wave, median_date, variable) %>%  # Group by wave, median_date, and variable
  dplyr::summarize(
    count_wave_variable = first(count_wave_variable),
    category_value = sum(category_numeric * weighted_sum, na.rm = TRUE),  # Weighted sum of category labels of wave
    average_category = category_value / sum(weighted_sum, na.rm = TRUE),  # Calculate the average category of wave
    variable_label = first(variable_label),
    
    # Calculate a more accurate weighted standard deviation
    std_dev = sqrt(sum(weighted_sum * (category_numeric - average_category)^2) / 
                     (sum(weighted_sum) - (sum(weighted_sum^2) / sum(weighted_sum)))),
    
    # Calculate the standard error based on the weighted sample size
    se = std_dev / sqrt(sum(weighted_sum)),
    
    # Calculate the 95% confidence intervals
    lower_ci = average_category - 1.96 * se,  # Lower bound of 95% CI
    upper_ci = average_category + 1.96 * se   # Upper bound of 95% CI
  ) %>%
  ungroup() %>%
  distinct()

# ------------------------------------------------------------------------------
# Save RDS Files for Overall Data
# ------------------------------------------------------------------------------
saveRDS(dat_percentage, file = file.path(DATA, "dat_percentage.rds"))
saveRDS(dat_means, file = file.path(DATA, "dat_means.rds"))

# ------------------------------------------------------------------------------
# Reshape / Aggregate for Grouped Means (Demographic Characteristics)
# ------------------------------------------------------------------------------
dat_grouped <- dat_category %>%
  filter(sd3 != "Moechte ich nicht sagen", sd3 != "Divers", sd3 != "Divers/Moechte ich nicht sagen") %>% # Filter out underrepresented genders
  filter(sd7 != "andere") # Filter out unknown education levels

dat_grouped <- dat_grouped %>%
  mutate(
    age = as.character(age),
    age = ifelse(age == ">= 70", "70", age), # Replace '>= 70' with '70'
    age = as.numeric(age)
  ) %>%
  rename(gender = sd3, education = sd7) %>%
  mutate(
    variable_label = label_to_basename[variable],
    variable_label = str_wrap(variable_label, width = 30)
  ) %>%
  
  # Convert factor to character to ensure case_when works on text
  mutate(category = as.character(category)) %>%
  
  # Map the character values to custom integers
  mutate(category_numeric = map_category_to_numeric(category)) %>%
  filter(category_numeric < 80) %>%
  
  # Define age groups (adaptable)
  mutate(age_group = case_when(
    age <= 29 ~ "16-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 ~ ">= 60",
    TRUE ~ ">= 60"  # else case '>= 70'
  )) %>%
  
  # Define education groups
  mutate(education_group = case_when(
    education %in% c(
      "Volksschule oder weniger", 
      "Hauptschule oder AHS Unterstufe", 
      "Polytechnikum", 
      "keine Schule besucht/abgeschlossen/Volksschule oder weniger"
    ) ~ "gering",
    education %in% c("Lehre, Berufsschule", "BMS (Fachschule)") ~ "mittel",
    education %in% c(
      "AHS mit Matura", 
      "BHS mit Matura", 
      "Hochschulverwandte Lehranstalt", 
      "Kolleg", 
      "Bachelor", 
      "Magister/Master/DI/FH", 
      "Doktor/PhD"
    ) ~ "hoch"
  ))

# ------------------------------------------------------------------------------
# Save RDS File for Grouped Data
# ------------------------------------------------------------------------------
saveRDS(dat_grouped, file = file.path(DATA, "dat_grouped.rds"))

# ------------------------------------------------------------------------------
# END OF DATA PREPARATION FILE
# ------------------------------------------------------------------------------

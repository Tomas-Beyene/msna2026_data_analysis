#  -----------------------------------------------------------------------------
# Title : Analysis - Initialization Scripts
#  -----------------------------------------------------------------------------

# Load Packages ----------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, openxlsx, sf, anytime, survey, srvyr,crayon, purrr)

# Github IMPACT Packages
# install.packages("devtools")
# devtools::install_github("impact-initiatives/impactR4PHU")
# devtools::install_github("impact-initiatives/analysistools")
# devtools::install_github("impact-initiatives-hppu/humind@v2026.4.0")
# devtools::install_github("impact-initiatives/addindicators")
# devtools::install_github("impact-initiatives/presentresults")

library(impactR4PHU)
library(analysistools)
library(humind)
library(addindicators)
library(presentresults)


# Parameters/Options Values ----------------------------------------------------
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
# strata with a single PSU (e.g. after NA-filtering a sub-question) -> centre at grand mean instead of erroring
options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)

# Load generic functions -------------------------------------------------------
source("src/utils_data_preprocessing.R")
source("src/utils_data_analysis.R")

# Load Data --------------------------------------------------------------------
# Kobo tool
cat("\n- reading Kobo tool ...\n")
tool_survey <- read_excel(kobo_tool, sheet="survey", col_types="text") %>% 
  rename_with(.,tolower) %>%
  filter(!is.na(name)) 

tool_choices <- read_excel(kobo_tool, sheet="choices", col_types="text") %>%
  rename_with(.,tolower) %>%
  filter(!is.na(list_name)) %>% 
  select(list_name, name, `label::english`) %>% 
  distinct()
cat(green("kobo tool read successfully!\n"))

################################################################################
#-------------------------------------------------------------------------------
# get all integer column names from kobo tool
cat("- getting all integer column names from kobo tool ...\n")
cols_numeric <- filter(tool_survey, type %in% c("integer", "decimal")) %>% pull(name)
cat(green("integer column names retrieved successfully!\n"))

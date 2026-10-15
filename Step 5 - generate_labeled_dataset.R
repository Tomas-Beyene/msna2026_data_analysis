#  -----------------------------------------------------------------------------
#
# Title : Analysis - Preparation Labeled Dataset
#    By : REACH NGA
#  Date : 09/2026
#    
#  -----------------------------------------------------------------------------

rm(list=ls())

# Load src scripts -------------------------------------------------------------
source("src/init.R")

# Load Data --------------------------------------------------------------------
# repeat for all sheets
dataset <- read_excel("./outputs/datasets/pre_analysis_dataset.xlsx", sheet = "hh data", guess_max = 100000)

tool_survey <- tool_survey %>%
  mutate(q_type=as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
           list_name=as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
           list_name=ifelse(str_starts(type, "select_"), list_name, NA))

tool_survey <- tool_survey %>% 
  filter(!(type %in% c("begin_group", "end_group", "beging_repeat", "end_repeat", "note"))) 

tool_choices <- tool_choices %>%
  filter(!(list_name %in% c("l_admin1", "l_admin2")))

################################################################################
# GENERATE LABELED JMMI DATASET
################################################################################

dataset_labeled <- dataset
NA0 <- sum(is.na(dataset_labeled))
test_0 <- data.frame(is.na(dataset_labeled))

# recode column names and values of select_one questions (name --> label)
col.names <- colnames(dataset)
for (i in 1:length(col.names)){
  print(col.names[i])
  # code responses of select_one questions
  if (col.names[i] %in% tool_survey$name){
    q_type <- tool_survey$q_type[tool_survey$name==col.names[i]]
    q_list_name <- tool_survey$list_name[tool_survey$name==col.names[i]]
    if (q_type=="select_one"){
      choices <- tool_choices %>% filter(list_name==q_list_name) %>%
        select(name, `label::english`) %>% dplyr::rename(label=`label::english`)
      d <- data.frame(col=as.character(dataset[[col.names[i]]])) %>% 
        left_join(choices, by=c("col"="name")) %>% select(label)
      dataset_labeled[[colnames(dataset_labeled)[i]]] <- d$label
    }
  }
  # code column name into label
  colnames(dataset_labeled)[i] <- name2label_question(tool_survey, tool_choices, col = col.names[i])
}

NA1 <- sum(is.na(dataset_labeled))
if(NA0 != NA1) stop("NA added to labeled datastet. Please!")


test_1 <- data.frame(is.na(dataset_labeled))

test <- data.frame()
for(i in 1: ncol(test_0)){
  if(!any(test_0[i] %in% test_1[i])) test <- rbind(test, colnames(test_0[i])) else print("matching") 
}

################################################################################

# SAVE Excel
write_xlsx(dataset_labeled, paste0("output/datasets/labeled_data.xlsx"))

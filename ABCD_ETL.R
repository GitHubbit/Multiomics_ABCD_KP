# ETL of ABCD Data

setwd('/Users/Kamileh/Work/ISB/NCATS_BiomedicalTranslator/Projects/ABCD/scripts/R')

install.packages("librarian")
librarian::shelf("data.table", "R.utils", "tidyverse", "tidyr", "stringr")

rm(list=ls())
abcd3 <- readRDS("/Volumes/TOSHIBA_EXT/ISB/ABCD/data/ABCD_release_2.0_rds/ABCD_releases_2.0.1_Rds/nda2.0.1.Rds")

unique(abcd_sub$event_name) # see what types eventnames there are (there are baseline, 1 year 6 month, etc readings)
unique(abcd_sub$eventname)
# get only baseline readings
abcd_baseline <- abcd3[abcd3$eventname %like% "baseline", ]

# get a subset of data easier to work with (get only baseline readings)
abcd_sub <- abcd_baseline[1:5000,]
rm(abcd3, abcd_baseline) # remove big data, work with subset

# retrieve data dictionary so we know what we're looking at 
dict_files <- list.files(path="/Volumes/TOSHIBA_EXT/ISB/ABCD/data/ABCDstudyDEAP_2.0/dictionary", pattern=".csv", full.names=T)

parse_dict_file <- function(file) {
  return(tryCatch(read.csv(file) %>% add_column(table_name=sub('\\.csv$', '', basename(file)), .before=1), error=function(e) NULL))
  # print(basename(file))
  # return(tryCatch(read.csv(file), error=function(e) NULL))
}

# see column types of abcd dataframe
cols_types <- unique(sapply(abcd_sub, typeof)) # there's only double and string

abcd_dict <- lapply(dict_files, parse_dict_file)
abcd_dict <- rbindlist(abcd_dict, fill=T)
# output dataframe of all cols in dataset to file to see
# write.csv(abcd_dict,"../outputs/abcd_cols.csv", row.names = FALSE)

types_in_df <- data.frame(sapply(abcd_sub, class))  # get the types of all the columns in the abcd dataset
# grab columns of relevance 
selected_cols <-  abcd_dict %>% filter(DataType == "Float" |
                                         ElementName == "anthroheightcalc|anthroweightcalc" |
                                         # grepl("_t$", ElementName) |  # there's a lot of ElementNames that end in _t that aren't t_scores and I only want t_scores
                                         grepl("t-score|T-Score|T-score|Mean|mean", ElementDescription) |
                                         ElementDescription %like% "how much|How much|how many|How many|how long|How long" |
                                         table_name == "abcd_otbi01" & grepl("age|old", ElementDescription) |
                                         table_name == "abcd_ps01" & DataType=="Integer" |
                                         table_name == "abcd_saiq02" & grepl("how many years", ElementDescription) |
                                         table_name == "abcd_tbi01" & grepl("SUM", ElementDescription) |
                                         # table_name == "abcd_tbss01" & grepl("agecorrected", ElementName)  # they computed T-scores later
                                         table_name == "abcd_y10ids01" & grepl("How many times", ElementDescription) |
                                         table_name == "abcd_yhr01" & grepl("ng/10 mg hair", ElementDescription) |
                                         table_name == "stq01" & grepl("ng/10 mg hair", ElementDescription) 
                                       
                                       # table_name == "abcd_yrb01" & grepl("how many", ElementDescription) |
                                       # table_name == "abcd_ysr01" & grepl("how many", ElementDescription) 
                                       )

# filter out MRI and other variables that are not float/numerical
selected_cols <- selected_cols %>% filter(!grepl("mri|ehi_y_ss_scoreb|rep1|rep2|rep3|hair_results_lan|hair_results_entityid|hair_results_clientcode|hair_results_section_begin|hair_results_section_end|_nt$|_nm$", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("mri|pps01|macv01|freesqc01|abcd_ypsq101|medsy01|pmq01", table_name))
selected_cols <- selected_cols %>% filter(!grepl("mri", Aliases))
selected_cols <- selected_cols %>% filter(!grepl(";", ValueRange))
selected_cols <- selected_cols %>% filter(!grepl("Raw Score|Missing Answers|Total Questions", ElementDescription))


# tack dhx01 back on 
dhx01 <-  abcd_dict %>% filter(table_name == "dhx01" &
                                 !grepl("GUID|Date", DataType) &
                                 !grepl("src_subject_id|interview_age|sex|visit|select_language", ElementName) &
                                 !grepl("Yes", Notes) &
                                 !grepl("Medication|Drug", ElementDescription))  # can't use questions related to Medication 1-4 or Drug 1-4 bc we have to know drug name, which is a String

selected_cols <- rbind(selected_cols, dhx01)
selected_cols <- selected_cols %>% filter(!grepl("kbi_p_c_best_friend_len", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("kbi_p_c_reg_friend_group_len", ElementName))

# tack 1 row from medsy01 back on why am i doing this
medsy01 <-  abcd_dict %>% filter(table_name == "medsy01" &
                                   grepl("caff_ago", ElementName)) 
selected_cols <- rbind(selected_cols, medsy01)

additional_desired_cols <- c("subjectid", "src_subject_id", "eventname")
# split Alias column on comma

aliases <- data.frame(str_split(selected_cols$Aliases, ",", simplify=TRUE))
aliases

selected_cols_names_only <- c(additional_desired_cols, selected_cols$ElementName, selected_cols$Notes, selected_cols$Condition, selected_cols$Aliases, aliases$X1, aliases$X2)
selected_cols_names_only <- unique(selected_cols_names_only[selected_cols_names_only != ""])

initial_kg <- abcd_sub[,(names(abcd_sub) %in% selected_cols_names_only)]

# initial_kg <- initial_kg[, colSums(initial_kg<=10)]

rm(dhx01, medsy01, abcd_cols)

# in our manual curation, we accidentally pulled some factor level columns that are genuinely factors, but there are some factor columns that aren't truly factors (these have factor level of 1)
# grab factor level columns that have maximum of 1 level to keep in initial_kg, and the 1 age column
factor_kg <- (Filter(is.factor, initial_kg))
factor_kg <- subset(factor_kg, select = -c(`subjectid`, `src_subject_id`, `eventname`))
factor_keep <- factor_kg %>% select_if(~ nlevels(.) == 1) # get cols with only 1 level bc those are numerical
factor_keep_age <- factor_kg[names(factor_kg) %like% 'Age|age'] # get cols related to age
# remove all the cols in factor_kg from initial_kg, the re-bind the valid columns from factor_keep and factor_keep_age back to initial_kg
numerical_kg <- initial_kg <- initial_kg[,!(names(initial_kg) %in% names(factor_kg))]
initial_kg <- cbind(initial_kg, factor_keep, factor_keep_age)

# grab cols from NDA Aliases
deap_aliases_updated <- read.csv(file = '/Volumes/TOSHIBA_EXT/ISB/ABCD/data/ABCD_release_2.0_rds/ABCD_releases_2.0.1_Rds/DEAP.aliases.updated.2.0.1.csv')

# names(factor_keep) # getting all keep factor columns 
# unique(initial_kg$devhx_8_alchohol_avg_dk_p) #  see what the factor columns we're keeping look like ... are they really numerical or not

dtype_cols <- names(initial_kg)[!names(initial_kg) %in% c("subjectid", "src_subject_id", "eventname")]
values <- abcd_dict[abcd_dict$ElementName %in% dtype_cols,"ValueRange"]
dtype_cols <- cbind(dtype_cols, values)

# first get all integer cols and see if can run correlations on that
table(sapply(numerical_kg,class)) # check that only first 3 columns are factor (subject id, ndar id, and eventname)

no_subject_id_kg <- numerical_kg[!names(numerical_kg) %in% c("subjectid", "src_subject_id", "eventname")]

filtering <- data.frame(colSums(numerical_kg != 0, na.rm=TRUE))  
filtering$log = ifelse(filtering$colSums.numerical_kg....0..na.rm...TRUE. > 0,TRUE,FALSE)










# find cols in RDS dataframe that are also in desired cols of dictionary
initial_kg <- abcd_sub[,(names(abcd_sub) %in% selected_cols_names_only)]
initial_kg_cols <- data.frame(names(initial_kg))
abcd_cols <- data.frame(names(abcd_sub))
missing_cols <- data.frame(selected_cols_names_only[which(!selected_cols_names_only %in% names(abcd_sub))])
colnames(missing_cols)[1] ="cols_missing"

missing_cols <- initial_kg_cols[(initial_kg_cols %in% abcd_cols)]

missing_cols_found_in_deap <- data.frame(missing_cols$cols_missing[(missing_cols$cols_missing %in% deap_aliases_updated$nda)])
colnames(missing_cols_found_in_deap)[1] ="deap_missing_cols"

test <- data.frame(names(initial_kg))
test <- setdiff(missing_cols_found_in_deap$deap_missing_cols, )

# cols_of_abcd <- data.frame(names(abcd_sub))
# cols_of_abcd2 <- data.frame(abcd_sub$eventname)
# cols_of_abcd3 <- data.frame(abcd_sub$event_name)
# 
# test <- names(abcd_sub)
# test[!str_detect(names(abcd_sub), "medhx_6c_times_l")]


# initial_kg_cols_2 <- abcd_sub[,abcd_sub %in% selected_cols_names_only)]

# initial_kg_cols <- abcd_sub[,(names(abcd_sub) %in% selected_cols_names_only)]

initial_kg[is.na(initial_kg)] <- 0

factor_cols <- names(Filter(is.factor, initial_kg))
factor_cols

REPLACE ALL NA WITH 0 (FIND WHICH ARE NUMERICAL COLUMNS AND WHICH ARE FACTOR)
ADD COLS TO FIND FROM NDA ALIAS FILE


View(data.frame(selected_cols_names_only))
View(data.frame(names(initial_kg_cols)))

length(selected_cols$ElementName)




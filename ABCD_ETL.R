# ETL of ABCD Data

setwd('/Users/Kamileh/Work/ISB/NCATS_BiomedicalTranslator/Projects/ABCD/scripts/R')

install.packages("librarian")
librarian::shelf("data.table", "R.utils", "tidyverse")

rm(list=ls())
abcd3 <- readRDS("/Volumes/TOSHIBA_EXT/ISB/ABCD/data/ABCD_release_2.0_rds/ABCD_releases_2.0.1_Rds/nda2.0.1.Rds")

unique(abcd_sub$event_name) # see what types eventnames there are (there are baseline, 1 year 6 month, etc readings)
unique(abcd_sub$eventname)
# get only baseline readings
abcd_baseline <- abcd3[abcd3$eventname %like% "baseline", ]

# get a subset of data easier to work with (get only baseline readings)
abcd_sub <- abcd_baseline[1:2000,]
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
# abcd_sub <- abcd_sub[,sapply(abcd_sub,is.float) | sapply(abcd_sub,is.integer)]
selected_cols <-  abcd_dict %>% filter(DataType == "Float" |
                                           # DataType == "GUID" |
            
                                           # ElementName == "interview_age" |
                                           # ElementName == "sex" |
                                           ElementName == "anthroheightcalc" |
                                           ElementName == "anthroweightcalc" |
                                           # grepl("_t$", ElementName) |  # there's a lot of ElementNames that end in _t that aren't t_scores and I only want t_scores
                                           grepl("t-score", ElementDescription) |
                                           grepl("T-Score", ElementDescription) |
                                           grepl("T-score", ElementDescription) |
                                           grepl("Mean", ElementDescription) |
                                           grepl("mean", ElementDescription) |
                                           ElementDescription %like% "how much" |
                                           ElementDescription %like% "How much" |
                                           ElementDescription %like% "how many" |
                                           ElementDescription %like% "How many" |
                                           ElementDescription %like% "how long" |
                                           ElementDescription %like% "How long" |
                                           table_name == "abcd_otbi01" & grepl("age", ElementDescription) |
                                           table_name == "abcd_otbi01" & grepl("old", ElementDescription) |
                                           table_name == "abcd_otbi01" & grepl("old", ElementDescription) |
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



selected_cols <- selected_cols %>% filter(!grepl("mri", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("mri", table_name))
selected_cols <- selected_cols %>% filter(!grepl("mri", Aliases))
selected_cols <- selected_cols %>% filter(!grepl(";", ValueRange))
selected_cols <- selected_cols %>% filter(!grepl("Raw Score", ElementDescription))
selected_cols <- selected_cols %>% filter(!grepl("Missing Answers", ElementDescription))
selected_cols <- selected_cols %>% filter(!grepl("Total Questions", ElementDescription))
selected_cols <- selected_cols %>% filter(!grepl("ehi_y_ss_scoreb", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("rep1", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("rep2", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("rep3", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("pps01", table_name))
selected_cols <- selected_cols %>% filter(!grepl("macv01", table_name))
selected_cols <- selected_cols %>% filter(!grepl("freesqc01", table_name))
selected_cols <- selected_cols %>% filter(!grepl("abcd_ypsq101", table_name))
selected_cols <- selected_cols %>% filter(!grepl("medsy01", table_name)) # although this has float data, it depends on medication name, which requires extra handling....will have to encode by medicaton name (RxNorm given) and then make factors of it/column names m
selected_cols <- selected_cols %>% filter(!grepl("hair_results_lan", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("hair_results_entityid", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("hair_results_clientcode", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("hair_results_section_begin", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("hair_results_section_end", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("_nt$", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("_nm$", ElementName))
selected_cols <- selected_cols %>% filter(!grepl("pmq01", table_name))

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





# temp_mri_rel <- filter(abcd_dict, table_name=="abcd_mid02"&DataType=="Float")
# abcd_float_cols <- rbind(abcd_float_cols, temp_mri_rel)

# keep age and sex of subject, only capture 1 row for each

length(unique(abcd_float_cols$table_name))
       
       

# hair_results_lan
# hair_results_entityid
# hair_results_clientcode
# hair_results_section_begin
# hair_results_section_end








#
# next if $file =~ /mid|mri|dti|rsi|nback|abcd_betnet02/;
# my $part = ($set =~ /mid|mri|dti|rsi|nback|abcd_betnet02|tr2|tfsst|tfab|tnbase|tfnbr|tfncr/);







# get average age as column!!!!



for (file in dict_files) {
  print(basename(file))
  read.csv(file)

}




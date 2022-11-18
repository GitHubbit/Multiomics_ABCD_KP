# ETL of ABCD Data

setwd('/Users/Kamileh/Work/ISB/NCATS_BiomedicalTranslator/Projects/ABCD/scripts/R')

install.packages("librarian")
librarian::shelf("data.table", "R.utils", "tidyverse",
                 "tidyr", "stringr", "tibble",
                 "corrplot", "Hmisc", "ggplot2",
                 "RColorBrewer", "rvest", "utils",
                 "futile.logger")

rm(list=ls())
abcd3 <- readRDS("/Volumes/TOSHIBA_EXT/ISB/ABCD/data/ABCD_release_2.0_rds/ABCD_releases_2.0.1_Rds/nda2.0.1.Rds")

unique(abcd_sub$event_name) # see what types eventnames there are (there are baseline, 1 year 6 month, etc readings)

# get only baseline readings
abcd_baseline <- abcd3[abcd3$eventname %like% "baseline", ]

# get a subset of data easier to work with (get only baseline readings)
# abcd_sub <- abcd_baseline[1:5000,]
abcd_sub <- abcd_baseline
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


rm(dhx01, medsy01)

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

# get columns where the total number of observations exceeds 10 
filtering <- data.frame(colSums(numerical_kg != 0, na.rm=TRUE)) 
# filtering$cols <- rownames(filtering)
# rownames(filtering) <- NULL
filtering$check = ifelse(filtering$colSums.numerical_kg....0..na.rm...TRUE. > 10,TRUE,FALSE)
filtering <- filter(filtering, check == TRUE)
filtering <- t(filtering)
numerical_kg_clean <- numerical_kg[colnames(numerical_kg) %in% colnames(filtering)]

# conduct Shapiro-wilk's test for normality on each column (won't work, we have >5000 observations for many cols)
# num_kg <- data.frame(sapply(numerical_kg_clean[, 4:ncol(numerical_kg_clean)], as.numeric)) # force all cols to numeric
num_kg <- sapply(numerical_kg_clean[, 4:ncol(numerical_kg_clean)], as.numeric) # force all cols to numeric


# tack the first 3 cols of metadata back on
numerical_kg_clean <- cbind(numerical_kg_clean$subjectid,
                            numerical_kg_clean$src_subject_id,
                            numerical_kg_clean$eventname,
                            num_kg)

values_count_per_col <- data.frame(colSums(num_kg !=0, na.rm=TRUE)) # get count of nonzero and non-na values in each column

# test for normality of data. Can't perform shapiro wilk bc size has to be btw 3-5000. 
# apply(num_kg,2,shapiro.test)

corr_mat <- rcorr(num_kg, type="spearman")
corr_mat$r[corr_mat$n < 10] <- NA # ignore less than 10 observations
corr_mat$adj_p <- matrix(p.adjust(corr_mat$P, method="BH"), ncol = ncol(corr_mat$P), dimnames = dimnames(corr_mat$r))

# let's visualize results in corr network
# flatten the matrices / pivot_long
# Note 1: we are building correlation network, so row and column pair should be non duplicated
# Note 1: in other words, housing_1(row) and smoking_4(column) have correlation of 0.87
# Note 1: we want to delete row where it's a repeat, smoking_4(row) and housing_1(column) have correlation of 0.87
# Note 1: if we don't do this, we will get bi-directional edges between the same 2 nodes
pivoted_r <- as.data.frame.table(corr_mat$r, responseName = "corr")
pivoted_r <- pivoted_r[!duplicated(t(apply(pivoted_r[,c(1,2)],1,sort))),]  # Note 1*: sort first 2 cols of vis dataframe, transpose, get non-duplicates
pivoted_r <- pivoted_r[pivoted_r$Var1 != pivoted_r$Var2,]
pivoted_r <- pivoted_r[!is.na(pivoted_r$corr),]

pivoted_n <- as.data.frame.table(corr_mat$n, responseName = "n")
pivoted_n <- pivoted_n[!duplicated(t(apply(pivoted_n[,c(1,2)],1,sort))),]  # sort first 2 cols of vis dataframe, transpose, get non-duplicates
pivoted_n <- pivoted_n[pivoted_n$Var1 != pivoted_n$Var2,]
pivoted_n <- pivoted_n[!is.na(pivoted_n$n),]

pivoted_p <- as.data.frame.table(corr_mat$P, responseName = "p_val")
pivoted_p <- pivoted_p[!duplicated(t(apply(pivoted_p[,c(1,2)],1,sort))),]  # sort first 2 cols of vis dataframe, transpose, get non-duplicates
pivoted_p <- pivoted_p[pivoted_p$Var1 != pivoted_p$Var2,]
pivoted_p <- pivoted_p[!is.na(pivoted_p$p_val),]

pivoted_padj <- as.data.frame.table(corr_mat$adj_p, responseName = "adj_p")
pivoted_padj <- pivoted_padj[!duplicated(t(apply(pivoted_padj[,c(1,2)],1,sort))),]  # sort first 2 cols of vis dataframe, transpose, get non-duplicates
pivoted_padj <- pivoted_padj[pivoted_padj$Var1 != pivoted_padj$Var2,]
pivoted_padj <- pivoted_padj[!is.na(pivoted_padj$adj_p),]

# join all pivoted matrices 
corr_info <- list(pivoted_r, pivoted_n, pivoted_p, pivoted_padj) %>% reduce(left_join, by=c("Var1","Var2"))
corr_info <- corr_info[complete.cases(corr_info), ] #unnecessary
rm(list = c("pivoted_padj","pivoted_p","pivoted_n","pivoted_r"))

# drop rows where adjusted p-val is <0.05 [optional: and r=1]
vis <- corr_info[corr_info$adj_p < 0.05, ]
# remove rows where there's 0 correlation
vis <- vis[vis$cor != 0, ]
#### add pseudocount for p-values that = 0 so it is plottable
vis["adj_p"][vis["adj_p"] == 0] <- 1E-20

vis <- vis[complete.cases(vis), ]
# remove duplicate rows 

# remove weakly correlated pairs (r between -0.5 and +0.5)
# vis <- subset(vis, cor < -0.5 | cor > 0.5)

vis <- vis[order(vis$adj_p, vis$cor),]
vis["neg_log_p_val"] <- data.frame(-log10(vis$adj_p))

p_histo <- hist(vis$neg_log_p_val,breaks=60) 
p_histo
# from plot, -log(p-val)=1.75 on x-axis = endpoint of first 2 bars
# x or adjusted p-val = 0.01778279
# let's capture all adj p-vals that are < 0.01778379
vis_sub <- subset(vis, adj_p<0.01778379)
p_histo <- hist(vis_sub$neg_log_p_val,breaks=60) 

# let's capture all correlations between any 2 variables not in the same table
vis$Var1_sub <- sub("\\_.*", "", vis$Var1)
vis$Var2_sub <- sub("\\_.*", "", vis$Var2)
vis_dtab <- vis[which(vis$Var1_sub != vis$Var2_sub),]

# plot distribution of correlation values
corr_histo_data <- hist(vis$cor, plot=F) # just to see counts per bin, etc
hist.data$counts


corr_histo <- ggplot(vis, aes(x=cor, y=log(..count..))) +
  geom_histogram(color="black", fill="red", binwidth = 0.1) +
  scale_y_continuous(breaks=seq(0,10,0.5)) +
  scale_x_continuous(breaks = seq(-1, 1, 0.1))

n_hist <- ggplot(vis, aes(x=n, y=log(..count..))) +
  geom_histogram(color="black", fill="red", binwidth=10) +
  # scale_y_continuous(breaks=seq(0,8000,500)) +
  scale_x_continuous(breaks = seq(0, 12000, 1000))
n_hist
  
# scatterplot of adj-p val vs Correlation
ggplot(vis, aes(x=adj_p, y=cor, color=n)) + 
  geom_point(size=3) +
  ggtitle("Scatterplot Adj p-val vs Corr") +
  scale_x_continuous(breaks = seq(0, 0.05, 0.005)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.1)) +
  scale_color_gradientn(colors=colorRampPalette(brewer.pal(name="YlOrRd", n = 8))(12), breaks=seq(0,12000,1000))
 

# scatterplot of -log(adj-p val) vs Correlation
ggplot(vis, aes(x=-log(adj_p), y=cor, color=n)) + 
  geom_point(size=3) +
  ggtitle("Scatterplot -log(Adj p-val) vs Corr") +
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  scale_y_continuous(breaks = seq(-1, 1, 0.1)) +
  scale_color_gradientn(colors=colorRampPalette(brewer.pal(name="YlOrRd", n = 8))(12), breaks=seq(0,12000,1000))

# write all of the various correlation tables to a CSV to open in Cytoscape as a network
write.csv(vis_dtab,
          file='abcd_spearman_corr.csv',
          row.names=FALSE)

# we need the descriptions of the tables bc the data dictionary isn't informative enough about what the column names/nodes in network mean
# web scrape the NIMH (https://nda.nih.gov/data_dictionary.html?source=ABCD%2BRelease%2B2.0&submission=ALL) data dictionary table descriptions to get better understanding of what column means
abcd_data_dict_2_url <- "https://nda.nih.gov/data_dictionary.html?source=ABCD%2BRelease%2B2.0&submission=ALL"
download.file(abcd_data_dict_2_url, destfile = 'abcd_data_dict_2.html') # uncomment to scrape
abcd_dict_2 <- read_html("abcd_data_dict_2.html")
tab_shortnames <- abcd_dict_2 %>% html_nodes("td.short-name-column") %>% html_text() 
tab_links <- abcd_dict_2 %>% html_elements("td.short-name-column") %>% html_elements("a") %>% html_attr("href")
tab_links <-  paste("https://nda.nih.gov", tab_links, sep="")
abcd_tabs <- data.frame(tab_shortnames, tab_links)

# function to retry downloading html pages
retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=3, sleep=0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
      flog.fatal(msg)
      # stop(msg)
      break
    } else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts,  maxErrors, 
                    capture.output(str(retval)))
      flog.error(msg)
      warning(msg)
      
    }
    if (sleep > 0) Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  return(retval)
}

# download all html pages, use retry function
download_all <- function(df_row) {
  table_url <- df_row[2]
  retry(download.file(table_url, destfile = paste(df_row[1],'.html', sep="")), maxErrors = 5, sleep = 20)
  
}


# download all tables html pages in ABCD 2.0 release and dump in folder if it doesn't exist already
# setting up the main directory
main_dir <- getwd()
# setting up the sub directory
sub_dir <- "abcd_tables_html"
if (file.exists(sub_dir)){
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))
} else {
  # create a new sub directory inside
  # the main path
  dir.create(file.path(main_dir, sub_dir))
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))
  apply(abcd_tabs, MARGIN=1, download_all)
  setwd("..")
}

# function to scrape all table descriptions from downloaded htmls
extract_table_des <- function(html_file) {
  table_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", html_file) # take off file extension (e.g. ".html")
  html_pg <- read_html(html_file)
  table_des <- html_pg %>% html_elements("div.ds-main-properties") %>% html_element("span")  %>% html_text() # get the table description
  table_info <- c(table_name, table_des)
  return(table_info)

}

# run extract_table_des function to scrape all table descriptions on folder of downloaded ABCD tables (.html files), dump into dataframe called tabled_details
if (file.exists(sub_dir)){
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))
  htmls <- list.files(".")
  table_details <- lapply(htmls, extract_table_des)
  table_details <- as.data.frame(do.call(rbind, table_details))
  colnames(table_details)[colnames(table_details) == "V1"] ="table_name"
  colnames(table_details)[colnames(table_details) == "V2"] ="table_description"
  
  setwd("..")
  
} else {
  print("Unable to scrape directory of downloaded ABCD tables, check the directory named abcd_tables_html")
  setwd(file.path(main_dir))
}

write.csv(table_details,
          file='abcd_tables_des.csv',
          row.names=FALSE,
          quote=FALSE)

# add the COLUMN descriptions to the vis table for better clarity about what the columns mean
# change col names of abcd_dict to allow merging, map column or ABCD descriptions to their columns

# conduct mapping for "Var1" column  
# JOIN BY ELEMENT DESCRIPTION AND TABLE NAME (when this is done, the KG blows up bc there is a column (interview_age) of same name in multiple tables)
# for clarity, see code between star dashes below
# ------- @@@ ------- !!!!!------- @@@ ------- !!!!!------- @@@ ------- !!!!!------- @@@ ------- !!!!!
colnames(vis)[colnames(vis) == "Var1"] ="ElementName"

test <- vis %>% left_join(abcd_dict[, c("table_name", "ElementName", "ElementDescription")], by="ElementName")
# rename cols
test <- test %>% 
  rename("Var1_tablename" = "table_name",
         "Var1_description" = "ElementDescription",
         "Var1" = "ElementName")

test1 <- aggregate(Var1_tablename ~., test, toString)
# ------- @@@ ------- !!!!!------- @@@ ------- !!!!!------- @@@ ------- !!!!!------- @@@ ------- !!!!!

# the data dictionary has multiple rows/tables for interview_age, interview_date, and subject_key which blows up size of KG/vis when I try and left_join
# make the data dictionary have only 1 table and 1 description
abcd_dict <- abcd_dict[!grepl("interview_age",abcd_dict$ElementName), ]
abcd_dict <- abcd_dict %>% add_row(table_name = NA,
                                   ElementName = "interview_age",
                                   DataType = "Integer",
                                   Size = NA,
                                   Required = NA,
                                   Condition = NA,
                                   ElementDescription = 'Age in months at the time of the interview/test/sampling/imaging.',
                                   ValueRange = "0::1260",
                                   Notes = "Age is rounded to chronological month. If the research participant is 15-days-old at time of interview, the appropriate value would be 0 months. If the participant is 16-days-old, the value would be 1 month.",
                                   Aliases = NA)

# check that that all rows are deleted and the correct row has been added in place 
# abcd_dict[abcd_dict$ElementName == 'interview_age',]

abcd_dict <- abcd_dict[!grepl("interview_date",abcd_dict$ElementName), ]
abcd_dict <- abcd_dict %>% add_row(table_name = NA,
                                   ElementName = "interview_date",
                                   DataType = "Date",
                                   Size = NA,
                                   Required = NA,
                                   Condition = NA,
                                   ElementDescription = 'Date on which the interview/genetic test/sampling/imaging/biospecimen was completed.',
                                   ValueRange = NA,
                                   Notes = NA,
                                   Aliases = NA)
# check that that all rows are deleted and the correct row has been added in place 
# abcd_dict[abcd_dict$ElementName == 'interview_date',]

abcd_dict <- abcd_dict[!grepl("subjectkey",abcd_dict$ElementName), ]
abcd_dict <- abcd_dict %>% add_row(table_name = NA,
                                   ElementName = "subjectkey",
                                   DataType = "GUID",
                                   Size = NA,
                                   Required = "Required",
                                   Condition = NA,
                                   ElementDescription = 'The NDAR Global Unique Identifier (GUID) for research subject',
                                   ValueRange = "NDAR*",
                                   Notes = NA,
                                   Aliases = NA)
# check that that all rows are deleted and the correct row has been added in place 
# abcd_dict[abcd_dict$ElementName == 'subjectkey',]

abcd_dict <- abcd_dict[!grepl("src_subject_id",abcd_dict$ElementName), ]
abcd_dict <- abcd_dict %>% add_row(table_name = NA,
                                   ElementName = "src_subject_id",
                                   DataType = "String",
                                   Size = 20,
                                   Required = "Required",
                                   Condition = NA,
                                   ElementDescription = "Subject ID how it's defined in lab/project",
                                   ValueRange = NA,
                                   Notes = NA,
                                   Aliases = NA)
# check that that all rows are deleted and the correct row has been added in place 
# abcd_dict[abcd_dict$ElementName == 'src_subject_id',]

abcd_dict <- abcd_dict[!grepl("sex",abcd_dict$ElementName), ]
abcd_dict <- abcd_dict %>% add_row(table_name = NA,
                                   ElementName = "sex",
                                   DataType = "String",
                                   Size = 20,
                                   Required = "Required",
                                   Condition = NA,
                                   ElementDescription = "Sex of the subject",
                                   ValueRange = NA,
                                   Notes = "M;F M = Male; F = Female",
                                   Aliases = "gender")
# check that that all rows are deleted and the correct row has been added in place 
# abcd_dict[abcd_dict$ElementName == 'sex',]


# NOW add the column descriptions and table names
colnames(vis)[colnames(vis) == "Var1"] ="ElementName"
vis <- vis %>% left_join(abcd_dict[, c("ElementName", "table_name", "ElementDescription", "Notes")], by="ElementName")
# rename cols so they correspond to the right variable
vis <- vis %>% rename("Var1_tablename" = "table_name",
                      "Var1_description" = "ElementDescription",
                      "Var1_notes" = "Notes",
                      "Var1" = "ElementName")
# repeat for Var2
colnames(vis)[colnames(vis) == "Var2"] ="ElementName"
vis <- vis %>% left_join(abcd_dict[, c("ElementName", "table_name", "ElementDescription", "Notes")], by="ElementName")
# rename cols so they correspond to the right variable
vis <- vis %>% rename("Var2_tablename" = "table_name",
                      "Var2_description" = "ElementDescription",
                      "Var2_notes" = "Notes",
                      "Var2" = "ElementName")


# some column and table descriptions did not get added because the column name is actually in the Aliases column
# join on Aliases to grab those tables and descriptions
# add the column descriptions and table names again, this time by trying to find the names in Aliases
colnames(vis)[colnames(vis) == "Var1"] ="Aliases"
vis <- vis %>% left_join(abcd_dict[, c("Aliases", "table_name", "ElementDescription", "Notes")], by="Aliases")
# rename cols so they correspond to the right variable
vis <- vis %>% rename("Var1_Alias_tablename" = "table_name",
                      "Var1_Alias_description" = "ElementDescription",
                      "Var1_Alias_notes" = "Notes",
                      "Var1_Alias" = "Aliases")

# repeat for Var2
colnames(vis)[colnames(vis) == "Var2"] ="Aliases"
vis <- vis %>% left_join(abcd_dict[, c("Aliases", "table_name", "ElementDescription", "Notes")], by="Aliases")
# rename cols so they correspond to the right variable
vis <- vis %>% rename("Var2_Alias_tablename" = "table_name",
                      "Var2_Alias_description" = "ElementDescription",
                      "Var2_Alias_notes" = "Notes",
                      "Var2_Alias" = "Aliases")








colnames(temp_dict)[colnames(temp_dict) == "ElementName"] ="Var1"
vis <- vis %>% left_join(temp_dict, by=c("row"))
colnames(vis)[colnames(vis) == "ElementDescription"] ="row_description"

# conduct mapping for "column" column
colnames(temp_dict)[colnames(temp_dict) == "row"] ="column"
vis <- vis %>% left_join(temp_dict, by=c("column"))
colnames(vis)[colnames(vis) == "ElementDescription"] ="column_description"

# add the TABLE names to the vis table for better clarity about what the columns mean
temp_dict <- subset(abcd_dict, select=c("table_name", "ElementName"))
colnames(temp_dict)[colnames(temp_dict) == "ElementName"] ="row"
vis <- vis %>% left_join(temp_dict, by=c("row"))
colnames(vis)[colnames(vis) == "table_name"] ="row_table_name"

colnames(temp_dict)[colnames(temp_dict) == "row"] ="column"
vis <- vis %>% left_join(temp_dict, by=c("column"))
colnames(vis)[colnames(vis) == "table_name"] ="col_table_name"

# add the TABLE descriptions to the vis table for better clarity about what the columns mean
colnames(vis)[colnames(vis) == "row_table_name"] ="table_name"
vis <- vis %>% left_join(table_details, by=c("table_name"))
colnames(vis)[colnames(vis) == "table_name"] ="row_table_name"
colnames(vis)[colnames(vis) == "table_description"] ="row_table_des"

colnames(vis)[colnames(vis) == "col_table_name"] ="table_name"
vis <- vis %>% left_join(table_details, by=c("table_name"))
colnames(vis)[colnames(vis) == "table_name"] ="col_table_name"
colnames(vis)[colnames(vis) == "table_description"] ="col_table_des"

# add the TABLE descriptions to the vis table for better clarity about what the columns mean
# write all of the various correlation tables to a CSV to open in Cytoscape as a network
write.csv(vis,
          file='abcd_spearman_corr_all.csv',
          row.names=FALSE)




#GOALS
OUTPUT FILE THAT HAS TABLES AND DESCRIPTIONS
OUTPUT ROWS AND EDGES TSVS

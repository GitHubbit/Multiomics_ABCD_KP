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
unique(abcd_sub$eventname)
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

# get columns where the total number of observations exceeds 10 
filtering <- data.frame(colSums(numerical_kg != 0, na.rm=TRUE) > 10) 
# filtering$cols <- rownames(filtering)
# rownames(filtering) <- NULL
filtering$log = ifelse(filtering$colSums.numerical_kg....0..na.rm...TRUE. > 0,TRUE,FALSE)
filtering <- filter(filtering, log == TRUE)
filtering <- t(filtering)
numerical_kg_clean <- numerical_kg[colnames(numerical_kg) %in% colnames(filtering)]

# conduct Shapiro-wilk's test for normality on each column
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

# function to flatten correlation matrix to rows
flat_cor_mat <- function(cor_r, cor_p, cor_n){
  #This function provides a simple formatting of a correlation matrix
  #into a table with 4 columns containing :
  # Column 1 : row names (variable 1 for the correlation test)
  # Column 2 : column names (variable 2 for the correlation test)
  # Column 3 : the correlation coefficients
  # Column 4 : the p-values of the correlations
  cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
  cor_r <- gather(cor_r, column, cor, -1)
  cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
  cor_p <- gather(cor_p, column, adj_p, -1)
  cor_n <- rownames_to_column(as.data.frame(cor_n), var = "row")
  cor_n <- gather(cor_n, column, n, -1)
  cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
  cor_p_matrix <- left_join(cor_p_matrix, cor_n, by = c("row", "column"))
  cor_p_matrix
}

flat_cor_matrix <- flat_cor_mat(corr_mat$r, corr_mat$adj_p, corr_mat$n) 
head(flat_cor_matrix)

# drop rows where adjusted p-val is <0.05 [optional: and r=1]
vis <- flat_cor_matrix[flat_cor_matrix$adj_p < 0.05, ]
# remove rows where there's 0 correlation
vis <- vis[vis$cor != 0, ]
#### add pseudocount for p-values that = 0 so it is plottable
vis["adj_p"][vis["adj_p"] == 0] <- 1E-20

vis <- vis[complete.cases(vis), ]
# remove duplicate rows 

# remove weakly correlated pairs (r between -0.5 and +0.5)
# vis <- subset(vis, cor < -0.5 | cor > 0.5)

# we are building correlation network, so row and column pair should be non duplicated
# in other words, housing_1(row) and smoking_4(column) have correlation of 0.87
# we want to delete row in vis where it's a repeat, smoking_4(row) and housing_1(column) have correlation of 0.87
# if we don't do this, we will get bi-directional edges between the same 2 nodes
vis <- vis[!duplicated(t(apply(vis[,c(1,2)],1,sort))),]  # sort first 2 cols of vis dataframe, transpose, get non-duplicates

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
vis$row_sub <- sub("\\_.*", "", vis$row)
vis$col_sub <- sub("\\_.*", "", vis$col)
vis_dtab <- vis[which(vis$row_sub != vis$col_sub),]

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
# download.file(abcd_data_dict_2_url, destfile = 'abcd_data_dict_2.html') # uncomment to scrape
abcd_dict_2 <- read_html("abcd_data_dict_2_url.html")
tab_shortnames <- abcd_dict_2 %>% html_nodes("td.short-name-column") %>% html_text() 
tab_links <- abcd_dict_2 %>% html_elements("td.short-name-column") %>% html_elements("a") %>% html_attr("href")
tab_links <-  paste("https://nda.nih.gov", tab_links, sep="")
abcd_tabs <- data.frame(tab_shortnames, tab_links)

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

download_all <- function(df_row) {
  # print(df_row)
  table_url <- df_row[2]
  # print(paste(df_row[1],'.html', sep=""))
  # download.file(table_url, destfile = paste(df_row[1],'.html')) # uncomment to scrape
  retry(download.file(table_url, destfile = paste(df_row[1],'.html', sep="")), maxErrors = 5, sleep = 20)
  
}

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


# we need the descriptions of the tables bc the data dictionary isn't informative enough about what the column names/nodes in network mean
# web scrape the NIMH (https://nda.nih.gov/data_dictionary.html?source=ABCD%2BRelease%2B2.0&submission=ALL) data dictionary table descriptions to get better understanding of what column means
# download.file(abcd_data_dict_2_url, destfile = 'abcd_data_dict_2.html') # uncomment to scrape
html_pg <- read_html("/Users/Kamileh/Work/ISB/NCATS_BiomedicalTranslator/Projects/ABCD/scripts/R/abcd_tables_html/acspsw03.html")
table_des <- html_pg %>% html_elements("div.ds-main-properties") %>% html_element("span")  %>% html_text() # get the table description


# write function to scrape all table descriptions
extract_table_des <- function(html_file) {
  table_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", html_file) # take off file extension (e.g. ".html")
  html_pg <- read_html(html_file)
  table_des <- html_pg %>% html_elements("div.ds-main-properties") %>% html_element("span")  %>% html_text() # get the table description
  table_info <- list(table_name, table_des)
  return(table_info)
  # print(table_info)
  
}

# run extract_table_des function to scrape all table descriptions on folder of downloaded ABCD tables (.html files)
if (file.exists(sub_dir)){
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))
  htmls <- list.files(".")

  table_details <- lapply(htmls, extract_table_des)
  table_details <- as.data.frame(do.call(rbind, table_details))
  
  setwd("..")
  
} else {
  print("Unable to scrape directory of downloaded ABCD tables, check the directory named abcd_tables_html")
  setwd(file.path(main_dir))
}







test <- 
setwd(file.path(main_dir, sub_dir))
htmls <- list.files(".")

lapply(htmls, extract_table_des)
setwd("..")


























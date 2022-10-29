# ETL of ABCD Data

setwd('/Users/Kamileh/Work/ISB/NCATS_BiomedicalTranslator/Projects/ABCD/scripts/R')

install.packages("librarian")
librarian::shelf("data.table", "R.utils", "tidyverse", "tidyr", "stringr", "tibble", "corrplot", "lares", "ggplot2")

rm(list=ls())
abcd3 <- readRDS("/Volumes/TOSHIBA_EXT/ISB/ABCD/data/ABCD_release_2.0_rds/ABCD_releases_2.0.1_Rds/nda2.0.1.Rds")

unique(abcd_sub$event_name) # see what types eventnames there are (there are baseline, 1 year 6 month, etc readings)
unique(abcd_sub$eventname)
# get only baseline readings
abcd_baseline <- abcd3[abcd3$eventname %like% "baseline", ]

# get a subset of data easier to work with (get only baseline readings)
# abcd_sub <- abcd_baseline[1:5000,]
abcd_sub <- abcd_baseline
rm(abcd3) # remove big data, work with subset

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

# get columns where the total number of observations exceeds 10 
filtering <- data.frame(colSums(numerical_kg != 0, na.rm=TRUE)) 
# filtering$cols <- rownames(filtering)
# rownames(filtering) <- NULL
filtering$log = ifelse(filtering$colSums.numerical_kg....0..na.rm...TRUE. > 0,TRUE,FALSE)
filtering <- filter(filtering, log == TRUE)
filtering <- t(filtering)
numerical_kg_clean <- numerical_kg[colnames(numerical_kg) %in% colnames(filtering)]

# conduct Shapiro-wilk's test for normality on each column
# num_kg <- data.frame(sapply(numerical_kg_clean[, 4:ncol(numerical_kg_clean)], as.numeric)) # force all cols to numeric
num_kg <- sapply(numerical_kg_clean[, 4:ncol(numerical_kg_clean)], as.numeric) # force all cols to numeric

# num_kg <- num_kg[which(apply(num_kg, 2, function(f) sum(!is.na(f)) >= 3))]
# data_test_numerics_only_filled_colums = data_test_numerics_only[which(apply(data_test_numerics_only, 2, function(f) sum(!is.na(f)) >= 3))]


# tack the first 3 cols of metadata back on
numerical_kg_clean <- cbind(numerical_kg_clean$subjectid,
                            numerical_kg_clean$src_subject_id,
                            numerical_kg_clean$eventname,
                            num_kg)

values_count_per_col <- data.frame(colSums(num_kg !=0, na.rm=TRUE)) # get count of nonzero and non-na values in each column
flat_cor_matrix[flat_cor_matrix$adj_p < 0.05, ]

# test for normality of data. Can't perform shapiro wilk bc size has to be btw 3-5000. 
# apply(num_kg,2,shapiro.test)

corr_mat <- rcorr(num_kg, type="spearman")
corr_mat$r[corr_mat$n < 10] <- NA # ignore less than 10 observations
corr_mat$adj_p <- matrix(p.adjust(corr_mat$P, method="BH"), ncol = ncol(corr_mat$P), dimnames = dimnames(corr_mat$r))


# let's visualize results in corr network

# function to flatten correlation matrix to rows
flat_cor_mat <- function(cor_r, cor_p){
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
  cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
  cor_p_matrix
}

flat_cor_matrix <- flat_cor_mat(corr_mat$r, corr_mat$adj_p) 
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



ggplot(vis, aes(x=neg_log_p_val, y = ifelse(..count.. < 4500, ..count.., 0))) +
  geom_histogram(bins=50, fill="red", color="maroon") +
  stat_bin(aes(label=..count..), geom="text", vjust = -0.7)

p_histo <- ggplot(vis, aes(x=neg_log_p_val, y = ifelse(..count.., ..count.., 0))) +
  geom_histogram(bins=50, fill="red", color="maroon") +
  stat_bin(aes(label=..count..), geom="text", vjust = -0.7)


hggplot() + aes(neg_log_p_val)+ geom_histogram(binwidth=1, colour="black", fill="white")


ggplot(neg_log_p_val, aes(x="neg_log_p_val", y = ifelse(..count.. > 2000, ..count.., 0))) +
  geom_histogram(bins=30) 



vis$concat <- paste(vis$row, " + ", vis$column)

p <- data.frame(vis$concat, vis$cor, vis$adj_p)

write.csv(vis,file='abcd_spearman_corr.csv', row.names=FALSE)


p_top <- head(p[order(p$vis.adj_p, p$vis.cor),], 50)
p_down <- head(p[order(p$vis.adj_p, -p$vis.cor),], 50)

bar_corr_top <- ggplot(p, aes(x=vis.cor, y=vis.concat)) + geom_bar(stat = "identity")
bar_corr_down



# turning it back into matrix
vis_c <- vis[,c("row", "column", "cor")]
vis_p <- vis[,c("row", "column", "adj_p")]

vis_c <- vis_c %>% pivot_wider(names_from = column, values_from = cor)
vis_c <- as.matrix(vis_c %>% remove_rownames() %>% column_to_rownames(var='row'))
vis_p <- vis_p %>% pivot_wider(names_from = column, values_from = adj_p)
vis_p <- as.matrix(vis_p %>% remove_rownames() %>% column_to_rownames(var='row'))


test <- corrplot(vis_c, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

test <- corr_cross(vis_c, rm.na = TRUE, max_pvalue = 0.01, top = 15)
test


vis_c

corrplot(vis_c, type="upper", is.corr = FALSE)





# drop rows where perfect correlation bc they are the same variable
# vis[!duplicated(df1[c("x1","x2")]),]
vis <- vis[order(vis$adj_p, vis$cor),]






corrplot(vis, order = "hclust")













#prepare to drop duplicates and correlations of 1     
corr_mat$sig_r[lower.tri(corr_mat$sig_r,diag=TRUE)] <- NA 
#drop perfect correlations
corr_mat$sig_r[corr_mat$sig_r == 1] <- NA 

#turn into a 3-column table
corr_mat$sig_r <- as.data.frame(as.table(corr_mat$sig_r))
#remove the NA values from above 
corr_mat$sig_r <- na.omit(corr_mat$sig_r) 



write.table(corr_mat$sig_r, file="test.txt") 

dirname(getwd())





















# *     ---------***** ~~~~~~~      ---------***** ~~~~~~~ ---------***** ~~~~~~~       * #
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




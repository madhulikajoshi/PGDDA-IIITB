
##########PRELIMINARY WORK START##########
## get the working directory
getwd()

# if the working directory is not correct, then set the wotking directory
setwd(".")

#list the files in the directory to see if the relevant files are there
list.files()


#install all the relevant packages

# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("stringr")
# install.packages("caret")
# install.packages("corrplot")

# load the packages
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(reshape2)
library(stringr)
library(caret)
library(corrplot)
##########PRELIMINARY WORK END##########



##########IMPORTING DATA START##########

##Reading loan.csv file and loading the data into data frame loan. 
##importing with stringsAsFactors as FALSE. Will convert the relevant strings to factors as needed
loan<-read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE) 
View(loan)

str(loan)
# gives 'data.frame':	39717 obs. of  111 variables:



##IMPORT DATA FROM dataDictionary EXCEL SHEET AND CHECK WHETHER LOAN DATAFRAME
##COLUMNS HAVE ALL THE COLUMNS  

dataDictionary<-read_excel("Data_Dictionary.xlsx")
dd_names<-as.character(na.omit(dataDictionary$LoanStatNew))
loan_names<-names(loan)
setdiff(dd_names,loan_names)

##THESE ARE THE COLUMNS WHICH DO NOT EXIST IN loan dataframe
##"fico_range_high"       "fico_range_low"        "last_fico_range_high" 
## "last_fico_range_low"   "total_rev_hi_lim  "    "verified_status_joint"


##########IMPORTING DATA END##########



##########PRELIMINARY DATA CLEANING START##########



#find the numeric columns in the loan dataframe  
numeric_columns <- sapply(loan, is.numeric)


#conversion of data frame to lowercase to avoid any duplication
loan <- mutate_each(loan, funs(tolower))

#the conversion to lowercase converts all numeric columns to characters. 
#reconvert them into numeric
loan[numeric_columns] <-lapply(loan[numeric_columns], as.numeric)
str(loan)# shows that the numeric columns are again being shown as numeric



## find the length of unique members for each column
as.data.frame(sapply(loan,function(x)length(unique(x))))
#                                          sapply(loan, function(x) length(unique(x)))
#id                                                                   39717
#member_id                                                            39717
#loan_amnt                                                              885
#funded_amnt                                                           1041
#funded_amnt_inv                                                       8205
#term                                                                     2
#int_rate                                                               371
#installment                                                          15383
#grade                                                                    7
#sub_grade                                                               35
#emp_title                                                            27445
#emp_length                                                              12
#home_ownership                                                           5
#annual_inc                                                            5318
#verification_status                                                      3
#issue_d                                                                 55
#loan_status                                                              3
#pymnt_plan                                                               1
#url                                                                  39717
#desc                                                                 26512
#purpose                                                                 14
#title                                                                17677
#zip_code                                                               823
#addr_state                                                              50
#dti                                                                   2868
#delinq_2yrs                                                             11
#earliest_cr_line                                                       526
#inq_last_6mths                                                           9
#mths_since_last_delinq                                                  96
#mths_since_last_record                                                 112
#open_acc                                                                40
#pub_rec                                                                  5
#revol_bal                                                            21711
#revol_util                                                            1090
#total_acc                                                               82
#initial_list_status                                                      1
#out_prncp                                                             1137
#out_prncp_inv                                                         1138
#total_pymnt                                                          37850
#total_pymnt_inv                                                      37518
#total_rec_prncp                                                       7976
#total_rec_int                                                        35148
#total_rec_late_fee                                                    1356
#recoveries                                                            4040
#collection_recovery_fee                                               2616
#last_pymnt_d                                                           102
#last_pymnt_amnt                                                      34930
#next_pymnt_d                                                             3
#last_credit_pull_d                                                     107
#collections_12_mths_ex_med                                               2
#mths_since_last_major_derog                                              1
#policy_code                                                              1
#application_type                                                         1
#annual_inc_joint                                                         1
#dti_joint                                                                1
#verification_status_joint                                                1
#acc_now_delinq                                                           1
#tot_coll_amt                                                             1
#tot_cur_bal                                                              1
#open_acc_6m                                                              1
#open_il_6m                                                               1
#open_il_12m                                                              1
#open_il_24m                                                              1
#mths_since_rcnt_il                                                       1
#total_bal_il                                                             1
#il_util                                                                  1
#open_rv_12m                                                              1
#open_rv_24m                                                              1
#max_bal_bc                                                               1
#all_util                                                                 1
#total_rev_hi_lim                                                         1
#inq_fi                                                                   1
#total_cu_tl                                                              1
#inq_last_12m                                                             1
#acc_open_past_24mths                                                     1
#avg_cur_bal                                                              1
#bc_open_to_buy                                                           1
#bc_util                                                                  1
#chargeoff_within_12_mths                                                 2
#delinq_amnt                                                              1
#mo_sin_old_il_acct                                                       1
#mo_sin_old_rev_tl_op                                                     1
#mo_sin_rcnt_rev_tl_op                                                    1
#mo_sin_rcnt_tl                                                           1
#mort_acc                                                                 1
#mths_since_recent_bc                                                     1
#mths_since_recent_bc_dlq                                                 1
#mths_since_recent_inq                                                    1
#mths_since_recent_revol_delinq                                           1
#num_accts_ever_120_pd                                                    1
#num_actv_bc_tl                                                           1
#num_actv_rev_tl                                                          1
#num_bc_sats                                                              1
#num_bc_tl                                                                1
#num_il_tl                                                                1
#num_op_rev_tl                                                            1
#num_rev_accts                                                            1
#num_rev_tl_bal_gt_0                                                      1
#num_sats                                                                 1
#num_tl_120dpd_2m                                                         1
#num_tl_30dpd                                                             1
#num_tl_90g_dpd_24m                                                       1
#num_tl_op_past_12m                                                       1
#pct_tl_nvr_dlq                                                           1
#percent_bc_gt_75                                                         1
#pub_rec_bankruptcies                                                     4
#tax_liens                                                                2
#tot_hi_cred_lim                                                          1
#total_bal_ex_mort                                                        1
#total_bc_limit                                                           1
#total_il_high_credit_limit                                               1
 
#check for any duplicated rows
nrow(loan) - nrow(unique(loan))
#gives 0

#Check for duplicate values
sum(duplicated(loan$id)) #no duplicates found
sum(duplicated(loan$member_id)) #no duplicates found


#Check for NA values
sum(is.na(loan)) # gives [1] 2208180

## find rows with missing id and memeber_id. These can be dropped 
sum(is.na(loan$id)) #none
sum(is.na(loan$member_id)) #none

# checking for the column with only NA values. These can be removed from the dataset as they add no value. 
sapply(loan, function(x) all(is.na((x)))) #gives a list of all columns and whether they are all NA's or not

sum(sapply(loan, function(x) all(is.na((x))))) # gives 54 columns with all NA's

#remove all the columns that have only NA's. This gives 57 columns which is correct as total_columns(111) - All NA Cols (54) = 57
loan<- loan[, !sapply(loan, function(x) all(is.na((x))))]
str(loan) # shows that numeric columns are retained as numeric

#find the columns that have only a single value in the entire column
sapply(loan, function(x) length(unique(x))==1)

sum(sapply(loan, function(x) length(unique(x))==1)) #gives 6

which(sapply(loan, function(x) length(unique(x))==1))
#gives
#pymnt_plan initial_list_status         policy_code    application_type      acc_now_delinq         delinq_amnt 
#18                  36                  51                  52                  53                  55

#remove all the columns that have single value. It gives 51 columns which is correct(57-6 = 51)
loan_final<- loan[, !sapply(loan, function(x) length(unique(x))==1)]

View(loan_final)

str(loan_final)

#####

#remove months from loan term column
loan_final$term<-gsub("months", "", loan_final$term, ignore.case = TRUE)

#remove % from interest rate  and revol_util column
loan_final$int_rate<-gsub("%", "", loan_final$int_rate, ignore.case = TRUE)

loan_final$revol_util<-gsub("%", "", loan_final$revol_util, ignore.case = TRUE)

str(loan_final)

#####CHECKING INDIVIDUAL COLUMNS START#####

# convert into factors - term                      : chr  " 36 " " 60 " " 36 " " 36 " ...
loan_final$term <- as.factor(trimws(loan_final$term))
# gives   $ term                    : Factor w/ 2 levels "36","60": 1 2 1 1 2 1 2 1 2 2 ...


# convert into numeric - int_rate                  : chr  "10.65" "15.27" "15.96" "13.49" ...
loan_final$int_rate <-as.numeric(loan_final$int_rate)
# gives  $ int_rate                  : num  10.7 15.3 16 13.5 12.7 ...


# convert into factors - grade                     : chr  "b" "c" "c" "c" ...
loan_final$grade <-as.factor(loan_final$grade)
#gives  $ grade                     : Factor w/ 7 levels "a","b","c","d",..: 2 3 3 3 2 1 3 5 6 2 ...


# convert into factors - sub_grade                 : chr  "b2" "c4" "c5" "c1" ...
loan_final$sub_grade <-as.factor(loan_final$sub_grade)
#gives $ sub_grade                 : Factor w/ 35 levels "a1","a2","a3",..: 7 14 15 11 10 4 15 21 27 10 ...


#convert into factors -  emp_length                : chr  "10+ years" "< 1 year" "10+ years" "10+ years" ...
loan_final$emp_length <-as.factor(loan_final$emp_length)


# convert into factors -  home_ownership            : chr  "rent" "rent" "rent" "rent" ...
loan_final$home_ownership <-as.factor(loan_final$home_ownership)
#gives  $ home_ownership            : Factor w/ 5 levels "mortgage","none",..: 5 5 5 5 5 5 5 5 4 5 ...


# convert into factors - verification_status       : chr  "verified" "source verified" "not verified" "source verified" ...
loan_final$verification_status <-as.factor(loan_final$verification_status)
# gives $ verification_status       : Factor w/ 3 levels "not verified",..: 3 2 1 2 2 2 1 2 2 3 ...


# Convert to date - issue_d                   : chr  "dec-11" "dec-11" "dec-11" "dec-11" ...
#add dummy day using paste()
loan_final$issue_d<-paste("01-",loan_final$issue_d, sep = "")
loan_final$issue_d<-as.Date(loan_final$issue_d, format="%d-%b-%y")
#gives  $ issue_d                   : Date, format: "2011-12-01" "2011-12-01" "2011-12-01" "2011-12-01" ...


# convert into factors - loan_status               : chr  "fully paid" "charged off" "fully paid" "fully paid" ...
loan_final$loan_status <-as.factor(loan_final$loan_status)
#gives  $ loan_status               : Factor w/ 3 levels "charged off",..: 3 1 3 3 2 3 3 3 1 1 ...


# convert into factors - purpose                   : chr  "credit_card" "car" "small_business" "other" ...
loan_final$purpose<-as.factor(loan_final$purpose)
#gives $ purpose                   : Factor w/ 14 levels "car","credit_card",..: 2 1 12 10 10 14 3 1 12 10 ...


#strip   zip_code                  : chr  "860xx" "309xx" "606xx" "917xx" ...
loan_final$zip_code=strtrim(loan_final$zip_code,3)
#gives zip_code               : chr  "860" "309" "606" "917" ...

# convert into factors - addr_state                : chr  "az" "ga" "il" "ca" ...
loan_final$addr_state <-as.factor(loan_final$addr_state)
#gives  $ addr_state                : Factor w/ 50 levels "ak","al","ar",..: 4 11 15 5 37 4 28 5 5 43 ...


# Convert to date - earliest_cr_line          : chr  "jan-85" "apr-99" "nov-01" "feb-96" ...
#add dummy day using paste()
loan_final$earliest_cr_line<-paste("01-",loan_final$earliest_cr_line, sep = "")
loan_final$earliest_cr_line<-as.Date(loan_final$earliest_cr_line, format="%d-%b-%y")
#gives $ earliest_cr_line          : Date, format: "1985-01-01" "1999-04-01" "2001-11-01" "1996-02-01" ...


#convert to numeric -  revol_util                : chr  "83.70" "9.40" "98.50" "21" ...
loan_final$revol_util <-as.numeric(loan_final$revol_util)
# gives  $ revol_util                : num  83.7 9.4 98.5 21 53.9 28.3 85.6 87.5 32.6 36.5 ...


# Convert to date - last_pymnt_d              : chr  "jan-15" "apr-13" "jun-14" "jan-15" ...
#add dummy day using paste()
loan_final$last_pymnt_d<-paste("01-",loan_final$last_pymnt_d, sep = "")
loan_final$last_pymnt_d<-as.Date(loan_final$last_pymnt_d, format="%d-%b-%y")
#gives  $ last_pymnt_d              : Date, format: "2015-01-01" "2013-04-01" "2014-06-01" "2015-01-01" ...


# Convert to date - next_pymnt_d              : chr  "" "" "" "" ...
#add dummy day using paste()
loan_final$next_pymnt_d<-paste("01-",loan_final$next_pymnt_d, sep = "")
loan_final$next_pymnt_d<-as.Date(loan_final$next_pymnt_d, format="%d-%b-%y")
#gives   $ next_pymnt_d              : Date, format: NA NA NA NA ...


# Convert to date - last_credit_pull_d        : chr  "may-16" "sep-13" "may-16" "apr-16" ...
#add dummy day using paste()
loan_final$last_credit_pull_d<-paste("01-",loan_final$last_credit_pull_d, sep = "")
loan_final$last_credit_pull_d<-as.Date(loan_final$last_credit_pull_d, format="%d-%b-%y")
#gives    $ last_credit_pull_d        : Date, format: "2016-05-01" "2013-09-01" "2016-05-01" "2016-04-01" ...


#find the names of the columns with NA data
colnames(loan_final)[colSums(is.na(loan_final))>0]
#gives
#[1] "title"                      "mths_since_last_delinq"     "mths_since_last_record"     "last_pymnt_d"              
#[5] "next_pymnt_d"               "last_credit_pull_d"         "collections_12_mths_ex_med" "chargeoff_within_12_mths"  
#[9] "pub_rec_bankruptcies"       "tax_liens"         


## we have 51 columns at this stage. We need to figute out if there are any more columns that are not a value add for the 
## data analysis and remove thise columns. 
View(loan_final)

## going through the columns there are some that are redundant and can be removed
## Columns url, desc, emp_title, title - do not add any value and can be dropped. 
#removes 4 more columns bringing the no of coulmns down to 47
loan_final <- subset(loan_final, select = -c(url, desc, emp_title, title))

summary(loan_final)
# gives 3 columns with all values either 0's or NA's - collections_12_mths_ex_med, chargeoff_within_12_mths, tax_liens
#Cross verify using unique
unique(loan_final$collections_12_mths_ex_med)
#gives 0 NA
unique(loan_final$chargeoff_within_12_mths)
#gives 0 NA
unique(loan_final$tax_liens)
#gives 0 NA

#remove these 3 columns as well
loan_final <- subset(loan_final, select = -c(collections_12_mths_ex_med, chargeoff_within_12_mths, tax_liens))

str(loan_final)
# gives 'data.frame':	39717 obs. of  44 variables


table(sapply(loan_final, class))

#gives
#character      Date    factor   numeric 
#        1         5         9        29 
#####CHECKING INDIVIDUAL COLUMNS END#####
##########PRELIMINARY DATA CLEANING END##########

##########DERIVED METRICS START##########

##Extract dates
loan_final$year_issue_d = str_split_fixed(loan_final$issue_d, "-", 3)[,1] # Extract Year
loan_final$month_issue_d = str_split_fixed(loan_final$issue_d, "-", 3)[,2] # Extract Month Ch

loan_final$year_earl_cr_line = str_split_fixed(loan_final$earliest_cr_line, "-", 3)[,1] # Extract Year
loan_final$month_earl_cr_line = str_split_fixed(loan_final$earliest_cr_line, "-", 3)[,2] # Extract Month Ch

loan_final$year_last_credit_pull_d = str_split_fixed(loan_final$last_credit_pull_d, "-", 3)[,1] # Extract Year
loan_final$month_last_credit_pull_d = str_split_fixed(loan_final$last_credit_pull_d, "-", 3)[,2] # Extract Month Ch

loan_final$year_next_pymnt_d = str_split_fixed(loan_final$next_pymnt_d, "-", 3)[,1] # Extract Year
loan_final$month_next_pymnt_d = str_split_fixed(loan_final$next_pymnt_d, "-", 3)[,2] # Extract Month Ch

loan_final$year_last_pymnt_d = str_split_fixed(loan_final$last_pymnt_d, "-", 3)[,1] # Extract Year
loan_final$month_last_pymnt_d = str_split_fixed(loan_final$last_pymnt_d, "-", 3)[,2] # Extract Month Ch


#Quantifying sub_grade
loan_final$sub_grade_n<-unclass(loan_final$sub_grade) %>% as.numeric 

##QUANTIFYING LOAN STATUS
loan_final$loan_status_n<-unclass(loan_final$loan_status)%>% as.numeric

## seperate Default/ Non Default on loan
loan_final$Default<-ifelse(loan_final$loan_status=="current" | loan_final$loan_status=="fully paid",0,1)

View(loan_final)
summary(loan_final)

write.csv(loan_final, file = "Loan_final.csv",row.names=FALSE)
##########DERIVED METRICS END##########


##########ANALYSIS START##########

#####UNIVARIATE ANALYSIS START#####

#####loan_amnt
summary(loan_final$loan_amnt)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#500    5500   10000   11220   15000   35000 
 
#####funded_amnt
summary(loan_final$funded_amnt)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#500    5400    9600   10950   15000   35000 
 
#####funded_amnt_inv
summary(loan_final$funded_amnt_inv)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    5000    8975   10400   14400   35000

#####term
table(loan_final$term)
#36    60 
#29096 10621

table(loan_final$term)/nrow(loan_final)
#      36       60 
#0.732583 0.267417 

##plot to see the distribution of loan term
tmp = loan_final %>% group_by(term) %>% summarise(ncount = n())
tmp$ncount = 100 * tmp$ncount/nrow(loan_final)
tmp$ncount_p = str_c(round(tmp$ncount,2),"%")
ggplot(tmp,aes(x=term,y=ncount,fill=term)) + geom_bar(stat="identity") + geom_text(aes(label=ncount_p))

#####int_rate
loan_final$int_rate <-as.numeric(loan_final$int_rate)
summary(loan_final$int_rate)##Varies from 5.42 to 24.59
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.42    9.25   11.86   12.02   14.59   24.59

#####installment
summary(loan_final$installment)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.69  167.00  280.20  324.60  430.80 1305.00 

#####grade
table(loan_final$grade)
#a     b     c     d     e     f     g 
#10085 12020  8098  5307  2842  1049   316 
ggplot(loan_final,aes(grade, fill = grade)) + geom_bar() + coord_polar(theta="y") 
ggplot(loan_final, aes(grade)) + geom_histogram(stat = "count",col="red",aes(fill=..count..)) 

#####sub_grade
table(loan_final$sub_grade)
#a1   a2   a3   a4   a5   b1   b2   b3   b4   b5   c1   c2   c3   c4   c5   d1   d2   d3   d4   d5   e1   e2   e3   e4   e5   f1   f2   f3   f4 
#1139 1508 1810 2886 2742 1830 2057 2917 2512 2704 2136 2011 1529 1236 1186  931 1348 1173  981  874  763  656  553  454  416  329  249  185  168 
#f5   g1   g2   g3   g4   g5 
#118  104   78   48   56   30  
ggplot(loan_final,aes(sub_grade, fill = sub_grade)) + geom_bar() + coord_polar(theta="y") 
ggplot(loan_final, aes(sub_grade)) + geom_histogram(stat = "count",col="red",aes(fill=..count..)) 

#####emp_length
summary(loan_final$emp_length)
#<  1 year    1 year 10+ years   2 years   3 years   4 years   5 years   6 years   7 years   8 years   9 years       n/a 
#     4583      3240      8879      4388      4095      3436      3282      2229      1773      1479      1258      1075
ggplot(loan_final,aes(emp_length, fill = emp_length)) + geom_bar()

#####home_ownership
summary(loan_final$home_ownership)
#mortgage     none    other      own     rent 
#   17659        3       98     3058    18899
ggplot(loan_final,aes(home_ownership, fill = home_ownership)) + geom_bar()

#####annual_inc
summary(loan_final$annual_inc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4000   40400   59000   68970   82300 6000000

#####verification_status
summary(loan_final$verification_status)
#   not verified source verified        verified 
#          16921            9987           12809 
ggplot(loan_final,aes(verification_status, fill = verification_status)) + geom_bar()

#####issue_d
summary(loan_final$issue_d)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2007-06-01" "2010-05-01" "2011-02-01" "2010-11-03" "2011-08-01"  "2011-12-01

#####loan_status
table(loan_final$loan_status)
#charged off     current  fully paid 
#5627        1140       32950 

table(loan_final$loan_status)/nrow(loan_final)
#charged off     current  fully paid 
#0.14167737  0.02870307  0.82961956

##plot to see the distribution of loan status
tmp = loan_final %>% group_by(loan_status) %>% summarise(ncount = n())
tmp$ncount = 100 * tmp$ncount/nrow(loan_final)
tmp$ncount_p = str_c(round(tmp$ncount,2),"%")
ggplot(tmp,aes(x=loan_status,y=ncount,fill=loan_status)) + geom_bar(stat="identity") + geom_text(aes(label=ncount_p))

#####purpose
summary(loan_final$purpose)
# car        credit_card debt_consolidation        educational   home_improvement              house     major_purchase 
#1549               5130              18641                325               2976                381               2187 
# medical             moving              other   renewable_energy     small_business           vacation            wedding 
#     693                583               3993                103               1828                381                947
ggplot(loan_final,aes(purpose, fill = purpose)) + geom_bar()

#####zip_code
summary(loan_final$zip_code)
#Length     Class      Mode 
#39717 character character 

#####addr_state
#summary(loan_final$addr_state)
#ak   al   ar   az   ca   co   ct   dc   de   fl   ga   hi   ia   id   il   in   ks   ky   la   ma   md   me   mi   mn   mo   ms   mt   nc   ne 
#80  452  245  879 7099  792  751  214  114 2866 1398  174    5    6 1525    9  271  325  436 1340 1049    3  720  615  686   19   85  788    5 
# nh   nj   nm   nv   ny   oh   ok   or   pa   ri   sc   sd   tn   tx   ut   va   vt   wa   wi   wv   wy 
#171 1850  189  497 3812 1223  299  451 1517  198  472   64   17 2727  258 1407   54  840  460  177   83

#####dti
summary(loan_final$dti)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    8.17   13.40   13.32   18.60   29.99 

#####delinq_2yrs
summary(loan_final$delinq_2yrs)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.1465  0.0000 11.0000 

#####earliest_cr_line
summary(loan_final$earliest_cr_line)
#Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#"1969-02-01" "1993-12-01" "1998-05-01" "1997-04-11" "2001-09-01" "2068-12-01" 

#####inq_last_6mths
summary(loan_final$inq_last_6mths)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  1.0000  0.8692  1.0000  8.0000 

#####mths_since_last_delinq
summary(loan_final$mths_since_last_delinq)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.0    18.0    34.0    35.9    52.0   120.0   25682 

#####mths_since_last_record
summary(loan_final$mths_since_last_record)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.0    22.0    90.0    69.7   104.0   129.0   36931 

#####open_acc
summary(loan_final$open_acc)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.000   6.000   9.000   9.294  12.000  44.000 

#####pub_rec
summary(loan_final$pub_rec)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.05506 0.00000 4.00000 

#####revol_bal
summary(loan_final$revol_bal)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    3703    8850   13380   17060  149600 

#####revol_util
summary(loan_final$revol_util)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   25.40   49.30   48.83   72.40   99.90      50 

#####total_acc
summary(loan_final$total_acc)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.00   13.00   20.00   22.09   29.00   90.00 

#####out_prncp
summary(loan_final$out_prncp)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.00    0.00   51.23    0.00 6311.00 

#####out_prncp_inv
summary(loan_final$out_prncp_inv)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.00    0.00   50.99    0.00 6307.00 

#####total_pymnt
summary(loan_final$total_pymnt)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    5577    9900   12150   16530   58560 

#####total_pymnt_inv
summary(loan_final$total_pymnt_inv)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    5112    9287   11570   15800   58560 

#####total_rec_prncp
summary(loan_final$total_rec_prncp)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    4600    8000    9793   13650   35000 

#####total_rec_int
summary(loan_final$total_rec_int)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   662.2  1349.0  2264.0  2833.0 23560.0 

#####total_rec_late_fee
summary(loan_final$total_rec_late_fee)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   1.363   0.000 180.200 

#####recoveries
summary(loan_final$recoveries)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.00     0.00     0.00    95.22     0.00 29620.00 

#####collection_recovery_fee
summary(loan_final$collection_recovery_fee)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.00    0.00   12.41    0.00 7002.00 

#####last_pymnt_d
summary(loan_final$last_pymnt_d)
#        Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
#"2008-01-01" "2012-04-01" "2013-04-01" "2013-04-10" "2014-06-01" "2016-05-01"         "71" 

#####last_pymnt_amnt
summary(loan_final$last_pymnt_amnt)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   218.7   546.1  2679.0  3293.0 36120.0 

#####next_pymnt_d
summary(loan_final$next_pymnt_d)
#        Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
#"2016-06-01" "2016-06-01" "2016-06-01" "2016-06-01" "2016-06-01" "2016-07-01"      "38577" 

#####last_credit_pull_d
summary(loan_final$last_credit_pull_d)
#        Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
#"2007-05-01" "2013-06-01" "2015-03-01" "2014-09-07" "2016-05-01" "2016-05-01"          "2" 

#####pub_rec_bankruptcies
summary(loan_final$pub_rec_bankruptcies)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0000  0.0000  0.0000  0.0433  0.0000  2.0000     697 

#####UNIVARIATE ANALYSIS END#####


#####BIVARIATE ANALYSIS START#####

#####INTEREST RATE ACROSS YEARS for all grades and sub grades
ggplot(loan_final, aes(x=grade, y=int_rate,fill=as.factor(year_issue_d))) + geom_bar(stat="identity",position="dodge") + ggtitle("Interest Rate(%)")

ggplot(loan_final, aes(x=sub_grade, y=int_rate,fill=as.factor(year_issue_d))) + geom_bar(stat="identity",position="dodge") + ggtitle("Interest Rate(%)")

#####Proportion of Default each year
barplot(as.matrix(prop.table(table(loan_final$Default,loan_final$year_issue_d),2)),main = "Standard/ Default Loans", xlab= "Year", ylab= "No of Cases", col = c("Red","Green"),legend = rownames(as.matrix(prop.table(table(loan_final$Default,loan_final$year_issue_d),2))))

#####LOAN TERM Vs STATUS
##in terms of absolute numbers 36 months has more loan defaults
table(loan_final$term, loan_final$loan_status)
#   charged off current fully paid
#36        3227       0      25869
#60        2400    1140       7081

##plot to see the distribution of term and loan status
ggplot(loan_final, aes(term, fill=loan_status)) + geom_bar() 

###60 months term shows more percentage of loan default###############
ggplot(subset(loan_final,term==36),aes(term, fill =loan_status)) + geom_bar() + coord_polar(theta="y")
ggplot(subset(loan_final,term==60),aes(term, fill =loan_status)) + geom_bar() + coord_polar(theta="y") 

#####Interest Rate across Loan status - shows that the median of interest rate of charged off loans is higher
boxplot(as.numeric(int_rate)~loan_status, data=loan_final)

##### Grade and Sub Grade
#####Interest Rate across sub grade - gives a direct correlation
boxplot(as.numeric(int_rate)~sub_grade, data=loan_final)

# loan status across all sub grades
ggplot(loan_final, aes(sub_grade, fill=loan_status))+ geom_bar()
ggplot(loan_final,aes(x=factor(sub_grade),fill=loan_status))+geom_histogram(stat="count",binwidth = 1,position = "dodge")+labs(x="Sub Grade")+scale_fill_discrete(name="Loan Status") 
#grades for each status
ggplot(loan_final, aes(x = loan_status,fill = grade))+ geom_bar(position = "dodge") + labs(x = "Status", y = "Number of Borrowers", fill = "Grades" )

# loan status across all sub grades - percentages default highest in f5, g3 and g5
ggplot(loan_final, aes(sub_grade, fill=loan_status))+ geom_bar(position = "fill")

#####Proportion of Default across Grade
barplot(as.matrix(prop.table(table(loan_final$Default,loan_final$grade),2)*100),main = "Standard/ Default Loans ", xlab= "Grade", ylab= "No of Cases", col = c("Green","Red"),legend = rownames(as.matrix(prop.table(table(loan_final$Default,loan_final$grade),2)*100)))

#####Employment Length
##Bar show that most of the loans are taken by people having more than 10 years of experience
ggplot(loan_final, aes(emp_length, fill=loan_status))+ geom_bar()

##But the default percentage is most for N/A experience
ggplot(loan_final, aes(emp_length, fill=loan_status))+ geom_bar(position = "fill")

#####home_ownership
ggplot(loan_final, aes(home_ownership, fill=loan_status))+ geom_bar()
ggplot(loan_final, aes(home_ownership, fill=loan_status))+ geom_bar(position = "fill")

#####verification_status
ggplot(loan_final, aes(verification_status, fill=loan_status))+ geom_bar()
#the verified status defaults more
ggplot(loan_final, aes(verification_status, fill=loan_status))+ geom_bar(position = "fill")

#####annual_inc shows that there are some outliers which should be replaced with an average income
plot(annual_inc~loan_status, data=loan_final)

#####purpose
ggplot(loan_final, aes(purpose, fill =loan_status)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(loan_final, aes(purpose, fill =loan_status)) + geom_bar(position = "fill") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

##segmented analysis show that loan for small business tend to default a lot more (almost 25%)
ggplot(subset(loan_final,purpose=='small_business'),aes(purpose, fill =loan_status)) + geom_bar() + coord_polar(theta="y")

##For other purpose of loan we could see a similar distribution
ggplot(subset(loan_final,purpose=='debt_consolidation'),aes(purpose, fill =loan_status)) + geom_bar() + coord_polar(theta="y")
ggplot(subset(loan_final,purpose=='credit_card'),aes(purpose, fill =loan_status)) + geom_bar() + coord_polar(theta="y")
ggplot(subset(loan_final,purpose=='home_improvement'),aes(purpose, fill =loan_status)) + geom_bar() + coord_polar(theta="y") 
ggplot(subset(loan_final,purpose=='major_purchase'),aes(purpose, fill =loan_status)) + geom_bar() + coord_polar(theta="y") 
ggplot(subset(loan_final,purpose=='other'),aes(purpose, fill =loan_status)) + geom_bar() + coord_polar(theta="y") 
ggplot(subset(loan_final,purpose=='major_purchase'),aes(purpose, fill =loan_status)) + geom_bar() + coord_polar(theta="y")
ggplot(subset(loan_final,purpose=='renewable_energy'),aes(purpose, fill =loan_status)) + geom_bar() + coord_polar(theta="y")

#####DTI
#DTI does not significantly impact the loan status#######
boxplot(as.numeric(dti)~loan_status, data=loan_final)

#####inq_last_6mths
#inq_last_6mths across loan status
ggplot(loan_final, aes(inq_last_6mths, fill =loan_status)) + geom_bar()
#inq_last_6mths across loan status relative percentage  - shows more defaults in 6,7,8
ggplot(loan_final, aes(inq_last_6mths, fill =loan_status)) + geom_bar(position="fill")

ggplot(subset(loan_final,inq_last_6mths=='0'),aes(inq_last_6mths, fill =loan_status)) + geom_bar() + coord_polar(theta="y")
ggplot(subset(loan_final,inq_last_6mths=='6'),aes(inq_last_6mths, fill =loan_status)) + geom_bar() + coord_polar(theta="y")
ggplot(subset(loan_final,inq_last_6mths=='7'),aes(inq_last_6mths, fill =loan_status)) + geom_bar() + coord_polar(theta="y")
ggplot(subset(loan_final,inq_last_6mths=='8'),aes(inq_last_6mths, fill =loan_status)) + geom_bar() + coord_polar(theta="y")


#####mths_since_last_delinq
##Similar boxplot means that the data is evenly distributed
boxplot(mths_since_last_delinq~loan_status, data=loan_final)

#####open_acc
boxplot(as.numeric(open_acc)~loan_status, data=loan_final)

#####revol_bal
boxplot(as.numeric(revol_bal)~loan_status, data=loan_final)

#####total_acc
boxplot(as.numeric(total_acc)~loan_status, data=loan_final)


##########We segment the loan_final data frams into loan_defaulted and loan_paid data frames for further analysis
loan_defaulted <- subset(loan_final,loan_status == "charged off")
loan_paid <- subset(loan_final,loan_status == "fully paid")

##loan status Vs Loan Amount

loan_final %>% group_by(.,loan_status) %>% summarise(.,median=median(loan_amnt),average=mean(loan_amnt),stdev=sd(loan_amnt) )
# A tibble: 3 × 4
#   loan_status median  average    stdev
#        <fctr>  <dbl>    <dbl>    <dbl>
#1  charged off  10000 12104.39 8085.732
#2      current  15825 17053.99 8651.052
#3   fully paid   9600 10866.46 7199.629

#Loan amount by status
ggplot(loan_final,aes(loan_status,loan_amnt))+geom_boxplot(aes(fill=loan_status))+theme(axis.text.x=element_blank())+labs(list(title="Loan amount by status",x="Status",y="Amount"))

##percentage of loan amount by Status charged off is around 15 %
loan_amnt_status<-loan_final %>% group_by(.,loan_status) %>% summarise(.,percentage=sum(loan_amnt)/sum(loan_final$loan_amnt)*100)
ggplot(loan_amnt_status,aes(x=loan_status,y=percentage,fill=percentage))+geom_bar(stat="identity")+labs(list(title="Percentage of Loan amount by status",x="Status",y="Percentage"))

##loan_amnt by issue_year sum,mean,median,stddev
loan_final %>% group_by(.,year_issue_d) %>% summarise(.,sum(loan_amnt)/sum(loan_final$loan_amnt)*100)
# A tibble: 5 × 2
#year_issue_d `sum(loan_amnt)/sum(loan_final$loan_am...`
#         <chr>                                      <dbl>
#1         2007                                   0.498039
#2         2008                                   3.229396
#3         2009                                  10.421016
#4         2010                                  27.389918
#5         2011                                  58.461631

loan_final %>% group_by(.,year_issue_d) %>% summarise(.,median=median(loan_amnt),average=mean(loan_amnt),stdev=sd(loan_amnt) )
# A tibble: 5 × 4
#   year_issue_d median   average    stdev
#         <chr>  <dbl>     <dbl>    <dbl>
#1         2007   6500  8841.733 6447.772
#2         2008   7500  9212.724 5774.294
#3         2009   9000  9846.549 5934.104
#4         2010   9600 10583.611 6602.360
#5         2011  10000 12029.303 8159.403

##loan_defaulted by issue_year sum,mean,median,stddev
loan_defaulted %>% group_by(.,year_issue_d) %>% summarise(.,sum(loan_amnt)/sum(loan_final$loan_amnt)*100)
# A tibble: 5 × 2
#   year_issue_d `sum(loan_amnt)/sum(loan_final$loan_am...`
#         <chr>                                      <dbl>
#1         2007                                  0.1096044
#2         2008                                  0.6148078
#3         2009                                  1.4040480
#4         2010                                  3.5909122
#5         2011                                  9.5658542

loan_defaulted %>% group_by(.,year_issue_d) %>% summarise(.,median=median(loan_amnt),average=mean(loan_amnt),stdev=sd(loan_amnt) )
# A tibble: 5 × 4
#   year_issue_d  median  average    stdev
#         <chr>   <dbl>    <dbl>    <dbl>
#1         2007  9000.0 10853.33 8133.629
#2         2008 10000.0 11091.50 6408.802
#3         2009  9287.5 10532.79 6351.897
#4         2010  9600.0 10775.22 6899.997
#5         2011 11500.0 13091.43 8810.036
 
##loan_paid by issue_year sum,mean,median,stddev
loan_paid %>% group_by(.,year_issue_d) %>% summarise(.,sum(loan_amnt)/sum(loan_final$loan_amnt)*100)
# A tibble: 5 × 2
#year_issue_d `sum(loan_amnt)/sum(loan_final$loan_am...`
#<chr>                                      <dbl>
#1         2007                                  0.3884346
#2         2008                                  2.6145884
#3         2009                                  9.0169684
#4         2010                                 23.7990057
#5         2011                                 44.5327973

loan_paid %>% group_by(.,year_issue_d) %>% summarise(.,median=median(loan_amnt),average=mean(loan_amnt),stdev=sd(loan_amnt) )
# A tibble: 5 × 4
#   year_issue_d  median   average    stdev
#         <chr>   <dbl>     <dbl>    <dbl>
#1         2007  6112.5  8402.306 5951.310
#2         2008  7500.0  8859.829 5579.648
#3         2009  8600.0  9747.659 5865.650
#4         2010  9500.0 10555.290 6557.103
#5         2011 10000.0 11497.064 7860.794

##INFERENCE DRAWN LOAN AMT IS CHARGED OFF MAXIMUM IN YEAR 2011
ggplot(loan_final,aes(y=loan_amnt,x=year_issue_d))+geom_bar(stat="identity",aes(fill=loan_status),position ="stack")+scale_fill_discrete(name="Loan Status") +labs(list(title="Loan amount by year - All",x="Year",y="Amount"))
ggplot(loan_defaulted,aes(y=loan_amnt,x=year_issue_d))+geom_bar(stat="identity",aes(fill=loan_status),position ="stack")+scale_fill_discrete(name="Loan Defaulters") +labs(list(title="Loan amount by year - Defaulters",x="Year",y="Amount"))
ggplot(loan_paid,aes(y=loan_amnt,x=year_issue_d))+geom_bar(stat="identity",aes(fill=loan_status),position ="stack")+scale_fill_discrete(name="Loan Paid") +labs(list(title="Loan amount by year - Paid",x="Year",y="Amount"))

##Total loan_amnt by purpose for all, defaulted and paid
loan_final %>% group_by(.,purpose) %>% summarise(.,median=median(loan_amnt),average=mean(loan_amnt),stdev=sd(loan_amnt) ) 
# A tibble: 14 × 4
#              purpose median   average    stdev
#               <fctr>  <dbl>     <dbl>    <dbl>
#1                 car   6000  6777.518 4106.818
#2         credit_card  10000 11723.616 6986.050
#3  debt_consolidation  11500 12694.990 7479.954
#4         educational   5400  6810.462 5141.443
#5    home_improvement  10000 11537.206 8085.741
#6               house  11000 13070.997 8285.097
#7      major_purchase   6000  8155.430 6125.921
#8             medical   6000  8263.672 6119.468
#9              moving   5000  6650.172 5797.798
#10              other   6000  8067.612 6421.720
#11   renewable_energy   6000  8717.233 7071.083
#12     small_business  12000 13567.273 8622.464
#13           vacation   4800  5463.320 4338.201
#14            wedding   8000  9876.030 6145.516

loan_defaulted %>% group_by(.,purpose) %>% summarise(.,median=median(loan_amnt),average=mean(loan_amnt),stdev=sd(loan_amnt) )
# A tibble: 14 × 4
#              purpose median   average    stdev
#               <fctr>  <dbl>     <dbl>    <dbl>
#1                 car   6000  7264.219 4493.115
#2         credit_card  12000 13014.945 7505.102
#3  debt_consolidation  12000 13517.926 8027.891
#4         educational   6000  7858.929 6032.845
#5    home_improvement  10000 11913.689 8375.945
#6               house  10000 12640.254 8846.778
#7      major_purchase   7000  9135.023 6617.444
#8             medical   6000  8568.396 6802.206
#9              moving   4650  5936.141 5316.276
#10              other   6000  8284.874 6990.215
#11   renewable_energy   4200  8421.053 7243.125
#12     small_business  12600 14604.895 8894.919
#13           vacation   5000  5875.943 4897.718
#14            wedding   8750 10786.979 7360.597

loan_paid %>% group_by(.,purpose) %>% summarise(.,median=median(loan_amnt),average=mean(loan_amnt),stdev=sd(loan_amnt) )
# A tibble: 14 × 4
#              purpose median   average    stdev
#               <fctr>  <dbl>     <dbl>    <dbl>
#1                 car   5900  6684.895 4006.420
#2         credit_card  10000 11396.968 6789.301
#3  debt_consolidation  10800 12319.847 7231.932
#4         educational   5000  6592.193 4920.505
#5    home_improvement   9600 11224.862 7927.675
#6               house  10000 12775.081 8014.092
#7      major_purchase   6000  7950.272 5991.417
#8             medical   6000  8040.609 5855.112
#9              moving   5000  6688.120 5791.317
#10              other   6000  7868.170 6174.891
#11   renewable_energy   6000  8564.759 6823.413
#12     small_business  10000 12904.672 8376.415
#13           vacation   4350  5342.158 4231.907
#14            wedding   8000  9636.596 5919.484


ggplot(loan_final,aes(purpose,loan_amnt))+geom_boxplot(aes(fill=purpose))+theme(axis.text.x=element_blank())+labs(list(title="Loan amount by Purpose - All",x="Purpose",y="Amount"))+scale_fill_discrete(name="Purpose")
ggplot(loan_defaulted,aes(purpose,loan_amnt))+geom_boxplot(aes(fill=purpose))+theme(axis.text.x=element_blank())+labs(list(title="Loan amount by Purpose - Defaulted",x="Purpose",y="Amount"))+scale_fill_discrete(name="Purpose")
ggplot(loan_paid,aes(purpose,loan_amnt))+geom_boxplot(aes(fill=purpose))+theme(axis.text.x=element_blank())+labs(list(title="Loan amount by Purpose - Paid",x="Purpose",y="Amount"))+scale_fill_discrete(name="Purpose")

#Correlations
##WITH LOAN AMT.
cor(loan_final$loan_amnt,loan_final$int_rate)#cor=0.3094153,since correlation is positive loan is increasing with interest rate.
cor(loan_defaulted$loan_amnt,loan_defaulted$int_rate)#0.3512155 FOR DEFAULTERS INTEREST RATE INCREASING with loan amnt
cor(loan_paid$loan_amnt,loan_paid$int_rate)#0.285371 FOR Paid INTEREST RATE decreasing with loan amnt

###loan amount is  more for lower annual income,Defaulters are in the range below 1 lac monthly income
ggplot(loan_defaulted,aes(x=annual_inc/12,y=loan_amnt))+geom_smooth()+labs(title= "Monthly Income - Defaulted", x="Monthy Income",y="Loan Amnt")
ggplot(loan_paid,aes(x=annual_inc/12,y=loan_amnt))+geom_smooth()+labs(title= "Monthly Income - Paid", x="Monthy Income",y="Loan Amnt")
ggplot(loan_final,aes(x=annual_inc/12,y=loan_amnt))+geom_smooth(aes(fill=loan_status))+labs(title= "Monthly Income - All", x="Monthy Income",y="Loan Amnt")+scale_fill_discrete(name="Loan Status")

#Correlation between DTI and int_rate
cor(loan_final$dti,loan_final$int_rate) #0.1111617
cor(loan_defaulted$dti,loan_defaulted$int_rate) #0.0406072
cor(loan_paid$dti,loan_paid$int_rate) #0.1146441

##dti is more for less monthly income,it decreases with increasing annual income ,finally goes to medium level of dti with max annual income 
ggplot(loan_final,aes(x=annual_inc/12,y=dti,fill=loan_status))+geom_smooth()+labs(title="Monthly Income Vs DTI - All", x="Monthly Income",y="DTI")+scale_fill_discrete(name="Loan Status")
ggplot(loan_defaulted,aes(x=annual_inc/12,y=dti,fill=loan_status))+geom_smooth()+labs(title="Monthly Income Vs DTI - Defaulted", x="Monthly Income",y="DTI")+scale_fill_discrete(name="Loan Defaulters")
ggplot(loan_paid,aes(x=annual_inc/12,y=dti,fill=loan_status))+geom_smooth()+labs(title="Monthly Income Vs DTI - Paid", x="Monthly Income",y="DTI")+scale_fill_discrete(name="Loan Paid")

##cor matrix of numeric variables
loan_defaultedcor<-loan_defaulted[,c("loan_amnt","int_rate","dti","annual_inc","sub_grade_n","total_acc" )]
head(loan_defaultedcor)
#   loan_amnt int_rate   dti annual_inc sub_grade_n total_acc
#2       2500    15.27  1.00      30000          14         4
#9       5600    21.28  5.55      40000          27        13
#10      5375    12.69 18.08      15000          10         3
#13      9000    13.49 10.08      30000          11         9
#15     10000    10.65  7.06     100000           7        29
#22     21000    12.42 13.22     105000           9        38
cormat<-round(cor(loan_defaultedcor),2)
head(cormat)
#            loan_amnt int_rate   dti annual_inc sub_grade_n total_acc
#loan_amnt        1.00     0.35  0.06       0.35        0.34      0.28
#int_rate         0.35     1.00  0.04       0.13        0.96     -0.01
#dti              0.06     0.04  1.00      -0.09        0.03      0.27
#annual_inc       0.35     0.13 -0.09       1.00        0.14      0.29
#sub_grade_n      0.34     0.96  0.03       0.14        1.00      0.00
#total_acc        0.28    -0.01  0.27       0.29        0.00      1.00

get_lower_tri<-function(cormat){
  cormat[(upper.tri(cormat))]<-NA
  return(cormat)
}
get_upper_tri<-function(cormat){
  cormat[lower.tri(cormat)]<-NA
  return(cormat)
}
upper_tri<-get_upper_tri(cormat)
upper_tri
#            loan_amnt int_rate  dti annual_inc sub_grade_n total_acc
#loan_amnt           1     0.35 0.06       0.35        0.34      0.28
#int_rate           NA     1.00 0.04       0.13        0.96     -0.01
#dti                NA       NA 1.00      -0.09        0.03      0.27
#annual_inc         NA       NA   NA       1.00        0.14      0.29
#sub_grade_n        NA       NA   NA         NA        1.00      0.00
#total_acc          NA       NA   NA         NA          NA      1.00
melted_cormat<-melt(upper_tri,na.rm=TRUE)
head(melted_cormat)
#        Var1      Var2 value
#1  loan_amnt loan_amnt  1.00
#7  loan_amnt  int_rate  0.35
#8   int_rate  int_rate  1.00
#13 loan_amnt       dti  0.06
#14  int_rate       dti  0.04
#15       dti       dti  1.00
ggplot(data=melted_cormat,aes(Var2,Var1,fill=value))+geom_tile(color="white")+scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0,limit=c(-1,1),space="Lab",name="Correlation")+theme_minimal()+theme(axis.text.x=element_text(angle=45,vjust=1,size=12,hjust=1))+coord_fixed()

# Correlation of variables in default
# correlation between Annual Income and Loan Amount
cor(loan_defaulted$annual_inc,loan_defaulted$loan_amnt) #0.3523593

#Debt to Income ratio and open credit lines are correlated
cor(loan_defaulted$dti,loan_defaulted$open_acc)#0.2992298

getNumericColumns<-function(t){
  tn = sapply(t,function(x){is.numeric(x)})
  return(names(tn)[which(tn)])
}

library(corrplot)
corrplot(cor(loan_final[getNumericColumns(loan_final)],use="na.or.complete"), title = "All Numeric Columns - Total Loan")
corrplot(cor(loan_paid[getNumericColumns(loan_paid)],use="na.or.complete"), title = "All Numeric Columns - Paid")
corrplot(cor(loan_defaulted[getNumericColumns(loan_defaulted)],use="na.or.complete"), title = "All Numeric Columns - Defaulted")


#####BIVARIATE ANALYSIS END#####
##########ANALYSIS END##########


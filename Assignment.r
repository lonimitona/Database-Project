# -----------------------------------------------------------------------------
# <Assignment.r>
# <Database Project>/<Scientific Computing and Healthcare>
# <R Script>/<15/02/2022>
# <Student ID>/<2149508>
# <Rcode/RStudio Project>
# <Copyright/2149508>
# -----------------------------------------------------------------------------
#Make sure the RPostgreSQL package is available.
library("RPostgreSQL")

#Access Tidyverse library
library(tidyverse)

library(crayon)     # Used for coloured console text.

# For 'qq()' which allows substitution of vars in strings.
library(GetoptLong)

library(lubridate)

#Specify what driver is needed to connect to the database.
drv = dbDriver("PostgreSQL")

#Connect to the database for the assignment.
con <- dbConnect(drv, dbname = "gp_practice_data", 
                 host = "localhost", port = 5432,
                 user = "postgres", password = rstudioapi::askForPassword())

# Confirm connection to database by displaying the available tables.
cat(green('The following tables are available:\n'))
print(dbListTables(con))
cat('\n')

# Get the list of columns in the database tables by creating the get_columns function.
get_columns <- function(table) {
    columns <- dbGetQuery(con,
                        qq('select column_name as name, ordinal_position as position,
           data_type as type, character_maximum_length as length,
           numeric_precision as precision
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'@{table}\';'))
  cat('\nThe table', table, 'has the following structure:\n', sep=' ')
  print(columns)
  return(columns)
}

#We can now get the columns from tables using the get_columns function
address_columns <- get_columns('address')

qof_indicator_columns <- get_columns('qof_indicator')

qof_achievement_columns <- get_columns('qof_achievement')

gp_data_up_to_2015_columns <- get_columns('gp_data_up_to_2015')


#Questions - PART 1
#user to select practice
chosen_practiceid <- readline('Select Practice ID: ') 

#to check that practice id entered by user follows the uniform pattern
is_practiceid_valid <- str_detect(chosen_practiceid,'^W[0-9]{5}$')
is_practiceid_valid

#Create function for practiceid entry
input_practiceid <- function() {
  is_practiceid_valid <- FALSE
  while(is_practiceid_valid == FALSE){
    chosen_practiceid <- readline('Select Practice ID: ')
    is_practiceid_valid <- str_detect(chosen_practiceid,'^W[0-9]{5}$')
    if (is_practiceid_valid==TRUE){
      print('Practice ID entered is correct')
    }else{
      cat(red('\nThis is not a Practice ID.'))
      cat(yellow('\nEnter Practice ID starting with W:\n'))
    } 
  }
  return(chosen_practiceid)
}
chosen_practiceid <- input_practiceid()

chosen_practiceid <- 'W92041'

#Q1(a) check if practice has medication information available
med_info <- dbGetQuery(con, qq('
    select * from gp_data_up_to_2015
    where practiceid = \'@{chosen_practiceid}\''))
med_info



#Q1(b) check if practice has QOF Data available
qof_info <- dbGetQuery(con, qq('
    select * from qof_achievement
    where orgcode = \'@{chosen_practiceid}\''))
qof_info


#Q1(ci) Calculate no of patients at Practice
no_of_patients <- qof_info %>% rename(practiceid=orgcode, no_of_patients=field4) %>%
    summarise(total=max(no_of_patients))
no_of_patients

#Create function to get no of patients
get_no_of_patients <- function(chosen_practiceid) {
  qof_info <- dbGetQuery(con, qq('
    select * from qof_achievement
    where orgcode = \'@{chosen_practiceid}\''))
  no_of_patients <- qof_info %>% rename(practiceid=orgcode, no_of_patients=field4) %>%
    summarise(total=max(no_of_patients))
  return(no_of_patients)
}



#Q1(cii) Calculate average amount spent per month on medication at the practice
# average amount spent on medication = Total costs / no of months
    
#Use ymd() from lubridate package to sort the date column
med_cost <- med_info %>% select(period, actcost) %>% rename(month=period) %>% 
    mutate(month=ym(month))

#use floor_date() function from lubridate to group month
med_cpp <- med_cost %>% group_by(month = lubridate::floor_date(month, "month")) %>%
    summarize(total_cost_meds = sum(actcost))

#Calculate the total cost of medication
sum_of_meds <- med_cpp %>% summarise(sum(total_cost_meds)) 

#To get number of months
no_of_months <- nrow(med_cpp)

#Calculate total average cost at practice
avg_cost <- sum_of_meds %/% no_of_months

#To save value as scalar
avg_cost <- avg_cost[[1]]
avg_cost

no_of_months <- no_of_months[[1]]

#Calculate average cost per month
avg_cost_meds_per_month <- avg_cost / no_of_months

avg_cost_meds_per_month <- round(avg_cost_meds_per_month, 1)

#Function
avg_spend_per_month <- function(chosen_practiceid){
  med_info <- dbGetQuery(con, qq('
    select * from gp_data_up_to_2015
    where practiceid = \'@{chosen_practiceid}\''))
  
  med_cost <- med_info %>% select(period, actcost) %>% rename(month=period) %>% 
    mutate(month=ym(month))
  
  med_cpp <- med_cost %>% group_by(month = lubridate::floor_date(month, "month")) %>%
    summarize(total_cost_meds = sum(actcost))
  
  sum_of_meds <- med_cpp %>% summarise(sum(total_cost_meds)) 
  
  no_of_months <- nrow(med_cpp)
  
  avg_cost <- sum_of_meds %/% no_of_months
  
  avg_cost <- avg_cost[[1]]
  
  no_of_months <- no_of_months[[1]]
  
  avg_cost_meds_per_month <- avg_cost / no_of_months
  
  avg_cost_meds_per_month <- round(avg_cost_meds_per_month, 1)
  return(avg_cost_meds_per_month)
}
avg_spend_per_month('W92041')

# Q1(ciii) Calculate cost of medication per patient compared with practices  
#in same postcode
#cost of medication per patient (cmpp) = sum(nic) / no_of_patients
#First Calculate for chosen practice
wal_qof_info <- dbGetQuery(con, "
    select * from qof_achievement
    ")

pop_each_practice <- wal_qof_info %>% rename(practiceid=orgcode, no_of_patients=field4) %>%
  group_by(practiceid) %>% summarise(total_pop=max(no_of_patients))

meds <- dbGetQuery(con, "
    select practiceid, sum(nic) as total_costs
    from gp_data_up_to_2015
    group by practiceid;
    ")

meds_pop <- meds %>% inner_join(pop_each_practice, by=c('practiceid'))

postcode <- dbGetQuery(con, "
    select practiceid, postcode
    from address
    ")

meds_postcode <- meds_pop %>% inner_join(postcode, by=c('practiceid'))
amt_meds_per_patient <- meds_postcode %>% 
  mutate(meds_per_patient=total_costs/total_pop)
#
one_postcode<- filter(meds_postcode, practiceid == chosen_practiceid)
#Bring out value of postcode from the column
pc_code <- one_postcode$postcode[1]
#Select first 2 letters of postcode
digits <- substring(pc_code,1,2)
#Select just first 2 letters of postcode
string_pattern <- str_interp('^${digits}')
#Select practices sharing same postcode with user's chosen practice
chosen_postcode <- amt_meds_per_patient %>% filter(str_detect(postcode, string_pattern)) 
#Visualization showing cost of medication per patient compared to other
#practices within same postcode area
barchart <- ggplot(data = chosen_postcode) + geom_bar(mapping = aes(x = practiceid, 
                                                                    fill = chosen_practiceid))
#Create a function to get postcode like practiceid chosen
get_chosen_postcode <- function(){
 #get medication data
  meds <- dbGetQuery(con, "
    select practiceid, sum(nic) as total_costs
    from gp_data_up_to_2015
    group by practiceid;
    ")
  
  #merge medication table and population table
  meds_pop <- meds %>% inner_join(pop_each_practice, by=c('practiceid'))
  #get postcode data
  postcode <- dbGetQuery(con, "
    select practiceid, postcode
    from address
    ")
  #merge postcode table with medication and population table
  meds_postcode <- postcode %>% inner_join(meds_pop, by=c('practiceid'))
  #Calculate amount spent on medication per patient
  amt_meds_per_patient <- meds_postcode %>% 
    mutate(meds_per_patient=total_costs/total_pop)
  #
  one_postcode<- filter(meds_postcode, practiceid == chosen_practiceid)
  #Bring out value of postcode from the column
  pc_code <- one_postcode$postcode[1]
  #Select first 2 letters of postcode
  digits <- substring(pc_code,1,2)
  #Select just first 2 letters of postcode
  string_pattern <- str_interp('^${digits}')
  #Select practices sharing same postcode with user's chosen practice
  chosen_postcode <- amt_meds_per_patient %>% filter(str_detect(postcode, string_pattern)) 
  #Visualization showing cost of medication per patient compared to other
  #practices within same postcode area
  barchart <- ggplot(data = chosen_postcode) + geom_bar(mapping = aes(x = practiceid, 
   fill = chosen_practiceid))
}

get_chosen_postcode(chosen_practiceid)

#Visualization showing cost of medication per patient compared to other
#practices within same postcode area
ggplot(data = chosen_postcode) +
  geom_bar(mapping = aes(x = practiceid, count = meds_per_patient, fill = chosen_practiceid))





#1c(iv) rate of Diabetes = no of patients with Diabetes at practice / 
#total no of patients at the practice = nO_of_patients

#Get no of patients with Diabetes at practice
patients_with_diabetes <- qof_info %>% select(orgcode, indicator, numerator) %>% 
    filter(str_detect(indicator,'^DM')) %>% 
     summarise(patients_with_diabetes=sum(numerator))

#Calculate rate of Diabetes at practice
rate_of_diabetes <- patients_with_diabetes / no_of_patients

#Multiply by 100 to get percentage
rate_of_diabetes <- rate_of_diabetes * 100

rate_diabetes_practice <- round(rate_of_diabetes, 1) #round to 1 decimal place

#Report rate of diabetes at Practice

#Create function to get rate of diabetes at practice
get_rate_of_diabetes <- function(chosen_practiceid){
  qof_info <- dbGetQuery(con, qq('
    select * from qof_achievement
    where orgcode = \'@{chosen_practiceid}\''))
  patients_with_diabetes <- qof_info %>% select(orgcode, indicator, numerator) %>% 
    filter(str_detect(indicator,'^DM')) %>% 
    summarise(patients_with_diabetes=sum(numerator))
  #Calculate rate of Diabetes at practice
  rate_of_diabetes <- patients_with_diabetes / no_of_patients
  #Multiply by 100 to get percentage
  rate_of_diabetes <- rate_of_diabetes * 100
  #round to 1 decimal place
  rate_diabetes_practice <- round(rate_of_diabetes, 1) 
  return(rate_diabetes_practice)
}
get_rate_of_diabetes(chosen_practiceid)

print(paste(rate_of_diabetes, ' of patients at Practice ', correct_practiceid, 'suffer from Diabetes '))
#37% of patients at W*** practice suffer from Diabetes.


#To get rate of Diabetes at Practice compared to other Practices in Wales
#Get population of practices in Wales
wal_qof_info <- dbGetQuery(con, "
    select * from qof_achievement
    ")

#Calculate patients with diabetes in Wales (numerator)
diabetes_each_practice <- wal_qof_info %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>% filter(str_detect(indicator,'^DM')) %>% 
  group_by(practiceid) %>% summarise(diabetes_patients=sum(numerator))

#Get total population of patients in each practice (denominator)
pop_each_practice <- wal_qof_info %>% rename(practiceid=orgcode, no_of_patients=field4) %>%
  group_by(practiceid) %>% summarise(total_pop=max(no_of_patients))

#Join Diabetes and total population tables
diabetes_pop <- diabetes_each_practice %>% full_join(pop_each_practice, 
  by=c('practiceid'))

#Calculate rate of Diabetes at each practice
rate_dm_practice <- diabetes_pop %>% group_by(practiceid) %>% 
  summarise(rate_diabetes = diabetes_patients / total_pop)

#Write code that compares chosen practice with other practices in Wales
wales_data <- filter(rate_dm_practice, practiceid=='WAL')

barplot(height = df$df.ctrl1, df$df.avg_treated)
?crayon

#Visualize rate of Diabetes at practice compared to others in Wales
ggplot(data = rate_dm_practice) +
  geom_bar(mapping = aes(x = practiceid, y = ..prop.., fill = practiceid))







#2(i) Compare rate of diabetes and rate of insulin prescription 
#at practice level
#bnfcode for insulin medications start with identical 6 digits
#rate of insulin prescription = total no of insulin prescriptions
#in the practice / total no of patients in the same practice

insulin_meds <- dbGetQuery(con, "
    select * from gp_data_up_to_2015
    where bnfcode like '060101%'
    ")
insulin_meds

#Calculate insulin prescriptions per practice (numerator)
ins_prsc_each_prac <- insulin_meds %>% select(practiceid, quantity, bnfname) %>% 
  rename(ins_meds=bnfname) %>% group_by(practiceid) %>% 
  summarise(insulin_prsc=sum(quantity))

#Calculate total number of prescriptions per practice (denominator)
total_drugs_per_practice <- dbGetQuery(con, "
    select practiceid, sum(quantity) as total_drugs_prescribed
    from gp_data_up_to_2015 
    group by practiceid
    ")

#Join insulin prescriptions and total number of prescriptions (inner join 
#excludes practices without insulin prescription)
ins_prsc <- ins_prsc_each_prac %>% inner_join(total_drugs_per_practice, 
    by=c('practiceid'))

#Calculate rate of Insulin prescription at each practice
rate_insulin <- ins_prsc %>% group_by(practiceid) %>% 
  summarise(rate_insulin = insulin_prsc / total_drugs_prescribed)

#Compare rate of Diabetes and rate of insulin prescription
#First join rate of Diabetes and rate of insulin prescription tables
diabetes_ins_rate <- rate_dm_practice %>% inner_join(rate_insulin, by=c('practiceid'))

#Visualize
plot(diabetes_ins_rate$rate_diabetes, diabetes_ins_rate_rate$rate_insulin, 
     main='Scatterplot of Diabetes and Insulin prescription',
     xlab='Rate of Diabetes', ylab='Rate of Insulin prescription')

#Test for significance using Pearson's correlation test
rel_dm_ins <- cor.test(diabetes_ins_rate$rate_diabetes, diabtes_ins_rate$rate_insulin)
rel_dm_ins

#The relationship between the rate of Diabetes and rate of insulin prescriptions
#was investigated using Pearson’s correlation. There was evidence 
#(p > 0.005) to suggest that there is no statistically significant relationship
#between rate of Diabetes and rate of insulin prescriptions.

#2(ii) Rate of Diabetes and rate of Metformin prescription
#rate of Metformin prescription = total no of metformin prescriptions
#in the practice / total no of other drug prescriptions in same practice
metformin_meds <- dbGetQuery(con, "
    select * from gp_data_up_to_2015
    where lower (bnfname) like 'metformin%' 
    ")

#Calculate metformin prescription each practice (numerator)
met_prsc_each_prac <- metformin_meds %>% select(practiceid, quantity, bnfname) %>% 
  rename(met_meds=bnfname) %>% group_by(practiceid) %>% 
  summarise(met_prsc=sum(quantity))

#Join metformin prescriptions and total number of prescriptions (inner join 
#excludes practices without insulin prescription)
met_prsc <- met_prsc_each_prac %>% inner_join(total_drugs_per_practice, 
                                              by=c('practiceid'))

#Calculate rate of metformin prescription at each practice
rate_metformin <- met_prsc %>% group_by(practiceid) %>% 
  summarise(rate_metformin = met_prsc / total_drugs_prescribed)

#Compare rate of Diabetes and rate of insulin prescription
#First join rate of Diabetes and rate of insulin prescription tables
diabetes_metformin_rate <- rate_dm_practice %>% inner_join(rate_metformin, 
  by=c('practiceid'))

#Visualize
plot(diabetes_metformin_rate$rate_diabetes, diabetes_metformin_rate$rate_metformin, 
     main='Scatterplot of Diabetes and Metformin prescription',
     xlab='Rate of Diabetes', ylab='Rate of Metformin prescription')

#Test for significance using Pearson's correlation test
rel_dm_met <- cor.test(diabetes_metformin_rate$rate_diabetes, 
  diabetes_metformin_rate$rate_metformin)
rel_dm_met
#The relationship between the rate of Diabetes and rate of Metformin prescriptions
#was investigated using Pearson’s correlation. There was evidence 
#(p < 0.005) to suggest that there is a statistically significant relationship
#between rate of Diabetes and rate of metformin prescriptions.
#The correlation co-efficient of 0.4032999 suggests that there is a strong positive 
#correlation between the rate of diabetes and rate of metformin prescription.
#

 
  


#PART 2

pop_of_wales <- dbGetQuery(con, "
 select * from qof_achievement
 where orgcode like 'WAL%' 
 ")

af_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^AF')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Atrial Fibrillation')
  
asthma_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^AST')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Asthma')

cancer_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^CAN')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Cancer')

chd_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^CHD')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Coronary Heart Disease')

copd_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^COPD')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Chronic Obstructive Pulmonary Disease')
    
dementia_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^DEM')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Dementia')

depression_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^DEP')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Depression')

diabetes_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^DM')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Diabetes Mellitus')

epilepsy_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^EP')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Epilepsy')

hf_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^HF')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Heart Failure')

hypertension_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^HYP')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Hypertension')

ld_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^LD')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Learning Disabilities')

mh_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^MH')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Mental Health')
 
obesity_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^OB')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Obesity')

ost_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^OST')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Osteoporosis')

pad_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^PAD')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Peripheral Arterial Disease')

ra_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^RA')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Rheumatoid Arthritis')

smoking_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^SMO')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Smoking')

stia_patients <- pop_of_wales %>% select(orgcode, indicator, numerator) %>% 
  rename(practiceid=orgcode) %>%  filter(str_detect(indicator,'^STIA')) %>%
  group_by(practiceid) %>% summarise(patients=sum(numerator)) %>% 
  add_column(area='Stroke and Transient Ischaemic Attacks')


#Combining the dataframes
disease_pop <- rbind(af_patients, asthma_patients, cancer_patients, chd_patients, copd_patients, 
      dementia_patients, depression_patients, diabetes_patients, epilepsy_patients, hf_patients, 
      hypertension_patients, ld_patients, mh_patients, obesity_patients, ost_patients, 
      pad_patients, ra_patients, smoking_patients, stia_patients)

# Close the connection and unload the drivers.
dbDisconnect(con)
dbUnloadDriver(drv)

cat('\nEnd of analysis. Thank you for using 2149508\'s code.\n',
    'For support, please contact 2149508 on\n',
    '2149508@swansea.ac.uk.')

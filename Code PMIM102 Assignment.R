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

library(tidyverse)  #Access Tidyverse library

library(crayon)     #For coloured console text.

# For 'qq()' which allows substitution of vars in strings.
library(GetoptLong)

library(lubridate)  #To manipulate date and time data

#Specify what driver is needed to connect to the database.
drv = dbDriver("PostgreSQL")

#Connect to the database for the assignment.
con <- dbConnect(drv, dbname = "gp_practice_data", 
                 host = "localhost", port = 5432,
                 user = "postgres", password = rstudioapi::askForPassword())

#Confirm connection to database by displaying the available tables.
cat(green('The following tables are available:\n'))
print(dbListTables(con))
cat('\n')

#Get the list of columns in the database tables by creating the get_columns function.
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

qof_achievement_columns <- get_columns('qof_achievement')

gp_data_up_to_2015_columns <- get_columns('gp_data_up_to_2015')

#PART 1 - Question 1

#Create a function to validate user's chosen practice
input_practiceid <- function() {
  is_practiceid_valid <- FALSE
  while(is_practiceid_valid == FALSE){
    #user to select practice
    chosen_practiceid <- readline('Select Practice ID: ')
    #Check that practice id entered by user follows the uniform pattern
    is_practiceid_valid <- str_detect(chosen_practiceid,'^W[0-9]{5}$')
    if (is_practiceid_valid ==TRUE){
      cat(green('Practice ID entered is correct\n'))
    }else{
      cat(red('\nThis is not a valid Practice ID.'))
      cat(yellow('\nEnter Practice ID starting with W:\n'))
    } 
  }
  return(chosen_practiceid)
}


#Create function to check if practice has medication info
has_medinfo <- function(chosen_practiceid) {
  med_info <- dbGetQuery(con, qq("
    select bnfcode, bnfname, practiceid
    from gp_data_up_to_2015
    where practiceid = \'@{chosen_practiceid}\'"))
  #
  has_medicinfo <- FALSE
  if (count(med_info) > 0){
    has_medicinfo <- TRUE
  }
  return(has_medicinfo)
}


#Create function to check if practice has qof info
has_qofinfo <- function(chosen_practiceid) {
 qof_info <- dbGetQuery(con, qq("
    select * 
    from qof_achievement
    where orgcode = \'@{chosen_practiceid}\'"))
  has_qofinformation <- FALSE
  if (count(qof_info) > 0){
    has_qofinformation <- TRUE
  }
  return(has_qofinformation)
}


#Create function to get no of patients
get_no_of_patients <- function(chosen_practiceid) {
  qof_info <- dbGetQuery(con, qq('
    select * 
    from qof_achievement
    where orgcode = \'@{chosen_practiceid}\''))
  #Calculate no of patients at Practice
  no_of_patients <- qof_info %>% rename(practiceid=orgcode, 
    no_of_patients=field4) %>% summarise(total=max(no_of_patients))
  return(no_of_patients)
}


#Create function to get average cost per month
get_avg_spend_per_month <- function(chosen_practiceid){
  med_info <- dbGetQuery(con, qq('
    select * 
    from gp_data_up_to_2015
    where practiceid = \'@{chosen_practiceid}\''))
  #Use ymd() from lubridate package to sort the date column
  med_cost <- med_info %>% select(period, actcost) %>% rename(month=period) %>% 
    mutate(month=ym(month))
  #use floor_date() function from lubridate to group month
  med_cpm <- med_cost %>% group_by(month = lubridate::floor_date(month, "month")) %>%
    summarize(total_cost_meds = sum(actcost))
  #Calculate the total cost of medication
  sum_of_meds <- med_cpm %>% summarise(sum(total_cost_meds)) 
  #To get number of months
  no_of_months <- nrow(med_cpm)
  #Calculate total average cost at practice
  avg_cost <- sum_of_meds %/% no_of_months
  #To save value as scalar
  avg_cost <- avg_cost[[1]]
  no_of_months <- no_of_months[[1]]
  #Calculate average cost per month
  avg_cost_meds_per_month <- avg_cost / no_of_months
  #round to 1 decimal place
  avg_cost_meds_per_month <- round(avg_cost_meds_per_month, 1)
  return(avg_cost_meds_per_month)
}

#Create a function to get postcode like practiceid chosen
get_chosen_postcode <- function(chosen_practiceid){
  #Get population of all patients
  wal_qof_info <- dbGetQuery(con, "
    select * 
    from qof_achievement
    ")
  #Get total population of patients in each practice 
  pop_each_practice <- wal_qof_info %>% rename(practiceid=orgcode, no_of_patients=field4) %>%
    group_by(practiceid) %>% summarise(total_pop=max(no_of_patients))
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
  one_postcode<- filter(meds_postcode, practiceid==chosen_practiceid)
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
  print(chosen_postcode)
}
get_chosen_postcode()

#Create function to report rate of diabetes at practice
get_rate_of_diabetes <- function(chosen_practiceid){
  qof_info <- dbGetQuery(con, qq('
    select * 
    from qof_achievement
    where orgcode = \'@{chosen_practiceid}\''))
  patients_with_diabetes <- qof_info %>% select(orgcode, indicator, numerator) %>% 
    filter(str_detect(indicator,'^DM')) %>% 
    summarise(patients_with_diabetes=sum(numerator))
  no_of_patients <- qof_info %>% rename(practiceid=orgcode, no_of_patients=field4) %>%
    summarise(total=max(no_of_patients))
  #Calculate rate of Diabetes at practice
  rate_of_diabetes <- patients_with_diabetes / no_of_patients
  #Multiply by 100 to get percentage
  rate_of_diabetes <- rate_of_diabetes * 100
  #round to 1 decimal place
  rate_diabetes_practice <- round(rate_of_diabetes, 1) 
  return(rate_diabetes_practice)
}



#Putting it all together
Question_1 <- function() {
  #accept user input
  correct_practiceid <- input_practiceid()
  #Check if the practice id has medication info available
  if (has_medinfo(correct_practiceid)) {
    cat(magenta(correct_practiceid, ' has medication information available\n'))
  }
  else{
    cat(green(correct_practiceid, ' does not have medication information available\n'))
  }
  #Check if the practice id has QOF data available
  if (has_qofinfo(correct_practiceid)) {
    cat(magenta(correct_practiceid, ' has QOF data available\n'))
    cat('\n')
  }
  else{
    cat(green(correct_practiceid, ' does not have QOF data available\n'))
  }  
  #If practice has both medication and QOF data, then display information for practice
  if (has_medinfo(correct_practiceid) & has_qofinfo(correct_practiceid)) {
    no_of_patients <- get_no_of_patients(correct_practiceid)
    cat(green('There are ', no_of_patients, ' patients at practice ', correct_practiceid, 
        '\n')) 
    cat('\n')
    avg_spend_per_month <- get_avg_spend_per_month(correct_practiceid)
    cat(blue('The average spend on medication per month at Practice ', 
      correct_practiceid, 'is ', avg_spend_per_month, 'pounds.\n'))
    cat('\n')
    get_chosen_postcode(correct_practiceid)
    cat('\n')
    cat(yellow('The table above shows the amount spent on medications per patient ', 
                'compared to other practices in same post code with ', 
                correct_practiceid, '\n' )) 
    cat('\n')
    rate_of_diabetes <- get_rate_of_diabetes(correct_practiceid)
    cat(green(rate_of_diabetes, '% of patients at Practice ', correct_practiceid, 
                'suffer from Diabetes.\n'))
    cat('\n')
  }
}
Question_1()


#Question 2
#Create a function to compare rate of diabetes and rate of insulin prescription 
#in Wales
get_dm_ins_rel <- function() {
  insulin_meds <- dbGetQuery(con, "
    select * 
    from gp_data_up_to_2015
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
  #Join insulin prescriptions and total number of prescriptions 
  ins_prsc <- ins_prsc_each_prac %>% inner_join(total_drugs_per_practice, 
    by=c('practiceid'))
  #Calculate rate of Insulin prescription at each practice
  rate_insulin <- ins_prsc %>% group_by(practiceid) %>% 
    summarise(rate_insulin = insulin_prsc / total_drugs_prescribed)
  #Compare rate of Diabetes and rate of insulin prescription
  #Get whole population 
  wal_qof_info <- dbGetQuery(con, "
    select * 
    from qof_achievement
    ")
  #Patients with diabetes in each practice
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
  #First join rate of Diabetes and rate of insulin prescription tables
  diabetes_ins_rate <- rate_dm_practice %>% inner_join(rate_insulin, by=c('practiceid'))
  #Visualize
  cat(yellow('See scatterplot showing relationship between rate of diabetes',
  'and rate of insulin prescriptions', '\n', 'in the console -> ', '\n'))
  cat('\n')
  plot(diabetes_ins_rate$rate_diabetes, diabetes_ins_rate$rate_insulin, 
       main='Scatterplot of Diabetes and Insulin prescription',
       xlab='Rate of Diabetes', ylab='Rate of Insulin prescription')
  #Test for significance using Pearson's correlation test
  rel_dm_ins <- cor.test(diabetes_ins_rate$rate_diabetes, diabetes_ins_rate$rate_insulin)
  #Interpret result
  cat(green('The relationship between the rate of Diabetes and rate of insulin prescriptions', '\n', 
       ' was investigated using Pearson’s correlation.', '\n',
       'There was evidence (p > 0.005) to suggest that there is no statistically significant', '\n',
       ' relationship between rate of Diabetes and rate of insulin prescriptions.\n'))
  cat('\n')
  print(rel_dm_ins)
  return(rel_dm_ins)
}
get_dm_ins_rel()

#Create a function to compare rate of diabetes and rate of metformin prescription 
#in Wales
get_dm_met_rel <- function(){
  metformin_meds <- dbGetQuery(con, "
    select * 
    from gp_data_up_to_2015
    where lower (bnfname) like 'metformin%' 
    ")
  #Calculate metformin prescription each practice (numerator)
  met_prsc_each_prac <- metformin_meds %>% select(practiceid, quantity, bnfname) %>% 
    rename(met_meds=bnfname) %>% group_by(practiceid) %>% 
    summarise(met_prsc=sum(quantity))
  #Get whole population 
  wal_qof_info <- dbGetQuery(con, "
    select * 
    from qof_achievement
    ")
  #Get total population of patients in each practice 
  pop_each_practice <- wal_qof_info %>% rename(practiceid=orgcode, no_of_patients=field4) %>%
    group_by(practiceid) %>% summarise(total_pop=max(no_of_patients))
  #Diabetes at ech prectice
  diabetes_each_practice <- wal_qof_info %>% select(orgcode, indicator, numerator) %>% 
    rename(practiceid=orgcode) %>% filter(str_detect(indicator,'^DM')) %>% 
    group_by(practiceid) %>% summarise(diabetes_patients=sum(numerator))
  #Diabetes population table
  diabetes_pop <- diabetes_each_practice %>% full_join(pop_each_practice, 
    by=c('practiceid'))
  #Calculate total number of prescriptions per practice (denominator)
  total_drugs_per_practice <- dbGetQuery(con, "
    select practiceid, sum(quantity) as total_drugs_prescribed
    from gp_data_up_to_2015 
    group by practiceid
    ")
  #Join metformin prescriptions and total number of prescriptions (inner join 
  #excludes practices without insulin prescription)
  met_prsc <- met_prsc_each_prac %>% inner_join(total_drugs_per_practice, 
    by=c('practiceid'))
  #Calculate rate of metformin prescription at each practice
  rate_metformin <- met_prsc %>% group_by(practiceid) %>% 
    summarise(rate_metformin = met_prsc / total_drugs_prescribed)
  #Compare rate of Diabetes and rate of insulin prescription
  rate_dm_practice <- diabetes_pop %>% group_by(practiceid) %>% 
    summarise(rate_diabetes = diabetes_patients / total_pop)
  #First join rate of Diabetes and rate of insulin prescription tables
  diabetes_metformin_rate <- rate_dm_practice %>% inner_join(rate_metformin, 
     by=c('practiceid'))
  #Visualize
  cat(yellow('See scatterplot showing relationship between rate of diabetes',
              'and rate of metformin prescriptions in the console -> ', '\n'))
  cat('\n')
  plot(diabetes_metformin_rate$rate_diabetes, diabetes_metformin_rate$rate_metformin, 
       main='Scatterplot of Diabetes and Metformin prescription',
       xlab='Rate of Diabetes', ylab='Rate of Metformin prescription')
  #Test for significance using Pearson's correlation test
  rel_dm_met <- cor.test(diabetes_metformin_rate$rate_diabetes, 
       diabetes_metformin_rate$rate_metformin)
  #Interpret result
  cat(green('The relationship between the rate of Diabetes and rate of Metformin prescriptions', '\n',
    'was investigated using Pearson’s correlation.', '\n',
    'There was evidence (p < 0.005) to suggest that there is a statistically significant relationship', '\n',
    'between rate of Diabetes and rate of metformin prescriptions.', '\n',
    'The correlation co-efficient of 0.4032999 suggests that there is a strong positive\', \n',
    'correlation between the rate of diabetes and rate of metformin prescription. \n'))
  cat('\n')
  print(rel_dm_met)
  return(rel_dm_met)
}
get_dm_met_rel()

# Close the connection and unload the drivers.
dbDisconnect(con)
dbUnloadDriver(drv)

cat(green('\nEnd of analysis. Thank you for using 2149508\'s code.\n',
    'For more information, please contact 2149508 on\n',
    '2149508@swansea.ac.uk.'))
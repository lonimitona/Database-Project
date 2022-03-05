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

chosen_practice_address <- dbGetQuery(con, qq('
    select * from address
    where practiceid = \'@{chosen_practiceid}\''))
chosen_practice_address

#to check that practice id entered by user follows the uniform pattern
practiceid_entered <- str_detect(chosen_practiceid,'^W[0-9]{5}$')
practiceid_entered

#Create function for practiceid entry
input_practiceid <- function() {
  practiceid_entered <- FALSE
  while(practiceid_entered == FALSE){
    chosen_practiceid <- readline('Select Practice ID: ')
    practiceid_entered <- str_detect(chosen_practiceid,'^W[0-9]{5}$')
    if (practiceid_entered==TRUE){
      print(chosen_practice_address)
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
    summarise(max=max(no_of_patients))
no_of_patients

#Create function to get no of patients
get_no_of_patients <- function(chosen_practiceid) {
  qof_info <- dbGetQuery(con, qq('
    select * from qof_achievement
    where orgcode = \'@{chosen_practiceid}\''))
  no_of_patients <- qof_info %>% rename(practiceid=orgcode, no_of_patients=field4) %>%
    summarise(max=max(no_of_patients))
  return(no_of_patients)
}



#Q1(cii) Calculate average amount spent per month on medication at the practice
# average amount spent on medication = sum (actcost) / no_of_patients
    
#Use ymd() from lubridate package to sort the date column
med_cost <- med_info %>% select(period, actcost) %>% rename(month=period) %>% 
    mutate(month=ym(month))

#use floor_date() function from lubridate to group month
med_cpp <- med_cost %>% group_by(month = lubridate::floor_date(month, "month")) %>%
    summarize(total_cost_meds = sum(actcost))

#Calculate the average cost
sum_of_meds <- med_cpp %>% summarise(sum(total_cost_meds)) 

no_of_months <- nrow(med_cpp)

avg_cost <- sum_of_meds %/% no_of_months

avg_cost <- avg_cost[[1]]
avg_cost

no_of_months <- no_of_months[[1]]

avg_cost_meds_per_month <- avg_cost / no_of_months

avg_spend_per_month <- function(practiceid){
  
}

# Q1(ciii) Calculate cost of medication per patient compared with practices  
#in same postcode
#cost of medication per patient = sum(nic) / no_of_patients
postcode <- - dbGetQuery(con, "
    select * from address
    where postcode like 'SA%'
    ")
postcode_sa

med_info <- dbGetQuery(con, qq('
    select * from gp_data_up_to_2015
    where practiceid = \'@{postcode_sa}\''))
med_info

med_info <- dbGetQuery(con, qq('
    select * from gp_data_up_to_2015
    where practiceid = \'@{chosen_practiceid}\''))
med_info

cmpp <- med_info %>% select(period, nic) %>% rename(year=period) %>%
    mutate(year=ym(year))

cmpp <- cmpp %>% group_by(year = lubridate::floor_date(year, "year")) %>%
    summarize(total_cost = sum(nic))

#Calculate the cost of medication per patient
cmpp <- cmpp %>% select(year, total_cost) %>% 
    mutate(cost_per_patient=total_cost %/% no_of_patients)
cmpp


#Compare with other practices in same postcode area
postcode <-  dbGetQuery(con, "
    select * from address
    ")

cmpp %>% select(year, cost_per_patient) 

meds_per_patient_same_postcode <- dbGetQuery(con, "
    select a.practiceid, street, cmpp.amt_per_patient, postcode 
    from address as a
    inner join cost_meds_per_patient as cmpp
    on a.practiceid = cmpp.practiceid
    where postcode like 'SA%'
    ")
meds_per_patient_same_postcode


#Visualization showing cost of medication per patient compared to other
#practices within same postcode area
mppsp <- meds_per_patient_same_postcode

ggplot(data = mppsp, mapping = aes(x = practiceid, y = amt_per_patient, 
                                   fill= practiceid == 'W92041')) +
  geom_col(width = 1) +
  geom_text(aes(label=amt_per_patient), vjust = 1.5) +
  coord_flip()

ggplot(data = mppsp, mapping = aes(x = practiceid, y = amt_per_patient, 
                                   fill= practiceid == 'W92041')) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label=amt_per_patient), vjust = -0.2) +
  coord_flip()

ggplot(data = mppsp, mapping = aes(x = practiceid, y = amt_per_patient)) +
  geom_col() +
  scale_fill_manual(values = c('W92041' = 'blue'))

rlang::last_error()
?geom_bar

#1c(iv) rate of Diabetes = no of patients with Diabetes at practice / 
#total no of patients at the practice


#Get the denominator
total_patient_pop_practice <- dbGetQuery(con, "
    select orgcode, sum(field4)as pop_per_practice
    from qof_achievement
    group by orgcode
    ")
total_patient_pop_practice


#Get the numerator
total_pop_diabetes_practice <- dbGetQuery(con, "
    select orgcode, sum(field4)as pop_with_diabetes
    from qof_achievement
    where indicator like 'DM%'
    group by orgcode
    ")


#Calculate the rate of diabetes per practice
rate_diabetes_practice <- dbGetQuery(con, "
    select tpdp.orgcode, 
       cast(tpdp.pop_with_diabetes as float) / 
       cast(tppp.pop_per_practice as float) as rate_diabetes
    from total_pop_diabetes_practice as tpdp
    inner join total_patient_pop_practice as tppp
    on tpdp.orgcode = tppp.orgcode;
    ")

#Round up result figures to 1 decimal place
rate_diabetes_per_patient <- dbGetQuery(con, "
    select orgcode, round(cast (rate_diabetes as numeric), 1) as 
       rate_diabetes
    from rate_diabetes_practice;
    ")

#Report rate of diabetes at Practice

#Visualize rate of Diabetes at Practice compared to other Practices


#2(i) Compare rate of diabetes and rate of insulin prescription 
#at practice level
#bnfcode for insulin medications start with identical 6 digits
#rate of insulin prescription = total no of insulin prescriptions
#in the practice / total no of other drug prescriptions in same practice

insulin_medications <- dbGetQuery(con, "
    select * from gp_data_up_to_2015
    where bnfcode like '060101%'
    ")
insulin_medications


#Calculate insulin prescriptions per practice
ins_prescription_per_practice <- dbGetQuery(con, "
    select practiceid, sum(quantity) as no_of_insulin_prescriptions
    from gp_data_up_to_2015 as gp
    where bnfcode like '060101%'
    group by practiceid
    ")


#Calculate total number of prescriptions per practice
total_drugs_per_practice <- dbGetQuery(con, "
    select practiceid, sum(quantity) as total_drugs_prescribed
    from gp_data_up_to_2015 
    group by practiceid
    ")


#Calculate rate of insulin prescription
rate_insulin_prescription <- dbGetQuery(con, "
    select ippp.practiceid, 
      cast(ippp.no_of_insulin_prescriptions as float) / 
      cast(tdpp.total_drugs_prescribed as float) as rate_insulin_prescription
    from ins_prescription_per_practice as ippp
    inner join total_drugs_per_practice as tdpp
    on ippp.practiceid = tdpp.practiceid
    ")

#Round up result figures to 1 decimal place
rate_of_insulin <- dbGetQuery(con, "
    select practiceid, round(cast (rate_insulin_prescription as numeric), 1) as 
       rate_of_insulin
    from rate_insulin_prescription
    ")

#Compare rate of Diabetes and rate of insulin prescription


#2(ii) Rate of Diabetes and rate of Metformin prescription
#rate of Metformin prescription = total no of metformin prescriptions
#in the practice / total no of other drug prescriptions in same practice
metformin_medications <- dbGetQuery(con, "
    select * from gp_data_up_to_2015
    where lower (bnfname) like 'metformin%' 
    ")


#Calculate metformin prescription per practice
met_prescription_per_practice <- dbGetQuery(con, "
    select practiceid, sum(quantity) as no_of_met_prescriptions
    from gp_data_up_to_2015 as gp
    where lower (bnfname) like 'metformin%'
    group by practiceid
    ")


#Calculate rate of metformin prescription
rate_metformin_prescription <- dbGetQuery(con, "
    select mppp.practiceid, 
       cast(mppp.no_of_met_prescriptions as float) / 
       cast(tdpp.total_drugs_prescribed as float) as rate_metformin_prescription
    from met_prescription_per_practice as mppp
    inner join total_drugs_per_practice as tdpp
    on mppp.practiceid = tdpp.practiceid
    ")

#Round up result figures to 1 decimal place
rate_of_metformin <- dbGetQuery(con, "
    select practiceid, round(cast (rate_metformin_prescription as numeric), 1) as 
       rate_of_metformin
    from rate_metformin_prescription
    ")

#Compare rate of Diabetes and rate of Metformin prescription



#PART 2




# Close the connection and unload the drivers.
dbDisconnect(con)
dbUnloadDriver(drv)

cat('\nEnd of analysis. Thank you for using 2149508\'s code.\n',
    'For support, please contact 2149508 on\n',
    '2149508@swansea.ac.uk.')

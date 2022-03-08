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

qof_achievement_columns <- get_columns('qof_achievement')

gp_data_up_to_2015_columns <- get_columns('gp_data_up_to_2015')

#PART 1 Questions

#Create a function to validate user's chosen practice
input_practiceid <- function() {
  is_practiceid_valid <- FALSE
  while(is_practiceid_valid == FALSE){
    #user to select practice
    chosen_practiceid <- readline('Select Practice ID: ')
    #Check that practice id entered by user follows the uniform pattern
    is_practiceid_valid <- str_detect(chosen_practiceid,'^W[0-9]{5}$')
    if (is_practiceid_valid ==TRUE){
      print('Practice ID entered is correct')
    }else{
      cat(red('\nThis is not a Practice ID.'))
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
    select * from qof_achievement
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
    select * from qof_achievement
    where orgcode = \'@{chosen_practiceid}\''))
  #Calculate no of patients at Practice
  no_of_patients <- qof_info %>% rename(practiceid=orgcode, 
    no_of_patients=field4) %>% summarise(total=max(no_of_patients))
  return(no_of_patients)
}


#Create function to get average cost per month
get_avg_spend_per_month <- function(chosen_practiceid){
  med_info <- dbGetQuery(con, qq('
    select * from gp_data_up_to_2015
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
  return(avg_cost_meds_per_month)
}

#Create a function to get postcode like practiceid chosen
chosen_postcode <- function(){
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
  ggplot(data = chosen_postcode) + geom_bar(mapping = aes(x = practiceid, 
     count = meds_per_patient, fill = chosen_practiceid))
}

#Putting it all together
Question_1 <- function() {
  #accept user input
  correct_practiceid <- input_practiceid()
  #Check if the practice id has medication info available
  if (has_medinfo(correct_practiceid)) {
    print(paste(correct_practiceid, ' has medication information available'))
  }
  else{
    print(paste(correct_practiceid, ' does not have medication information available'))
  }
  #Check if the practice id has QOF data available
  if (has_qofinfo(correct_practiceid)) {
    print(paste(correct_practiceid, ' has QOF data available'))
  }
  else{
    print(paste(correct_practiceid, ' does not have QOF data available'))
  }  
  #If practice has both medication and QOF data, then display information for practice
  if (has_medinfo(correct_practiceid) & has_qofinfo(correct_practiceid)) {
    no_of_patients <- get_no_of_patients(correct_practiceid)
    print(paste('Number of patients at Practice ', correct_practiceid, 'is ', 
                no_of_patients)) 
    avg_spend_per_month <- get_avg_spend_per_month(correct_practiceid)
    print(paste('The average spend on medication per month at Practice ', 
      correct_practiceid, 'is ', avg_spend_per_month)) 
  }
}
Question_1()


# Close the connection and unload the drivers.
dbDisconnect(con)
dbUnloadDriver(drv)

cat('\nEnd of analysis. Thank you for using 2149508\'s code.\n',
    'For more information, please contact 2149508 on\n',
    '2149508@swansea.ac.uk.')
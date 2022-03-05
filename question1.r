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

# For 'qq()' which allows substitution of vars in strings.
library(GetoptLong)

#Specify what driver is needed to connect to the database.
drv = dbDriver("PostgreSQL")

#Connect to the database for the assignment.
con <- dbConnect(drv, dbname = "gp_practice_data", 
                 host = "localhost", port = 5432,
                 user = "postgres", password = rstudioapi::askForPassword())

# Confirm connection to database by displaying the available tables.
cat('The following tables are available:\n')
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
get_columns('address')

qof_indicator_columns <- get_columns('qof_indicator')

gp_data_up_to_2015_columns <- get_columns('gp_data_up_to_2015')


#Questions - PART 1
#user to select practice
choose_practice <- readline('Select Practice ID: ') 

user_practice <- dbGetQuery(con, qq("
    select * from address
    where practiceid = \'@{choose_practice}\'"))
user_practice

#to check that practice id entered follows the uniform pattern
user_entry <- str_detect(choose_practice,'^W[0-9]{5}$')
user_entry

if (user_entry==TRUE){
  print(user_practice)
}   else{
  stop('This is not a Practice ID!\n')
}   

#Create function for practiceid entry
input_practiceid <- function() {
  user_entry <- FALSE
  while(user_entry == FALSE){
    choose_practice <- readline('Select Practice ID: ')
    user_entry <- str_detect(choose_practice,'^W[0-9]{5}$')
    if (user_entry==TRUE){
      print(user_practice)
    }else{
      cat('\nThis is not a Practice ID.')
      cat('\nEnter Practice ID starting with W\n')
    } 
  }
  return(choose_practice)
}
user_input <- input_practiceid()


choose_practice<-'W92041'
#Q1(a) check if practice has medication information available



#Checking what type of result we expect from the data, in this case it's a list
count(med_info)

#Create function to check if practice has medication info
has_medinfo <- function(chosen_practiceid) {
  med_info <- dbGetQuery(con, qq("
    select bnfcode, bnfname, practiceid
    from gp_data_up_to_2015
    where practiceid = \'@{chosen_practiceid}\'"))
  has_medicinfo <- FALSE
  if (count(med_info) > 0){
    has_medicinfo <- TRUE
  }
  return(has_medicinfo)
}
has_medinfo('W92041')  

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
has_qofinfo('W92641')  

#Calculate the average cost
average_cost <- med_cpp %>% select(month, total_cost_meds) %>% 
  mutate(avg_cost=total_cost_meds %/% no_of_patients)

  


qof_info <- dbGetQuery(con, qq("
    select * from qof_achievement
    where orgcode = \'@{choose_practice}\'"))
qof_info

count(qof_info)

if (count(qof_info) > 0){
  mqof_info == TRUE
  print(qof_info)
}else{
  qof_info == FALSE
  print('Practice ID has no QOF information available')
}

if(hasMedinfo & hasQF){
  
}

if (practiceid_entered==TRUE){
  print(chosen_practice_address)
}   else{
  stop('This is not a Practice ID!\n')
}    
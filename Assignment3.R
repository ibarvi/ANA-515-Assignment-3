library(tidyverse)
library(tidyr)

#1
events = read.csv("StormEvents_details-ftp_v1.0_d1984_c20210803.csv")

#2.	Limit the dataframe to the following columns: (10 points)
columns = c("BEGIN_YEARMONTH","BEGIN_DAY","BEGIN_TIME","END_YEARMONTH","END_DAY","END_TIME","EPISODE_ID","EVENT_ID","STATE","STATE_FIPS","CZ_NAME","CZ_TYPE","CZ_FIPS","EVENT_TYPE","DATA_SOURCE","BEGIN_LAT","BEGIN_LON","END_LAT","END_LON")
new_events = events[columns]

#3.	Arrange the data by beginning year and month (BEGIN_YEARMONTH) (5 points)
new_events = new_events %>% arrange(BEGIN_YEARMONTH)

#4.	Change state and county names to title case (e.g., “New Jersey” instead of “NEW JERSEY”) (5 points) 
new_events = new_events %>% mutate(STATE = str_to_title(STATE), CZ_NAME = str_to_title(CZ_NAME))

#5.	Limit to the events listed by county FIPS (CZ_TYPE of “C”) and then remove the CZ_TYPE column (5 points) 
new_events = new_events %>% filter(CZ_TYPE == "C") %>% select(-CZ_TYPE)

#6.	Pad the state and county FIPS with a “0” at the beginning (hint: there’s a function in stringr to do this) and then unite the two columns to make one fips column with the 5 or 6-digit county FIPS code (5 points) 
new_events = new_events %>% mutate(STATE_FIPS = str_pad(STATE_FIPS, width = 2, pad = "0", side = "left"), CZ_FIPS = str_pad(CZ_FIPS, width = 3, pad = "0", side = "left")) %>% 
                            unite("FIPS", STATE_FIPS, CZ_FIPS, sep = "0")

#7.	Change all the column names to lower case (you may want to try the rename_all function for this) (5 points) 
new_events = new_events %>% rename_all(tolower)

#8.	There is data that comes with base R on U.S. states (data("state")). Use that to create a dataframe with these three columns: state name, area, and region (5 points)
data("state")
states  = data.frame(state_name = state.name, area = state.area, region = state.region)

#9.	Create a dataframe with the number of events per state in the year of your birth. Merge in the state information dataframe you just created in step 8. Remove any states that are not in the state information dataframe. (5 points) 
states_events = new_events %>% group_by(state) %>% summarise(num_events = n())
states_merged = states %>% left_join(states_events, by = c("state_name" = "state"))

#10.	Create the following plot (10 points): 
ggplot(states_merged, aes(x = area, y = num_events, col = region)) + geom_point() + labs(x = "Land area (square miles)", y = "# of storm events in 1984")

rm(list=ls())

library('dplyr')
library('tidyverse')
#install.packages('compare')
library('compare')
#install.packages('rioja')
library('rioja')
#install.packages('testthat')
library('testthat')

visits <- read.csv("C:/Users/Neha Desai/Desktop/Fall 2018/CMDA 4864/Data/emporium_visits.csv", header = T)

enrollments <- read.csv("C:/Users/Neha Desai/Desktop/Fall 2018/CMDA 4864/Data/enrollments.csv", header = T)

student <- read.csv("C:/Users/Neha Desai/Desktop/Fall 2018/CMDA 4864/Data/student.csv", header = T)

terms <- read.csv("C:/Users/Neha Desai/Desktop/Fall 2018/CMDA 4864/Data/terms.csv", header = T)

purpose <- read.csv("C:/Users/Neha Desai/Desktop/Fall 2018/CMDA 4864/Data/purpose.csv", header = T)

deadlines <- read.csv("C:/Users/Neha Desai/Desktop/Fall 2018/CMDA 4864/Data/deadlines.csv", header = T)

courses <- read.csv("C:/Users/Neha Desai/Desktop/Fall 2018/CMDA 4864/Data/courses.csv", header = T)


# Removing 'id' and 'code' columns from terms ####
terms <- terms[, c(2,3,4,6)]
####


# Eliminating repeated rows in deadlines df ####
deadlines <-  distinct(deadlines, begin_at, end_at, viewable_at, location_restriction,
                       term_id, student_id, course_id, title, .keep_all = TRUE)
deadlines <- deadlines[, c(2, 3, 4, 5, 6, 7, 8, 9)] # Eliminating id number 
####



# Deadlines without specific student ID number ####
noSID_dead <- deadlines[is.na(deadlines$student_id), ]
noSID_dead <- noSID_dead[, c(1,2,3,4,5,7,8)]
####


# Deadlines referring to specific student ID number ####
SID_dead <- deadlines[!(is.na(deadlines$student_id)), ]
####



# Merging purpose and visits ####
purp_vis <- merge(purpose, visits, 
                  by.x= 'id',
                  by.y = 'purpose_id')
####



# merging purpose, visits, and student ####
purp_vis <- merge(purp_vis, student,
                  by.x = 'student_id',
                  by.y = 'tmp_id')
# purp_vis_stud #### 

# Separating data and time in purp_vis ####





#c <- merge(deadlines, purp_, 
#           by.x = 'student_id',
#           by.y = 'student_id')



# Merging courses and enrollments ####
course_enrollment <- merge(courses, enrollments,
                           by.x = 'tmp_id',
                           by.y = 'course_id')
# course_enrollment ####

purp_vis$checkin_at <- as.character(purp_vis$checkin_at)
purp_vis$checkout_at <- as.character(purp_vis$checkout_at)


purp_vis$time_checkin <- format(as.POSIXct(strptime(purp_vis$checkin_at, "%d/%m/%Y %H:%M:%S", tz ="")),
                                format = "%H:%M:%S")

purp_vis$date_checkin <- format(as.POSIXct(strptime(purp_vis$checkin_at,"%d/%m/%Y %H:%M",tz="")),
                                format = "%Y/%m/%d")


purp_vis$time_checkout <- format(as.POSIXct(strptime(purp_vis$checkout_at, "%d/%m/%Y %H:%M:%S", tz ="")),
                                 format = "%H:%M:%S")

purp_vis$date_checkout <- format(as.POSIXct(strptime(purp_vis$checkout_at,"%d/%m/%Y %H:%M",tz="")),
                                 format = "%Y/%m/%d")


purp_vis$checkin_at <- NULL
purp_vis$checkout_at <- NULL


#new_df <- data.frame(num_stud = integer(),
#                     time_block = integer())

week_checkin <- format(as.POSIXct(strptime(purp_vis$date_checkin,"%y/%m/%d", tz="")),
                       format = "%Y/%W")
purp_vis$week_checkin <- week_checkin

#week_checkout <- format(as.POSIXct(strptime(purp_vis$checkout_at,"%d/%m/%Y %H:%M",tz="")),format = "%y/%m/%d")


purp_vis$days <- weekdays(as.Date(purp_vis$date_checkin))


fall2014 <- purp_vis[purp_vis$date_checkin >= "2014/08/25" & purp_vis$date_checkin <= "2014/12/18", ]

winter2015 <- purp_vis[purp_vis$date_checkin >= "2015/01/02" & purp_vis$date_checkin <= "2015/01/17", ]
spring2015 <- purp_vis[purp_vis$date_checkin >= "2015/01/20" & purp_vis$date_checkin <= "2015/05/13", ]
summ1_2015 <- purp_vis[purp_vis$date_checkin >= "2015/05/26" & purp_vis$date_checkin <= "2015/07/06", ]
summ2_2015 <- purp_vis[purp_vis$date_checkin >= "2015/07/07" & purp_vis$date_checkin <= "2015/08/15", ]
fall2015 <- purp_vis[purp_vis$date_checkin >= "2015/08/24" & purp_vis$date_checkin <= "2015/12/17", ]

winter2016 <- purp_vis[purp_vis$date_checkin >= "2016/01/04" & purp_vis$date_checkin <= "2016/01/16", ]
spring2016 <- purp_vis[purp_vis$date_checkin >= "2016/01/19" & purp_vis$date_checkin <= "2016/05/11", ]
summ1_2016 <- purp_vis[purp_vis$date_checkin >= "2016/05/23" & purp_vis$date_checkin <= "2016/07/02", ]
summ2_2016 <- purp_vis[purp_vis$date_checkin >= "2016/07/05" & purp_vis$date_checkin <= "2016/08/13", ]
fall2016 <- purp_vis[purp_vis$date_checkin >= "2016/08/22" & purp_vis$date_checkin <= "2016/12/15", ]

winter2017 <- purp_vis[purp_vis$date_checkin >= "2017/01/02" & purp_vis$date_checkin <= "2017/01/14", ]
spring2017 <- purp_vis[purp_vis$date_checkin >= "2017/01/17" & purp_vis$date_checkin <= "2017/05/10", ]
summ1_2017 <- purp_vis[purp_vis$date_checkin >= "2017/05/22" & purp_vis$date_checkin <= "2017/07/01", ]
summ2_2017 <- purp_vis[purp_vis$date_checkin >= "2017/07/05" & purp_vis$date_checkin <= "2017/08/14", ]
fall2017 <- purp_vis[purp_vis$date_checkin >= "2017/08/28" & purp_vis$date_checkin <= "2017/12/20", ]

winter2018 <- purp_vis[purp_vis$date_checkin >= "2018/01/03" & purp_vis$date_checkin <= "2018/01/13", ]
spring2018 <- purp_vis[purp_vis$date_checkin >= "2018/01/16" & purp_vis$date_checkin <= "2018/05/09", ]
summ1_2018 <- purp_vis[purp_vis$date_checkin >= "2018/05/21" & purp_vis$date_checkin <= "2018/06/30", ]
summ2_2018 <- purp_vis[purp_vis$date_checkin >= "2018/07/02" & purp_vis$date_checkin <= "2018/08/11", ]
fall2018 <- purp_vis[purp_vis$date_checkin >= "2018/08/20" & purp_vis$date_checkin <= "2018/12/13", ]


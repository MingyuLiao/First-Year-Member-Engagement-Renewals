library(data.table)
library(dplyr)
library(writexl)
library(lubridate)
library(stringr)
library(tidyverse)
setwd("~/OneDrive - USC Marshall School of Business/DSO545_Statistical Computing and Data Visualization/Sam's Club/Sams POS/Datasets")
demo =  read.csv("member_metrics_demo_data.csv")
demo$renew_ind_1or0 =as.factor(demo$renew_ind_1or0)
# age
summary(demo$hhh_age_desc)

age = demo %>% 
  filter(MEMBERSHIP_TYPE_DESC == "SAVINGS") %>%
  group_by(renew_ind_1or0,hhh_age_desc) %>% 
  summarise(count = n())

age %>% ggplot(aes(x= hhh_age_desc, fill = renew_ind_1or0))+
  geom_density()


age = left_join(age,
                age %>% 
                  group_by(renew_ind_1or0) %>% 
                  summarise(total = sum(count)),
                  by = c("renew_ind_1or0" = "renew_ind_1or0"))

age$density = age$count/age$total
age %>% 
  ggplot(aes(x=hhh_age_desc, y = density, group = renew_ind_1or0))+
  geom_line()

write.csv(age, "age.csv")




# miles to club
summary(demo$MILES_TO_CLUB)
## business first
boxplot(demo$MILES_TO_CLUB)
miles = demo %>% 
  filter(MEMBERSHIP_TYPE_DESC == "BUSINESS"&MILES_TO_CLUB<30) %>% 
  group_by(renew_ind_1or0, MILES_TO_CLUB) %>% 
  summarise(count = n())
miles$MILES_TO_CLUB = as.integer(miles$MILES_TO_CLUB/0.5)*0.5

miles = miles %>% 
  group_by(renew_ind_1or0 ,MILES_TO_CLUB) %>% 
  summarise(t = sum(count))
ggplot(miles, aes(x= MILES_TO_CLUB, y = t, fill = renew_ind_1or0))+
  geom_col(position = "dodge")

miles = left_join(miles,
                  miles %>% 
                    group_by(renew_ind_1or0) %>% 
                    summarise(total = sum(t)),
                  by = c("renew_ind_1or0" = "renew_ind_1or0"))

miles$density = miles$t/miles$total

miles_wide = miles
miles_wide$t =NULL
miles_wide$total =NULL
miles_wide = spread(miles_wide,
                    key =renew_ind_1or0, 
                    value =density)
write_xlsx(miles_wide, "miles_business.xlsx")
# for savings
miles = demo %>% 
  filter(MEMBERSHIP_TYPE_DESC == "SAVINGS"&MILES_TO_CLUB<30) %>% 
  group_by(renew_ind_1or0, MILES_TO_CLUB) %>% 
  summarise(count = n())
miles$MILES_TO_CLUB = as.integer(miles$MILES_TO_CLUB/0.5)*0.5

miles = miles %>% 
  group_by(renew_ind_1or0 ,MILES_TO_CLUB) %>% 
  summarise(t = sum(count))
ggplot(miles, aes(x= MILES_TO_CLUB, y = t, fill = renew_ind_1or0))+
  geom_col(position = "dodge")

miles = left_join(miles,
                    miles %>% 
                      group_by(renew_ind_1or0) %>% 
                      summarise(total = sum(t)),
                    by = c("renew_ind_1or0" = "renew_ind_1or0"))

miles$density = miles$t/miles$total

miles_wide = miles
miles_wide$t =NULL
miles_wide$total =NULL
miles_wide = spread(miles_wide,
                      key =renew_ind_1or0, 
                      value =density)
write_xlsx(miles_wide, "miles_savings.xlsx")

# for all if there is no differnce
miles = demo %>% 
  filter(MILES_TO_CLUB<30) %>% 
  group_by(renew_ind_1or0, MILES_TO_CLUB) %>% 
  summarise(count = n())
miles$MILES_TO_CLUB = as.integer(miles$MILES_TO_CLUB/0.5)*0.5

miles = miles %>% 
  group_by(renew_ind_1or0 ,MILES_TO_CLUB) %>% 
  summarise(t = sum(count))
ggplot(miles, aes(x= MILES_TO_CLUB, y = t, fill = renew_ind_1or0))+
  geom_col(position = "dodge")

miles = left_join(miles,
                  miles %>% 
                    group_by(renew_ind_1or0) %>% 
                    summarise(total = sum(t)),
                  by = c("renew_ind_1or0" = "renew_ind_1or0"))

miles$density = miles$t/miles$total

miles_wide = miles
miles_wide$t =NULL
miles_wide$total =NULL
miles_wide = spread(miles_wide,
                    key =renew_ind_1or0, 
                    value =density)
write_xlsx(miles_wide, "miles_all.xlsx")



# income_desc
income = spread(
demo %>% 
  group_by(renew_ind_1or0, income_desc) %>% 
  summarise(count = n()),
key = renew_ind_1or0,
value = count)

write_xlsx(income, "income_all.xlsx")

# auto_renewal
auto = spread(
  demo %>% 
    group_by(autorenew_ind,renew_flag) %>% 
    summarise(count =n()),
  key =autorenew_ind,
  value = count
)

# payroll_deduct
payroll = spread(
  demo %>% 
    group_by(payroll_deduct_ind,renew_flag) %>% 
    summarise(count =n()),
  key =payroll_deduct_ind,
  value = count
)

# marital_status
marital = spread(
  demo %>% 
    filter(MEMBERSHIP_TYPE_DESC == "SAVINGS") %>% 
    group_by(marital_status_desc,renew_ind_1or0) %>% 
    summarise(count= n()),
  key = marital_status_desc,
  value =count
)
write_xlsx(marital, "marital.xlsx")

# nbr_children
demo %>% 
  filter(MEMBERSHIP_TYPE_DESC =="SAVINGS") %>% 
  group_by(nbr_children_desc, RENEW_IND) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x=nbr_children_desc,y = count, fill= RENEW_IND))+
  geom_col(position = "dodge")
  
children = spread(
  demo %>% 
    filter(MEMBERSHIP_TYPE_DESC =="SAVINGS") %>% 
    group_by(nbr_children_desc, RENEW_IND) %>% 
    summarise(count = n()),
  key = nbr_children_desc,
  value = count
)

write_xlsx(children, "children.xlsx")

# hh_size_desc
size = demo %>% 
  filter(MEMBERSHIP_TYPE_DESC =="SAVINGS") %>% 
  group_by(renew_flag, hh_size_desc) %>% 
  summarise(count = n())
  
size = left_join(size,
                  size %>% 
                        group_by(renew_flag) %>% 
                        summarise(total = sum(count)),
                      by = c("renew_flag" = "renew_flag"))
  ggplot(aes(x=hh_size_desc, fill =renew_flag)) +
  geom_bar(position = "dodge")
size$per = size$count/size$total

size %>% 
  ggplot(aes(x=hh_size_desc, y = per,color = renew_flag))+
  geom_line()
# member who renewed plus have larger family size
size = demo %>% 
  filter(MEMBERSHIP_TYPE_DESC =="SAVINGS") %>% 
  group_by(renew_ind_1or0,hh_size_desc) %>% 
  summarise(count = n())

size = left_join(size,
                 size %>% 
                   group_by(renew_ind_1or0) %>% 
                   summarise(total = sum(count)),
                 by = c("renew_ind_1or0" = "renew_ind_1or0"))
ggplot(aes(x=hh_size_desc, fill =renew_ind_1or0)) +
  geom_bar(position = "dodge")
size$per = size$count/size$total

size %>% 
  ggplot(aes(x=hh_size_desc, y = per,color = renew_ind_1or0))+
  geom_line()

size$count =NULL
size$total =NULL
size = spread(size,
              key = renew_ind_1or0,
              value = per)

write_xlsx(size, "size.xlsx")

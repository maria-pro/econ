#library
library(shiny)
library(tidyverse)
library(vroom)


#set bookmarking - to remember user input

enableBookmarking("url")




#READ DATA
#tab 1 - industry - australia
employ<-vroom("data/employ.csv")

employ_options<-employ%>%
  distinct(industry_group, var)

employ_options<-with(employ_options, split((var),
                       factor(industry_group, levels = unique(industry_group), ordered=TRUE)))

#tab 2 - occupation - australia
occupation<-vroom("data/occupation.csv")

occupation_options<-occupation%>%
  distinct(occupation_group, var)

occupation_options<-with(occupation_options, split((var),
                                           factor(occupation_group, levels = unique(occupation_group), ordered=TRUE)))

#tab 3 - state - australia
employ_state<-vroom("data/employ_state.csv")

#tab 4 - industry - qld and wa
employ_qld<-vroom("data/employ_qld.csv")

employ_wa<-vroom("data/employ_wa.csv")

#tab 5 - occupation - qld and wa
occupation_qld<-vroom("data/occupation_qld.csv")

occupation_wa<-vroom("data/occupation_wa.csv")

#tab 5 - occupation - qld and wa

employ_sa4<-st_read("data/employ_sa4_geo.geojson")

ind_employ<-vroom("data/ind_employ.csv")

occ_employ<-vroom("data/occ_employ.csv")

employ_state<-vroom("data/employ_state.csv")

industry_dic<-vroom("data/dictionary/industry.csv", col_names=FALSE)

region_dic<-vroom("data/dictionary/regions.csv", col_names=FALSE)

occupation_dic<-vroom("data/dictionary/occupation.csv", col_names=FALSE)

#assign names

empl_codes<-setNames(industry_dic$X3, unique(employ$var)) #employ$var second should be names - a character vector of names to assign to the object



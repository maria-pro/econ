library(tidyverse)
library(gganimate)


#functions

#loading data
load_data<-function(file="data/data_mp.xlsx", sheet="Raw - Base", range="B1:AJ85", base="base"){

  data<-readxl::read_excel(path="data/data_mp.xlsx", sheet=sheet, range=range)

  if (base=="base"){
    names(data)<-c("var", seq(2017, 2051, 1))
    data$state<-"base"
  }else if (base=="zero") {
    names(data)<-c("var", seq(2020, 2051, 1))
    data$state<-"zero"
  }

  else if (base=="misc_base") {

    names(data)<-c("var", seq(2020, 2050, 1))
    data$state<-"base"
    } else if(base=="misc_zero"){

      names(data)<-c("var", seq(2020, 2050, 1))
      data$state<-"zero"
    }

  data$var<-str_trim(gsub('[0-9.]', '', data$var))

  data<-data%>% pivot_longer(-c(var, state),
                             names_to="year",
                             values_to="value")

  data
}

#-----------data

#if (!exists("employ_base")) {
setwd("/Users/e5028514/Library/CloudStorage/OneDrive-VictoriaUniversity/econ/NetZero")

  #EMPLOY = employment data - total
  employ_base<-load_data("data/data_mp.xlsx", sheet="Raw - Base", range="A1:AJ85")


  #EMPLOY97 = employment per occupation - total
  occupation_base<-load_data("data/data_mp.xlsx", sheet="Raw - Base", range="A88:AJ185")


  #EMPLOY97 = employment per state - total
  employ_state_base<-load_data("data/data_mp.xlsx", sheet="Raw - Base", range="A188:AJ196")

  #EMPLOY97 = employment - Qld
  employ_qld_base<-load_data("data/data_mp.xlsx", sheet="Raw - Base", range="A199:AJ283")

  #EMPLOY97 = employment - WA
  employ_wa_base<-load_data("data/data_mp.xlsx", sheet="Raw - Base", range="A286:AJ370")

  #EMPLOY97 = occupation - Qld
  occupation_qld_base<-load_data("data/data_mp.xlsx", sheet="Raw - Base", range="A373:AJ470")

  #EMPLOY97 = occupation - WA
  occupation_wa_base<-load_data("data/data_mp.xlsx", sheet="Raw - Base", range="A473:AJ570")

  #EMPLOY97 = employment - region sa4
  employ_sa4_base<-load_data("data/data_mp.xlsx", sheet="Raw - Base", range="A573:AJ661")


  #---------zero

  #EMPLOY = employment data - total
  employ_zero<-load_data("data/data_mp.xlsx", sheet="Raw - Zero", range="A1:AG85", base="zero")


  #EMPLOY97 = employment per occupation - total
  occupation_zero<-load_data("data/data_mp.xlsx", sheet="Raw - Zero", range="A88:AG185", base="zero")

  #EMPLOY97 = employment per state - total z
  employ_state_zero<-load_data("data/data_mp.xlsx", sheet="Raw - Zero", range="A188:AG196", base="zero")

  #EMPLOY97 = employment - Qld z
  employ_qld_zero<-load_data("data/data_mp.xlsx", sheet="Raw - Zero", range="A199:AG283", base="zero")

  #EMPLOY97 = employment - WA
  employ_wa_zero<-load_data("data/data_mp.xlsx", sheet="Raw - Zero", range="A286:AG370", base="zero")

  #EMPLOY97 = occupation - Qld
  occupation_qld_zero<-load_data("data/data_mp.xlsx", sheet="Raw - Zero", range="A373:AG470", base="zero")

  #EMPLOY97 = occupation - WA
  occupation_wa_zero<-load_data("data/data_mp.xlsx", sheet="Raw - Zero", range="A473:AG570", base="zero")

  #EMPLOY97 = employment - region sa4
  employ_sa4_zero<-load_data("data/data_mp.xlsx", sheet="Raw - Zero", range="A573:AG661", base="zero")

  #----------------

#sheet: "Industry employment, Australia"
## Head count ('000 persons) employment by Industry, Australia
ind_employ_base<-load_data("data/data_mp.xlsx", sheet="Industry employment, Australia", range="A3:AF84", base="misc_base")

ind_employ_zero<-load_data("data/data_mp.xlsx", sheet="Industry employment, Australia", range="A87:AF168", base="misc_base")

#sheet: Occupation employment, Australi
#Head count ('000 persons) employment by Minor group occupation Australia

occ_employ_base<-load_data("data/data_mp.xlsx", sheet="Occupation employment, Australi", range="A3:AF99", base="misc_base")

occ_employ_zero<-load_data("data/data_mp.xlsx", sheet="Occupation employment, Australi", range="A102:AF198", base="misc_zero")

#sheet: Employment, State

employ_state_base<-load_data("data/data_mp.xlsx", sheet="Employment, State", range="A3:AF11", base="misc_base")

employ_state_zero<-load_data("data/data_mp.xlsx", sheet="Employment, State", range="A14:AF22", base="misc_zero")


#merge

employ<-bind_rows(employ_base,employ_zero)

occupation<-bind_rows(occupation_base,occupation_zero)

employ_state<-bind_rows(employ_state_base,employ_state_zero)

employ_qld<-bind_rows(employ_qld_base,employ_qld_zero)

employ_wa<-bind_rows(employ_wa_base,employ_wa_zero)

occupation_qld<-bind_rows(occupation_qld_base,occupation_qld_zero)

occupation_wa<-bind_rows(occupation_wa_base,occupation_wa_zero)

employ_sa4<-bind_rows(employ_sa4_base,employ_sa4_zero)

ind_employ<-bind_rows(ind_employ_base,ind_employ_zero)

occ_employ<-bind_rows(occ_employ_base,occ_employ_zero)

employ_state<-bind_rows(employ_state_base, employ_state_zero)


#SAVE DATA

employ%>%write_csv("data/employ.csv")

occupation%>%write_csv("data/occupation.csv")

employ_state%>%write_csv("data/employ_state.csv")

employ_qld%>%write_csv("data/employ_qld.csv")

employ_wa%>%write_csv("data/employ_wa.csv")

occupation_qld%>%write_csv("data/occupation_qld.csv")

occupation_wa%>%write_csv("data/occupation_wa.csv")

employ_sa4%>%write_csv("data/employ_sa4.csv")

ind_employ%>%write_csv("data/ind_employ.csv")

occ_employ%>%write_csv("data/occ_employ.csv")

employ_state%>%write_csv("data/employ_state.csv")

#changes to values per Peter

library(vroom)
#READ DATA

employ<-vroom("data/employ.csv")

occupation<-vroom("data/occupation.csv")

employ_state<-vroom("data/employ_state.csv")

employ_qld<-vroom("data/employ_qld.csv")

employ_wa<-vroom("data/employ_wa.csv")

occupation_qld<-vroom("data/occupation_qld.csv")

occupation_wa<-vroom("data/occupation_wa.csv")

employ_sa4<-vroom("data/employ_sa4.csv")

ind_employ<-vroom("data/ind_employ.csv")

occ_employ<-vroom("data/occ_employ.csv")

employ_state<-vroom("data/employ_state.csv")


var <- setNames(employ$var, employ$var)
state <- setNames(employ$state, employ$state)
years<-setNames(employ$year, employ$year)


#----------
#data change per peter
#employ<-read_csv("data/employ.csv")
#
#occupation<-read_csv("data/occ_employ.csv")
#employ_state<-read_csv("data/employ_state.csv")

employ_state<-read_csv("data/sa4_transform.csv")


#change dataset
test<-employ_state%>%
  pivot_wider(
  names_from=state,
  values_from=value
)%>%filter(
  year>2022
)

#for sa4_transform only

test<-employ_state%>%filter(
    year>2022
  )

test_ref_2023<-test%>%filter(
  year==2023)%>%mutate(
    netNew2023=base
  )%>%
  select(-c(base, zero, year))

test_ref_2030<-test%>%filter(
  year==2030)%>%mutate(
    netNew2030=zero
  )%>%
  select(-c(base, zero, year))


test_merge<-left_join(test, test_ref_2023, by=c("var",
                                                "occupation_group" #for occupation
                                                #"industry_group" #for employ
                                                ))

#for employ_state
#test_merge<-left_join(test, test_ref_2023)

test_merge<-left_join(test_merge, test_ref_2030, by=c("var",
                                                      "occupation_group" #for occupation
                                                      #"industry_group" #for employ
                                                      ))
#for state_employ
#test_merge<-left_join(test_merge, test_ref_2030)

test_2023_2031<-test_merge%>%filter(
  year>2022 &
    year< 2032
)

test_2023_2031<-test_2023_2031%>%mutate(
  zero2030_base2023=netNew2030-netNew2023
)%>%group_by(var) %>%
  mutate(
    zerolead_zero=case_when(year==2023~0,
                            TRUE~dplyr::lead(zero, n = 1, default = NA)-zero),
    sum_changes=sum(zerolead_zero, na.rm=TRUE),
    prop=zerolead_zero/sum_changes)%>%
  ungroup()%>%
  filter(year!=2031)%>%
  group_by(var) |>
#  arrange(year) |>
  mutate(temp = if_else(year == 2023, netNew2023, prop*zero2030_base2023),
         var4 = cumsum(temp)) |>
  ungroup()


#test_2023_2031%>%write_csv("employ_new.csv")
#test_2023_2031%>%write_csv("occupation_new.csv")
#test_2023_2031%>%write_csv("employ_state_new.csv")
#test_2023_2031%>%write_csv("employ_sa4_new.csv")
test_2023_2031%>%write_csv("sa4_transform_new.csv")

m<-test_2023_2031%>%select(
  -c(
    sum_changes,
    prop,
    zerolead_zero,
    zero2030_base2023,
    netNew2023,
    temp,
    netNew2030,
    zero
  )
)%>%
  rename(
    "zero"="var4"
  )%>%
  pivot_longer(
    cols=c("base","zero"),
    names_to="state",
  )


#merging with data

test_main<-employ_state%>%filter(
  year>2030
)

#only for sa4_transform
test_main<-employ_state%>%filter(
  year>2030
)%>%
  pivot_longer(
    cols=c("base","zero"),
    names_to="state",
  )

data_new<-rbind(test_main, m)

#data_new%>%write_csv("employ_new.csv")
#data_new%>%write_csv("occupation_new.csv")

#data_new%>%write_csv("employ_state_new.csv")
#data_new%>%write_csv("employ_sa4_new.csv")
data_new%>%write_csv("sa4_transform_new.csv")
#----------
#sum per group

employ<-read_csv("data/employ_new.csv")

test<-employ%>%group_by(year, industry_group, state)%>%
  filter(n()>1)%>%
  summarise(
    value=sum(value)
  )%>%ungroup()%>%
  mutate(
    var="All sector"
  )

employ<-rbind(employ,test)
employ%>%write_csv("data/employ_new.csv")

#occupation
occupation<-read_csv("data/occupation_new.csv")

test<-occupation%>%group_by(year, occupation_group, state)%>%
  filter(n()>1)%>%
  summarise(
    value=sum(value)
  )%>%ungroup()%>%
  mutate(
    var="All occupation group"
  )

occupation<-rbind(occupation,test)
occupation%>%write_csv("data/occupation_new.csv")


#------------------------
#exploration and dataviz

### employ

employ%>%write_csv("employ.csv")

employ%>% filter(state=="base" & var=="Sheep and Beef cattle")%>%
  ggplot(aes(x=year, y=value,
           colour=var))+
  geom_point()+
  transition_states(
    year
  )+
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')



  theme(
    axis.text.x = element_text(angle = 90, size = 12)
  )

employ%>% filter(state=="zero")%>%
  ggplot(aes(x=year, y=value,
             colour=var))+
  geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, size = 12)
  )

## occupation

occupation%>% filter(state=="base")%>%
  ggplot(aes(x=year, y=value,
             colour=var))+
  geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, size = 12)
  )

occupation%>% filter(state=="zero")%>%
  ggplot(aes(x=year, y=value,
             colour=var))+
  geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, size = 12)
  )


#--------

#transform to shapefile

employ_state_new<-employ_state<-read_csv("data/sa4_transform_new3.csv",col_types=cols(sa4_code_2016 = col_character()))%>%
  filter(year==2030 | year==2050)%>% left_join(absmapsdata::sa42016)%>%
  mutate(
    base=round(base, 1),
    zero=round(zero, 1),
  )%>%
  st_as_sf(sf_column_name="geometry")


employ_state_new%>%st_write("data/sa4_transform_new3.shp")

employ_state_new<-sf::st_read("data/sa4_transform_new3.shp")


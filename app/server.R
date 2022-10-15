#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(DT)


#loading data
load_data<-function(file="data/data.xlsx", sheet="Raw - Base", range="A1:AJ85", base="base"){

  data<-readxl::read_excel(path="data/data.xlsx", sheet=sheet, range=range)

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
setwd("/Users/e5028514/Library/CloudStorage/OneDrive-VictoriaUniversity/econ/")

#EMPLOY = employment data - total
employ_base<-load_data("data/data.xlsx", sheet="Raw - Base", range="A1:AJ85")


#EMPLOY97 = employment per occupation - total
occupation_base<-load_data("data/data.xlsx", sheet="Raw - Base", range="A88:AJ185")


#EMPLOY97 = employment per state - total
employ_state_base<-load_data("data/data.xlsx", sheet="Raw - Base", range="A188:AJ196")

#EMPLOY97 = employment - Qld
employ_qld_base<-load_data("data/data.xlsx", sheet="Raw - Base", range="A199:AJ283")

#EMPLOY97 = employment - WA
employ_wa_base<-load_data("data/data.xlsx", sheet="Raw - Base", range="A286:AJ370")

#EMPLOY97 = occupation - Qld
occupation_qld_base<-load_data("data/data.xlsx", sheet="Raw - Base", range="A373:AJ470")

#EMPLOY97 = occupation - WA
occupation_wa_base<-load_data("data/data.xlsx", sheet="Raw - Base", range="A473:AJ570")

#EMPLOY97 = employment - region sa4
employ_sa4_base<-load_data("data/data.xlsx", sheet="Raw - Base", range="A573:AJ661")


#---------zero

#EMPLOY = employment data - total
employ_zero<-load_data("data/data.xlsx", sheet="Raw - Zero", range="A1:AG85", base="zero")


#EMPLOY97 = employment per occupation - total
occupation_zero<-load_data("data/data.xlsx", sheet="Raw - Zero", range="A88:AG185", base="zero")

#EMPLOY97 = employment per state - total z
employ_state_zero<-load_data("data/data.xlsx", sheet="Raw - Zero", range="A188:AG196", base="zero")

#EMPLOY97 = employment - Qld z
employ_qld_zero<-load_data("data/data.xlsx", sheet="Raw - Zero", range="A199:AG283", base="zero")

#EMPLOY97 = employment - WA
employ_wa_zero<-load_data("data/data.xlsx", sheet="Raw - Zero", range="A286:AG370", base="zero")

#EMPLOY97 = occupation - Qld
occupation_qld_zero<-load_data("data/data.xlsx", sheet="Raw - Zero", range="A373:AG470", base="zero")

#EMPLOY97 = occupation - WA
occupation_wa_zero<-load_data("data/data.xlsx", sheet="Raw - Zero", range="A473:AG570", base="zero")

#EMPLOY97 = employment - region sa4
employ_sa4_zero<-load_data("data/data.xlsx", sheet="Raw - Zero", range="A573:AG661", base="zero")

#----------------

#sheet: "Industry employment, Australia"
## Head count ('000 persons) employment by Industry, Australia
ind_employ_base<-load_data("data/data.xlsx", sheet="Industry employment, Australia", range="A3:AF84", base="misc_base")

ind_employ_zero<-load_data("data/data.xlsx", sheet="Industry employment, Australia", range="A87:AF168", base="misc_base")

#sheet: Occupation employment, Australi
#Head count ('000 persons) employment by Minor group occupation Australia

occ_employ_base<-load_data("data/data.xlsx", sheet="Occupation employment, Australi", range="A3:AF99", base="misc_base")

occ_employ_zero<-load_data("data/data.xlsx", sheet="Occupation employment, Australi", range="A102:AF198", base="misc_zero")

#sheet: Employment, State

employ_state_base<-load_data("data/data.xlsx", sheet="Employment, State", range="A3:AF11", base="misc_base")

employ_state_zero<-load_data("data/data.xlsx", sheet="Employment, State", range="A14:AF22", base="misc_zero")


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



state_codes <- setNames(employ_state$var, employ_state$var)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  selected <- reactive(employ_state %>% filter(var == input$code))

  summary <- reactive({
    selected()
  })

  output$employ_state_graph <- renderPlot({
    summary()%>%
      ggplot(aes(x=year, y=value,
                 colour=var,
                 alpha=state))+
      geom_point()+
      theme(
        axis.text.x = element_text(angle = 90, size = 12)
      )
  })



})

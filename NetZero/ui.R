#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


#--------
#--------
#individual components

#--------
#sidebar

sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("By industry", tabName="Item1"),
    menuItem("By occupation", tabName="Item2"),
    menuItem("By state", tabName="Item3"),
    menuItem("By industry-Qld/WA", tabName="Item4"),
    menuItem("By occupation-Qld/WA", tabName="Item5"),
    menuItem("Job losses", tabName="Item6"),
    menuItem("Job gains", tabName="Item7")

  )#sidebarMenu
)#dashboardSidebar

#--------

#body

body<- dashboardBody(
  tabItems(



#item1
  tabItem(tabName='Item1',
          h2("Employment by industry"),
          #start
          fluidRow(
            box(width=12,
              title="Inputs",
              status="primary",
              ("Please select values to display on the chart below"),
              br(),
              br(),

          sliderInput(inputId = "years",
                      label = ("Select time frame"),
                      min = 2022,
                      max = 2051,
                      value = c(2025, 2040),
                      sep="",
                      step=1),
          #br(),
          checkboxInput(inputId="base",
                        label=("Compare to business-as-usual")
                      #  value="base"
                        ),


          #fluidrow
       #   fluidRow(
          selectizeInput(inputId="var1",
                      width='40%',
                      selected=c(1),
                      label="Select industry 1",
                      choices=(employ_options)),

          selectizeInput(inputId="var2",
                         width='40%',
                         selected=NULL,
                         label="Select industry 2",
                         choices=
                           #(employ$var)
                           (employ_options)
                           )
        #  )

            ) #box
          ), #fluidRow

fluidRow(
  box(width=12,
 #plotOutput("plot1"),
 plotlyOutput("employ1_plot"),
   plotOutput("distPlot"),
   verbatimTextOutput("text1"),
  ) #box
)#fluidRow
#end
          ),#tabItem
#item2
  tabItem(tabName="Item2",
          h2("Employment by occupation"),

          #start
          fluidRow(
            box(width=12,
                title="Inputs",
                status="primary",
                ("Please select values to display on the chart below"),
                br(),
                br(),

               sliderInput(inputId = "years_occ",
                           label = ("Select time frame"),
                            min = 2022,
                           max = 2051,
                            value = c(2025, 2040),
                           sep="",
                           step=1),
                #br(),
                checkboxInput(inputId="base_occ",
                              label=("Compare to business-as-usual")
                              #  value="base"
                ),


                #fluidrow
                #   fluidRow(
                selectizeInput(inputId="var1_occ",
                               width='40%',
                               selected=c(1),
                               label="Select occupation 1",
                               choices=(
                                 #unique(occupation$var)
                                 occupation_options
                                 )),

                selectizeInput(inputId="var2_occ",
                               width='40%',
                               selected=NULL,
                               label="Select occupation 2",
                               choices=(
                                 #occupation$var
                                 occupation_options
                                 ))
                #  )

            ) #box
          ), #fluidRow

          fluidRow(
            box(width=12,
                #plotOutput("plot1"),
                plotlyOutput("occ_plot"),

         #       plotOutput("distPlot"),
        #        verbatimTextOutput("text1"),
            ) #box
          ),#fluidRow
          ),#tabItem

#item3
tabItem(tabName="Item3",
        h2("Employment by state"),
        fluidRow(
          box(width=12,
              #plotOutput("plot1"),
              plotlyOutput("state_plot"),

              #       plotOutput("distPlot"),
              #        verbatimTextOutput("text1"),
          ) #box
        ),#fluidRow



        ),#tabItem

#item4
tabItem(tabName="Item4",
        h2("Employment by industry-Qld/WA"),
),#tabItem

#item5
tabItem(tabName="Item5",
        h2("Employment by occupation-Qld/WA"),
),#tabItem

#item6
tabItem(tabName="Item6",
        h2("Job losses"),
),#tabItem

#item7
tabItem(tabName="Item7",
        h2("Job gains"),
)#tabItem

  )#tabItems


)#dashboardBody

#--------
#--------
#MAIN

# Define UI for application
shinyUI(

dashboardPage
(
  dashboardHeader(title="Net Zero in Australia"),
  sidebar,
  body

) #dashboardPage

) #shinyUI



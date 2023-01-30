#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #_______________________
   #tab1
   #plotOutput("employ1"),
   output$employ1_plot<-renderPlotly({
     #new_years = input$years
     employ_ggplot<-employ
     if (!input$base) {
       employ_ggplot<-employ%>% filter(state=="zero")
     }

     res<-employ_ggplot%>%
       #filter(state=="zero"& var==input$var1)%>%
 filter(var==input$var1|var==input$var2)%>%
   filter(year>=input$years[1] & year<=input$years[2])%>%
       ggplot()+
       geom_point(
                  aes(x=year, y=value, group=interaction(state, var),
                    colour=state,
                      shape=var,
                      text = paste("Industry: ", var,
                                   "\nValue: $", value,
                                   "\nStatus: ", state,
                                   "\nYear: ", year)))+
       theme(
         legend.position="none",
        panel.grid = element_blank(),
         panel.background = element_blank(),
 axis.ticks=element_line(colour = "black"),

 axis.line = element_line(colour = "black"),
       )+
     scale_x_continuous(name="Selected years", breaks=seq(input$years[1], input$years[2], by=1))+
       scale_y_continuous(name="Effect")


    empl_plot<-ggplotly(
      res,

#      dynamicTicks=TRUE,
   tooltip=c("text")
      )%>%
      layout(hovermode = "x unified")

     return(empl_plot)

     #plot(mtcars$wt, mtcars$mpg)
   })

   #_______________________
   #tab2
#occupation
   output$occ_plot<-renderPlotly({
     #new_years = input$years
     occ_ggplot<-occupation
     if (!input$base_occ) {
       occ_ggplot<-occupation%>% filter(state=="zero" & year>2022)
     }

     res<-occ_ggplot%>%
       #filter(state=="zero"& var==input$var1)%>%
       filter(var==input$var1_occ|var==input$var2_occ)%>%
   #    filter(year>2022)%>%
       filter(year>=input$years_occ[1] & year<=input$years_occ[2])%>%
       ggplot()+
       geom_point(
         aes(x=year, y=value, group=interaction(state, var),
             colour=state,
             shape=var,
             frame=year,
             text = paste("Occupation: ", var,
                          "\nValue: $", value,
                          "\nStatus: ", state,
                          "\nYear: ", year)))+
       theme(
         legend.position="none",
         panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.ticks=element_line(colour = "black"),

         axis.line = element_line(colour = "black"),
       )+
 #      scale_x_continuous(name="Selected years", breaks=seq(input$years[1], input$years[2], by=1))+
       scale_y_continuous(name="Effect")


     occ_ggplot<-ggplotly(
       res,

       #      dynamicTicks=TRUE,
       tooltip=c("text")
     )%>%
       layout(hovermode = "x unified")


     occ_ggplot<-occ_ggplot %>%
       animation_button(
         x = 0, xanchor = "right", y = 0, yanchor = "bottom"
       )

     return(occ_ggplot)

     #plot(mtcars$wt, mtcars$mpg)
   })


#_______________________
   #tab3
   #state
   output$state_plot<-renderPlotly({

     fig<-employ_state%>%filter(state=="base")
     fig%>%

       test2<-employ_sa4_geo%>%filter(
         # var %in% c("Sheep and Beef cattle", "Grains production")
         year==2022 &
           state=="base"
       )%>%
       filter(
         state_au=="NSW"
       )


}
  )

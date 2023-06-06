#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(bslib)
library(thematic)
library(showtext)
library(patchwork)
library(htmlwidgets)
library(glue)
library(showtext) # Needed for custom font support
# Setup the bslib theme object



library(RColorBrewer)
library(sf)
library(leaflet)


source('helpers.R')

my_theme <- bs_theme(
  bootswatch = "cerulean",
  base_font = font_google("Roboto")
                     )%>%
  bs_add_rules(sass::sass_file("styles.scss"))

# Let thematic know to use the font from bs_lib
thematic_shiny(font = "auto")

#---------
#load data

employ<-read_csv("data/employ_new.csv",show_col_types = FALSE) %>%
  filter(year<2051)%>%
  mutate(
    value=round(value, 1)
  )

#tab 2 - occupation - australia
occupation<-read_csv("data/occupation_new.csv",show_col_types = FALSE)%>%
filter(year<2051)%>%
  mutate(
    value=round(value, 1)
  )


#tab 3 - state - australia
#employ_state<-read_csv("data/employ_state_new.csv")%>%filter(year==2030 | year==2050)%>%
#  pivot_wider(names_from=state, values_from=value)

#tab 4 Region #Explore the impact on 85 regions across Australia. = sa4
employ_state<-read_csv("data/sa4_transform_new3.csv", show_col_types = FALSE)%>%
  mutate(
    sa4_code_2016 = as.character(sa4_code_2016)
    )%>%filter(year==2030 | year==2050)#%>%
  #  pivot_wider(names_from=state, values_from=value)


#-----------------

#-----ui/server var
colors <- RColorBrewer::brewer.pal(3, "Paired")[c(1, 2)]
names(colors) <- c("base", "zero")

xaxis <- list(title = "",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 5,
              tickfont = list(family = 'Arial',
                              size = 12,
                              color = 'rgb(82, 82, 82)'))

yaxis <- list(title = "Employment (‘000s)",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE)



#-----------------
#-----------------

# Define UI for application that draws a histogram
ui <- fluidPage(

  #theme
  theme = my_theme,

  title = "Net Zero in Australia",

  div(id = "page-top",
  #    radioButtons("current_theme", "App Theme:", c("Light" = "cerulean", "Dark" = "darkly"), inline = TRUE),
  #    div(class = "source_link", a(href = "https://github.com/rstudio/shiny_app_stories/blob/master/weather_lookup/", "View source code on github", icon("github"))),
  ),

  div(class = "page-header",
    img(src='img/cops.jpg', height=150),
    img(src='img/vu-mitchell.jpg', height=90)
  ),
  div(
    class = "hero",
    div(
      class = "container-fluid",
      h1("Net Zero in Australia")
    )
  ),
  h2("Summary"),
  p("
    This application helps explore the impact of net zero on employment, industries, occupations, and regions.
    It shows two scenarios.
    The first is the base-case, which means Australia doesn’t reduce its emissions.
    The second is a net-zero scenario, where Australia transitions to net zero emissions by 2050.
    The results show that all regions will continue to grow in a net zero scenario, but growth rates change.
  "),
  p("Click on the icons below to explore the data."),

  div(class = "net-zero", tabsetPanel(
    tabPanel(
      "About",


    p("
    The transition to a net zero economy is one of the most important challenges facing Australia. It is a transition that Australia must make."),

    p("Research undertaken by

     ", HTML("<a href='https://www.vu.edu.au/centre-of-policy-studies-cops/about-the-centre'>Victoria University’s Centre of Policy studies</a>"),

      "shows the impact of the transition to net zero on industries, jobs and regions. This research modelled two scenarios."),

p("The first, a ‘business as usual’ scenario where Australia continues to rely on fossil fuels and does not reduce its emissions."),

      p("The second models a scenario where Australia commits to net zero emissions by 2050."),

p("This research shows that all regions in Australia will continue to grow in a transition to net zero emissions."),

        p("What does change are growth patterns. There are certain industries and regions that are most affected. It is these regions where

        ", HTML("<a href='https://www.vu.edu.au/mitchell-institute'>Victoria University’s Mitchell Institute</a>"),
      "recommend a special focus to mitigate the impacts of change.
        "),
      # put outputs here
      # ...
      icon = icon("globe")
    ), #tabPanel


    tabPanel(
      "Industry",
      h3("Explore the impact on your industry"),
      div(
        class = "input-selector",
        # industry selection - 2 boxes
       # checkboxGroupInput(
      #  inputId = "base_ind",
      #    label = ("Scenario"),
      #    choiceValues = list("base", "zero"),
      #    choiceNames = list("Business as usual", "Net zero emissions"),
      #    selected = c("base", "zero"),
      #    inline = TRUE
      #),
        # sector
        selectizeInput(
          "select_industry",
          "Select sector",
          choices = c(sort(unique(employ$industry_group))),
          multiple = FALSE,
          selected = 'Sector 10 - Information media & telecommunications (J)'
        ),
        # output - industry selection
        uiOutput("select_industry_group")
      ),
      div(
 #       style = 'display:flex',
      # output - plot
        fluidRow(
          actionButton("anim0", "Animate both plots"),
        ),
        fluidRow(
          column(8, plotlyOutput("employ1_plot2")),
          column(4, plotlyOutput("employ1_plot_bar"))
        )
      ),
      icon = icon("industry")
    ), #tabPanel

    tabPanel(
      "Occupation",
      h3("Explore the impact on your occupation"),
      div(
        class = "input-selector",
      #  checkboxGroupInput(
       #   inputId = "base_occ",
      #    label = ("Scenario"),
       #   choiceValues = list("base", "zero"),
       #   choiceNames = list("Business as usual", "Net zero emissions"),
       #   selected = c("base", "zero"),
      #    inline = TRUE
     #   ),
     fluidRow(
              selectizeInput(
          "select_occupation",
          "Occupation groups",
          choices = c(sort(unique(occupation$occupation_group))),
          multiple = FALSE,
          selected = 'O1 MANAGERS'
        )
        ),
     fluidRow(
        uiOutput("select_occupation_group")
      )
     ),
     fluidRow(
       actionButton("anim", "Animate both plots"),
       ),
     fluidRow(
              column(8, plotlyOutput("occ_plot")),
              column(4,plotlyOutput("occ_lot_bar"))
      ),

      icon = icon("user-doctor")
    ), #tabPanel

    tabPanel(
      "Region",
      h3("Explore the impact on Australia’s regions"),
      div(
        class = "input-selector",
        radioButtons(
          'year_map',
          "Year:",
          c("2030" = "2030",
            "2050" = "2050"
        )
        ),
      leafletOutput("map_state")
      ),
      icon = icon("map")
      )
    ), #tabPanel

  ) #tabsetPanel
#  ) #container

)


#--------
#SERVER

server <- function(input, output, session) {

  #select industry
  #reactive data set





  output$select_industry_group <- renderUI({

    # check whether user wants to filter by cyl;
    # if not, then filter by selection
    if ("All industry groups" %in% input$select_industry) {
      dat <- employ
    } else {
      dat <- employ %>%
        filter(industry_group %in% input$select_industry)
    }

    # get available industry values
    group <- sort(unique(dat$var))

   # factor(unique(testdata$INJ_SVRTY_NEW), levels = possible_values)

    # render selectizeInput
    selectizeInput("select_industry_item", "Industry",
                   choices = group,
                   multiple = FALSE,
                   selected="Communication services")
  })


  output$employ1_plot2<-renderPlotly({

    req(input$select_industry_item)
    req(input$select_industry)

#    employ_ggplot<-employ%>%
 #     filter(var==input$select_industry_item, state %in% input$base_ind)

   employ_ggplot<-employ%>%filter(var %in% input$select_industry_item & industry_group==input$select_industry)

    res<-employ_ggplot%>%
      plot_ly(
        x=~year,
        #  frame=~year,
        y=~value,
        color=~state,
        colors = colors,
        hoverinfo = "text",
  #      type = 'scatter',
   #     mode = 'lines',
        showlegend = TRUE
      )%>%add_lines()

    #moving dot
    res<-res%>%add_trace(
      x=~year,
      frame=~year,
      y=~value,
      color=~state,
      colors = colors,
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      showlegend = FALSE
    )%>%
      add_trace(
        x=~year,
        y=~value,
        color=~state,
        colors = colors,
        hoverinfo = "text",
  #      type = 'scatter',
  #      mode = 'markers',
        showlegend = FALSE
      )%>%
      add_trace(
        x =2030,
        type = 'scatter',
        mode = 'lines',
        showlegend = FALSE,
        line = list(color = 'rgb(205, 12, 24)',
                    width = 1,
                    dash = 'dash'),
        name = '')%>%
      add_trace(
        x =2050,
        type = 'scatter',
        mode = 'lines',
        showlegend = FALSE,
        line = list(color = 'rgb(205, 12, 24)',
                    width = 1,
                    dash = 'dash'),
        name = '')


    res<-res%>% layout(title = "",
                       xaxis = xaxis,
                       yaxis = yaxis,
                       #width=1500,
                       # margin = margin,
                       autosize = TRUE)


    data_annot <- employ_ggplot[employ_ggplot$year %in% c(2030, 2050),]


    res <- add_trace(res, x=data_annot$year, y=data_annot$value, color=data_annot$state,
                     # colors="BrBG",
                     type="scatter", mode="markers",
                     text = paste(round(data_annot$value, 1)),
                     #     name="",
                     textposition = "top right",
                     showlegend = FALSE)


    res%>%
      animation_button(visible = FALSE) %>%
      onRender("
        function(el,x){
          $('#anim0').on('click', function(){Plotly.animate(el);});
        }")

  })


  #bar plot same page

  output$employ1_plot_bar<-renderPlotly({

    req(input$select_industry_item)
    req(input$select_industry)

 #   employ_ggplot<-employ_subset()

    employ_bar<-employ%>%
           filter(var==input$select_industry_item)%>%
      pivot_wider(
        names_from=state,
        values_from = value
      )%>%
      mutate(
        offset=zero-base
      )

    employ_bar%>%
      plot_ly(
        x=~var,
        y=~offset,
        frame=~year,
        #y=~value,
        #color=~state,
        #colors = colors,
        #hoverinfo = "text",
        type = 'bar',
        width = 0.5,
        textposition = "none",
        showlegend = FALSE)%>%
      layout(yaxis = list(title = "Employment difference between net zero <br> and business as usual (‘000s)", title.size=0.8),
             xaxis= list(title = ~var)
      )%>%
      animation_button(visible = FALSE) %>%
      onRender("
        function(el,x){
          $('#anim0').on('click', function(){Plotly.animate(el);});
        }")


  })

  output$employ1_plot_groups<-renderPlotly({
    req(input$select_industry)
    req(input$select_industry_item)

    employ_group<-employ%>%
      filter(industry_group==input$select_industry)

    employ_group%>%
      plot_ly(
        x=~year,
        y=~value,
        frame=~year,
        shape=~industry_group,
        #y=~value,
        color=~var,
        size=~value,
        #colors = colors,
        #hoverinfo = "text",
        mode='markers',
        #  width = 0.5,
        #  textposition = "none",
        showlegend = TRUE
        )%>%
      layout(yaxis = list(title = 'Employment (‘000s) in industry group'),
             xaxis= list(title = "Year")
      )


  })

  #tab2

  #select occupation
  output$select_occupation_group <- renderUI({

    # check whether user wants to filter by cyl;
    # if not, then filter by selection
    if ("All occupation groups" %in% input$select_occupation) {
      dat <- occupation
    } else {
      dat <- occupation %>%
        filter(occupation_group %in% input$select_occupation)
    }

    # get available carb values
    group <- sort(unique(dat$var))

    # render selectizeInput
    selectizeInput("select_occupation_item", "Occupation",
                   choices = c(group),
                   multiple = FALSE,
                   selected="Retail Managers")
  })


  #occupation - plot
  output$occ_plot<-renderPlotly({
    req(input$select_occupation_item)
    req(input$select_occupation)

    #    employ_ggplot<-employ%>%
    #     filter(var==input$select_industry_item, state %in% input$base_ind)

    employ_ggplot<-occupation%>%filter(var %in% input$select_occupation_item & occupation_group==input$select_occupation)

    res<-employ_ggplot%>%
      plot_ly(
        x=~year,
        #  frame=~year,
        y=~value,
        color=~state,
        colors = colors,
        hoverinfo = "text",
        #      type = 'scatter',
        #     mode = 'lines',
        showlegend = TRUE
      )%>%add_lines()

    #moving dot
    res<-res%>%add_trace(
      x=~year,
      frame=~year,
      y=~value,
      color=~state,
      colors = colors,
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      showlegend = FALSE
    )%>%
      add_trace(
        x=~year,
        y=~value,
        color=~state,
        colors = colors,
        hoverinfo = "text",
        #      type = 'scatter',
        #      mode = 'markers',
        showlegend = FALSE
      )%>%
      add_trace(
        x =2030,
        type = 'scatter',
        mode = 'lines',
        showlegend = FALSE,
        line = list(color = 'rgb(205, 12, 24)',
                    width = 1,
                    dash = 'dash'),
        name = '')%>%
      add_trace(
        x =2050,
        type = 'scatter',
        mode = 'lines',
        showlegend = FALSE,
        line = list(color = 'rgb(205, 12, 24)',
                    width = 1,
                    dash = 'dash'),
        name = '')


    res<-res%>% layout(title = "",
                       xaxis = xaxis,
                       yaxis = yaxis,
                       #width=1500,
                       # margin = margin,
                       autosize = TRUE)


    data_annot <- employ_ggplot[employ_ggplot$year %in% c(2030, 2050),]


    res <- add_trace(res, x=data_annot$year, y=data_annot$value, color=data_annot$state,
                     # colors="BrBG",
                     type="scatter", mode="markers",
                     text = paste(round(data_annot$value, 1)),
                     #     name="",
                     textposition = "top right",
                     showlegend = FALSE)


    res%>%
      animation_button(visible = FALSE) %>%
      onRender("
        function(el,x){
          $('#anim').on('click', function(){Plotly.animate(el);});
        }")
  })


  #bar plot - 2 occupation

  output$occ_lot_bar<-renderPlotly({

    req(input$select_occupation_item)
    req(input$select_occupation)


  occ_bar<-occupation%>%
    filter(var==input$select_occupation_item)%>%
    pivot_wider(
      names_from=state,
      values_from = value
    )%>%
    mutate(
      offset=zero-base
    )

  occ_bar%>%
    plot_ly(
      x=~var,
      y=~offset,
      frame=~year,
      #y=~value,
      #color=~state,
      #colors = colors,
      #hoverinfo = "text",
      type = 'bar',
      width = 0.5,
      textposition = "none",
      showlegend = FALSE)%>%
    layout(yaxis = list(title = 'Employment difference between net zero <br> and business as usual (‘000s)'),
           xaxis= list(title = ~var)
    )%>%
    animation_button(visible = FALSE) %>%
    onRender("
        function(el,x){
          $('#anim').on('click', function(){Plotly.animate(el);});
        }")

})

  #_______________________
  #tab3
  #state

  map_df_state= reactive({
    #change to state
employ_state %>%
    filter(year==parse_number(input$year_map)) %>%
      left_join(absmapsdata::sa42016)%>%
      mutate(
        base=round(base, 1),
        zero=round(zero, 1),
      )%>%
      st_as_sf(sf_column_name="geometry")

  })


  output$map_state <- renderLeaflet({

    qpal <- colorQuantile("Greens", employ_state$diff, n = 7)
 #   pal <- colorNumeric("Greens", domain = employ_state$diff) ## create a color palette

    leaflet(map_df_state(), options = leafletOptions(zoomControl = FALSE)) %>%
           addProviderTiles("CartoDB.Positron") %>%
      addTiles() %>%
      addPolygons(data=map_df_state(),

                #  fillColor = "grey70",
                fillColor = ~qpal(diff),
                  weight = 2,
                  opacity = 1,
                 color = "white",
              #    color = ~qpal(base),
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),


                  popup = ~ paste(
                    "Year:", year, "<br/>",
                    "Region:", var, "<br/>",
                    "<br/>",
                    "<b> Total employment (‘000s)</b> <br/>",
                    "Business as usual:", base, "<br/>",
                    "Net zero:", zero,"<br/>",
                    "Current employment:", state2023))%>%
      setView(133.88032299669788, -23.6981557160157, zoom = 3.5)%>%
      addLegend("topright",
           pal = qpal,
                title = "Total employment growth in transition to net zero",
              #  labFormat = labelFormat(prefix = "$"),
              values=~diff,
           opacity = 1

      )

  })

  #_______________________
  #tab4
  #state
  map_df = reactive({
    employ_state %>%
      filter(year==parse_number(input$year_map)) %>%
      left_join(absmapsdata::sa42016)%>%
    #  mutate(
    #    base=round(value, 1)
    #  )%>%
      st_as_sf(sf_column_name="geometry")

  })


  output$map <- renderLeaflet({
    leaflet() %>%
      #     addProviderTiles("CartoDB.Positron") %>%
      addTiles() %>%
      addPolygons(data=map_df(),
                  fillColor = "grey70",
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),

                  popup = ~ paste(
                    "Year:", year, "<br/>",
                    "Region:", sa4_name_2016, "<br/>",
                    "Business as usual:", base, "<br/>",
                    "Net zero:", zero
                    )
                  )%>%
      setView(133.88032299669788, -23.6981557160157, zoom = 3.5)

  })


  #----------

#change themes
  observe({
    # Make sure theme is kept current with desired
    session$setCurrentTheme(
      bs_theme_update(my_theme, bootswatch = input$current_theme)
    )
  })
}

shinyApp(ui, server)

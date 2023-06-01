test<-employ%>%
  filter(var=="Sheep and Beef cattle")%>%
  pivot_wider(
    names_from=state,
    values_from = value
  )%>%
  mutate(
    offset=base-zero
  )


test%>%
  plot_ly(
    x=var,
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
  layout(yaxis = list(title = 'Business as usual$ - Net zero$'),
              xaxis= list(title = ~var)
  )


employ_group<-employ%>%
  filter(industry_group=="Sector 1 - Agriculture, forestry and fishing (A)")

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
  #  type = 'marker',
    #  width = 0.5,
    #  textposition = "none",
    showlegend = FALSE)%>%
  layout(yaxis = list(title = 'Effect in $'),
         xaxis= list(title = "Year")
  )


#-------------
employ_ggplot<-employ%>%filter(var =="Sheep and Beef cattle")


fig<-test%>%ggplot(aes(
    year, value
   ))+geom_line()


fig <- plot_ly(test, x = ~year, y = ~value)%>%add_lines()

res<-test%>%
  plot_ly(
    x=~year,
    #  frame=~year,
    y=~value,
 #   color=~state,
  #  colors = colors,
 #   hoverinfo = "text",
   type = 'scatter',
    mode = 'lines',
  #  showlegend = TRUE
  )

test%>%ggplot(aes(
  year, value
))+geom_line()

test<-employ_ggplot%>% filter(state=="base")

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
    type = 'scatter',
    mode = 'markers',
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
                 text = paste(data_annot$value, " $mln"),
                 #     name="",
                 textposition = "top right",
                 showlegend = FALSE)


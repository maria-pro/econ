library(plotly)

df <- employ
fig <- df %>%
  plot_ly(
    x = ~year,
    y = ~value,
  #  size = ~pop,
    color = ~var,
    frame = ~year,
    text = ~value,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )
fig <- fig %>% layout(
  xaxis = list(
    type = "log"
  )
)

fig


ggplot()+
  geom_point(
    aes(x=year, y=value, group=interaction(state, var),
        colour=state, shape=var,
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


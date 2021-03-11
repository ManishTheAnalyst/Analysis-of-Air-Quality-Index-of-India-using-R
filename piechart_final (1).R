# multiple graphs combines code _ piechart, treemap, animation scatterplot
library(plotrix)
ab<-read.csv("piechartfinal.csv")
View(ab)
slices <- ab$percent
lbls <- ab$means
pie3D(slices,labels=lbls,explode=0.1,
      main="Sources of Air Pollution")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Sources of Air Pollution")

shrmp<-read.csv("CleanedAQI_removedNA.csv")
View(shrmp)
#-----------------------------------------------------------------------------------------
library(highcharter) 
library(dplyr)
library(viridisLite)
library(treemap)
library(RColorBrewer)
mytree<-treemap(shrmp,index=c("type", "state"), 
                vSize="so2", 
                vColor="so2", 
                fontsize.labels=c(24,11),
                type="value",fun.aggregate = "sum" ,
                palette=" RdYlBu", 
                title="Indian States V.S Sulphur Dioxide")
mytree %>% hctreemap(allowDrillToNode =
                       TRUE, layoutAlgorithm = "squarified")%>% 
  hc_tooltip(pointFormat = "{point.name}, <br> with {point.value}")




#------------------------------------------------------------------------------------------
library(highcharter) 
library(dplyr)
library(viridisLite) 
library(treemap) 
library(RColorBrewer)

hctreemap2(data = shrmp,
           group_vars = c("state"),
           size_var = "so2",
           color_var = "no2",
           layoutAlgorithm = "squarified",
           levelIsConstant = FALSE,
           levels = list(
             list(level = 1, dataLabels = list(enabled = TRUE)),
             list(level = 2, dataLabels = list(enabled = FALSE)),
             list(level = 3, dataLabels = list(enabled = FALSE))
           )) %>%
  hc_colorAxis(minColor = brewer.pal(7, "Greens")[1],
               maxColor = brewer.pal(7, "Greens")[7]) %>% hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                        SO2: {point.value:,.0f}<br>
                        No2: {point.colorValue:,.0f}")

#-------------------------------------------------------------------------------------
library(plotly)
plot_ly(shrmp, y =~state , x=~rspm, color = ~type, type = "box")

#-------------------------------------------------------------------------------------
library(plotly)
library(gapminder)
df <- shrmp
fig <- df %>%
  plot_ly(
    x = ~so2,
    y = ~no2,
    color = ~type,
    frame = ~date,
    text = ~state,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers')
fig <- fig %>% layout(
  xaxis = list(
    type = "log" ))
fig <- fig %>% animation_opts(
  1000, easing = "elastic", redraw = FALSE)

fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom")

fig <- fig %>% 
  animation_slider(currentvalue = list(prefix = "Date ", font = list(color="black")))
fig

#------------------------------------------------------------------------------------
library(NMF)
library(dplyr) 
library(d3heatmap)
qa<-read.csv("Mrate.csv")
View(qa)
row.names(qa)<-qa$States
RdEATH<- select(qa,'X2009','X2010','X2011','X2012','X2013','X2014') 
R_matrix<-data.matrix(RdEATH)
d3heatmap(R_matrix, col=brewer.pal(9,"Reds"),scale = "column", title="Heatmap for Respiratory Death State of India 2009-2014")
#-----------------------------------------------------------------------------------
# Chart Effects of pollution on Temperature and Ozone Layer
dat <- airquality
Sys.setenv(TZ = "EST")

apply(apply(dat, 2, is.na), 2, sum)
for (i in 1:ncol(dat)) {
  if(sum(is.na(dat[, i])) > 0) {
    temp <- median(dat[, i], na.rm = TRUE)
    dat[is.na(dat[, i]), i] <- temp
  }
}
dat$Year <- as.integer(2010)
dat$Dt <- paste(dat$Month , dat$Day , dat$Year, sep = ".")
library(lubridate)
dat$Dt <- mdy(dat$Dt)

library(tidyr)
dat <- gather(dat, index, measure, 1:4)

library(plotly)
with(dat, plot_ly(x = Dt, y = measure, mode = "lines",xlab="Months with highest population",ylab="Level of hike", color = index))




















load("covid.RData")
data = us[[2]]

################################################################################
# setting up data
# displayNames
displayNames = sapply(data, function(x) x$display_name)
# geoid 
geoid = sapply(data, function(x) x$geoid)
geoid.Cal = geoid[which(displayNames == "California")] # california geoid
# hierarchy
hierarchy = sapply(data, function(x) x$hierarchy)

# get counties in california
calcounty.ind = which(sapply(hierarchy, function(x){
  x[3] == geoid.Cal
}))
calcounty.data = data[calcounty.ind]
calcounty.names = sapply(calcounty.data, function(x) x$display_name)
calcounty.data = calcounty.data[which(calcounty.names != "Unknown")]

# create data frame for each county
covidData = function(county){
  name = county$display_name
  fullName = county$long_name
  pop = county$population
  # get vectors of dates, total confirmed and death cases, daily confirmed and death cases
  dateRange = county$range
  dates = seq(as.Date(dateRange[1]), as.Date(dateRange[2]), "day") # date vector
  cumCase = county$cases # cum. cases vector
  cumDeath = county$deaths # cum. deaths vector
  dailyCase = c(cumCase[1]) # daily cases vector
  dailyDeath = c(cumDeath[1]) # daily deaths vector
  for (i in 2:length(dates)){
    dailyCase = c(dailyCase, cumCase[i] - cumCase[i-1])
    dailyDeath = c(dailyDeath, cumDeath[i] - cumDeath[i-1])
  }
  result = data.frame(`County` = rep(name, length(dates)),
                      `FullName` = rep(fullName, length(dates)),
                      `Date` = dates,
                      `DailyCases` = dailyCase,
                      `CumCases` = cumCase,
                      `DailyDeaths` = dailyDeath,
                      `CumDeaths` = cumDeath,
                      `PercentCasesPerCapita` = dailyCase/pop*100,
                      `PercentDeathsPerCapita` = dailyDeath/pop*100,
                      `Population` = pop)
  return (result)
}

# get data frame for all counties
covidDataByCounty = lapply(calcounty.data, covidData)
# combine data frames
covidDataCal = do.call("rbind", covidDataByCounty)
covidDataCal$County = as.factor(covidDataCal$County)

######################################################################
# interactive trend plot
# make ggplot
library(ggplot2)
library(plotly)
library(dplyr)
trendPlot = covidDataCal %>%
  highlight_key(~County) %>%
  ggplot(aes(x = Date, y = DailyCases, color = County, group = 1,
             text = paste("County: ", FullName, 
                          "\nDate: ", Date,
                          "\nDaily Cases: ", DailyCases,
                          "\nCum. Cases: ", CumCases, 
                          "\nDaily Deaths: ", DailyDeaths,
                          "\nCum. Deaths: ", CumDeaths,
                          "\n% Cases/Capita (Day): ", round(PercentCasesPerCapita, 4),
                          "\n% Deaths/Capita (Day):", round(PercentDeathsPerCapita, 4),sep = ""))) +
  geom_line() + 
  ggtitle("Covid-19 Cases in California by County") + ylab("Daily Cases")
trendPlot

# make plotly
trendPlot.plotly = ggplotly(trendPlot, tooltip = "text", dynamicTicks = TRUE) %>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick", 
            persistent = getOption("persistent", TRUE)) %>%
  layout(updatemenus = list(
    list(type = "buttons",
         direction = "right",
         xanchor = "left",
         yanchor = "top",
         showactive = FALSE,
         x = 1,
         y = 1.2,
         buttons = list(
           list(method = "restyle",
                args = list("visible", "all"),
                label = "show all"),
           list(method = "restyle",
                args = list("visible", "legendonly"),
                label = "hide all")
         )
    )
  ))
trendPlot.plotly$sizingPolicy$browser$fill = FALSE
trendPlot.plotly

# save plot as html widget
library(htmlwidgets)
saveWidget(trendPlot.plotly, "covidTrend.html", selfcontained = FALSE, title = "Covid-19 Trend in California")

################################################################
# interactive covid map
library(maps)
# join covidDataCal with mapCounty
calCountyMap = map_data('county', 'california')
calCountyMap$subregion = stringr::str_to_title(calCountyMap$subregion)
plotData = inner_join(calCountyMap, covidDataCal, by = c("subregion" = "County"))
# make the ggplot
options(scipen = 10)
plotData$Date = as.factor(plotData$Date)
plotData$casesPer10k = plotData$CumCases/plotData$Population*10000
covidMap = ggplot(data = plotData, aes(frame = Date, 
                                      text = paste("County: ", FullName, 
                                                "\nDate: ", Date,
                                                "\nCase/10k Pop.: ", casesPer10k,
                                                "\nDaily Cases: ", DailyCases,
                                                "\nCum. Cases: ", CumCases, 
                                                "\nDaily Deaths: ", DailyDeaths,
                                                "\nCum. Deaths: ", CumDeaths,
                                                "\n% Cases/Capita (Day): ", round(PercentCasesPerCapita, 4),
                                                "\n% Deaths/Capita (Day):", round(PercentDeathsPerCapita, 4),sep = ""))) + 
  geom_polygon(aes(x=long, y = lat, fill=casesPer10k, group = group), color="black") + 
  geom_text(aes(y=40, x=-116, label=as.Date(Date)), check_overlap = TRUE, size=4, fontface="bold") +
  lims(x = c(-125, -114), y = c(32,42.5)) +
  scale_fill_gradientn(colors = rev(heat.colors(10)), na.value="grey80") +
  labs(title="Rate of COVID-19 Cases", x = "longitude", y = "latitude", fill = "Cases Per 10k Pop.")
# make interactive plot using plotly
covidMap.plotly = ggplotly(covidMap, tooltip = "text")
covidMap.plotly
saveWidget(covidMap.plotly, "covidMap.html", selfcontained = FALSE, title = "Covid-19 Cases by California Counties")

#######################################################################
# gif of covid map
library(gganimate)
library(gifski)
animatedMap <- covidMap +
  transition_manual(Date)
# get plot in GIF
mapGIF = animate(animatedMap, duration = 10, fps = 30, width = 500, height = 500, renderer = gifski_renderer())
# display gif
mapGIF
# save gif
anim_save("covidCal.gif", animation=mapGIF)


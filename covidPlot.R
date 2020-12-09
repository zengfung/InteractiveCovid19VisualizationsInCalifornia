load("covid.RData")
data = us[[2]]

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

# make ggplot
library(ggplot2)
library(plotly)
library(dplyr)
plt = ggplot(covidDataCal, aes(x = Date, y = DailyCases, color = County, group = 1,
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
plt

# make plotly
pltly = ggplotly(plt, tooltip = "text")
# pltly$sizingPolicy$browser$fill = FALSE
pltly

# save plot as html widget
library(htmlwidgets)
saveWidget(pltly, "covidTrend.html", selfcontained = FALSE)

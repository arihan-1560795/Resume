library(dplyr)
library(gdata)
library(plotly)

setwd("~/GitHub/a6-mapping-shootings-arihan-1560795")
shootings.df <- read.csv('data/shootings-2016.csv', stringsAsFactors = FALSE)

#The following code answers the "Summary information" section of the assignment
total.count <- nrow(shootings.df)
total.dead <- sum(shootings.df$killed)
top.5.count <- tail(names(sort(table(shootings.df$city))), 5)

city.most.killings <- (aggregate(shootings.df$killed, by=list(Category = shootings.df$city), FUN=sum)) %>% arrange(desc(x))
top.5.dead <- head(city.most.killings, 5)


paste0("**Total number of shootings: **", total.count)
paste0("**Total lives lost in shootings: **", total.dead)
paste0("**Most affected cities (by number of shootouts): **", (paste(shQuote(top.5.count, type="cmd"), collapse=", ")))
paste0("**Table of most affected cities by sum of people dead in shootouts**")
colnames(top.5.dead) <- c("City", "Total People Killed")
top.5.dead
#Summary Table showing the sum of people killed in shootings in each state
(aggregate(shootings.df$killed, by=list(Category = shootings.df$state), FUN=sum))

#Description of a particular incident
incident.worst <- shootings.df %>% filter(killed == max(killed))
paste0("On ", incident.worst$date, ", ", incident.worst$killed, " people were killed and ", 
       incident.worst$injured, "others injured in a shootout at a gay nightclub in ", incident.worst$city, 
       ", ", incident.worst$state, ". The gunman, Omar Mateen, 29, of Fort Pierce, Florida allegiance to ISIS
       in a phonecall he made to 911 during the shootout. Omar was interviewed by the FBI in 2013 but was
       not found to be a threat, and his wife claims his actions were a result of being bipolar. 
       After a 3 hour standoff, the police crashed into the building with an armored vehicle
       and stun grenades and killed Omar.")

paste0("Source: [CNN: Orlando shooting: 49 killed, shooter pledged ISIS allegiance](http://www.cnn.com/2016/06/12/us/orlando-nightclub-shooting/)")



################################

library(plotly)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

p <- plot_geo(shootings.df, lat = ~lat, lon = ~lng) %>%
  add_markers(
    text = ~paste0(city, ", ", state, "<br />", date, "<br />", paste("Killed:", killed), "<br />", paste("Injured:", injured)), 
    opacity = 1, symbol = I("circle"), hoverinfo = "text", color = "rgb(256, 0, 0)", size = I(as.numeric((shootings.df$killed + shootings.df$injured) * 10))
  ) %>%
  layout(
    title = 'Shootouts across the United States by incident<br />(Hover for details)', geo = g
  )

adf <- shootings.df
adf$date <- as.Date(adf$date, format="%B %d, %Y")
adf <- adf %>% mutate(killed + injured)
View(adf)
adf<- adf %>% group_by(pd.TimeGrouper(freq="M"))

View(adf %>% arrange(date))


p1 <- plot_ly(type = 'scatter',
  adf, x = ~date, y = ~(killed + injured),
  color = 'rgb(256,0,0)', size = ~I((killed + injured) * 10),
      text = ~paste0(city, ", ", state, "<br />", date, "<br />", paste("Killed:", killed), "<br />", paste("Injured:", injured)), 
      hoverinfo = "text"
    ) 
p1

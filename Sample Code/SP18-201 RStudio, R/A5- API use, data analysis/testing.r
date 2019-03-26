library(dplyr)
library(jsonlite)
library(httr)
library(knitr)
library(stringr)

zip.code <- '98105'
query.params <- list(zip=zip.code)


dist.url <- 'https://congress.api.sunlightfoundation.com/districts/locate'
dist.response <- GET(dist.url, query = query.params)
dist.body <- content(dist.response, "text")
dist.results <- fromJSON(dist.body)
dist.flattened <- flatten(dist.results$results)

legi.url <- 'https://congress.api.sunlightfoundation.com/legislators/locate'
legi.response <- GET(legi.url, query = query.params)
legi.body <- content(legi.response, "text")
legi.results <- fromJSON(legi.body)
legi.flattened.raw <- flatten(legi.results$results)
View(legi.flattened.raw)
legi.flattened <- legi.flattened.raw %>% select(first_name, last_name, title, party, chamber, phone, website, twitter_id)
legi.flattened <- legi.flattened %>% mutate(website = paste('[link](', website, ')', sep=""))
legi.flattened <- legi.flattened %>% mutate(twitter_id = paste('[link](https://twitter.com/', twitter_id, ')', sep=""))
legi.flattened <- rename(legi.flattened, twitter = twitter_id)
colnames(legi.flattened) <- gsub("_", " ", colnames(legi.flattened))
colnames(legi.flattened) <- str_to_title(colnames(legi.flattened))
View(legi.flattened)

CommitteesServed <- function(id){
  comm.url <- 'https://congress.api.sunlightfoundation.com/committees?'
  comm.query.params <- list(member_ids = id)
  comm.response <- GET(comm.url, query = comm.query.params)
  comm.body <- content(comm.response, "text")
  return(as.data.frame(fromJSON(comm.body)))
#  comm.results <- fromJSON(comm.body)
#  return(flatten(comm.results$results))
}

comm.by.mem <- lapply(legi.flattened.raw$bioguide_id, function(x) CommitteesServed(x))
View(comm.by.mem[[1]])
names <- paste(legi.flattened.raw$first_name, " ", legi.flattened.raw$last_name, sep = "")
no.comm <- sapply(comm.by.mem, NROW)

par(mar=c(4,7,2,1))
barplot(no.comm, main="Committees Served on by Each Rep", horiz=TRUE, names.arg=names, xlab = "# Committees", border=par(las=1))







#FOLLOWING CODE IS FOR In-depth Committee Information
#The following chunk of code provides an in-depth analysis on the first committee that is 
#not sub-committee for the first legislator for which this holds true. 

#The following snippet of code loops through all the legislators until it finds a committee
#they are in that is not a sub-committee.
i = 0
comm = NULL
while(is.null(comm)){
  i = i + 1
  comm <- comm.by.mem[[i]] %>% filter(results.subcommittee == FALSE) %>%  filter(row_number()==1)
}
#The following snippet of code creates a DataFrame of the legislators in the given non sub-commitee
#using the SunlightFoundation's congress API
comm.url <- 'https://congress.api.sunlightfoundation.com/committees?'
comm.query.params <- list(committee_id = comm$results.committee_id, fields = 'members')
comm.response <- GET(p.comm.url, query = p.comm.query.params)
body <- fromJSON(content(p.comm.response,"text"))$results$members[[1]] %>% flatten()

#Finds who is the chair of the committee (first and last name)
chair.df <- body %>% filter(title == 'Chair') %>% select(legislator.first_name, legislator.last_name)
chair.name <- paste(chair.df$legislator.first_name, " ", chair.df$legislator.last_name, sep = "")

#Finds many people are on the committee
p.n.comm <- nrow(body)

#Finds if the given representative is on the majority or minority side of the committee
id <- (legi.flattened.raw %>% filter(row_number()==i))$bioguide_id
side <- body %>% 
  filter(legislator.bioguide_id == id) %>% 
  select(side)

#Write a few sentences about what the committee does (this requires outside research)
#The U.S. House Committee on the Budget, commonly known as the House Budget Committee, is a standing committee of the United States House of Representatives. Its responsibilities include legislative oversight of the federal budget process, reviewing all bills and resolutions on the budget, and monitoring agencies and programs funded outside of the budgetary process. 

#What is the gender distribution on the committee (%M, %F)
m.n.comm <- round(nrow(filter(body, legislator.gender == 'M')) * 100 / nrow(body), digits = 1)
f.n.comm <- round(nrow(filter(body, legislator.gender == 'F')) * 100 / nrow(body), digits = 1)

#Prints a paragraph about the information obtained above
paste0("Our representative ,**", rep.name, "** is a part of the **", comm$results.name, "**, and their party is on the ", side, " side.")
paste0("This committee is responsible forlegislative oversight on the federal budget, including its bills and resolutions.")
paste0(comm$results.name, " is chaired by **", chair.name, "**.")
paste0(comm$results.name, " has **", p.n.comm, "** people on the committee, of which **", m.n.comm, "%** are male and **", f.n.comm, "% **are female.")

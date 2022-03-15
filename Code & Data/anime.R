library(ggplot2)
library(ggradar)
library(plyr)
library(dplyr)
library(magrittr)
library(tidyr)
library(reshape2)

#Reading in my data set
anime <- read.csv("AnimeList.csv", header=T)

#My first question I want to visualize from my data set is "Which years since 2000 have the most high rated anime?"
#Filtering to only show anime with an average score over 8, the limit i deemed "High Rated"
#as well as only anime that have finished airing and scored by more than 200 people
#finally mutating a year column for analysis
anime_filtered <- anime %>% filter(scored_by >= 200, airing == "False", score >= 8.00)
anime_filtered %<>% mutate(Year = substr(aired_string, nchar(aired_string)-4+1, nchar(aired_string)))

#Taking all after the year 200 anime from my filtered list and totaling how many "High Rated"
#anime came out in those years to plot for my first question
TopAnimeByYear <- anime_filtered %>% group_by(Year) %>% filter(Year >= 2000) %>% 
                  dplyr::summarise(Year_Total = n())

#The plot for my first question "Which years since 2000 have the most high rated anime?"
AnimeByYear <- ggplot(TopAnimeByYear, aes(x=Year, y=Year_Total, fill=Year)) + geom_bar(stat = "identity")
AnimeByYear

#Sub-question for this is "For the top 5 years what are the top 10 genres within them?"
#To answer I group by Year and genre, filter to post 2000, separate out the rows for 
#each genre then summarise how many came out per year using summarise and n()
#Finally subset the data to the top 5 years, 2013 - 2017
GenresPerYear <- anime_filtered %>% group_by(Year, genre) %>% 
                 filter(Year >= 2000) %>% separate_rows(genre, sep=", ") %>%
                 group_by(Year, genre) %>% dplyr::summarise(Total_Genre = n()) %>% 
                 subset(Year >= 2013, Year <= 2017)

#I want to use a radar graph per year so make a function for plotting those radars
custom_radar <- function(df, title ="", colour=""){
                ggradar(df, base.size = 2, values.radar = c("0", "13", "26"), grid.min = 0, 
                grid.mid = 15, grid.max = 30, gridline.min.linetype = "solid",
                gridline.max.linetype = "solid", group.point.size = 5, group.colours = colour) + ggtitle(title)}
  
#Cleaning, Sub-setting and arranging data for each radar and plotting them
Radar13 <- GenresPerYear %>% subset(Year == 2013) %>% rename(group = Year) %>% 
           arrange(desc(Total_Genre)) %>% head(10) %>% dcast(group ~ genre) %>%
           custom_radar(title = "Top 10 Genres of 2013", colour = "#7f96ff")
Radar13                       

Radar14 <- GenresPerYear %>% subset(Year == 2014) %>% rename(group = Year) %>% 
           arrange(desc(Total_Genre)) %>% head(10) %>% dcast(group ~ genre) %>%
           custom_radar(title = "Top 10 Genres of 2014", colour = "#bc81ff")
Radar14

Radar15 <- GenresPerYear %>% subset(Year == 2015) %>% rename(group = Year) %>% 
           arrange(desc(Total_Genre)) %>% head(10) %>% dcast(group ~ genre) %>%
           custom_radar(title = "Top 10 Genres of 2015", colour = "#e26ef7")
Radar15

Radar16 <- GenresPerYear %>% subset(Year == 2016) %>% rename(group = Year) %>% 
           arrange(desc(Total_Genre)) %>% head(10) %>% dcast(group ~ genre) %>%
           custom_radar(title = "Top 10 Genres of 2016", colour = "#f863df")
Radar16

Radar17 <- GenresPerYear %>% subset(Year == 2017) %>% rename(group = Year) %>% 
           arrange(desc(Total_Genre)) %>% head(10) %>% dcast(group ~ genre) %>%
           custom_radar(title = "Top 10 Genres of 2017", colour = "#ff62bf")
Radar17

############################

#My second question is "Which of the source materials has the most high rated anime?"
#For this I use my anime_filtered sheet and group each anime by source and summarise them into a
#"source_total" column so i can plot them.
TopAnimeBySource <- anime_filtered %>% group_by(source) %>% filter(Year >= 2000) %>% 
  dplyr::summarise(source_total = n())

#The plot for "Which source material has the most high rated anime"
AnimeBySource <- ggplot(TopAnimeBySource, aes(x=source, y=source_total, fill=source)) + geom_bar(stat = "identity")
AnimeBySource+scale_fill_brewer(palette="Set3")

#Subquestion: "For the most popular source, Manga, which type of anime is most commonly high rated?"
#To start once again we use the anime_filtered sheet but this time group by type and filter that
#to only anime with the source Manga. 
#After that summarise to a column called "type_total" for plotting.
TopMangaByType <- anime_filtered %>% group_by(type) %>% filter(Year >= 2000, source == "Manga") %>% 
  dplyr::summarise(type_total = n())

#The plot for "For the most popular source, Manga, which type of anime is most commonly high rated?"
MangaBySource <- ggplot(TopMangaByType, aes(x=type, y=type_total, fill = type)) + geom_bar(stat = "identity")
MangaBySource+scale_fill_brewer(palette="Dark2")

#Subquestion: "What is the highest rating an anime has had for each source material?"
#Again starting with anime_filtered and grouping it by source, then filtering by down to the 
#desired year range.
#Then sorting by source and descending score, then using the sumamarise first function
#to thake the first score of each group of sources to get the highest rating per source.
TopAnimePerSource <- anime_filtered %>% group_by(source) %>% filter(Year >= 2000) %>% 
  arrange(source, desc(score)) %>% dplyr::summarise( score = first(score))

#The plot for "What is the highest rating an anime has had for each source material?"
TopRatedSource <- ggplot(TopAnimePerSource, aes(x=source, y=score, fill = source)) + geom_bar(stat = "identity")
TopRatedSource + coord_cartesian(ylim=c(8.0,10.0))+scale_fill_brewer(palette="Set3")

############################
#My third question is "Which years since 2000 have the most bad anime?"
#Will need a new base sheet similar to "anime_filtered" but for bad anime
#So filter with the same requirement of users scored and finished airing but 
#need a score above 0 and bellow 5
bad_anime_filtered <- anime %>% filter(scored_by >= 200, airing == "False", score > 0.00, score <= 5.00)
bad_anime_filtered %<>% mutate(Year = substr(aired_string, nchar(aired_string)-4+1, nchar(aired_string)))

#To get the anime per year do similar to initial question 1's transform to group them by year
#filter to after 2000s and total each year in a new column
TopBadAnimeByYear <- bad_anime_filtered %>% group_by(Year) %>% filter(Year >= 2000) %>% 
  dplyr::summarise(Year_Total = n())

#Plot for "Which years since 2000 have the most bad anime?"
BadAnimeByYear <- ggplot(TopBadAnimeByYear, aes(x=Year, y=Year_Total, fill=Year)) + geom_bar(stat = "identity")
BadAnimeByYear

#Sub-question: "Which genres have the most bad anime?"
#Need to separate rows for each individual genre tag for an anime then summarise them
#into 1 column to plot. Also filter over 2 data points to clear clutter from data set
BadGenresPerYear <- bad_anime_filtered %>% group_by(Year, genre) %>% filter(Year >= 2000) %>% 
  separate_rows(genre, sep=", ") %>% group_by(Year, genre) %>% dplyr::summarise(Total_Genre = n()) %>% 
  filter(Total_Genre > 2)

#Plot for "Which genres have the most bad anime?"
BadGenres <- ggplot(BadGenresPerYear, aes(x=genre, y=Total_Genre, fill=genre)) + geom_bar(stat = "identity")
BadGenres+scale_fill_brewer(palette="Set3")





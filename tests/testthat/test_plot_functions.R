context('Testing Plot Functions')
library(egvisualizations)
library(vdiffr)
myData <- readRDS('./Argentina-2019-01-25.RDS')

expect_doppelganger(title = 'egbarplotcount',
                    egbarplotcount(myData$news, "media_name", "..count..", "media_name","a","b", "test"))
# This plot is not testable with vdiffr
expect_doppelganger(title = 'egwordcloud',
                    {
                      set.seed(40)
                      a <- myData$news_wordcloud %>% group_by(word) %>% count()
                      egwordcloud(a$word %>% head(20), a$n %>% head(20))
                    })

expect_doppelganger(title = 'egdataseries',
                    egdataseries(myData$tweets, "created_at", "screen_name","Date","Count","Candidate")
)

expect_doppelganger(title = 'egdataseries2',
                    #Default options are set with Sys.date, so cannot be tested.
                    #For testing always use forceDatePeriod
                    egdataseries(myData$youtube_videos, "publication_date", NA,"Date","Count","Candidate",
                                 forceDatePeriod = c(as.Date('2018-10-10'),as.Date('2018-10-17')))
)

expect_doppelganger(title = 'egheatmap',
                    {a <- egdataanalysis::network_structure_relation_mentions(myData$entities, 
                                                                              myData$news_entities_mentions)
                    test <- a[[2]] %>% left_join(a[[1]] %>% select(id, title) %>% rename(totitle = title), by = c("to"="id")) %>% left_join(a[[1]] %>% select(id, title) %>% rename(fromtitle = title), by = c("from"="id"))
                    
                    egvisualizations::egheatmap(test, "totitle","fromtitle","length")}
)

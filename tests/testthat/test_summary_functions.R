context('Emotion Summary')
library(syuzhet)
library(syuzhetVis)

test_that('A summary is corrrectly produced ',{

  myData <- get_nrc_sentiment(spanish_tweets$text[1:10])
  test1 <- get_emotion_summary(myData)
  expect_equal(sum((names(test1)%in%get_nrc_dimensions())) , 8)
  expect_error(get_emotion_summary(data.frame()))
})

test_that('Grouped summary is correctly produced',{
  myData <- get_nrc_sentiment(spanish_tweets$text[1:20])
  test1 <- get_emotion_summary(myData)
  myData$group <- c(rep('Group 1',10),rep('Group 2',10))
  test2 <- get_emotion_summary(myData)

  expect_identical(test1,test2,
                   label = 'Identical DFs returned when called without grouping parameter')

  test3 <- get_emotion_summary(myData, grouping.var = 'group')
  expect_equal(sum((names(test3)%in%get_nrc_dimensions())) , 8)
  expect_error(get_emotion_summary(data.frame(),grouping.var = 'group'),
               label = 'Empty frame grouping.var specified')

})






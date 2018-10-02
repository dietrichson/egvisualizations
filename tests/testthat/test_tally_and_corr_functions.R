context('Tally')
library(syuzhet)
library(syuzhetVis)

test_that('A tally is returned when called correctly, and error when not (get_emotion_tally)',{

  myData <- get_nrc_sentiment(spanish_tweets$text[1:10])
  test1 <- get_emotion_tally(myData)
  expect_equal(sum(test1$variable%in%get_nrc_dimensions()),8)
  expect_error(get_emotion_tally(data.frame()))

}
)

test_that('Grouped Tally',{
  myData <- get_nrc_sentiment(spanish_tweets$text[1:20])
  test1 <- get_emotion_tally(myData)
  myData$group <- c(rep('Group 1',10),rep('Group 2',10))
  test2 <- get_emotion_tally(myData)
  
  expect_identical(test1,test2, 
                   label = 'Identical DFs returned when called without grouping parameter')
  
  expect_equal(sum(test1$variable%in%get_nrc_dimensions()), 8, 
               label = 'Test1 has eight columns corresponding to eight emotions')
  expect_equal(sum(test2$variable%in%get_nrc_dimensions()), 8,
               label = 'Test2 has eight columns corresponding to eight emotions')
  
  test3 <- get_emotion_tally(myData, grouping.var = 'group')
  
  expect_equal(sum(unique(test3$variable)%in%get_nrc_dimensions()), 8,
               label = 'Test3 has eight columns corresponding to eight emotions')
  
  expect_false(identical(test1,test3),
         label = 'Different DFs are returned when group called with grouping parameter')
})

context('get_emotion_correlation()')

test_that('A Correlation Matrix is returned or error thrown',{

  myData <- get_nrc_sentiment(spanish_tweets$text[1:10])
  test1 <- get_emotion_correlation(myData)
  expect_equal(class(test1),'matrix')
}


)

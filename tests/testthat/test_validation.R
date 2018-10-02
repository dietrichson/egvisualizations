context('Validation')
library(syuzhet)
test_that('Validation works for has_emotion_cols',{

  myData <- get_nrc_sentiment(spanish_tweets$text[1:10])
  expect_true(has_emotion_cols(myData))
  expect_false(has_emotion_cols(data.frame()))
}
)

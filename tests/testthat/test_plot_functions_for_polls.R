context('Testing Plot Functions for Polls - Timelines')
library(egvisualizations)
library(vdiffr)
library(dplyr)
myData <- readRDS('./poll_test_data.RDS')


expect_error(eg_poll_timeline())# Empty call

expect_error(eg_poll_timeline(myData %>% select(fullname))) #Missing columns

#Test the plot functions
expect_doppelganger('Timeline One Candidate Approval/Disapproval',
                      myData %>% 
                      dplyr::filter(shortname=='aBarnecheaG') %>% 
                      eg_poll_timeline()
                    )

expect_doppelganger('Timeline One Candidate One Variable',
                      myData %>% 
                      dplyr::filter(shortname=='aBarnecheaG',variable=='Approval') %>% 
                      eg_poll_timeline()
                    )

expect_doppelganger('Timeline All Candidates Approval/Disapproval',
                    myData %>% eg_poll_timeline()
)

expect_doppelganger('Timeline All Candidates Approval Only',
                    myData %>% dplyr::filter(variable=='Approval') %>% eg_poll_timeline()
)

# Test Boxplots
context('Testing Plot Functions for Polls - Boxplots')
expect_error(eg_poll_boxplot(),
             label = 'eg_poll_boxplot called empty.')# Empty call

expect_error(eg_poll_boxplot(myData %>% select(fullname)),
             label='eg_poll_boxplot called with missing columns.')

expect_doppelganger('Boxplot All Candidates Two Variables',
                    myData %>% eg_poll_boxplot()
                    )

expect_doppelganger('Boxplot All Candidates One Variable',
                    myData %>% dplyr::filter(variable=='Approval') %>% 
                      eg_poll_boxplot()
                    )

expect_doppelganger('Boxplot One Candidate Approval/Disapproval',
                      myData %>% 
                      dplyr::filter(shortname=='aBarnecheaG') %>% 
                      eg_poll_boxplot()
                    )

expect_doppelganger('Boxplot One Candidate One Variable',
                      myData %>% 
                      dplyr::filter(shortname=='aBarnecheaG',variable=='Approval') %>% 
                      eg_poll_boxplot()
                    )
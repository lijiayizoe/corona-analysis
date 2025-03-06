library(dplyr)
library(readr)
library(ggplot2)

misinfo_d<-read_csv('../data/life-under-lockdown-wave2-extract.csv')

knowledge_levels <- c(
  'Nothing at all',
  'Not very much',
  'A fair amount',
  'A great deal'
)

misinfo_d <- misinfo_d %>%
  mutate(
    know_facebook = know_facebook %>%
      ordered(
        levels = knowledge_levels
      )
  )

misinfo_d$gender %>% unique

misinfo_d$age %>% summary

misinfo_d$beliefs %>% summary

misinfo_d %>%
  ggplot(
    aes(x=age)
  )+
  geom_histogram(binwidth=1)

misinfo_d %>%
  ggplot(
    aes(x=gender)
  )+
  geom_bar()

misinfo_d %>%
  ggplot(
    aes(x=know_facebook)
  )+
  geom_bar()

misinfo_d %>%
  ggplot(
    aes(x=beliefs)
  )+
  geom_histogram(binwidth = 1)

misinfo_d %>%
  group_by(gender) %>%
  summarise(n=n(),M=mean(beliefs),Mdn=median(beliefs),SD=sd(beliefs))

misinfo_d %>%
  filter(gender !='In another way') %>%
  t.test(
    beliefs~gender,
    data=.
  )
  
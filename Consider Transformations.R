library(dplyr)
library(ggplot2)
library(dslabs)
ds_theme_set()
data.frame(candidate=c("Clinton","Trump"), electoral_votes = c(232, 306)) %>% 
  ggplot(aes(candidate, electoral_votes)) + 
  geom_bar(stat = "identity", width=0.5, color =1, fill = c("Blue","Red")) + 
  coord_cartesian(ylim=c(200,310)) + ylab("Electoral Votes") + xlab("") + 
  ggtitle("Result of Presidential Election 2016")

library(dplyr)
library(ggplot2)
library(dslabs)
library(gridExtra)
data(us_contagious_diseases)
p1 <- us_contagious_diseases %>% 
  filter(year == 1928 & disease=="Measles" & count>0 & !is.na(population)) %>% 
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("")
p2 <- us_contagious_diseases %>% 
  filter(year == 1928 & disease=="Measles" & count>0 & !is.na(population)) %>% 
  mutate(rate = count / population * 10000*52 / weeks_reporting) %>%
  mutate(state = reorder(state, rate)) %>%
  ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("")
grid.arrange(p1, p2, ncol = 2)
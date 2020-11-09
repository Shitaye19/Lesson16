
library(tidyverse)
library(gapminder)
library(gridExtra)

gapminder

str(gapminder$continent)

nlevels(gapminder$continent)

class(gapminder$continent)

levels(gapminder$continent)


gapminder %>% 
  count(continent) # it gives a s a label, use this for my chromosome markers

nlevels(gapminder$country)

h_countries <- c("Egypt", "Haiti", "Romania", "Thailand", "Veenzuela")

nlevels(h_countries)

h_gap <- gapminder %>% 
  
  filter(country%in% h_countries)


h_gap %>% 
  
  ggplot(aes(year, lifeExp, col = country)) +
  
  geom_line()


h_gap_dropped <- h_gap %>% 
  
droplevels()

nlevels(h_gap_dropped$country)
nlevels(h_gap_dropped$continent)



h_gap$country %>% 
fct_drop()



gapminder$continent %>% 
  levels()


gapminder %>% 
  count(continent)

gapminder$continent %>% 
  fct_infreq() %>% 
  fct_rev() %>% 
  levels()

p1 <-  gapminder %>% 
  
  ggplot(aes(x = continent))+
geom_bar()+
  coord_flip()


p1

p2 <-  gapminder %>% 
  
  ggplot(aes(x = fct_infreq(continent)))+
  geom_bar()+
  coord_flip()




p3 <-  gapminder %>% 
  mutate(continent = fct_infreq(continent)) %>% 
  
  ggplot(aes(x = continent))+
  geom_bar()+
  coord_flip()


gap_asia_2007 <- gapminder %>% 
  filter(year == 2007, continent == "Asia")


p1<-gap_asia_2007 %>% 
  ggplot(aes(x=lifeExp, y = country))+
  geom_point()


p2<-gap_asia_2007 %>% 
  ggplot(aes(x=lifeExp, y = fct_reorder(country,lifeExp)))+
    geom_point()


grid.arrange(p1,p2, nrow= 1)

##the second plot looks much easier to interpret.

gapminder$country %>% 
 levels()

gapminder$country %>% 
  levels() %>% 
  head()
  
  
fct_reorder(gapminder$country, gapminder$lifeExp,min)%>% 
  levels() %>% 
  head()
?fct_reorder


fct_reorder(gapminder$country, gapminder$lifeExp,.desc = TRUE)%>% 
  levels() %>% 
  head()




  
  
  
  
  
  
  
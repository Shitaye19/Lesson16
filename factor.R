
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
  
  filter(country%in% h_countries) %>% 
  droplevels()


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

gap_asia_2007_ordered <- gap_asia_2007 %>% 
  mutate(country = fct_reorder(country, gdpPercap)) %>% 
droplevels() # levles work with a single variable
  
levels(gap_asia_2007_ordered$country)

h_gap

p1 <-  ggplot(h_gap, aes(x = year, y = lifeExp, color = country))+
geom_line()


p2<- ggplot(h_gap, aes(x = year, y = lifeExp, color = fct_reorder2(country,year, lifeExp)))+
  
geom_line() +
  labs(color = "country") # this is to color legnend

grid.arrange(p1, p2, nrow = 1)


##reorder2 to relavel based on two varibles

h_gap$country %>%  levels()

h_gap$country %>% fct_relevel("Egypt")





####

gap_asia_2007 <- gapminder %>% 
  filter(year == 2007, continent == "Asia")

gap_asia_2007$country %>% 
  mutate(country = fct_relevel((fct_reorder(country, gdpPercap), Thiland))) # nested but the code is not working


## Check from the course and edit even the function part 1 I didn't finish the code I have to get it and edit


gap_asia_2007$country%>% 
  mutate(country = fct_relevel(fct_reorder(country,gdpPercap)))%>% 
           ggplot(aes(gdpPercap, fct_relevel(country, "Thailand")))+  ### not workign edit from the website
           geom_point()
         
  

 
 
 i_gap <- gapminder %>% 
   filter(country %in% c("United States", "Sweden", "Australia"))%>% 
   droplevels()
 
 i_gap$country %>%   
   fct_recode("USA" ="United States", "Oz" = "Australia") %>% levels()
      
      
      
      
      
  
  
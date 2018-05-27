---
title: "Project Report"
author: "Bron, Luca and Lapierre, Hadrien"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  html_document:
    toc: true
    toc_depth: 2
    number_sections: true
    theme: united
    highlight: tango
  pdf_document: default
---

# Introduction
## Overview and Motivation:

Last week the “Forum des 100” was held at the University of Lausanne. This year one of the key subject was mobility in Switzerland, and in particular its future. We therefore ask ourselves whether there is a certain trend in the evolution of practices (in terms of passenger transport in particular). We both changed our behavior regarding transportation over the years for various reasons: practical, economic, health, even personal reasons (ecology etc.) or because public authorities have encouraged the use of a particular mean of transport. Now that other means are under study or already in development (flying cars, drones, hyperloop, etc) we are curious to explore a bit the overall situation and behavioral changes of other users in recent years.

The project goal is to have a better understanding of transportation dynamics that exist in our country. We will try to identify correlations and other causal links.
Understanding market past & current behavior helps to define needs, risks and eventually solutions for the future. We believe that having this information in mind can help decision makers. 

## Related Work: 

Anything that inspired you, such as a paper, a website, or something we discussed in class.

####Initial Questions: 

What questions are you trying to answer? How did these questions evolve over the course of the project? What new questions did you consider in the course of your analysis?

- How has the development of public transport affected the use of private vehicles over the years? To what extent? 
- What is the link between the investments made in improving public transport networks and the fluctuations in their use? 



####Exploratory Analysis: 

What visualizations did you use to look at your data in different ways? Justify the decisions you made, and show any major changes to your ideas. How did you reach these conclusions?

####Modeling: 

What are the different statistical methods you considered? Why did you choose a given model? How about competing approaches?

####Final Analysis: 

What did you learn about the data? How did you answer the questions? How can you justify your answers?

###R init
```{r setup, echo = FALSE, warning = FALSE, message = FALSE}
#loading packages
library(knitr)
library(tidyverse)
library(lubridate)
library(modelr)
library(broom)
library(ggrepel)
library(forcats)
library(directlabels)
library(ggrepel)
library(readxl)
```

####Data

Source, scraping method, cleanup, etc.
```{r setup2, echo = FALSE, message = FALSE}
options(digits = 3) # set precision digits

#Init import
parc_tourisme <- read_csv(file = "../data/parc_tourisme_90.csv") %>% as.tibble()
mec_tourisme <- read_csv(file = "../data/mec_tourisme2.csv") %>% as.tibble()
parc_moto <- read_csv2(file = "../data/parc_moto.csv") %>% as.tibble()

#Growth rate calculations
parc_growth <- parc_tourisme %>% 
  transmute(Carburant = Carburant, "2000" = NA, "2001" = (X2001/X2000 - 1), "2002" = (X2002/X2001 - 1), "2003" = (X2003/X2002 - 1), "2004" = (X2004/X2003 - 1), "2005" = (X2005/X2004 - 1), "2006" = (X2006/X2005 - 1) , "2007" = (X2007/X2006 - 1), "2008" = (X2008/X2007 - 1), "2009" = (X2009/X2008 - 1), "2010" = (X2010/X2009 - 1), "2011" = (X2011/X2010 - 1), "2012" = (X2012/X2011 - 1), "2013" = (X2013/X2012 - 1), "2014" = (X2014/X2013 - 1), "2015" = (X2015/X2014 - 1), "2016" = (X2016/X2015 - 1), "2017" = (X2017/X2016 - 1))

#Growth rate formating
parc_growth <- parc_growth %>% gather(`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017` , key = "Annee", value = "T")
parc_growth <- parc_growth %>% mutate(Annee = as.numeric(Annee))

#mec_tourism formating (old R version)
# mec_tourisme <- mec_tourisme %>% gather(`X2005`, `X2006`, `X2007`, `X2008`, `X2009`, `X2010`, `X2011`, `X2012`, `X2013`, `X2014`, `X2015`, `X2016`, `X2017` , key = "Annee", value = "N")
# mec_tourisme <- mec_tourisme %>% mutate(Annee = substring(Annee,2))

#mec_tourism formating (new R version)
mec_tourisme <- mec_tourisme %>% gather(`2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017` , key = "Annee", value = "N")
mec_tourisme <- mec_tourisme %>% mutate(Annee = as.numeric(Annee))


#parc_tourism formating
parc_tourisme <- parc_tourisme %>% gather(`X2000`, `X2001`, `X2002`, `X2003`, `X2004`, `X2005`, `X2006`, `X2007`, `X2008`, `X2009`, `X2010`, `X2011`, `X2012`, `X2013`, `X2014`, `X2015`, `X2016`, `X2017`, key = "Annee", value = "N")
parc_tourisme <- parc_tourisme %>% mutate(Annee = substring(Annee,2))
parc_tourisme <- parc_tourisme %>% mutate(Annee = as.numeric(Annee))
# colnames(parc_tourisme)[3] <- "Number"

#parc_moto formating
parc_moto <- parc_moto %>% gather(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017` , key = "Annee", value = "N")
parc_moto <- parc_moto %>% mutate(Annee = as.numeric(Annee))
```

## Exploratory Analysis: 

To begin with, we looked passenger cars in Switzerland and particularly their fuel type. We see on the graph below that electric vehicles are gradually making their way onto Swiss roads. Even if the electric proportion in the total number of vehicles put into service remains relatively low, we can see from the following graph that there is indeed a trend towards this new mean of propulsion. The data are from the `r mec_tourisme %>% filter(Type == "Voitures de tourisme") %>% summarise(sum(N,na.rm = TRUE)) %>% kable(col.names = "Total passenger cars put into circulation between 2005 and 2017.")` cars  put into circulation between 2005 and 2017.

```{r echo = FALSE}
mec_tourisme %>% filter(Type == "Voitures de tourisme") %>% 
  ggplot() + geom_bar(aes(x=Annee,  y=N, fill=fct_reorder(Carburant,N)), position = "fill", stat="identity") +
  labs(title="Proportion of passenger cars put into circulation in Switzerland per fuel type",x="Annee", y="Proportion", fill="Carburant")+
  scale_fill_brewer(palette="Paired",labels=c("No engine","Electric-diesel","Other", "Electric","Gas","Electric-gasoline","Diesel", "Gasoline"))
```

While the growth rate of diesel and petrol cars have stagnated for several years, the growth rate of electric vehicles is relatively high since 2009 and demonstrates a booming market. Nevertheless, it should be noted that this very high variation (+60%) between 2012 and 2015 has tended to run out of steam since then for an increase of "only" 36% in 2017.

```{r echo = FALSE, warning = FALSE}
#Let's remove Inf data
parc_growth %>% filter(Carburant != "Autres")  %>% 
  ggplot(aes(x=Annee, y=T, color=Carburant, na.rm = TRUE)) + 
  geom_line() + scale_y_continuous(labels = scales::percent) + 
  labs(title="Passenger car fleet Growth per year and fuel type", x="Year", y="Growth Rate") + 
  geom_label_repel(data = . %>% filter(Annee == "2017") %>% mutate(T = round(T,2)), aes(x=Annee, y=T, label = T))+
  scale_color_discrete(labels=c("Diesel","Electric","Gasoline"))
```

Now let's take a look at the passenger transport vehicles (light automobiles, heavy automobiles, buses, minibuses) put into circulation in Switzerland for the last decade. It seems that 99% of the 50'000 vehicles put into circulation are diesel-powered. The data are from the `r mec_tourisme %>% filter(Type == "Vehicules de transport de personnes") %>% summarise(sum(N,na.rm = TRUE)) %>% kable(col.names = "Total passenger transport vehicles put into circulation between 2005 and 2017.")` cars  put into circulation between 2005 and 2017.

```{r echo = FALSE}
mec_tourisme %>% filter(Type == "Vehicules de transport de personnes") %>% 
  ggplot() + geom_bar(aes(x=Annee,  y=N, fill=fct_reorder(Carburant,N)), position = "fill", stat="identity") +
  labs(title="Proportion of passenger transport vehicles put into circulation in Switzerland per fuel type",x="Year", y="Proportion", fill="Fuel") +
  scale_fill_brewer(palette="Paired",labels=c("Electric-gasoline","No engine","Other", "Electric","Gas","Diesel-electric", "Gasoline","Diesel"))
```

Finally we will take a look at motorcycles :

```{r echo = FALSE} 
# parc_tourisme %>% aggregate(N, by=list(Annee=Annee), fun=SUM)
# parc_moto %>% filter(X1 == "Total_moto") %>% ggplot(aes(x=Annee, y=N)) + geom_line()
# 
# mec_tourisme %>% filter(Type == "Motocycles") %>% 
#   ggplot() + geom_bar(aes(x=Annee,  y=N, fill=fct_reorder(Carburant,N)), position = "fill", stat="identity") +
#   labs(title="Proportion of motocycles put into circulation in Switzerland per fuel type",x="Year", y="Proportion", fill="Fuel") +
#   scale_fill_brewer(palette="Paired")
```

# Difference between types of vehicules

Now that we have done some exploratory data analysis on the consumer choice of car engine, we will now look at the tradeoff between taking the car or other means of transportation.  

```{r echo=FALSE}
#Data preparation kilometer-vehicule
kil_vehic <- read_excel("../data/personnes_prestations.xlsx")%>% as.tibble()
kil_vehic <- kil_vehic %>% gather("Train", "Tram","Trolleybus","Autobus","Private_car","Private_bus","Motorbike","Moped", key = "Type", value = "Vehic_km") %>% mutate(Year = as.numeric(Year), Vehic_km = as.numeric(Vehic_km))
kil_vehic <- kil_vehic %>% filter(Year!=2017)

#Data preparation kilometer-person
kil_pers <- read_excel("../data/personnes_prestations2.xls")%>% as.tibble()
kil_pers <- kil_pers %>% gather("Train", "Tram","Trolleybus","Autobus","Private_car","Private_bus","Motorbike","Moped", key = "Type", value = "Pers_km") %>%
mutate(Year = as.numeric(Year), Pers_km = as.numeric(Pers_km))
kil_pers <- kil_pers %>% filter(Year!=2017)

kilometers <- kil_vehic %>% full_join(kil_pers) 
```

## Infrastructure and vehicule-kilometer

For this first analysis, we observe the amount of vehicule-kilometer which corresponds in billion to the number of kilometers traveled by each vehicule type.  
This statistic by itself is not the most interesting. It's like comparing apple to oranges since a car can contain one to six people whereas a train contains hundreds. The first graph show that private car is the main transport by far, and this needs to be taken with a grain of salt.  
However, this statistic is interesting when used to observe trends within each type. It is a good proxy of how the infrastructure and its ecological impact grow.

```{r echo = FALSE}
kilometers %>% 
  ggplot() +
    geom_area(aes(x=Year,  y=Vehic_km, fill=fct_reorder(Type,Vehic_km))) +
    labs(title="Number of kilometers per vehicule",x="Year", y="Proportion", fill="Type")+
    scale_fill_brewer(palette="Paired")
```

We build a model to see the increase of each transport type.

```{r}
by_transport <- kilometers %>% group_by(Type) %>% nest
#nest by transport type
transport_model1 <- function(df) {lm(Vehic_km ~ Year, data=df)}
#creates the models fitting functions

by_transport1 <- by_transport %>% 
  mutate(model = purrr::map(data, transport_model1))
#create a new column for the nested models so that the related objects are stored together

by_transport1 <- by_transport1 %>% 
  mutate(predictions = purrr::map2(data, model, add_predictions), resids = purrr::map2(data, model, add_residuals))
#create a new column for the nested predictions and residuals of the model

by_transport3 <- by_transport1 %>% 
  mutate(tidy = purrr::map(model, broom::tidy)) %>% 
  unnest(tidy, .drop = TRUE) %>%
  filter(term=="Year")
#unnesting the data. Retrieving the coefficients and their statistics

by_transport3 %>% 
  select(-term,-statistic,-p.value) %>%
  kable(caption="Yearly increase", align='c',digits=3, 
  col.names = c("Type","Prediction", "STDE"))
##showing the coefficients and their statistics

by_transport1 <- unnest(by_transport1,predictions) %>% full_join(unnest(by_transport1,resids)) %>% mutate(prop_resid=resid/pred)
##recreate the two tibble and adding a column with proportional residuals

by_transport1 <- by_transport1 %>% left_join(by_transport3) %>% select(-term,-std.error,-statistic,-p.value) %>% mutate(index=map(pred,pred[1]))
##merge the two tables

# by_transport1 %>%
#   ggplot(aes(x=Year, y=growth, color=Type))+
#     geom_line()
#     labs(title="Passenger car fleet Growth per year and fuel type", x="Year", y="Growth Rate") + 
#     geom_label_repel(data = . %>% filter(Annee == "2017") %>% mutate(T = round(T,2)), aes(x=Annee, y=T, label = T))+
#     scale_color_discrete(labels=c("Diesel","Electric","Gasoline"))
  

by_transport1 %>% 
  ggplot(aes(Type, prop_resid))+
  geom_boxplot()+
  labs(title="Proportional residuals per type of transport", x="Year",y="Residuals")
#Generates graph of residuals per type of car

by_transport1 %>%
  ggplot(aes(Year, prop_resid, group=Type)) +
    geom_line()+
    facet_wrap(~Type,scales = "free_y")+
    labs(y="Residuals over prediction")
#plot the residuals variance
```

The first table shows the predicition of the yearly increase for each transportation type. The raw data is the not the most interesting and that's why the second table was build to see the flat increase year by year as well as the percentage increase compared to 2005. This last result was then plotted to compare they different transport types.  
Looking at the residuals, it was first needed to be able to compare them between classes. Since Private_car was so big compared to the other, we create a measure of the residuals proportional to their predictors. Looking at the output, we see that the growth is much more stable for cars and trains than for trams. Lastly, a graph was made to see if there's a correlation of the residuals between types of tranportation. If we see a pattern of residuals through all the graphs, that would mean that there were some similare fluctuations in different types at the same time. However, we do not observe this here.


## Behaviour and person-kilometer

Similarly from the statistic that was used previously, here the analysis is on person-kilometer which is in billion the nummber of kilometers traveled by people for each vehicule type. Here, the statistic is much more understandable and that's the one that'd be use in the rest of this section.    

This first graph shows that the people in Switzerland always travel more.  

```{r echo=FALSE}
kilometers %>% group_by(Year) %>% summarize(Pers_km=sum(Pers_km)) %>%
  ggplot(aes(x=Year, y=(Pers_km)))+
    geom_line()+
    labs(title="Increase of kilometers per person", x="Year",y="Kilometers per person")
```

From this second graph, we can see that private car dominates. Almost 75% of the kilometers travelled in Switzerland are in a private car.  

```{r echo = FALSE}
kilometers %>% 
  ggplot() + geom_bar(aes(x=Year,  y=Pers_km, fill=fct_reorder(Type,Pers_km)), position = "fill", stat="identity") +
 labs(title="Number of kilometers per person",x="Year", y="Proportion", fill="Type")+
  scale_fill_brewer(palette="Paired")
```

Again, we build a model to see the increase of each transport type.

```{r echo=FALSE}
transport_model2 <- function(df) {lm(Pers_km ~ Year, data=df)}
#creates the models fitting functions

by_transport2 <- by_transport %>% 
  mutate(model = purrr::map(data, transport_model2))
#create a new column for the nested models so that the related objects are stored together

by_transport2 <- by_transport2 %>% 
  mutate(predictions = purrr::map2(data, model, add_predictions), resids = purrr::map2(data, model, add_residuals))
#create a new column for the nested predictions and residuals of the model

by_transport4 <- by_transport2 %>% 
  mutate(tidy = purrr::map(model, broom::tidy)) %>% 
  unnest(tidy, .drop = TRUE) %>%
  filter(term=="Year")
#unnesting the data. Retrieving the coefficients and their statistics

by_transport4 %>% 
  select(-statistic,-p.value,-term) %>%
  kable(caption="Yearly increase", align='c',digits=3, 
  col.names = c("Type","Prediction", "STDE"))
##retriewing the coefficients and their statistics

by_transport2 <- unnest(by_transport2,predictions) %>% full_join(unnest(by_transport2,resids)) %>% mutate(prop_resid=resid/pred)
##recreate the two tibble and adding a column with proportional residuals

by_transport2 %>% 
  ggplot(aes(Type, prop_resid))+
  geom_boxplot()+
  labs(title="Proportional residuals per type of transport", x="Year",y="Residuals")
#Generates graph of residuals per type of car

by_transport2 %>%
  ggplot(aes(Year, prop_resid, group=Type)) +
    geom_line()+
    facet_wrap(~Type,scales = "free_y")+
    labs(y="Residuals over prediction")
#plot the residuals variance
```

The results are similar to what we had for the vehicule-kilometer model.  
The one meaningful difference that is observable is in the graph of the relative growth of the different types.
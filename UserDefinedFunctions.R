## Name: Evelyn Gakinya
## Date: 26th March, 2020
## Description: User Defined Functions

library(tidyverse)
library(rlang)
library(nycflights13)


## Write a function that displays the following message:

## The wafanyikazi dataset has 500 observations and 10 variables.
## The infert dataset has 248 observations and 8 variables.
## The flights dataset has 336776 observations and 19 variables.

## Writing the function
statement <- function(jina,dat){
  z <- paste0("The ", jina, " dataset has ", nrow(dat)," observations and ", 
              ncol(dat), " variables.")
  return(z)
}
df <- rChambua::wafanyikazi
df2 <- infert

## Call the function
jina <- "Wafanyikazi"
dat <- df
wafanyikazi_statement <- statement(jina,dat)
wafanyikazi_statement

jina1 <- "infert"
dat1 <- df2
infert_statement <- statement(jina1,dat1)
infert_statement

jina2 <- "flights"
dat2 <- flights
flights_statement <- statement(jina2, dat2)
flights_statement

##4. Write a function that generates frequency tables for
##each of the categorical variables in the wafanyikazi dataset.

##Let us start with two different tables

## Role
tab_Role <- df %>% 
  group_by(Role) %>% 
  summarise(Count = n()) %>% 
  mutate(Perc = round(Count/sum(Count)*100,1))

## Deparment
tab_Department <- df %>% 
  group_by(Department) %>% 
  summarise(Count = n()) %>% 
  mutate(Perc = round(Count/sum(Count)*100,1))


## Option 1: When the variable name is enclosed with quotes

summ_stats1 <- function(var){
  
  tab_anyvar <- df %>% 
    group_by(!!sym(var)) %>% 
    summarise(Count = n()) %>% 
    mutate(Percentage = round(Count/sum(Count)*100,0))
  
  return(tab_anyvar)
  
}

## Call the function

tab_Role <- summ_stats1("Role")
tab_Role

tab_MS <- summ_stats1("Marital_Status")
tab_MS

tab_County <- summ_stats1("County")
tab_County

## Option 2: When the variable name is not enclosed with quotes

summ_stats2 <- function(var){
  
  tab_anyvar <- df %>% 
    group_by({{ var }}) %>% 
    summarise(Count = n()) %>% 
    mutate(Percentage = round(Count/sum(Count)*100,0))
  
  return(tab_anyvar)
  
}


## Call the function

tab_Role2 <- summ_stats2(Role)
tab_Role2

tab_MS2 <- summ_stats2(Marital_Status)
tab_MS2

tab_County2 <- summ_stats2(County)
tab_County2

##Write a function that generates frequency tables and graphs for
##each of the categorical variables in the wafanyikazi dataset.

### Writing the function
tabgraphs <- function(var){
  ## Code that generates a table
  tab <- df %>% 
    group_by(!!sym(var)) %>% 
    summarise(Count = n()) %>% 
    mutate(Perc = round(Count / sum(Count) *100,1))
  
  ## Code that generates a bar graph
  amandla_theme <- theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
                         axis.title = element_text(size = 14),
                         axis.text = element_text(size = 12),
                         axis.line = element_line(size = 1.5),
                         plot.subtitle = element_text(hjust = 0.5, size = 14),
                         plot.caption = element_text(face = "bold", size = 12),
                         panel.background = element_rect(fill = NA))
  
  plot <- ggplot(data = tab, aes_string(x = var, y = "Perc"))+
          geom_bar(stat = "identity", fill = "#F9A825")+
          geom_text(aes(label = paste0(Perc,"%")), hjust = 0.5, vjust = -0.25, size = 5) +
          amandla_theme+
          scale_y_continuous(labels = function(x) paste0(x, "%"))+
          labs(title = paste0("Distribution of Respondents \n by \n", var),
           x = var, y = "Percentage")
    
  output <- list(tab, plot)
 return(output)
}

### Calling / implementing the function

#### County
county_list <- tabgraphs("County")
county_tab <- county_list[[1]]
county_plot <- county_list[[2]]

#### Department
Department_list <- tabgraphs("Department")
Department_tab <- Department_list[[1]]
Department_tab
Department_plot <- Department_list[[2]]
Department_plot

## Promotion
Promotion_list <- tabgraphs("Promotion")
Promotion_tab <- Promotion_list[[1]]
Promotion_tab
Promotion_plot <- Promotion_list[[2]]
Promotion_plot

## Marital Status
MS_list <- tabgraphs("Marital_Status")
MS_tab <- MS_list[[1]]
MS_tab
MS_plot <- MS_list[[2]]
MS_plot

### A function that takes in the variable, and the color as arguments.
### Writing the function
tabgraphs2 <- function(var, rangi){
  ## Code that generates a table
  tab <- df %>% 
    group_by(!!sym(var)) %>% 
    summarise(Count = n()) %>% 
    mutate(Perc = round(Count / sum(Count) *100,1))
  
  ## Code that generates a bar graph
  amandla_theme <- theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
                         axis.title = element_text(size = 14),
                         axis.text = element_text(size = 12),
                         axis.line = element_line(size = 1.5),
                         plot.subtitle = element_text(hjust = 0.5, size = 14),
                         plot.caption = element_text(face = "bold", size = 12),
                         panel.background = element_rect(fill = NA))
  
  plot <- ggplot(data = tab, aes_string(x = var, y = "Perc"))+
    geom_bar(stat = "identity", fill = rangi)+
    geom_text(aes(label = paste0(Perc,"%")), hjust = 0.5, vjust = -0.25, size = 5) +
    amandla_theme+
    scale_y_continuous(labels = function(x) paste0(x, "%"))+
    labs(title = paste0("Distribution of Respondents \n by \n", var),
         x = var, y = "Percentage")
  
  output <- list(tab, plot)
  return(output)
}



### Calling / implementing the function

#### County
county_list <- tabgraphs2("County", "#993A69")
county_tab <- county_list[[1]]
county_tab
county_plot <- county_list[[2]]
county_plot

#### Department
Department_list <- tabgraphs2("Department", "#e40000")
Department_tab <- Department_list[[1]]
Department_tab
Department_plot <- Department_list[[2]]
Department_plot


## Promotion
Promotion_list <- tabgraphs2("Promotion","purple")
Promotion_tab <- Promotion_list[[1]]
Promotion_tab
Promotion_plot <- Promotion_list[[2]]
Promotion_plot

## Marital Status
MS_list <- tabgraphs2("Marital_Status","#309C3F")
MS_tab <- MS_list[[1]]
MS_tab
MS_plot <- MS_list[[2]]
MS_plot
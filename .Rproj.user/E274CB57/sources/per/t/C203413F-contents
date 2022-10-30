# STATISTICAL COMPUTING (R-Home work)
# Leah Jimmiy Shitindi
# 20/10/2022#


########### packages##############################################|
library(here)
library(tidyverse)
library(data.table)
library(tidyr)
library(stringr)
library(kableExtra)
library(readxl)

############### QUESTION ONE #######################################|

 # loading the data and storing in df or tibble

women<- read_xls(here("women2.xls"))

 
names(women)[1] <- "CountryGender"# rename the first column for convenience 
women_tb <- women

############### QUESTION TWO #######################################|
  
 # Split the column into two

women_tb <- women_tb %>% 
  separate(CountryGender,c("Country","Gender"),sep=-1);head(women_tb,3)



############### QUESTION THREE ######################################|

 #sort the countries alphabetically

(women_sort <- women_tb %>% arrange(Country))

head(women_sort,3);tail(women_sort,3)



############### QUESTION FOUR #######################################|
 
 #sum per group

system.time(total <- women_sort %>%
  group_by(Gender) %>% 
  summarise(W=sum(W),w=sum(w),H=sum(H),`?`=sum(women_sort$`?`)))

total %>%
  kbl(caption = "Total Count Per Gender and Group") %>%
  kable_classic(full_width = F, html_font = "Cambria")
 

############### QUESTION FIVE #######################################|
 
#Q5 reshape into long format

women_long <- women_sort %>%
  gather(W,w,H,`?`,  key="Work",value="Freq");head(women_long,3)


############### QUESTION SIX #######################################|
 
# group total

system.time(by_gender <- women_long %>% 
  group_by(Work,Gender) %>% 
  summarise(Total=sum(Freq)))

spread(by_gender,key=Work,value=Total) %>% 
  kbl(caption = "Total Frequency of Work Status Categories By  Gender") %>%
  kable_classic(full_width = F, html_font = "Cambria")
  

############### QUESTION SEVEN #######################################|

#q7 grouped_bar chart

bar_chart <- women_long %>% 
  ggplot(mapping=aes(fill=Gender,y=Freq,x=Country))+
  geom_bar(width=0.7, position=position_dodge(width = 0.9),
            stat = 'identity')+
  theme_minimal()+
  labs(title=paste("Responses About Women with schoolchild work status per Country "),
     y=paste("Frequency of Responses"))+
  theme(legend.position="top",
        axis.text.x = element_text( size =6, angle = 90,
                                    hjust = .5, vjust = .5,))
  
print(bar_chart)
 


############### QUESTION EIGHT #######################################|
 
# stacked bar chart
stacked_chart <- ggplot(women_long,aes(x=Gender,fill=Work,y=Freq))+
  geom_bar(stat="identity")+
  facet_wrap(~Country,nrow=4)+
  theme_light()+
  scale_fill_discrete(name="Women working status")+
  labs(title=paste("Freqency of Responses About Women With SchoolChild \nWorking Status by Country and Gender "),
       y="Frequency")
print(stacked_chart)



############ QUESTION NINE############################################|
# SOURE THE FUNCTION GKTau from GKTau.R script and run the test for women data

source("GKTAU.R")
GKTau(as.matrix(women[,2:5]),rowResponse = FALSE) 



############### QUESTION TEN ###################################|
  
# Creating an S3 method for GKTau function

GKTau <- function(x, rowResponse = TRUE) {
  # function checks!
  if(any(x<0) | !is.matrix(x))stop('x must be a matrix of positive values',call. = )
  # 1: grand total
  N <- sum(x)
  # 2: relative frequency
  p <- x/N
  # 3: check whether the rows are the response or the columns
  rho <- ifelse(rowResponse==TRUE,1,0)
  n_j <- ncol(p)
  n_i <- nrow(p)
  
  #  inner sum of the numerator
  inner_sum<- function(i){
    (p[i,1:n_j]-rowSums(p)[i]*
       colSums(p)[1:ncol(p)])^2/
      (((rowSums(p)[i])^(1-rho))*((colSums(p)[1:ncol(p)])^rho))
  }
  # numerator part of the function
  numerat<-sum(sapply(1:n_i,
                      function(i)inner_sum(i)))
  
  # denominator part of the function
  
  ssq_row <- sum(sapply(1:n_i,function(i)(rowSums(p)[i])^2))
  ssq_col <- sum(sapply(1:n_j,function(j)(colSums(p)[j])^2))
  denom <- 1-((ssq_row)^rho)*((ssq_col)^(1-rho))
  
  tau <- numerat/denom
  
  # set a class for the return
  class(tau)<-"GKTau"
  #create a method print for GKTau class object
  print.GKTau <- function(x,...){
    cat(" Goodman-Kruskal tau:",x,sep="  ")
  }
  
  print(tau)
}

GKTau(as.matrix(women[, 2:5]), rowResponse = FALSE)




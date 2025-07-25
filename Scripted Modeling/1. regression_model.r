rm(list=ls())

library(data.table)
library(foreign)
library(stringr)
library(lubridate)
library(ggplot2)
library(lfe)
library(dplyr)
library(stargazer)
library(haven)

library(httr)
library(tidyverse)
library(DataCombine)
library(tm)
library(data.table)

### Carlo Cabusora ###

setwd("/Users/carlocabusora/Desktop/directory")

data <- read.csv("Sales_Training.csv")

names(data)
head(data)

#1: ABC, a US based firm, operates in several states. Recently, the firm offered its employees an AI based sales training program. The program is believed to advance the sales skills, and it costs ABC $80 to offer this sales program per sales person. The data Sales_Training.csv contains information about the sales people who went through this training program and those who did not. The sales of the sales persons before this training program was offered, and after are also provided in this data. In addition, the reagion and industry division in which a sales person operates are also provided. ABC is debating if it should continue with this sales program for all the sales people across the country, or whether it should focus on certain states or product divisions. Please use the data that is provided to you to answer these question. 50 Points

#convert TimePeriod and ReceivedTraining to allow regression modeling
data$TimePeriod[data$TimePeriod=="Before"] <- 0
data$TimePeriod[data$TimePeriod=="After"] <- 1

data$Received_Training[data$Received_Training=="No"] <- 0
data$Received_Training[data$Received_Training=="Yes"] <- 1

data$train <- as.numeric(data$Received_Training)
data$time <- as.numeric(data$TimePeriod)

data$trt <- data$time*data$train

#run linear model to get marginal effect of receiving training
model <- lm(sales_generated_. ~ TimePeriod + Received_Training + trt,data=data)
summary(model)

### find all states in data and run the models ###
unique(data$State)

data_PA <- data[data$State=="PA",]
model1 <- lm(sales_generated_. ~ TimePeriod + Received_Training + trt,data=data_PA)
summary(model1)
#Receiving training in Pennsylvania increases sales generated per person
#by $68 but is not justifiable as the gains do not surpass the $80 invetment

data_CA <- data[data$State=="CA",]
model2 <- lm(sales_generated_. ~ TimePeriod + Received_Training + trt,data=data_CA)
summary(model2)
#The effect of training on California is $33 more dollars per person. Training is 
#not justified here because it does not exceed $80 and is not significant.

data_SC <- data[data$State=="SC",]
model3 <- lm(sales_generated_. ~ TimePeriod + Received_Training + trt,data=data_SC)
summary(model3)
#The effect of receiving training in South Carolina is significant and is 
# $176 per person, so trianing is justified in this state because the gains surpass the investment.

data_FL <- data[data$State=="FL",]
model4 <- lm(sales_generated_. ~ TimePeriod + Received_Training + trt,data=data_FL)
summary(model4)
#The effect of training in Florida is not signficant and is negative, so
#training here is not justifiable.

data_TX <- data[data$State=="TX",]
model5 <- lm(sales_generated_. ~ TimePeriod + Received_Training + trt,data=data_TX)
summary(model5)
#The effect of training in Texas increases sales per person by $46 dollars. Training
# is not justifiable here because it does not exceed the investement and is insignificant.

data_VA <- data[data$State=="VA",]
model6 <- lm(sales_generated_. ~ TimePeriod + Received_Training + trt,data=data_VA)
summary(model6)
#Training in Virginia raised generated sales per person by $30. It is not justifiable to train
# in this state because the value is insignificant and does not surpass $80.

### find all the product divisions ###
unique(data$Product_Division)

Data_D1<- data[data$Product_Division=="1",]
model6 <-lm(sales_generated_. ~ TimePeriod + Received_Training + trt, data = Data_D1)
summary(model6)
#Sales generated for each sales person in Division 1 increases by approx $108.82.
#Training in this division is justified because it's worth the investment and significant.

Data_D2<- data[data$Product_Division=="2",]
model7 <-lm(sales_generated_. ~ TimePeriod + Received_Training + trt, data = Data_D2)
summary(model7)
#Sales generated for each sales person in Division 2 increases by approx $7.
#Training in this division is  not justified because it's not worth the investment and insignificant.

Data_D3<- data[data$Product_Division=="3",]
model8 <-lm(sales_generated_. ~ TimePeriod + Received_Training + trt, data = Data_D3)
summary(model8)
#Sales generated for each sales person in Division 3 increases by approx $212.
#Training in this division is justified because it's  very worth the investment and despite being insignificant.

Data_D4<- data[data$Product_Division=="4",]
model9 <-lm(sales_generated_. ~ TimePeriod + Received_Training + trt, data = Data_D4)
summary(model9)
#Sales generated for each sales person in Division 4 increases by approx $44.
#Training in this division isn't justified because it's not worth the investment and despite being significant.

Data_D5<- data[data$Product_Division=="5",]
model10 <-lm(sales_generated_. ~ TimePeriod + Received_Training + trt, data = Data_D5)
summary(model10)
#Sales generated for each sales person in Division 5 increases by approx $121.
#Training in this division is justified because it's worth the investment despite being insignificant.

Data_D7<- data[data$Product_Division=="7",]
model11 <-lm(sales_generated_. ~ TimePeriod + Received_Training + trt, data = Data_D7)
summary(model11)
#Sales generated for each sales person in Division 7 increases by approx $43.
#Training in this division is not justified because it's not worth the investment and insignificant.

#Data_D9<- data[data$Product_Division=="9",]
#model12 <-lm(sales_generated_. ~ TimePeriod + Received_Training + trt, data = Data_D9)
#summary(model12)
### checking product division 9 
#str(Data_D9)
#There is not enough data to justify traiining or not training in Divison 9
#because there are not enough data

Data_D10<- data[data$Product_Division=="10",]
model13 <-lm(sales_generated_. ~ TimePeriod + Received_Training + trt, data = Data_D10)
summary(model13)
#Sales generated for each sales person in Division 5 increases by approx $75.
#Training in this division is not justified because it's not worth the investment adnd is insignificant.

#2: In addition, ABC also wants to find out the average probability with which sales people in different states and product divisions are likely to undergo through this training program. 50 Points

#subset to have data on before training for modeling
data_before <- droplevels(subset(data, TimePeriod == 0))

model_prob <- glm(as.numeric(Received_Training) ~ as.factor(Product_Division) + State, data=data_before, family = "binomial")
summary(model_prob)

data_before$prob_logit <- predict(model_prob,data_before,type="response")

###predicted probabilities
mean(data_before$prob_logit[data_before$State=="PA"])
#PA probability is 46.5%
mean(data_before$prob_logit[data_before$State=="CA"])
#CA probability is 53.7%
mean(data_before$prob_logit[data_before$State=="SC"])
#SC probability is 50.7%
mean(data_before$prob_logit[data_before$State=="FL"])
#FL probability is 57.1%
mean(data_before$prob_logit[data_before$State=="TX"])
#TX probability is 45.5%
mean(data_before$prob_logit[data_before$State=="VA"])
#VA probability is 45.5%

#3: ABC regularly collects feedback on its sales people from the clients that they serve. This textual information is provided in FeedBack_Text column of the data (this is not the actual feedback, though). Please provide all the steps that you would take if you want to investigate whether the AI based sales training program improved the positivity in the textual feedback collected by ABC. You do not have to run any model here. However, I want you to provide the entire R-code that is required to do this analysis. 100 Points
###steps
# 1. create an object for an API
# 2. set the temperature of the AI and create a function to loop over the data frame
# 3. create a input prompt to run through the AI and loop it through the text data
# the goal here to create quantitative data out of the feedback to measure positivity.
# AI does this by grading the performance of sales-persons with values such as their politeness,
# speed, and knowledge. These outputs will allow for modeling with feedback
# 4. run the model

###create an object for an API
my_API <- Sys.getenv("OPENAI_API_KEY")

###set the temperature of the AI and create the function to loop over the data frame
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      temperature = 0,
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(httr::content(chat_GPT_answer)$choices[[1]]$message$content)
}

### create a input prompt to run through the AI and loop it through the text data
data$FeedBack_Rating <- 0

input <- "The following text contains a review provided by a customer of a salesperson. To what extent the review reflects the positive aspects of the employee? Provide the answer on a scale of 1 to 5, where 1 means the feedback is not at all positive about the employees and 5 means the review is very positive about the employees: The review text is as follows:"

counter = 0

for(i in 1:nrow(data))
{
  input <- paste(input,data$data[FeedBack_Text], sep="; ")
  
  out <- hey_chatGPT(input)
  while(length(out) == 0)
  {
    out <- hey_chatGPT(input)
    counter = counter + 1
    print(counter)
    if(counter == 10)
    {out = 9999
    counter = 0
    }
  }
  data[FeedBack_Rating] <- as.integer(out)
  
  input <- "The following text contains feedback provided by a customer of a salesperson. To what extent the review reflects the positive aspects of the employees? Provide the answer on a scale of 1 to 5, where 1 means the review is not at all positive about the employees and 5 means the review is very positive about the employees: The review text is as follows:"
  
}

###run a model
model_API <- lm(FeedBack_Rating ~ Received_Training + State + Product_Division + trt ,data=data)
summary(model_API)

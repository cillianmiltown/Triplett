rm(list = ls())
# install.packages("simulator")
# devtools::install_github("jacobbien/simulator")

suppressMessages(library(simulator))
library(tidyverse)
library(truncnorm)
library(psych)
suppressMessages(library(faux))


#N <- 300*4
#session_seed <- 4

load("N.RData")

df <- read.csv("sample_data.csv")

df$gender <- as.factor(df$gender)
df$order <- as.factor(df$order)
df$condition <- as.factor(df$condition)
df$group <- as.factor(df$group)



# create empty data frame
empty_df <- df[0,]
x <- empty_df


x <- data.frame(
  matrix(NA,nrow = N,ncol = length(df))
) %>% 
  `colnames<-`(colnames(x))

x$group <- rep(levels(empty_df$group), length(x$group)/2)



a <- x[which(x$group=="a"),]
b <- x[which(x$group=="b"),]



#### sort out group "a" ####

a$order <- rep(levels(empty_df$order),N/8)
a$condition <- rep(c("alone", "alone", "together", "together"), N/8)
a$condition2 <- rep(c("alone1", "alone2", "together1", "together2"), N/8)



a_make_ID_fun <- function(x){
  c(
    c(1:(N/8))[x],
    c(1:(N/8))[x],
    c(1:(N/8))[x],
    c(1:(N/8))[x]
  )
}


a$ID <- unlist(lapply(c(1:(N/8)), a_make_ID_fun))


y <- a[which(a$order=="1"),]
y$time <- rnorm(n=(N/8),mean = 39.4, sd = 3.3)
round1 <- y
y <- a[which(a$order=="2"),]
y$time <- rnorm(n=(N/8),mean = 39, sd = 3.5)
round2 <- y
y <- a[which(a$order=="3"),]
y$time <- rnorm(n=(N/8),mean = 37.3, sd = 3.1)
round3 <- y
y <- a[which(a$order=="4"),]
y$time <- rnorm(n=(N/8),mean = 37.5, sd = 2.9)
round4 <- y

a <- rbind(round1, round2, round3, round4)




#### sort out group "b" ####

b$order <- rep(levels(empty_df$order),N/8)
b$condition <- rep(c("alone", "together", "alone", "together"), N/8)
b$condition2 <- rep(c("alone1", "together1", "alone2", "together2"), N/8)


b_make_ID_fun <- function(x){
  c(
    c((N/8+1):((N/8)+(N/8)))[x],
    c((N/8+1):((N/8)+(N/8)))[x],
    c((N/8+1):((N/8)+(N/8)))[x],
    c((N/8+1):((N/8)+(N/8)))[x]
  )
}



b$ID <- unlist(lapply(c(1:(N/8)), b_make_ID_fun))


y <- b[which(b$order=="1"),]
y$time <- rnorm(n=(N/8),mean = 39.6, sd = 3.4)
round1 <- y
y <- b[which(b$order=="2"),]
y$time <- rnorm(n=(N/8),mean = 37.7, sd = 3.1)
round2 <- y
y <- b[which(b$order=="3"),]
y$time <- rnorm(n=(N/8),mean = 37.9, sd = 3.2)
round3 <- y
y <- b[which(b$order=="4"),]
y$time <- rnorm(n=(N/8),mean = 37.1, sd = 3.9)
round4 <- y

b <- rbind(round1, round2, round3, round4)


#### merge a and b ####

x <- rbind(a,b)

##### make age and gender for whole sample ####

x <- x[order(x$ID),]

make_age_fun <- function(x){
  age_range <- sample(8:17, (N/4), replace = T)
  
  c(
    age_range[x],
    age_range[x],
    age_range[x],
    age_range[x]
  )
}

x$age <- unlist(lapply(c(1:(N/4)), make_age_fun))


make_gender_fun <- function(x){
  gender_range <- sample(c("male","female"), (N/4), replace = T)
  
  c(
    gender_range[x],
    gender_range[x],
    gender_range[x],
    gender_range[x]
  )
}

x$gender <- unlist(lapply(c(1:(N/4)), make_gender_fun))


x <- x[order(x$ID),]
sort(x$ID)

df <- x


df$gender <- as.factor(df$gender)
df$order <- as.factor(df$order)
df$condition <- as.factor(df$condition)
df$group <- as.factor(df$group)

df$time2 <- scale(df$time, scale = FALSE)

ls()
rm(
  "a","a_make_ID_fun","b","b_make_ID_fun",
  "empty_df","make_age_fun","make_gender_fun",
  "N","round1","round2","round3", "round4","x","y")

df$age <- as.numeric(df$age)
#df$gender_cont <-
#car::recode(df$gender, "'male'='0'; 'female'='1'")


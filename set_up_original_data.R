rm(list = ls())

library(tidyverse)


df <- haven::read_sav("../data/Triplett1898_RawData_Untouched.sav")

df_wide <- df

df$condition <- rep(NA)


GroupA <- df[which(df$Group==1),]

GroupB <- df[which(df$Group==2),]

df_long <- rbind.data.frame(
  pivot_longer(GroupA, T1:T6
               , names_to = "trial"
               , values_to = "time") %>% 
    mutate(condition = recode(
      trial
      , "T1" = "alone"
      , "T2" = "together"
      , "T3" = "alone"
      , "T4" = "together"
      , "T5" = "alone"
      , "T6" = "together"
    ))
  ,
  pivot_longer(GroupB, T1:T6
               , names_to = "trial"
               , values_to = "time") %>% 
    mutate(condition = recode(
      trial
      , "T1" = "alone"
      , "T2" = "alone"
      , "T3" = "together"
      , "T4" = "alone"
      , "T5" = "together"
      , "T6" = "alone"
    ))
)


df <- df_long
df$order <- df$trial

df$ID <- deidentifyr::deidentify(df, Name)$id

x <- df
model1 <- lmerTest::lmer(time ~ 
                           condition
                         + (1+condition|ID), data=x)

summary(model1)
anova(model1)


## Current Make up

#     GroupA  GroupB
# T1  alone   alone
# T2  comp    alone
# T3  alone   comp
# T4  comp    alone
# T5  alone   comp
# T6  comp    alone


## Revised make up (to match our data collection)
#     GroupA  GroupB
# T1  alone   alone
# T2  comp    alone
# T3  alone   comp
# T4  comp    
# T5          comp
# T6      


df_cleaned <- rbind.data.frame(
  df %>% filter(Group == 1
                & trial %in% c("T1","T2","T3","T4")
  ) %>% 
    mutate(order = recode(
      order
      , "T1" = "1"
      , "T2" = "2"
      , "T3" = "3"
      , "T4" = "4"
    )
    )
  ,
  
  df %>% filter(Group == 2
                & trial %in% c("T1","T2","T3","T5")
  ) %>% 
    mutate(order = recode(
      order
      , "T1" = "1"
      , "T2" = "2"
      , "T3" = "3"
      , "T5" = "4"
    )
    )
)

df <- df_cleaned

x <- df
model1 <- lmerTest::lmer(time ~ 
                           condition
                         + (1+condition|ID), data=x)

summary(model1)
anova(model1)

rm(model1, x)

df <- df_cleaned


GroupA_clean <- df[which(df$Group==1),]

GroupB_clean <- df[which(df$Group==2),]


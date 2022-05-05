library(simr)

source("set_up_original_data.R")

rep4 <- function(x){rep(x, 4)}

load("N_power.RData")



df <- GroupA_clean
group_a_sample <- sample(df$ID[which(duplicated(df$ID)==FALSE)], n1, replace = F)
#group_a_sample <- sample(group_a_sample, n2, replace = T)
A_sampled <- df %>% 
  filter(ID %in% group_a_sample)


temp <- A_sampled %>% pivot_wider(names_from = c(trial, condition, order), values_from = time)

temp <- temp[sample(nrow(temp),n2,replace=T),]
temp$ID_old <- temp$ID
temp$ID <- c(1:n2)

temp <- temp %>% pivot_longer(T1_alone_1:T4_together_4
                      , names_to = "trial"
                      , values_to = "time") %>% 
  mutate(
    condition = recode(
    trial
    , "T1_alone_1" = "alone"
    , "T2_together_2" = "together"
    , "T3_alone_3" = "alone"
    , "T4_together_4" = "together"
  )
  ,order = recode(
    trial
    , "T1_alone_1" = "1"
    , "T2_together_2" = "2"
    , "T3_alone_3" = "3"
    , "T4_together_4" = "4"
  )
  ,recode(
    trial
    , "T1_alone_1" = "T1"
    , "T2_together_2" = "T2"
    , "T3_alone_3" = "T3"
    , "T4_together_4" = "T4"
  )
  )


A_sampled <- temp # A_sampled[sample(nrow(A_sampled),n2,replace=T),]


df <- GroupB_clean
group_b_sample <- sample(df$ID[which(duplicated(df$ID)==FALSE)], n1, replace = F)
#group_b_sample <- sample(group_b_sample, n2, replace = T)
B_sampled <- df %>% 
  filter(ID %in% group_b_sample)

temp <- B_sampled %>% pivot_wider(names_from = c(trial, condition, order), values_from = time)

temp <- temp[sample(nrow(temp),n2,replace=T),]
temp$ID_old <- temp$ID
temp$ID <- c((n2+1):(2*n2))

temp <- temp %>% pivot_longer(T1_alone_1:T5_together_4
                              , names_to = "trial"
                              , values_to = "time") %>% 
  mutate(
    condition = recode(
      trial
      , "T1_alone_1" = "alone"
      , "T2_alone_2" = "alone"
      , "T3_together_3" = "together"
      , "T5_together_4" = "together"
    )
    ,order = recode(
      trial
      , "T1_alone_1" = "1"
      , "T2_alone_2" = "2"
      , "T3_together_3" = "3"
      , "T5_together_4" = "4"
    )
    ,recode(
      trial
      , "T1_alone_1" = "T1"
      , "T2_alone_2" = "T2"
      , "T3_together_3" = "T3"
      , "T5_together_4" = "T5"
    )
  )

B_sampled <- temp # B_sampled[sample(nrow(B_sampled),n2,replace=T),]


df_sampled <- rbind.data.frame(A_sampled, B_sampled)

df <- df_sampled

# df$ID_old <- df$ID
# c <- lapply(1:(n2*2), rep4)
# df$ID <- as.vector(unlist(c))

x <- df
model1 <- lmerTest::lmer(time ~ 
                           condition
                         + (1+condition|ID), data=x)

summary(model1)
test <- anova(model1)


# f <- MuMIn::r.squaredGLMM(model1)[1]/(1-MuMIn::r.squaredGLMM(model1)[1])

# model1 <- lme4::glmer(time ~ 
#                   condition
#                 + (1+condition|ID)
#                 , data = x
#                 , family="gaussian"
#                )
# # load functions
# source("https://slcladal.github.io/rscripts/SampleSizeMLR.r")
# source("https://slcladal.github.io/rscripts/ExpR.r")
# # check if sample size is sufficient
# smplesz(model1)


# summary(model1)
# model1
# 
# fixef(model1)

# fixef(model1)["conditiontogether"] <- .5 #b
# sim <- powerSim(model1, nsim = 10 )#, test = fixed("conditiontogether", "z"))
# sim

pwr::pwr.f2.test(u = test$NumDF, v = test$DenDF, f2 = f, sig.level =  .05)

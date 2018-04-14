library(alr4)
onlineNewsData <- read.csv("C:/Users/Jiali/OneDrive/Documents/UCSB/PSTAT 126/Project/OnlineNewsPopularity/OnlineNewsPopularity1.csv")

#Model 1

lm1 <- lm(shares ~ is_weekend*n_tokens_content + 
            is_weekend*num_hrefs + is_weekend*num_imgs + is_weekend*average_token_length +
            is_weekend*num_videos + is_weekend*global_rate_positive_words + is_weekend*global_rate_negative_words +
            is_weekend*n_unique_tokens, onlineNewsData)
summary(lm1)
avPlots(lm1)
pairs(onlineNewsData[c("n_tokens_content","num_hrefs","num_imgs","average_token_length","num_videos","is_weekend", 
                       "global_rate_positive_words", "global_rate_negative_words")])
plot(lm1, which = 1)
plot(lm1, which = 2)
plot(lm1, which = 3)

boxCox(lm1)

#Model 2: Log Transformation

lm2 <- lm(1000*log(shares) ~ is_weekend*n_tokens_content + 
            is_weekend*num_hrefs + is_weekend*num_imgs + is_weekend*average_token_length +
            is_weekend*num_videos + is_weekend*global_rate_positive_words + is_weekend*global_rate_negative_words +
            is_weekend*n_unique_tokens, onlineNewsData)
summary(lm2)
avPlots(lm2)
plot(lm2, which = 1)
plot(lm2, which = 2)
plot(lm2, which = 3)

#Model 3: Backward Selection

full = ~ is_weekend*n_tokens_content + 
  is_weekend*num_hrefs + is_weekend*num_imgs + is_weekend*average_token_length +
  is_weekend*num_videos + is_weekend*global_rate_positive_words + is_weekend*global_rate_negative_words +
  is_weekend*n_unique_tokens
m0 <- lm(1000*log(shares) ~ is_weekend, onlineNewsData)
m1 <- update(m0, full)
lm3 <- step(m1, scope = c(lower = ~ is_weekend), direction = "backward")

summary(lm3)
avPlots(lm3)
plot(lm3, which = 1)
plot(lm3, which = 2)
plot(lm3, which = 3)

#Partial F-test

partial <- lm(1000*log(shares) ~ is_weekend + n_tokens_content + num_hrefs + num_imgs + average_token_length +
  num_videos + global_rate_positive_words + global_rate_negative_words + n_unique_tokens, onlineNewsData)

anova(partial, lm3)


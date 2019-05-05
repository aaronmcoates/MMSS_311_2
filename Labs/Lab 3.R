setwd("~/Documents/GitHub/MMSS_311_2")

library(dplyr)
library(broom)
data(starwars)

group_by(starwars, species) %>%
  summarize(mean=mean(height, na.rm=T))

starwars2 <- mutate(starwars, ratio=mass/height) %>%
  arrange(desc(ratio))

reg <- lm(starwars$mass ~ starwars$height)
summary(reg)

#Coefficient is .6386

table <- tidy(reg)
table
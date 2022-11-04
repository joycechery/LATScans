library(multcompView)
library(dplyr)
library(ggplot2)

setwd("~/Desktop")

# analysis of variance
datum <- read.csv("Dataset_S2.csv")
head(datum)
datum$Feature <- factor(datum$Feature,levels = c("Vessel", "G_fibers", "Sclerenchyma", "Suber", "Heartwood"))

anova <- aov(Mean ~ Feature, data = datum)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(datum, Feature) %>%
  summarise(mean=mean(Mean), quant = quantile(Mean, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Feature)
Tk$cld <- cld$Letters
print(Tk)

# Violin plots
ggplot(datum, aes(Feature, Mean)) +
  geom_violin() + geom_jitter(aes(color = Sample), position=position_jitter(.2), size=5, alpha = 0.6) +
  labs(x="Feature", y="Mean Fluorescence") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = Tk, aes(x = Feature, y = 230, label = cld))


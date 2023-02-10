library(multcompView)
library(dplyr)
library(ggplot2)

setwd("~/Desktop")

# analysis of variance
datum <- read.csv("Dataset_S2.csv")
head(datum)
datum$Feature <- factor(datum$Feature,levels = c("Vessel_walls", "G_fibers_walls", "Sclerenchyma_cell_walls", "Cork_cell_walls", "Chemical_boundary_cell_walls"))

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
  geom_violin() + geom_jitter(aes(color = Sample), position=position_jitter(.2), size=3, alpha = 0.6) +
  geom_boxplot(width=0.1, color="black", alpha=0.2, outlier.shape = NA) +
  labs(x="Feature", y="Mean Fluorescence") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = Tk, aes(x = Feature, y = 230, label = cld))


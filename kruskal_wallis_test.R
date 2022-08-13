## Edneide Ramalho
## From: https://statsandr.com/blog/kruskal-wallis-test-nonparametric-version-anova/
## 13 August 2022

# 1. Packages -----------------------
library(palmerpenguins)
library(tidyverse)
library(doBy) # summary by group 
library(FSA) # Dunn test 
library(ggstatsplot)

# 2. Data ----------------------------
dat <- penguins %>% 
  select(species, flipper_length_mm)

# 3. Summary --------------------------
## 3.1. entire sample -------
summary(dat)

## 3.2. by group ------
summaryBy(flipper_length_mm ~ species,
          data = dat,
          FUN = median,
          na.rm = TRUE)

## 3.3. boxplot by species -----
dat %>% 
  ggplot(aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  theme(legend.position = "none")

# penguins from the Adelie species seem to have the smallest flippers, 
# while those from the Gentoo species seem to have the biggest flippers.

# 4. Kruskal-Wallis test -------------
kruskal.test(flipper_length_mm ~ species, 
             data = dat)


# at least one species is different in terms of flippers length (p-value < 0.001)

# 5. Post-hoc tests ----------------------

## Most common post-hoc tests:
# Dunn test *
# Pairwise Wilcoxon test *
# Conover test
# Nemenyi test
# * most common

## 5.1. Dunn test ---------------
dunnTest(flipper_length_mm ~ species,
         data = dat,
         method = "holm")

# all 3 species differ in terms of flipper length.

## 5.2. Pairwise Wilcoxon test ---------------
pairwise.wilcox.test(dat$flipper_length_mm, dat$species,
                     p.adjust.method = "holm")

# Here again, we conclude that all 3 species are significantly different 
# in terms of flipper length (p-values < 0.001).

# 6. Combination of statistical results and plot ----------------

ggbetweenstats(
  data = dat,
  x = species, 
  y = flipper_length_mm,
  type = "nonparametric", 
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)

ggsave("wilcoxon_test.png")








## Inheritance: 26 December 1583 ##

library(tidyverse)
library(debkeepr)
library(glue)
library(ggtext)
library(hrbrthemes)

# Will need to figure out way to create this plot with data from transactions.rds
# The current values are taken from my notes on DFL 12

siblings <- c("Jan", "Anna", "Marten", "Carlo", "Jacques",
              "Steven", "Maria", "Hester", "Cornelia")

paternal <- deb_lsd(rep(5000, 9), 0, 0)
maternal <- deb_lsd(l = c(0, 4040, 6555, 0, 1714, 5286, 0, 7065, 7009),
                    s = c(0, 12, 6, 0, 10, 14, 0, 10, 19),
                    d = c(0, 2, 1, 0, 9, 2, 0, 10, 1.5))
sororal <- deb_lsd(l = c(rep(1501, 8), 0),
                   s = c(rep(4, 8), 0),
                   d = c(rep(9, 8), 0))


inheritance <- tibble(heir = rep(siblings, 3),
                      account = c(rep("Paternal", 9),
                                  rep("Maternal", 9),
                                  rep("Sororal", 9)),
                      lsd = c(paternal, maternal, sororal)) %>% 
  mutate(value = as.numeric(lsd))

# Reorder heirs by total inheritance owed
fct_order <- inheritance %>% 
  group_by(heir) %>% 
  summarise(lsd = sum(lsd)) %>% 
  arrange(lsd) %>% 
  pull(heir)

inheritance$heir <- factor(inheritance$heir, levels = fct_order)

# Reorder types of accounts
inheritance$account <- factor(inheritance$account, levels = c("Sororal", "Maternal", "Paternal"))

# Plot
hex <- scales::hue_pal()(3)
styled_title <- glue("
      <span style='color:{hex[[3]]};'>Paternal</span>, 
      <span style='color:{hex[[2]]};'>Maternal</span>, and 
      <span style='color:{hex[[1]]};'>Sororal</span> inheritance on 26 Dec 1583")

ggplot(inheritance) + 
  geom_bar(aes(x = heir, weight = value, fill = account)) + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) + 
  coord_flip() + 
  labs(title = styled_title,
       caption = "Figure 4") + 
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_markdown(),
        legend.position = "none")

ggsave("plots/inheritance-1583-12-26.png", width = 8, height = 5)

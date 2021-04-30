library(tidyverse)
library(debkeepr)
library(glue)
library(ggtext)
library(patchwork)
library(hrbrthemes)

# Create bar chart of sources and distribution of profits in 1583
# This uses groups of accounts instead of individual accounts to
# simplify the bar chart.

# Data
transactions <- read_rds("data/transactions.rds")
accounts <- read_csv("data/accounts.csv") %>% 
  select(account_id, label, group)

profits <- "dfl12_038"
maternal <- c("Hester", "Cornelia", "Marten", "Steven", "Anna", "Jacques")

# Credit: Sources of profits
credit <- transactions %>% 
  filter(credit == profits) %>% 
  left_join(accounts, by = c("debit" = "account_id")) %>% 
  group_by(group) %>% 
  summarise(lsd = sum(lsd), .groups = "drop") %>% 
  mutate(value = as.numeric(lsd),
         group = str_replace(group, 
                             "Frederico Perenot Heer van Champagny",
                             "Heer van Champagny"),
         group = fct_reorder(group, value))

p1 <- ggplot(data = credit) + 
  geom_bar(aes(x = group, y = value), stat = "identity") + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) + 
  coord_flip() + 
  theme_ipsum(base_size = 14) + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) + 
  ggtitle("Sources of profits, 1579 to 15 Dec 1583")

# Debit: Distribution of profits
debit <- transactions %>% 
  filter(debit == profits) %>% 
  left_join(accounts, by = c("credit" = "account_id")) %>% 
  # Use group of accounts instead of individual accounts
  group_by(group) %>% 
  summarise(lsd = sum(lsd), .groups = "drop") %>% 
  mutate(value = as.numeric(lsd),
         group = fct_reorder(group, value),
         label = case_when(group %in% maternal ~ "Maternal",
                           group == "Jan de Oude" ~ "Paternal"),
         label = factor(label, levels = c("Paternal", "Maternal"))) %>% 
  # filter out very minor accounts
  filter(lsd > 1)


hex <- scales::hue_pal()(2)
styled_title <- glue("
      Distribution of <span style='color:{hex[[1]]};'>Paternal</span> and
      <span style='color:{hex[[2]]};'>Maternal</span> profits  
      <span style='font-size:11pt'>
        With some 
        <span style='color:{gray(0.4)};'>expenses/losses</span>
      </span>")

p2 <- ggplot(data = debit) + 
  geom_bar(aes(x = group, y = value, fill = label), stat = "identity") + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) + 
  coord_flip() + 
  labs(title = styled_title,
       caption = "Figure 3") + 
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_markdown(),
        legend.position = "none")

# Build plot
p1 / p2

ggsave("plots/profits-1583.png", width = 8, height = 8)

## Credits and debits of heirs ##

library(tidyverse)
library(debkeepr)

# Heir color
heir_color <- tibble(
  sibling = c("Jan", "Anna", "Marten", "Carlo", "Jacques",
              "Steven", "Maria", "Hester", "Cornelia"),
  color = scales::hue_pal()(9))

# Data
transactions <- read_rds("data/transactions.rds")
accounts <- read_csv("data/accounts.csv") %>%
  select(account_id, group, type)

inheritance_ids <- accounts %>%
  filter(type == "Inheritance") %>%
  pull(account_id)

transactions %>%
  filter(credit %in% inheritance_ids) %>%
  left_join(accounts, by = c("credit" = "account_id")) %>%
  group_by(group) %>%
  summarise(lsd = sum(lsd), .groups = "drop_last")

transactions %>%
  filter(debit %in% inheritance_ids) %>%
  left_join(accounts, by = c("debit" = "account_id")) %>%
  group_by(group) %>%
  summarise(lsd = sum(lsd), .groups = "drop_last")

## Marten
transactions %>%
  filter(debit %in% inheritance_ids) %>%
  left_join(accounts, by = c("debit" = "account_id")) %>%
  filter(group == "Marten") %>%
  View()

## Before 15 March 1585
payments_to_1585 <- transactions %>%
  filter(debit %in% inheritance_ids,
         date <= lubridate::ymd("1585-03-15")) %>%
  left_join(accounts, by = c("debit" = "account_id")) %>%
  group_by(group) %>%
  summarise(lsd = sum(lsd), .groups = "drop_last") %>%
  mutate(value = as.numeric(lsd)) %>%
  filter(group != "Cornelia") %>%
  left_join(heir_color, by = c("group" = "sibling"))


ggplot(payments_to_1585) +
  geom_bar(aes(x = fct_reorder(group, value),
               weight = value,
               fill = color)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  scale_fill_identity(guide = "none") +
  labs(title = "Total disbursement of inheritance",
       subtitle = "8 November 1582 to 15 March 1585") +
  coord_flip() +
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/disbursals-1585-03-15.png", width = 8, height = 5)

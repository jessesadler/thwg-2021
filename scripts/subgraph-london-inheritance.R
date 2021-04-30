## Subgraph of London Inheritance in 1594 ##
# Script concentrates on account of Branch of London Nieuwe rekening
# Uses Branch of London Nieuwe rekening and accounts of the division
# of the inheritance in London.

library(tidyverse)
library(igraph)
library(ggraph)
library(debkeepr)
library(hrbrthemes)

# Load data
transactions <- read_rds("data/transactions.rds")
accounts <- read_csv("data/accounts.csv") %>% 
  select(account_id, label, group, type)

siblings_fct <- c("Jan", "Anna", "Marten", "Carlo", "Jacques",
                  "Steven", "Maria", "Hester", "Cornelia")

## Create subset of transactions that deal directly with heirs
london_inheritance_accounts <- c("dfl12_477", "dfl12_478", "dfl12_479", "dfl12_489")
transactions_london <- transactions %>%
  filter(debit %in% london_inheritance_accounts | credit %in% london_inheritance_accounts)

## Sum of transactions
transactions_sum <- transactions_london %>% 
  group_by(credit, debit) %>% 
  summarise(lsd = sum(lsd), .groups = "drop") %>% 
  mutate(l = deb_decimal(lsd, unit = "l"))

# All groups that are in transactions_london
london_credit_ids <- transactions_london %>% 
  distinct(credit) %>%
  rename(account_id = credit)

london_debit_ids <- transactions_london %>% 
  distinct(debit) %>%
  rename(account_id = debit)

london_accounts <- full_join(london_credit_ids, london_debit_ids, by = "account_id")

## Total debit of accounts within london transactions
london_debit <- transactions_london %>% 
  deb_debit() %>% 
  mutate(debit = deb_decimal(lsd, unit = "l")) %>% 
  select(-lsd)

london_credit <- transactions_london %>% 
  deb_credit() %>% 
  mutate(credit = deb_decimal(lsd, unit = "l")) %>% 
  select(-lsd)

# Build nodes from parts
nodes <- london_accounts %>% 
  left_join(accounts, by = "account_id") %>% 
  left_join(london_credit, by = "account_id") %>% 
  left_join(london_debit, by = "account_id") %>% 
  mutate(group = if_else(account_id == "dfl12_489", "Maria", group),
         text = if_else(group %in% siblings_fct, group, label),
         text = str_replace(text, "Branch.*", "Branch of London"),
         color = if_else(group %in% siblings_fct, group, NA_character_))

# Create igraph object
# Creates vertices from inheritances_transactions data
london_inheritance <- graph_from_data_frame(d = transactions_sum,
                                            vertices = nodes, directed = TRUE)

set.seed(240)
ggraph(london_inheritance, layout = "kk") + 
  geom_edge_fan(aes(edge_alpha = l),
                arrow = arrow(length = unit(3, 'mm')), 
                end_cap = circle(2, 'mm')) + 
  scale_edge_alpha(labels = scales::dollar_format(prefix = "£")) + 
  geom_node_point(aes(size = debit, color = color), alpha = 0.9) + 
  geom_node_text(aes(label = text), repel = TRUE) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format(prefix = "£")) + 
  scale_color_discrete(labels = c(siblings_fct, "Other")) + 
  labs(title = "Subgraph of the inheritance from London, 1594",
       caption = "Figure 8",
       size = "Total debit",
       edge_alpha = "Transactions",
       color = "Heirs") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1)) + 
  theme_ipsum(grid = FALSE,
              caption_face = "plain",
              caption_size = 14) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

ggsave("plots/london-inheritance.png", width = 10, height = 8)

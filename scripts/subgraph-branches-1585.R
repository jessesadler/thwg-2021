## Branches subgraph to 15 March 1585 ##

# Script shows movement of capital between branches
# Uses account and transactions groups

library(tidyverse)
library(igraph)
library(ggraph)
library(debkeepr)
library(hrbrthemes)

# Load data
transactions_group <- read_rds("data/transactions-group.rds") %>%
  filter(date <= lubridate::ymd("1585-03-15"))
accounts_group <- read_csv("data/accounts-group.csv") %>%
  select(id, group, type) %>%
  mutate(group = str_replace(group, "Balance on 8 November", ""),
         group = str_replace(group, "Profits and losses", ""))

# Siblings by age order to create factor levels
siblings_fct <- c("Jan", "Anna", "Marten", "Carlo", "Jacques",
                  "Steven", "Maria", "Hester", "Cornelia")

## Create subset of transactions that deal with branches
branch_accounts <- accounts_group %>%
  filter(type == "Branch") %>%
  pull(id)

transactions_branches <- transactions_group %>%
  filter(debit %in% branch_accounts | credit %in% branch_accounts)

## Aggregate accounts dealing with branches

## Sum of transactions
transactions_sum <- transactions_branches %>%
  group_by(credit, debit) %>%
  summarise(lsd = sum(lsd), .groups = "drop") %>%
  mutate(l = deb_decimal(lsd, unit = "l"))

# All groups that are in transactions_branches
branch_credit_ids <- transactions_branches %>%
  distinct(credit) %>%
  rename(id = credit)

branch_debit_ids <- transactions_branches %>%
  distinct(debit) %>%
  rename(id = debit)

branches_groups <- full_join(branch_credit_ids, branch_debit_ids, by = "id")

## Total debit of accounts within branches transactions
branches_debit <- transactions_branches %>%
  deb_debit() %>%
  mutate(debit = deb_decimal(lsd, unit = "l")) %>%
  select(-lsd)

branches_credit <- transactions_branches %>%
  deb_credit() %>%
  mutate(credit = deb_decimal(lsd, unit = "l")) %>%
  select(-lsd)

# Build nodes from parts
nodes <- branches_groups %>%
  left_join(accounts_group, by = "id") %>%
  left_join(branches_credit, by = c("id" = "account_id")) %>%
  left_join(branches_debit, by = c("id" = "account_id")) %>%
  # Labels
  mutate(label = if_else(credit > 2500 & type != "Inheritance" | debit > 2500 & type != "Inheritance",
                         paste(group), NA_character_),
         color = if_else(type == "Inheritance", paste(group), NA_character_),
         color = factor(color, levels = siblings_fct))

# Create igraph object
# Creates vertices from branches data
branches <- graph_from_data_frame(d = transactions_sum,
                                  vertices = nodes, directed = TRUE)

# Size is total debit
set.seed(240)
ggraph(branches, layout = "kk") +
  geom_edge_fan(aes(edge_alpha = l),
                arrow = arrow(length = unit(3, 'mm')),
                end_cap = circle(2, 'mm')) +
  scale_edge_alpha(labels = scales::dollar_format(prefix = "£")) +
  geom_node_point(aes(size = debit, color = color), alpha = 0.9) +
  geom_node_text(aes(label = label), repel = TRUE) +
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format(prefix = "£")) +
  scale_color_discrete(labels = c(siblings_fct, "Other")) +
  labs(title = "Subgraph of the branches in the trade of Jan de Oude",
       subtitle = "8 November 1582 to 15 March 1585",
       size = "Total debit",
       edge_alpha = "Transactions",
       color = "Heirs") +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1)) +
  theme_ipsum(grid = FALSE,
              plot_title_size = 20,
              subtitle_size = 14) +
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

ggsave("plots/branches-1585-03-15.png", width = 10, height = 8)

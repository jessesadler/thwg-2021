### Import data ###

library(tidyverse)
library(debkeepr)

# Transactions data -------------------------------------------------------

transactions <- read_csv("data-raw/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(credit:debit, date:denarii) %>% 
  rename(l = librae, s = solidi, d = denarii)

transactions <- deb_gather_lsd(transactions, replace = TRUE)

write_rds(transactions, "data/transactions.rds")


# Accounts data -----------------------------------------------------------

accounts <- read_csv("data-raw/accounts.csv") %>% 
  select(account_id = id, account:location) %>% 
  rowid_to_column("id")

write_csv(accounts, "data/accounts.csv")


# Transactions and accounts by group --------------------------------------

# Get tibble of distinct groups and create ids
# Convert Heir type to Inheritance so Jacques' accounts are under inheritance.
# This also makes Hester and Cornelia account under inheritance, but this
# should be fine.

accounts <- accounts %>% 
  mutate(type = str_replace_all(type, "Heir", "Inheritance"))

groups <- accounts %>% 
  distinct(group) %>% 
  rowid_to_column("id_group")

accounts_group <- left_join(groups, accounts, by = "group") %>% 
  distinct(group, .keep_all = TRUE) %>% 
  select(-id) %>% 
  rename(id = id_group)

# Transactions
# Get the group id for each account
accounts_group_id <- left_join(accounts, groups, by = "group") %>% 
  select(id = id_group, account_id, group)

# Join group data to transactions by account_id
transactions_group <- transactions %>% 
  left_join(accounts_group_id, by = c("credit" = "account_id")) %>% 
  left_join(accounts_group_id, by = c("debit" = "account_id")) %>% 
  select(-credit, -debit) %>% 
  rename(credit = id.x, debit = id.y) %>% 
  select(credit, debit, date, lsd) %>% 
  filter(credit != debit) # Remove transactions between the same group

write_csv(accounts_group, "data/accounts-group.csv")
write_rds(transactions_group, "data/transactions-group.rds")

## Function to convert running accounts to xts and from xts ##

library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(timetk)
library(xts, warn.conflicts = FALSE)
library(debkeepr)

# Create running accounts by group function takes a transactions data frame and
# vector of ids to filter by. Returns a running account with group, date, and
# deb_lsd column.
# Can also be used as cumulative for single group, such as single heir.
deb_running <- function(transactions, accounts, label, ids) {
  label <- enquo(label)
  
  credit <- transactions %>% 
    group_by(credit, date) %>% 
    summarise(lsd = sum(lsd), .groups = "drop") %>% 
    rename(id = credit)
  debit <- transactions %>% 
    group_by(debit, date) %>% 
    summarise(lsd = -sum(lsd), .groups = "drop") %>% 
    rename(id = debit)
  
  account_groups <- accounts %>%
    select(id = account_id, !! label)
  
  bind_rows(credit, debit) %>% 
    filter(id %in% ids) %>% 
    left_join(account_groups, by = "id") %>% 
    group_by(!! label, date) %>% 
    summarise(lsd = sum(lsd), .groups = "drop_last") %>% 
    mutate(current = cumsum(lsd)) %>% 
    select(-lsd)
}

# Cumulative running value of a set of accounts. Same as above, but instead of
# grouping by a group, this only groups by date. Output is tibble with date and
# cumulative column. This can work with above to add cumulative to a group of
# running accounts. Creates a Cumulative grouping.

deb_running_cumulative <- function(transactions, accounts, ids, account_column) {
  account_column <- enquo(account_column)
  account_column <- quo_name(account_column)
  
  credit <- transactions %>% 
    group_by(credit, date) %>% 
    summarise(lsd = sum(lsd), .groups = "drop") %>% 
    rename(id = credit)
  debit <- transactions %>% 
    group_by(debit, date) %>% 
    summarise(lsd = -sum(lsd), .groups = "drop") %>% 
    rename(id = debit)
  
  bind_rows(credit, debit) %>% 
    filter(id %in% ids) %>% 
    group_by(date) %>% 
    summarise(lsd = sum(lsd)) %>% 
    mutate(current = cumsum(lsd)) %>% 
    select(-lsd) %>% 
    add_column(!! account_column := "Cumulative", .before = 1)
}

# To xts takes a running account tbl with date, id/group, and current value columns
# It uses tidyr to widen the tbl, timetk to convert to xts object,
# and the na.locf function to fill in amounts for each day
# Returns an xts object with one observation for each id for each day

to_fill_xts <- function(df) {
  
  df_wide <- df %>% 
    mutate(current = as.numeric(current)) %>% 
    spread(group, current)
  df_xts <- tk_xts(df_wide, silent = TRUE)
  na.locf(merge(df_xts, seq(min(df$date),
                            max(df$date), by = 1)))
}

# From xts to tbl
# Reverses `to_fill_xts`, but with value for each day for each id/group
# Only needs xts object with dates as row names, ids as column names, and values
# Result is a tibble with date, id, and current variables

from_fill_xts <- function(xts) {
  tbl <- tk_tbl(xts, rename_index = "date")
  gather(tbl, id, current, -date)
}

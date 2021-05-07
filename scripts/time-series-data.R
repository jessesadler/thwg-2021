## Create time series data ##

# Create time series data, fill in missing dates,
# and have all open accounts go to the end of the book

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(debkeepr)

transactions <- read_rds("data/transactions.rds")


# Create running accounts and fill data -----------------------------------

# Cast to deb_decimal
# tidyr::fill() does not work with deb_lsd
# And it speeds things up
transactions_dec <- transactions %>%
  arrange(date) %>%
  mutate(lsd = deb_as_decimal(lsd))

# Gather credit and debit into separate tibbles
credit <- transactions_dec %>%
  select(account_id = credit, date, lsd)
debit <- transactions_dec %>%
  select(account_id = debit, date, lsd) %>%
  mutate(lsd = -lsd) # Turn debits negative

# Create running account data, summarise by day, create current
running <- bind_rows(credit, debit) %>%
  group_by(account_id, date) %>%
  # Make sure tibble is still grouped by account_id of cumsum() and complete()
  # Per day
  summarise(day = sum(lsd), .groups = "drop_last") %>%
  # Cumulative
  mutate(current = cumsum(day))


# Fill in missing days to last day with a transaction for each account
running_fill <- running %>% # running has to be grouped by account_id
  tidyr::complete(date = seq(min(date), max(date), by = "1 day"),
                  # Days with no transactions = 0
                  fill = list(day = 0)) %>%
  # Current stays the same until next day with transaction
  tidyr::fill(current)


# Fill in values to last day of book for open accounts --------------------

# Distinguish between closed accounts
closed_ids <- running_fill %>%
  filter(date == max(date) & near(current, 0)) %>%
  pull(account_id)

open_ids <- running_fill %>%
  filter(date == max(date) & !near(current, 0)) %>%
  pull(account_id)

# Check that lengths are correct
length(c(closed_ids, open_ids)) == length(unique(running$account_id))

# Create separate closed and open tibbles
closed_running <- running_fill %>%
  filter(account_id %in% closed_ids)

open_running <- running_fill %>%
  filter(account_id %in% open_ids)

# Check that lengths are correct
nrow(closed_running) + nrow(open_running) == nrow(running_fill)

# Fill dates of open account to end of account book
open_full_running <- open_running %>%
  tidyr::complete(date = seq(min(date), max(transactions$date), by = "1 day"),
                  # Days with no transactions = 0
                  fill = list(day = 0)) %>%
  # Current stays the same until next day with transaction
  tidyr::fill(current)

# Check work

# deb_open has same values as open_full_running on last day
open <- deb_open(transactions)

y <- open_full_running %>%
  filter(date == max(date)) %>%
  mutate(current = deb_as_lsd(current)) %>%
  pull(current)

all.equal(open$lsd, y)

# Last date for all open accounts is max(date) of transactions
open_full_running %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  distinct(date) %>%
  pull()


# Bring things together ---------------------------------------------------

complete_running_fill <- bind_rows(closed_running, open_full_running) %>%
  # Take out observations in gap between 15 March 1585 and 5 September 1593
  filter(date <= ymd("1585-03-16") | date >= ymd("1594-09-04")) %>%
  arrange(date, account_id) %>%
  # Cast back to deb_lsd
  mutate(day = deb_as_lsd(day),
         current = deb_as_lsd(current)) %>%
  # Now ungroup
  ungroup()

# Check work: Amount of observations added to open accounts
nrow(open_full_running) - nrow(open_running)
nrow(bind_rows(closed_running, open_full_running)) - nrow(running_fill)

last_day <- complete_running_fill %>%
  # Need to group_by() account since most accounts do not go to last day of books
  group_by(account_id) %>%
  filter(date == max(date) & current != 0)

all.equal(open$lsd, last_day$current)

# Sum of daily transactions should be £0
# Credits - debits
sum(complete_running_fill$day)

# Write data --------------------------------------------------------------

write_rds(complete_running_fill, "data/running_accounts.rds")

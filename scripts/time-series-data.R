## Create time series data ##

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(debkeepr)

transactions <- read_rds("data/transactions.rds")

# Cast to deb_decimal
# tidyr::fill() does not work with deb_lsd
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

# Fill in missing days
running_fill <- running %>% # running has to be grouped by account_id
  tidyr::complete(date = seq(min(date), max(date), by = "1 day"),
                  # Days with no transactions = 0
                  fill = list(day = 0)) %>%
  # Current stays the same until next day with transaction
  tidyr::fill(current) %>%
  # Cast back to deb_lsd
  mutate(day = deb_as_lsd(day),
         current = deb_as_lsd(current)) %>%
  # Now ungroup
  ungroup() %>%
  # Take out observations in gap between 15 March 1585 and 5 September 1593
  filter(date <= ymd("1585-03-16") | date >= ymd("1594-09-04"))


# Check work:

open <- deb_open(transactions)
last_day <- running_fill %>%
  # Need to group_by() account since most accounts do not go to last day of books
  group_by(account_id) %>%
  filter(date == max(date) & current != 0)

all.equal(open$lsd, last_day$current)

# Dealing with gap of dates
dfl12_002 <- running_fill %>%
  filter(account_id == "dfl12_002")

ymd("1585-03-16") - ymd("1582-11-08") + ymd("1594-12-20") - ymd("1594-09-04")


gap <- dfl12_002 %>%
  filter(date <= lubridate::ymd("1585-03-16") | date >= lubridate::ymd("1594-09-04"))

library(ggplot2)

ggplot(dfl12_002) +
  geom_line(aes(x = date, y = as.numeric(current), group = account_id))

ggplot(dfl12_002) +
  geom_line(aes(x = date, y = as.numeric(current), group = account_id))

ggplot(dfl12_002) +
  geom_point(aes(x = date, y = as.numeric(current)))

ggplot(gap) +
  geom_point(aes(x = date, y = as.numeric(current)))

## Time series inheritance ##

# Get xts object of running value of cumulative credit of all nine heirs

library(tidyverse)
library(xts)
library(timetk)
library(lubridate)
library(dygraphs)
library(debkeepr)
library(ggrepel)
library(hrbrthemes)
source("time-series-functions.R")

# This script uses tbl-xts functions

# Load data
transactions <- read_rds("data/transactions.rds")
accounts <- read_csv("data/accounts.csv")

siblings_fct <- c("Jan", "Anna", "Marten", "Carlo", "Jacques",
                  "Steven", "Maria", "Hester", "Cornelia")

inheritance_ids <- filter(accounts, type == "Inheritance") %>% 
  pull(account_id)

# Create inheritance running with groups from inheritance accounts
inheritance_running <- deb_running(transactions, accounts, label = group, inheritance_ids)

# To xts
inheritance_xts <- to_fill_xts(inheritance_running) %>% 
  replace_na(0)

# To tibble
inheritance_tbl <- from_fill_xts(inheritance_xts) %>% 
  mutate(lsd = deb_as_lsd(current),
         text = deb_text(lsd),
         label = case_when(date == ymd("1582-11-08") & current > 0 & id != "Hester" ~ text,
                           current == max(current) & date == ymd("1583-12-26") ~
                             paste("26 December 1583", text, sep = ": ")),
         id = factor(id, levels = siblings_fct))

## Breaks ##
date_break1 <- ymd("1585-03-16")
date_break2 <- ymd("1594-09-01")

inheritance1 <- inheritance_tbl %>% 
  filter(date < date_break1)

inheritance2 <- inheritance_tbl %>% 
  filter(date >= date_break2)

# Facet plots -------------------------------------------------------------

# Facet: Full plot
ggplot(inheritance_tbl) + 
  geom_line(aes(x = date, y = current, group = id, color = id)) + 
  scale_y_continuous(breaks = c(0, 5000, 10000),
                     labels = scales::dollar_format(prefix = "£")) + 
  scale_x_date(breaks = scales::pretty_breaks(), date_labels = "%Y") + 
  labs(y = NULL, x = NULL, color = "Heirs") + 
  facet_wrap(~ id, nrow = 3) + 
  theme_ipsum(base_size = 14) + 
  theme(legend.position = "none") + 
  ggtitle("Inheritance due to the heirs of Jan de Oude",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")


# Facet: 1582-1585
ggplot(inheritance1) + 
  geom_line(aes(x = date, y = current, group = id, color = id)) + 
  scale_y_continuous(breaks = c(0, 5000, 10000),
                     labels = scales::dollar_format(prefix = "£")) + 
  scale_x_date(breaks = scales::breaks_width("1 year"), date_labels = "%Y") + 
  labs(y = NULL, x = NULL, color = "Heirs") + 
  facet_wrap(~ id, nrow = 3) + 
  theme_ipsum(base_size = 12,
              base_family = "Avenir Next") + 
  theme(legend.position = "none") + 
  ggtitle("Inheritance due to the heirs of Jan de Oude",
          subtitle = "November 1582 to March 1585")

# Facet: 1594
ggplot(inheritance2) + 
  geom_line(aes(x = date, y = current, group = id, color = id)) + 
#  gghighlight::gghighlight(label_key = id, use_direct_label = FALSE) + 
  scale_y_continuous(breaks = c(0, 2000, 4000),
                     labels = scales::dollar_format(prefix = "£")) + 
  scale_x_date(breaks = scales::breaks_width("1 month"),
               labels = scales::label_date_short()) + 
  labs(y = NULL, x = NULL, color = "Heirs") + 
  facet_wrap(~ id, nrow = 3) + 
  theme_ipsum(base_size = 12,
              base_family = "Avenir Next") + 
  theme(legend.position = "none") + 
  ggtitle("Inheritance due to the heirs of Jan de Oude",
          subtitle = "September 1594 to 16 December 1594")


# Single plots ------------------------------------------------------------


# Plot
ggplot(inheritance_tbl) + 
  geom_line(aes(x = date, y = current, group = id, color = id)) + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + 
  labs(y = NULL, x = NULL, color = "Heirs") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Inheritance due to the heirs of Jan de Oude",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots/inheritance-running.png", width = 10, height = 8)

# Plot with only before 16 March 1585 and after 1 December 1594


# Find max lsd
inheritance_tbl %>% 
  filter(date < date_break1) %>% 
  filter(current == max(current))

ggplot(inheritance1) + 
  geom_line(aes(x = date, y = current, group = id, color = id)) + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) + 
  scale_x_date(breaks = scales::breaks_width("1 year"), 
               labels = scales::label_date_short(),
               expand = c(0.1, 0.1)) + 
  labs(y = NULL, x = NULL, color = "Heirs") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Inheritance due to the heirs of Jan de Oude",
          subtitle = "November 1582 to March 1585")

ggsave("plots-aans/inheritance-running-1.png", width = 10, height = 8)

# Find when Hester is zeroed out
inheritance_tbl %>% 
  filter(date >= date_break2) %>% 
  filter(current == 0 & id == "Hester")

inheritance2 <- inheritance_tbl %>% 
  filter(date >= date_break2) %>% 
  mutate(label = case_when(date == ymd("1594-12-16") & (l > 10 | l < 0) ~ lsd,
                           current == 0 & date == ymd("1594-10-15") ~ 
                             paste("", "", "Hester: 15 October 1594,", lsd, sep = "\n")))

ggplot(inheritance2) + 
  geom_line(aes(x = date, y = l, group = id, color = id), size = 1) + 
  geom_text(aes(x = date, y = l, label = label),
            nudge_y = -50,
            nudge_x = 4) + 
  geom_hline(yintercept = 0, size = 1) + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y",
               expand = c(0.1, 0.1)) + 
  labs(y = NULL, x = NULL, color = "Heirs") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Inheritance due to the heirs of Jan de Oude",
          subtitle = "September 1594 to 16 December 1594")

ggsave("plots-aans/inheritance-running-2.png", width = 10, height = 8)
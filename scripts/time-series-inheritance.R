## Time series inheritance ##

# Get xts object of running value of cumulative credit of all nine heirs

library(tidyverse)
library(xts)
library(timetk)
library(lubridate)
library(debkeepr)
library(hrbrthemes)
library(glue)
library(ggtext)
source("scripts/time-series-functions.R")

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
  labs(title = "Inheritance due to the heirs of Jan de Oude",
       subtitle = "November 1582 to March 1585",
       caption = "Figure 5") +
  facet_wrap(~ id, nrow = 3) +
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/inheritance-1583-1585-facet.png", width = 8, height = 6)

# Facet: 1594

# End value for text annotation
end_value <- inheritance2 %>%
  filter(date == last(date)) %>%
  mutate(text = paste0("End value: ", text))

ggplot(inheritance2) +
  geom_line(aes(x = date, y = current, group = id, color = id)) +
  geom_text(data = end_value, aes(x = ymd("1594-11-01"), y = 4200, label = text), size = 3.5) +
  scale_y_continuous(breaks = c(0, 2000, 4000),
                     labels = scales::dollar_format(prefix = "£")) +
  scale_x_date(breaks = scales::breaks_width("1 month"),
               labels = scales::label_date_short()) +
  labs(title = "Inheritance due to the heirs of Jan de Oude",
       subtitle = "September 1594 to 16 December 1594",
       caption = "Figure 10") +
  facet_wrap(~ id, nrow = 3) +
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/inheritance-1594-facet.png", width = 8, height = 6)

# Single plot -------------------------------------------------------------

# Full: gghighlight
hex <- scales::hue_pal()(9)
styled_subtitle <- glue("Highlighting the inheritance of
                        <span style='color:{hex[[8]]};'>Hester</span> and
                        <span style='color:{hex[[4]]};'>Carlo</span>")

ggplot(inheritance_tbl) +
  geom_line(aes(x = date, y = current, group = id, color = id), size = 1) +
  gghighlight::gghighlight(id == "Hester" | id == "Carlo",
                           label_key = id,
                           use_group_by = FALSE,
                           use_direct_label = FALSE) +
  scale_color_manual(values = c(hex[[4]], hex[[8]])) +
  scale_y_continuous(breaks = c(0, 5000, 10000),
                     labels = scales::dollar_format(prefix = "£")) +
  scale_x_date(breaks = scales::pretty_breaks(), date_labels = "%Y") +
  labs(title = "Inheritance of the heirs of Jan de Oude, 1582–1594",
       subtitle = styled_subtitle,
       caption = "Figure 6") +
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_markdown())

ggsave("plots/inheritance-running-gghighlight.png", width = 8, height = 6)

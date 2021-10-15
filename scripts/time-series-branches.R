library(tidyverse)
library(debkeepr)
library(lubridate)
library(hrbrthemes)
library(glue)
library(ggtext)

# Read data
running <- read_rds("data/running_accounts.rds")
accounts <- read_csv("data/accounts.csv") %>%
  select(account_id, group, type)

branch_ids <- accounts %>%
  filter(type == "Branch") %>%
  pull(account_id)

branch_running <- running %>%
  filter(account_id %in% branch_ids) %>%
  # Cast to numeric for speed and for complete and fill
  mutate(day = as.numeric(day)) %>%
  left_join(accounts, by = "account_id") %>%
  # Group and summarise by heirs and date
  group_by(group, date) %>%
  summarise(day = sum(day), .groups = "drop_last") %>%
  mutate(current = cumsum(day)) %>%
  ungroup()



# Split data between 1582-85 and 1594 -------------------------------------

## Breaks ##
date_break1 <- ymd("1585-03-16")
date_break2 <- ymd("1594-09-01")

branch1 <- branch_running %>%
  filter(date < date_break1)

branch2 <- branch_running %>%
  filter(date >= date_break2)

ggplot(branch1) +
  geom_line(aes(x = date, y = current, group = group, color = group), size = 1) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  scale_x_date(breaks = scales::breaks_width("1 year"), date_labels = "%Y") +
  labs(title = "Phase 1: Novemeber 1582 to March 1585",
       subtitle = styled_subtitle,
       color = "Branches") +
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_markdown())

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
  mutate(current = cumsum(day),
         current = -current) %>%
  ungroup()



# Split data between 1582-85 and 1594 -------------------------------------

## Breaks ##
date_break1 <- ymd("1585-03-16")
date_break2 <- ymd("1594-09-01")

branch1 <- branch_running %>%
  filter(date < date_break1)

branch2 <- branch_running %>%
  filter(date >= date_break2)

hex <- scales::hue_pal()(3)
styled_title <- glue("
      Capital held by the branches of
      <span style='color:{hex[[1]]};'>London</span>,
      <span style='color:{hex[[2]]};'>Venice</span>, and
      <span style='color:{hex[[3]]};'>Verona</span>")

ggplot(branch1) +
  geom_line(aes(x = date, y = current, group = group, color = group), size = 1) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  scale_x_date(breaks = scales::breaks_width("1 year"), date_labels = "%Y") +
  labs(title = styled_title,
       subtitle = "8 November 1582 to 15 March 1585") +
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_markdown())

ggsave("plots/branches-running-1585.png", width = 8, height = 6)

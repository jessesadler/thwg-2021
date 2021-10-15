## Time series of inheritance accounts ##

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

# Filter running accounts to inheritance accounts
siblings_fct <- c("Jan", "Anna", "Marten", "Carlo", "Jacques",
                  "Steven", "Maria", "Hester", "Cornelia")

inheritance_ids <- accounts %>%
  filter(type == "Inheritance") %>%
  pull(account_id)

inher_running <- running %>%
  filter(account_id %in% inheritance_ids) %>%
  # Cast to numeric for speed and for complete and fill
  mutate(day = as.numeric(day)) %>%
  left_join(accounts, by = "account_id") %>%
  # Group and summarise by heirs and date
  group_by(group, date) %>%
  summarise(day = sum(day), .groups = "drop_last") %>%
  mutate(current = cumsum(day),
         group = factor(group, levels = siblings_fct)) %>%
  ungroup()



# Split data between 1582-85 and 1594 -------------------------------------

## Breaks ##
date_break1 <- ymd("1585-03-16")
date_break2 <- ymd("1594-09-01")

inher1 <- inher_running %>%
  filter(date < date_break1)

inher2 <- inher_running %>%
  filter(date >= date_break2)


# Facet plots -------------------------------------------------------------

# Facet: Full plot
ggplot(inher_running) +
  geom_line(aes(x = date, y = current, group = group, color = group)) +
  scale_y_continuous(breaks = c(0, 5000, 10000),
                     labels = scales::dollar_format(prefix = "£")) +
  scale_x_date(breaks = scales::pretty_breaks(), date_labels = "%Y") +
  labs(y = NULL, x = NULL, color = "Heirs") +
  facet_wrap(~ group, nrow = 3) +
  theme_ipsum(base_size = 14) +
  theme(legend.position = "none") +
  ggtitle("Inheritance due to the heirs of Jan de Oude",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")


# Facet: 1582-1585
ggplot(inher1) +
  geom_line(aes(x = date, y = current, group = group, color = group)) +
  scale_y_continuous(breaks = c(0, 5000, 10000),
                     labels = scales::dollar_format(prefix = "£")) +
  scale_x_date(breaks = scales::breaks_width("1 year"), date_labels = "%Y") +
  labs(title = "Inheritance due to the heirs of Jan de Oude",
       subtitle = "November 1582 to March 1585",
       caption = "Figure 5") +
  facet_wrap(~ group, nrow = 3) +
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/inheritance-1583-1585-facet.png", width = 8, height = 6)


# Facet: 1594

# End value for text annotation
end_value <- inher2 %>%
  group_by(group) %>%
  filter(date == max(date)) %>%
  mutate(lsd = deb_as_lsd(current),
         text = paste0("End value: ", deb_text(lsd)))

ggplot(inher2) +
  geom_line(aes(x = date, y = current, group = group, color = group)) +
  geom_text(data = end_value, aes(x = ymd("1594-11-01"), y = 4200, label = text), size = 3.5) +
  scale_y_continuous(breaks = c(0, 2000, 4000),
                     labels = scales::dollar_format(prefix = "£")) +
  scale_x_date(breaks = scales::breaks_width("1 month"),
               labels = scales::label_date_short()) +
  labs(title = "Inheritance due to the heirs of Jan de Oude",
       subtitle = "September 1594 to 16 December 1594",
       caption = "Figure 10") +
  facet_wrap(~ group, nrow = 3) +
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/inheritance-1594-facet.png", width = 8, height = 6)


# Single plot -------------------------------------------------------------

# Full: gghighlight
hex <- scales::hue_pal()(length(siblings_fct))
styled_subtitle <- glue("Highlighting the inheritance of
                        <span style='color:{hex[[8]]};'>Hester</span> and
                        <span style='color:{hex[[4]]};'>Carlo</span>")

ggplot(inher_running) +
  geom_line(aes(x = date, y = as.numeric(current), group = group, color = group), size = 1) +
  gghighlight::gghighlight(group == "Hester" | group == "Carlo",
                           label_key = group,
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

# Phase 1: 1582-1585
ggplot(inher1) +
  geom_line(aes(x = date, y = current, group = group, color = group), size = 1) +
  gghighlight::gghighlight(group == "Hester" | group == "Carlo",
                           label_key = group,
                           use_group_by = FALSE,
                           use_direct_label = FALSE) +
  scale_color_manual(values = c(hex[[4]], hex[[8]])) +
  scale_y_continuous(breaks = c(0, 5000, 10000),
                     labels = scales::dollar_format(prefix = "£")) +
  scale_x_date(breaks = scales::breaks_width("1 year"), date_labels = "%Y") +
  labs(title = "Phase 1: Novemeber 1582 to March 1585",
       subtitle = styled_subtitle,
       color = "Heirs") +
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_markdown())

ggsave("plots/phase-1.png", width = 8, height = 6)

# Phase 3: 1594
ggplot(inher2_reorder) +
  geom_line(aes(x = date, y = current, group = group, color = group), size = 1) +
  gghighlight::gghighlight(group == "Hester" | group == "Carlo",
                           label_key = group,
                           use_group_by = FALSE,
                           use_direct_label = FALSE) +
  scale_color_manual(values = c(hex[[4]], hex[[8]])) +
  scale_y_continuous(breaks = c(0, 2000, 4000),
                     labels = scales::dollar_format(prefix = "£")) +
  scale_x_date(breaks = scales::breaks_width("1 month"),
               labels = scales::label_date_short()) +
  labs(title = "Phase 3: September to December 1594",
       subtitle = styled_subtitle,
       color = "Heirs") +
  theme_ipsum(base_size = 14,
              caption_face = "plain",
              caption_size = 14) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_markdown())

ggsave("plots/phase-3.png", width = 8, height = 6)

## Circles for opening and closing values ##

library(tidyverse)
library(debkeepr)
library(packcircles)
library(hrbrthemes)

# Data
transactions <- read_rds("data/transactions.rds")
accounts <- read_csv("data/accounts.csv") %>% 
  select(id = account_id, label)

# Opening values ----------------------------------------------------------

# Simplify account names that will be used for labels
accounts$label <- str_replace(accounts$label,
                              "Jan van Borne running account",
                              "Jan van Borne \n running account")
accounts$label <- str_replace(accounts$label,
                              "Jan de Backere and Sevaes Wouters",
                              "Jan de Backere \n and Sevaes Wouters")
accounts$label <- str_replace(accounts$label,
                              "Gillis del Ponte and Hilaria della Faille",
                              "Gillis del Ponte \n and Hilaria della Faille")

opening_debit <- transactions %>% 
  filter(credit == "dfl12_001") %>% 
  select(id = debit, lsd) %>% 
  add_column(relation = "Debtor", .after = 1)

opening_credit <- transactions %>% 
  filter(debit == "dfl12_001") %>% 
  select(id = credit, lsd) %>% 
  add_column(relation = "Creditor", .after = 1)

opening <- bind_rows(opening_debit, opening_credit) %>% 
  left_join(accounts, by = "id") %>% 
  mutate(value = as.numeric(lsd),
         label = if_else(lsd >= 1000, paste(label, deb_text(lsd), sep = "-"), ""),
         # Relation as factor to make debit be red
         relation = factor(relation, levels = c("Debtor", "Creditor")))

## Make circles ##
packing_opening <- circleProgressiveLayout(opening$value, sizetype = "area")
opening_circles <- bind_cols(opening, packing_opening)

# Add type to circles data
relation_id_opening <- opening %>% 
  rowid_to_column("row_id") %>% 
  select(id = row_id, relation)

dat.gg_opening <- circleLayoutVertices(packing_opening, npoints = 50) %>% 
  left_join(relation_id_opening, by = "id")

# Total value at opening
debit_text <- transactions %>% 
  deb_account("dfl12_001") %>% 
  mutate(text = deb_text(lsd)) %>% 
  pull(text)

# Plot
ggplot() + 
  geom_polygon(data = dat.gg_opening, aes(x, y, group = id, fill = relation),
               color = "black", alpha = 0.6) +
  geom_text(data = opening_circles, aes(x, y, size = value,
                                        label = str_replace_all(label, "-", "\n"))) +
  scale_size_continuous(range = c(1, 4.5)) +
  guides(size = FALSE) + 
  labs(title = "Value of accounts in the estate of Jan della Faille de Oude, 8 December 1582",
       subtitle = paste0("Opening value of the estate: ", debit_text[[2]]),
       caption = "Figure 2",
       fill = "Relation") + 
  theme_ipsum(plot_margin = margin(20, 20, 20, 20),
              grid = FALSE,
              caption_face = "plain",
              caption_size = 14) + 
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14)) + 
  coord_equal()

ggsave("plots/circles-opening.png", width = 10, height = 8)


# Closing values ----------------------------------------------------------

# Clean accounts data
accounts <- read_csv("data/accounts.csv") %>% 
  select(id = account_id, label)

# Simplify account names that will be used for labels
accounts$label <- str_replace_all(accounts$label, 
                                  "Heirs of Maria.*",
                                  "Heirs of Maria")
accounts$label <- str_replace(accounts$label, 
                              "Marten della Faille’s.*",
                              "Heirs of \n Anna de Hane")
accounts$label <- str_replace(accounts$label, 
                              "Jacques, Jan.*",
                              "Jacques, Jan, \n Carlo, and Hester")
accounts$label <- str_replace(accounts$label,
                              "Branch of Venice new.*",
                              "Branch of Venice")
accounts$label <- str_replace(accounts$label,
                              "Marten, Steven.*",
                              "Marten, Steven, \n and Anna")
accounts$label <- str_replace(accounts$label,
                              "Heirs of Gilles.*",
                              "Heirs of \n Gilles Hasebaert")
accounts$label <- str_replace(accounts$label,
                              "Expenses of the lawsuit.*",
                              "Lawsuit against \n Daniel de Hane")
accounts$label <- str_replace(accounts$label,
                              "Creditors of the Book",
                              "Creditors of \n the Book")
accounts$label <- str_replace(accounts$label,
                              "Expenses of English wool",
                              "Expenses of \n English wool")
accounts$label <- str_replace(accounts$label,
                              "Accounts to be written off",
                              "Written off")

closing <- deb_open(transactions) %>% 
  left_join(accounts, by = c("account_id" = "id")) %>% 
  mutate(relation = if_else(lsd > 0, "Creditor", "Debtor"),
         # Relation as factor to make debit be red
         relation = factor(relation, levels = c("Debtor", "Creditor")),
         # make all values positive for size and labeling
         lsd = if_else(lsd > 0 , lsd, -lsd),
         value = as.numeric(lsd),
         label = if_else(lsd >= 200, paste(label, deb_text(lsd), sep = "-"), ""))

## Make circles ##
packing_closing <- circleProgressiveLayout(closing$value, sizetype = "area")
closing_circles <- bind_cols(closing, packing_closing)

# Add type to circles data
relation_id_closing <- closing %>% 
  rowid_to_column("row_id") %>% 
  select(id = row_id, relation)

dat.gg_closing <- circleLayoutVertices(packing_closing, npoints = 50) %>% 
  left_join(relation_id_closing, by = "id")

# Total value at end of 1594
balance_text <- deb_balance(transactions) %>% 
  mutate(text = deb_text(lsd)) %>% 
  pull(text)

# Plot
ggplot() + 
  geom_polygon(data = dat.gg_closing, aes(x, y, group = id, fill = relation),
               color = "black", alpha = 0.6) +
  geom_text(data = closing_circles, aes(x, y, size = value,
                                        label = str_replace_all(label, "-", "\n"))) +
  scale_size_continuous(range = c(1, 4)) +
  guides(size = FALSE) + 
  labs(title = "Value of accounts in the estate of Jan della Faille de Oude, 31 December 1594",
       subtitle = paste0("Capital remaining in the estate at the close of the books: ", balance_text[[1]]),
       caption = "Figure 9",
       fill = "Relation") + 
  theme_ipsum(plot_margin = margin(20, 20, 20, 20),
              grid = FALSE,
              caption_face = "plain",
              caption_size = 14) + 
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14)) + 
  coord_equal()

ggsave("plots/circles-closing.png", width = 10, height = 8)


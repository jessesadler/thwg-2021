accounts <- read_csv("data/accounts.csv")

max <- running %>%
  mutate(current = if_else(current > 0, current, -current)) %>%
  group_by(account_id) %>%
  summarise(max = max(current), .groups = "drop") %>%
  arrange(desc(max))


min_max <- running %>%
  group_by(account_id) %>%
  summarise(max = max(current),
            min = min(current), .groups = "drop") %>%
  arrange(desc(max))


min <- min_max %>%
  filter(min < -1000)
max <- min_max %>%
  filter(max > 1000)

min_accounts <- accounts %>%
  filter(account_id %in% min$account_id)
max_accounts <- accounts %>%
  filter(account_id %in% max$account_id)

# Debtor accounts
running %>%
  filter(account_id %in% min_accounts$account_id) %>%
  left_join(accounts, by = "account_id") %>%
  ggplot() +
  geom_line(aes(x = date, y = as.numeric(current), group = account_id, color = type))

# Creditor accounts
running %>%
  filter(account_id %in% max_accounts$account_id) %>%
  left_join(accounts, by = "account_id") %>%
  ggplot() +
  geom_line(aes(x = date, y = as.numeric(current), group = account_id, color = type))

min %>%
  left_join(accounts, by = "account_id") %>%
  mutate(label = fct_reorder(label, -as.numeric(min))) %>%
ggplot() +
  geom_col(aes(x = label, y = -as.numeric(min))) +
  coord_flip()

max %>%
  left_join(accounts, by = "account_id") %>%
  mutate(account_id = fct_reorder(account_id, as.numeric(max))) %>%
  ggplot() +
  geom_col(aes(x = account_id, y = as.numeric(max), fill = type)) +
  coord_flip()

running %>%
  left_join(accounts, by = "account_id") %>%
  filter(type == "Branch") %>%
  ggplot() +
  geom_line(aes(x = date, y = as.numeric(current), group = account_id, color = group))

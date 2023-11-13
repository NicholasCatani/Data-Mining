# Libraries

library(ggplot2) # plot library
library(tidyverse) # for data manipulation
library(gridExtra) # multiple plots in 1
library(scales) # show the colors
library(ggrepel) # for graph repel (labels)
library(scales) # for % in density plots


# Read in the data
data <- read_csv()


# Preprocessing steps
data <- data %>% 
  # remove columns with 1 constant value
  dplyr::select(-zipcodeOri, -zipMerchant) %>% 
  
  # remove comas
  mutate(customer = gsub("^.|.$", "", customer),
         age = gsub("^.|.$", "", age),
         gender = gsub("^.|.$", "", gender),
         merchant = gsub("^.|.$", "", merchant),
         category = gsub("^.|.$", "", category)) %>% 
  
  # remove es_ from "category"
  mutate(category = sub("es_", "", category)) %>% 
  
  # remove Unknown from Gender
  filter(gender != "U")

# Replace U in Age with "7"
data$age[which(data$age == "U")]<-"7"

# Create Amount Thresholds
data <- data %>% 
  mutate(amount_thresh = ifelse(amount<= 500, "0-500",
                                ifelse(amount<= 1000, "500-1000",
                                       ifelse(amount<= 1500, "1000-1500",
                                              ifelse(amount<= 2000, "1500-2000",
                                                     ifelse(amount<= 2500, "2000-2500",
                                                            ifelse(amount<= 3000, "2500-3000", ">3000")))))))

# Check data
data %>% head()


-------------------------------------------------------
  # Predefined theme
  my_theme <- theme(plot.background = element_rect(fill = "grey97", color = "grey25"),
                    panel.background = element_rect(fill = "grey97"),
                    panel.grid.major = element_line(colour = "grey87"),
                    text = element_text(color = "grey25"),
                    plot.title = element_text(size = 18),
                    plot.subtitle = element_text(size = 14),
                    axis.title = element_text(size = 11),
                    legend.box.background = element_rect(color = "grey25", fill = "grey97", size = 0.5),
                    legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5))
--------------------------------------------------------
  
  options(repr.plot.width=18, repr.plot.height=7)

data %>% 
  
  ggplot(aes(x = amount)) +
  geom_density(alpha = 0.1, size = 1, color = colors60s[2], fill = colors60s[2]) +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 0.08)) +
  labs(x = "Amount", y = "Density", title = "Amound Distribution", subtitle = "extreme skewness to right - maximum value 8329") +
  my_theme



gender <- data %>% 
  group_by(gender) %>% 
  summarise(avg_spent = mean(amount)) %>% 
  gather("Metric", "Value", 1:1)

age <- data %>% 
  group_by(age) %>% 
  summarise(avg_spent = mean(amount)) %>% 
  gather("Metric", "Value", 1:1)

category <- data %>% 
  group_by(category) %>% 
  summarise(avg_spent = mean(amount)) %>% 
  gather("Metric", "Value", 1:1)

options(repr.plot.width=18, repr.plot.height=7)

bind_rows(gender, age, category) %>% 
  mutate(Metric = factor(Metric, levels = c("category", "age", "gender"))) %>%
  
  ggplot(aes(x = reorder(Value, avg_spent), y = avg_spent)) +
  geom_bar(stat = "identity", aes(fill = as.factor(Metric))) +
  facet_wrap(~Metric, scales = "free", labeller = as_labeller(c("category"="Category", "age"="Age", "gender"="Gender"))) +
  
  coord_flip() +
  geom_label(aes(label = formatC(avg_spent, format="f", big.mark=",", digits=0)), size = 3) +
  scale_fill_manual(values = colors60s[c(3, 4, 5)], guide = "none") +
  my_theme +
  labs(x = "Value", y = "Average Spent ($)", title = "Biggest Spenders", subtitle = "average expenditure ($)")



options(repr.plot.width=18, repr.plot.height=7)

data %>% 
  mutate(amount_thresh = factor(amount_thresh, levels = c("0-500", "500-1000", "1000-1500", "1500-2000",
                                                          "2000-2500", "2500-3000", ">3000"))) %>% 
  
  ggplot(aes(x = amount_thresh, fill = as.factor(fraud))) +
  geom_bar(stat = "count", position = "fill") +
  
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  
  scale_fill_manual(values = colors60s[c(4, 2)]) +
  my_theme +
  labs(x = "Amount Threshold", y = "Percent%", title = "Fraud Percentage for Amount Thresholds", subtitle = "fraud probability increases with the increase of spent amount", fill = "Fraud")



options(repr.plot.width=18, repr.plot.height=7)

data %>% 
  mutate(flag = ifelse(amount <= 250, "below $250", "above $250")) %>% 
  group_by(fraud, flag) %>% 
  summarise(n = n()) %>% 
  
  ggplot(aes(x = as.factor(fraud), y = n)) +
  geom_bar(aes(fill = as.factor(flag)), stat = "identity") +
  facet_wrap(~ flag, scales = "free") +
  
  geom_label(aes(label = formatC(n, format="f", big.mark=",", digits=0)), size = 3.2) +
  scale_fill_manual(values = colors60s[c(2, 3)], guide = "none") +
  my_theme + theme(axis.text.y = element_blank()) +
  labs(x = "Is Fraud", y = "Frequency", title = "Fraud above and below $250/transaction", subtitle = "higher fraud probability above $250")



gender <- data %>% 
  filter(amount > 500) %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  gather("Metric", "Value", 1:1)

age <- data %>% 
  filter(amount > 500) %>% 
  group_by(age) %>% 
  summarise(n = n()) %>% 
  gather("Metric", "Value", 1:1)

category <- data %>% 
  filter(amount > 500) %>% 
  group_by(category) %>% 
  summarise(n = n()) %>% 
  gather("Metric", "Value", 1:1)

options(repr.plot.width=18, repr.plot.height=7)

bind_rows(gender, age, category) %>% 
  mutate(Metric = factor(Metric, levels = c("category", "age", "gender"))) %>%
  
  ggplot(aes(x = reorder(Value, n), y = n)) +
  geom_bar(stat = "identity", aes(fill = as.factor(Metric))) +
  facet_wrap(~Metric, scales = "free", labeller = as_labeller(c("category"="Category", "age"="Age", "gender"="Gender"))) +
  
  coord_flip() +
  geom_label(aes(label = formatC(n, format="f", big.mark=",", digits=0)), size = 3) +
  scale_fill_manual(values = colors60s[c(3, 4, 5)], guide = "none") +
  my_theme +
  labs(x = "Value", y = "Frequency", title = "Amount Outliers", subtitle = "how do the highest transactions look?")



options(repr.plot.width=18, repr.plot.height=7)

data %>% 
  group_by(fraud) %>% 
  summarise(n = n()) %>% 
  
  ggplot(aes(x = "", y = n)) +
  geom_bar(stat = "identity", aes(fill = as.factor(fraud)), position = "stack") +
  coord_polar("y", start=0) +
  #geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), label = formatC(n, format="f", big.mark=",", digits=0)), size=5) +
  
  geom_label(aes(y = n/2.5 + c(0, cumsum(n)[-length(n)]), label = formatC(n, format="f", big.mark=",", digits=0)), size=3.5) +
  scale_fill_manual(values = colors60s[c(2, 5)], guide = "none") +
  my_theme + theme(axis.text = element_blank(), axis.title = element_blank()) +
  labs(title = "Fraud Frequency", subtitle = "there are 1.2% fraud cases")



options(repr.plot.width=18, repr.plot.height=7)

data %>% 
  group_by(gender, fraud) %>% 
  summarise(n = n()) %>% 
  
  ggplot(aes(x = reorder(as.factor(gender), n), y = n)) +
  geom_bar(stat = "identity", aes(fill = as.factor(fraud))) +
  coord_flip() +
  facet_wrap(~ fraud, scales = "free", labeller = as_labeller(c("0"="Not Fraud", "1"="Fraud"))) +
  
  geom_label(aes(label = formatC(n, format="f", big.mark=",", digits=0)), size = 3.2) +
  scale_fill_manual(values = colors60s[c(4, 2)], guide = "none") +
  my_theme + theme(axis.text.x = element_blank()) +
  labs(x = "Gender", y = "Frequency", title = "Fraud Transactions for Gender", subtitle = "most frauds happen to females")



options(repr.plot.width=18, repr.plot.height=7)

data %>% 
  group_by(age, fraud) %>% 
  summarise(n = n()) %>% 
  
  ggplot(aes(x = reorder(as.factor(age), n), y = n)) +
  geom_bar(stat = "identity", aes(fill = as.factor(fraud))) +
  coord_flip() +
  facet_wrap(~ fraud, scales = "free", labeller = as_labeller(c("0"="Not Fraud", "1"="Fraud"))) +
  
  geom_label(aes(label = formatC(n, format="f", big.mark=",", digits=0)), size = 3.2) +
  scale_fill_manual(values = colors60s[c(4, 2)], guide = "none") +
  my_theme + theme(axis.text.x = element_blank()) +
  labs(x = "Age", y = "Frequency", title = "Fraud Transactions for Age", subtitle = "most frauds happen to people between 26 and 45 years old")



options(repr.plot.width=18, repr.plot.height=7)

data %>% 
  filter(category != "transportation") %>% 
  group_by(category) %>% 
  summarise(n = n()) %>% 
  
  ggplot(aes(x = reorder(as.factor(category), n), y = n)) +
  geom_bar(stat = "identity", aes(fill = n)) +
  coord_flip() +
  
  geom_label(aes(label = formatC(n, format="f", big.mark=",", digits=0)), size = 3.2) +
  scale_fill_gradient(low = colors60s[5], high = colors60s[3], guide = "none") +
  my_theme + theme(axis.text.x = element_blank()) +
  labs(x = "Category", y = "Frequency", title = "Category Transaction Frequency", subtitle = "transportation has a little bit above 500,000 transactions")



options(repr.plot.width=18, repr.plot.height=7)

data %>% 
  filter(fraud == 1) %>% 
  group_by(category, fraud) %>% 
  summarise(n = n()) %>% 
  
  ggplot(aes(x = reorder(as.factor(category), n), y = n)) +
  geom_bar(stat = "identity", aes(fill = n)) +
  coord_flip() +
  
  geom_label(aes(label = formatC(n, format="f", big.mark=",", digits=0)), size = 3.2) +
  scale_fill_gradient(low = colors60s[5], high = colors60s[1], guide = "none") +
  my_theme + theme(axis.text.x = element_blank()) +
  labs(x = "Category", y = "Frequency", title = "Fraud in Category Transaction", subtitle = "free time and travel have the most fraud cases")



options(repr.plot.width=18, repr.plot.height=7)

data %>% 
  mutate(category = factor(category, 
                           levels = c("contents", "food", "transportation", "fashion", "barsandrestaurants",
                                      "hyper", "wellnessandbeauty", "tech", "health", "home", "otherservices",
                                      "hotelservices", "sportsandtoys", "travel", "leisure"))) %>% 
  
  ggplot(aes(x = category, fill = as.factor(fraud))) +
  geom_bar(stat = "count", position = "fill") +
  
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  
  scale_fill_manual(values = colors60s[c(4, 6)]) +
  my_theme +
  labs(x = "Category", y = "Percent%", title = "Fraud Percentage for Categories", subtitle = "some categories have high chanse of fraud", fill = "Fraud")





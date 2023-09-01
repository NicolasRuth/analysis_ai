library(tidyverse)
library(wordcloud)
library(ggpubr)
theme_set(theme_bw())

# Load data
df <- read.csv("data/data_ai.csv", sep = ";")
# Load overall data (ratio between articles with and without AI)
dat_all <- read.csv("data/data_all.csv", sep = ";")

# 1. Amount of media coverage over the years 2016-2022

df_yearly <- df %>%
  group_by(year) %>%
  summarise(number = n()) %>%
  arrange(year)

df_yearly

plot_yearly <- ggplot(df, aes(as.factor(year)))
plot_yearly + geom_bar(aes(fill = magazine)) +
  labs(fill = "Magazines",
       x = "Years",
       y = "Number of articles")

plot_yearly

## Differences between magazines

df_by_magazine <- df %>%
  group_by(magazine) %>%
  summarise(number = n()) %>%
  arrange(-number)

df_by_magazine <- rbind(df_by_magazine, c("Musik & Bildung", 0))
df_by_magazine <- rbind(df_by_magazine, c("Rondo", 0))
df_by_magazine$number <- as.numeric(df_by_magazine$number)

plot_by_magazine <- ggplot(df_by_magazine,
                           aes(x = reorder(magazine, -number), y = number)) +
  geom_bar(stat = "identity", aes(fill = magazine)) +
  coord_flip() +
  labs(x = "Magazines",
       y = "Number of articles") +
  scale_fill_brewer(palette = "Reds") +
  theme(legend.position = "none")

plot_by_magazine

# 2. AI as main or sub theme

df_main_vs_sub <- df %>%
  group_by(main_sub) %>%
  summarise(Anzahl = n())

df_main_vs_sub # 1 = main, 2 = subtheme

# 3. Specific AI topics

df_ai_topics <- df %>%
  select(ai_topic_1, ai_topic_2, ai_topic_3) %>%
  gather(key = "Category", value = "Topic") %>%
  group_by(Topic) %>%
  summarise(Number = n()) %>%
  arrange(-Number)

df_ai_topics # KI and Künstliche Intelligenz is German for AI and Artificial Int.

# 4. Overall topics

df_general_topic <- df %>%
  group_by(topic) %>%
  summarise(Number = n()) %>%
  arrange(-Number)

df_general_topic

wordcloud(words = df_general_topic$topic,
          freq = df_general_topic$Number, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

# 5. People and organisations
## People

df_people <- df %>%
  select(starts_with("people")) %>%
  gather(key = "PeopleNum", value = "People") %>%
  group_by(People) %>%
  summarise(Number = n()) %>%
  arrange(-Number)

df_people

## Organisations
df_companies <- df %>%
  select(starts_with("companies")) %>%
  gather(key = "CompaniesNum", value = "Companies") %>%
  group_by(Companies) %>%
  summarise(Number = n()) %>%
  arrange(-Number)

df_companies

# 6. Framing of articles
df_framing <- df %>%
  group_by(framing) %>%
  summarise(Number = n())

df_framing

plot_framing <- ggplot(df_framing,
                       aes(x = framing,
                           y = Number,
                       fill = factor(framing))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Negative to positive framing",
       y = "Number of articles") +
  theme(legend.position = "none")

plot_framing

xtabs(~df$framing+df$magazine)

group_by(df, magazine) %>%
  summarise(
    mean = mean(framing, na.rm = TRUE),
    sd = sd(framing, na.rm = TRUE)
  ) %>%
  arrange(-mean)

# 7. TAM
## Usability
df_easeofuse <- df %>%
  group_by(eou_tam) %>%
  summarise(Number = n())

df_easeofuse

plot_easeofuse <- ggplot(df_easeofuse,
                         aes(x = eou_tam, y = Number)) +
  geom_bar(stat = "identity", fill = "brown3") +
  ylim(0, 10) +
  labs(x = "Perceived ease of use (TAM)",
       y = "Number of articles")

plot_easeofuse

## Usefulness
df_usability <- df %>%
  group_by(usa_tam) %>%
  summarise(Number = n())

df_usability

plot_usability <- ggplot(df_usability, aes(x = usa_tam,
                                             y = Number)) +
  geom_bar(stat = "identity", fill = "cornsilk3") +
  labs(x = "Perceived usability (TAM)",
       y = "Number of articles")

plot_usability

## Enjoyment
df_enjoyment <- df %>%
  group_by(enj_tam) %>%
  summarise(Number = n())

df_enjoyment

plot_enjoyment <- ggplot(df_enjoyment, aes(x = enj_tam,
                                           y = Number)) +
  geom_bar(stat = "identity", fill = "darksalmon") +
  labs(x = "Perceived enjoyment (TAM)",
       y = "Number of articles")

plot_enjoyment

combinedTAM <- ggarrange(plot_easeofuse, plot_usability, plot_enjoyment,
                           ncol = 3, nrow = 1)

combinedTAM

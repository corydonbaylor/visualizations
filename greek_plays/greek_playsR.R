library(gutenbergr)
library(tidyverse)
library(patchwork)
library(ggthemes)


orestia = gutenberg_download("8604")

agg = orestia[73:2601,]
lib = orestia[2606:4298,]
furies = orestia[4303:5902,]


agg_plot = agg %>%
  mutate(linenumber = row_number())%>%
  unnest_tokens(word, text)%>%
  inner_join(get_sentiments("bing"))%>%
  # %/% performs integer divison, rounding down to the nearest whole number
  mutate(index = linenumber %/% (round(nrow(agg)/50)))%>% 
  group_by(index, sentiment)%>%
  summarise(count = n())%>%
  spread(sentiment, count, fill = 0) %>%
  mutate(sentiment = positive - negative,
         sentiment_group = ifelse(sentiment > 0, "pos", "neg"))%>%
  ungroup()%>%
  mutate(play = "The Agamemnon")

lib_plot = lib %>%
  mutate(linenumber = row_number())%>%
  unnest_tokens(word, text)%>%
  inner_join(get_sentiments("bing"))%>%
  # %/% performs integer divison, rounding down to the nearest whole number
  mutate(index = linenumber %/% (round(nrow(lib)/50)))%>% 
  group_by(index, sentiment)%>%
  summarise(count = n())%>%
  spread(sentiment, count, fill = 0) %>%
  mutate(sentiment = positive - negative,
         sentiment_group = ifelse(sentiment > 0, "pos", "neg"))%>%
  ungroup()%>%
  mutate(play = "The Libation Bearers")

furies_plot = furies %>%
  mutate(linenumber = row_number())%>%
  unnest_tokens(word, text)%>%
  inner_join(get_sentiments("bing"))%>%
  # %/% performs integer divison, rounding down to the nearest whole number
  mutate(index = linenumber %/% (round(nrow(furies)/50)))%>% 
  group_by(index, sentiment)%>%
  summarise(count = n())%>%
  spread(sentiment, count, fill = 0) %>%
  mutate(sentiment = positive - negative,
         sentiment_group = ifelse(sentiment > 0, "pos", "neg"))%>%
  ungroup()%>%
  mutate(play = "The Furies")


aes = rbind(agg_plot, lib_plot, furies_plot)

aes$play = factor(aes$play, ordered = T,
                  levels = c("The Agamemnon", "The Libation Bearers", "The Furies"))

aes_plot = aes%>%
  ggplot() +
  geom_bar(aes(x = index, y = sentiment, fill = sentiment_group), stat = "identity")+
  facet_grid(~play)+
  theme_wsj()+
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank()
  )+
  scale_fill_manual(values = c("darkred", "darkgreen"))+
  labs(x = "",
       y = "Sentiment Score")



# oedipus -----------------------------------------------------------------

oedipus = gutenberg_download("31")%>%
  mutate(num = row_number())


rex = oedipus[1:2560,]
colonus = oedipus[2561:5607,]
antigone = oedipus[5607:7575,]

rex_plot = rex %>%
  mutate(linenumber = row_number())%>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))%>%
  # %/% performs integer divison, rounding down to the nearest whole number
  mutate(index = linenumber %/% (round(nrow(rex)/50)))%>% 
  group_by(index, sentiment)%>%
  summarise(count = n())%>%
  spread(sentiment, count, fill = 0) %>%
  mutate(sentiment = positive - negative,
         sentiment_group = ifelse(sentiment > 0, "pos", "neg"))%>%
  ungroup()%>%
  mutate(play = "Oedipus Rex")

colonus_plot = colonus %>%
  mutate(linenumber = row_number())%>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))%>%
  # %/% performs integer divison, rounding down to the nearest whole number
  mutate(index = linenumber %/% (round(nrow(colonus)/50)))%>% 
  group_by(index, sentiment)%>%
  summarise(count = n())%>%
  spread(sentiment, count, fill = 0) %>%
  mutate(sentiment = positive - negative,
         sentiment_group = ifelse(sentiment > 0, "pos", "neg"))%>%
  ungroup()%>%
  mutate(play = "Oedipus at Colonus")



antigone_plot = antigone %>%
  mutate(linenumber = row_number())%>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))%>%
  # %/% performs integer divison, rounding down to the nearest whole number
  mutate(index = linenumber %/% (round(nrow(antigone)/50)))%>% 
  group_by(index, sentiment)%>%
  summarise(count = n())%>%
  spread(sentiment, count, fill = 0) %>%
  mutate(sentiment = positive - negative,
         sentiment_group = ifelse(sentiment > 0, "pos", "neg"))%>%
  ungroup()%>%
  mutate(play = "Antigone")

oedipus = rbind(rex_plot, colonus_plot, antigone_plot)

oedipus$play = factor(oedipus$play, ordered = T,
                      levels = c("Oedipus Rex", "Oedipus at Colonus", "Antigone"))


oedipus_plot = oedipus%>%
  ggplot() +
  geom_bar(aes(x = index, y = sentiment, fill = sentiment_group), stat = "identity")+
  facet_grid(~play)+
  theme_wsj()+
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank()
  )+
  scale_fill_manual(values = c("darkred", "darkgreen"))+
  labs(x = "",
       y = "Sentiment Score")



frogs = gutenberg_download("7998")
frogs_plot = frogs %>%
  mutate(linenumber = row_number())%>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))%>%
  # %/% performs integer divison, rounding down to the nearest whole number
  mutate(index = linenumber %/% (round(nrow(frogs)/50)))%>% 
  group_by(index, sentiment)%>%
  summarise(count = n())%>%
  spread(sentiment, count, fill = 0) %>%
  mutate(sentiment = positive - negative,
         sentiment_group = ifelse(sentiment > 0, "pos", "neg"))%>%
  ungroup()%>%
  mutate(play = "The Frogs")


birds = gutenberg_download("3013")

birds_plot = birds %>%
  mutate(linenumber = row_number())%>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))%>%
  # %/% performs integer divison, rounding down to the nearest whole number
  mutate(index = linenumber %/% (round(nrow(birds)/50)))%>% 
  group_by(index, sentiment)%>%
  summarise(count = n())%>%
  spread(sentiment, count, fill = 0) %>%
  mutate(sentiment = positive - negative,
         sentiment_group = ifelse(sentiment > 0, "pos", "neg"))%>%
  ungroup()%>%
  mutate(play = "The Birds")

# starting ggplot

clouds = gutenberg_download("2562")

clouds_plot = clouds %>%
  mutate(linenumber = row_number())%>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))%>%
  # %/% performs integer divison, rounding down to the nearest whole number
  mutate(index = linenumber %/% (round(nrow(clouds)/50)))%>% 
  group_by(index, sentiment)%>%
  summarise(count = n())%>%
  spread(sentiment, count, fill = 0) %>%
  mutate(sentiment = positive - negative,
         sentiment_group = ifelse(sentiment > 0, "pos", "neg"))%>%
  ungroup()%>%
  mutate(play = "The Clouds")

comedies = rbind(frogs_plot, birds_plot, clouds_plot)

comedies_plot = comedies%>%
  ggplot() +
  geom_bar(aes(x = index, y = sentiment, fill = sentiment_group), stat = "identity")+
  facet_grid(~play)+
  theme_wsj()+
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_text(size = 10),
    axis.text.y = element_blank()
  )+
  scale_fill_manual(values = c("darkred", "darkgreen"))+
  labs(x = "",
       y = "Sentiment Score")+
  scale_y_continuous()

comedies_plot

ggsave("comedies_plot.png")

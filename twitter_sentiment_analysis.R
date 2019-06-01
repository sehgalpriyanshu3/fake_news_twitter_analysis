library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
library(tm)
library(widyr)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(rtweet)
library(devtools)
library(maps)
library(openxlsx)

install.packages("base64enc")
devtools::install_github("mkearney/rtweet")

create_token(
  app = "fake_news_twitter_sentiment",
  consumer_key = "62fkmpSMl1kaoYoadr7C3lmWH",
  consumer_secret = "zBorkpKQpkZIuPfxYRfxS2PiYGQ8vMxVyaY1MTfy2moQGmC3x0",
  access_token = "1110291032267214849-D8lZfOEZdlXMEB3p6LbkqiwShNoiHj",
  access_secret = "GrP1Hg6NCg672NUEiU5xFIMb4knsXnqYm4tSOIGRrh4XJ")

x <- search_tweets("#trump OR #democrats OR #republicans OR 
                    clinton OR #uselections OR #american OR #wikileaks OR
                    washington OR USpolitics OR #whitehousepolitics",geocode = lookup_coords("usa"),
                   n = 20000, 
                   include_rts = FALSE)

head(dg)
dg1=subset(dg,select = -c(status_id))

head(dg1)
dat=subset(dg1,select=-c(urls_url,urls_t.co,urls_expanded_url,
                         media_url,media_t.co,media_expanded_url))

data=subset(dat,select = -c(media_type,ext_media_url,ext_media_t.co,
                            ext_media_expanded_url,ext_media_type,quoted_favorite_count,
                            quoted_retweet_count,quoted_user_id,quoted_screen_name,
                            quoted_name,quoted_followers_count,quoted_friends_count,
                            quoted_statuses_count,quoted_location,quoted_description,
                            quoted_verified,retweet_status_id,retweet_user_id,
                            retweet_created_at,retweet_source,retweet_favorite_count,
                            retweet_user_id,retweet_screen_name,retweet_name,
                            retweet_followers_count,retweet_friends_count,retweet_statuses_count,
                            retweet_location,retweet_description,retweet_verified,place_url,
                            status_url,url,protected,profile_url,profile_expanded_url,profile_banner_url,
                            profile_background_url,profile_image_url))


setwd("~/Desktop/minor2")

x<- apply(x,2,as.character)
write.csv(x,file = "dataset_twitter_rtweet.csv")
rt3<-read_csv("dataset_twitter_rtweet_predicted.csv")

#for false tweets
ts.plot(rt3[rt$label=="false",], "10mins" ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "DateTime", y = "Frequency",
    title = "Frequency of False Tweets from past 6 hours",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#for true tweets
ts_plot(rt3, "10mins") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "DateTime", y = "Frequency",
    title = "Frequency of Tweets from past 6 hours",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#taking the label col out of the csv file
v<-rt3[,90]
rt3[,89]=v[1]

rt3=rt3[,2:89]
rt3[,67]=d[1]
rt3[,68]=d[2]
rt3[,69]=d[3]

par(mar = c(0, 0, 0, 0))

maps::map("state", lwd = .99)

rt3<-lat_lng(rt3, coords = c("coords_coords", "bbox_coords", "geo_coords"))

qFalse<-rt3[rt3$label=="false",]

qhalf_true<-rt3[rt3$label=="half-true",]

qmostly_true<-rt3[rt3$label=="mostly-true",]

qtrue<-rt3[rt3$label=="true",]

qbarely_true<-rt3[rt3$label=="barely-true",]

qverified<-rt3[rt3$verified== TRUE,]

qverified_fake<-qverified[qverified$label=="false",]

par(mar = c(0, 0, 0, 0))

maps::map("state", lwd = .99)

with(rt3, points(lng, lat, pch = 20, cex =1, col = "blue"))



with(qtrue, points(lng, lat, pch = 20, cex =1, col = "green"))

with(qmostly_true, points(lng, lat, pch =20 , cex =1, col = "blue"))

with(qhalf_true, points(lng, lat, pch = 20, cex =1, col = "red"))

with(qFalse, points(lng, lat, pch = 20, cex =1, col = "red"))

with(qbarely_true, points(lng, lat, pch = 20, cex =1, col = "green"))

plot_ly(rt3,x=~label,type = "histogram")


dev.off()

rt3 %>% group_by(source)%>% summarise(Total=n()) %>% arrange(desc(Total)) %>% 
  head(10) %>% ggplot(aes(reorder(source, Total), Total, fill = source)) +
  geom_bar(stat="identity") + coord_flip() + 
  labs(caption = "\nSource: Data collected from Twitter's REST API via rtweet")+ 
  theme(text = element_text(size=20))

qFalse$stripped_text <- gsub("http.*","", qFalse$text)

qFalse$stripped_text <- gsub("https.*","", qFalse$stripped_text)

qFalse_clean <- qFalse %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

cleaned_tweet_words <- qFalse_clean %>%
  anti_join(stop_words)

cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet") +
  theme(text = element_text(size=20))


tmls <- get_timelines(c("cnni", "cnn"), n =1000, since = '2019-04-29', until= '2019-04-30')

tmsl1<- get_timelines("cnn", n =1000, since = '2019-04-29', until= '2019-04-30')

tmsl1 %>%
  dplyr::filter(created_at > "2019-04-30" ) %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("50mins") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "DateTime", y = "Frequency"
  )+ theme(text = element_text(size=20))

tmls %>%
  dplyr::filter(created_at > "2019-04-30" ) %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("50mins") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "DateTime", y = "Frequency"
  ) + theme(text = element_text(size=20))




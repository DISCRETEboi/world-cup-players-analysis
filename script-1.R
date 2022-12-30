
## world cup players analysis ##
library(rvest)
library(stringr)
library(ggplot2)
library(dplyr)

webpage <- read_html("https://www.goal.com/en-ng/lists/world-cup-2022-squads/blt5286b592f720ce72#cs12c78799ac7fbe61")
tbl_rows <- html_nodes(webpage, "tr")
countries <- html_nodes(webpage, "h2")
tr_text <- html_text2(tbl_rows)
cntrs_text <- str_trim(html_text(countries))
data_text <- strsplit(tr_text, "\t")
data_tbl <- data.frame(position=character(0), player=character(0), dob=character(0),
                       club=character(0), country=character(0))
cntry_ind <- 1
for (i in data_text[3:857]) {
  if (i[1] == "Position") {
    cntry_ind <- cntry_ind + 1
  }
  if (!(i[1] %in% c("GK", "DF", "MF", "FW"))) {
    next
  }
  if (length(i) == 3) {
    i[4] <- NA
  }
  if (length(i) > 4) {
    i <- i[1:4]
  }
  data_tbl <- rbind(data_tbl, c(i, cntrs_text[cntry_ind]))
}
names(data_tbl) <- c("position", "player", "dob", "club", "country")
write.csv(data_tbl, "world-cup-squad.csv", row.names = F)

data_tbl$club[data_tbl$club == "West Ham"] <- "West Ham United"
data_tbl$club[data_tbl$club == "Leicester"] <- "Leicester City"
data_tbl$club[data_tbl$club == "Man Utd"] <- "Manchester United"
data_tbl$club[data_tbl$club == "Wolves"] <- "Wolverhampton Wanderers"
data_tbl$club[data_tbl$club == "Tottenham"] <- "Tottenham Hotspur"
data_tbl$club[data_tbl$club == "Sporting"] <- "Sporting CP"

club_subdf <- data_tbl %>% group_by(club) %>% summarise(appearance = length(club)) %>%
  arrange(desc(appearance)) %>% na.omit()
club_subdf <- club_subdf[1:15, ]

ggplot(club_subdf, aes(x = reorder(club, appearance), y = appearance, fill = club)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.background = element_blank(), axis.ticks.y = element_blank()) +
  geom_text(aes(label = appearance), hjust = -0.3, vjust = 0.2, size = 3.0, lineheight = 5) +
  labs(x = "", y = "", subtitle = "Number of players from the most represented clubs in the 2022 World Cup",
       title = "MAN CITY AND BARCA PLAYERS TOP WORLD CUP 2022 APPEARANCE")














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

data_tbl$final_stage <- rep("Not qualified", nrow(data_tbl))
final <- c("Argentina", "France")
smfinals <- c("Morocco", "Croatia")
qrtrfinals <- c("England", "Portugal", "Netherlands", "Brazil")
rof16 <- c("Switzerland", "Spain", "South Korea", "Japan", "Senegal", "Poland", "Australia", "USMNT")
groupstg <- c("Qatar", "Canada", "Ecuador", "Germany", "Costa Rica", "Belgium", "Iran", "Wales", "Mexico",
            "Saudi Arabia", "Denmark", "Tunisia", "Uruguay", "Ghana", "Cameroon", "Serbia")
data_tbl$final_stage[data_tbl$country %in% final] <- "Final"
data_tbl$final_stage[data_tbl$country %in% smfinals] <- "Semifinals"
data_tbl$final_stage[data_tbl$country %in% qrtrfinals] <- "Quarterfinals"
data_tbl$final_stage[data_tbl$country %in% rof16] <- "Round of 16"
data_tbl$final_stage[data_tbl$country %in% groupstg] <- "Group stage"

club_subdf_full <- data_tbl %>% group_by(club) %>% summarise(appearance = length(club)) %>%
  arrange(desc(appearance)) %>% na.omit()
club_subdf <- club_subdf_full[1:15, ]

ggplot(club_subdf, aes(x = reorder(club, appearance), y = appearance, fill = club)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.background = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(size = 25, face = "bold"),
        plot.subtitle = element_text(size = 16, colour = "#333333"),
        axis.text = element_text(size = 11, colour = "#333333")) +
  geom_text(aes(label = appearance), hjust = -0.3, vjust = 0.2, size = 3.5, lineheight = 7) +
  labs(x = "", y = "", subtitle = "Number of players from the most represented clubs in the\n2022 World Cup",
       title = "Man City and Barca Players Top\nWorld Cup 2022 Appearance")













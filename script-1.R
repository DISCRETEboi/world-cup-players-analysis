
## world cup players analysis ##
library(rvest)
library(stringr)

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
  #print(i[1])
  data_tbl <- rbind(data_tbl, c(i, cntrs_text[cntry_ind]))
}
names(data_tbl) <- c("position", "player", "dob", "club", "country")















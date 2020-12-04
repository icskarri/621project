#loading packages
library(gt)
library(lubridate)
library(rvest)
library(tidytext)
library(tidyverse)
library(tm)

#pass in url
mainUrl <- read_html("https://en.wikipedia.org/wiki/List_of_English_football_transfers_summer_2020")

#pass html nodes into object
deals <- mainUrl %>%
  html_nodes(xpath = '//tr')

#scrape text from web page
deals <- deals %>%
  html_text() %>%
  as_tibble()

#adding index column
deals <- deals %>%
  mutate(index = seq.int(nrow(deals))) %>%
  select(index, value)

#separating text into multiple columns
deals <- deals %>%
  separate(value, into = c('one', 'two', 'three', 'four', 'five',
                           'six', 'seven', 'eight', 'nine'), sep = '\n')

#add boolean variable for rows with dates
deals <- deals %>%
  mutate(boolean = str_detect(one,
                              pattern = '^[:digit:]'))

#create index_date object for working with dates
index_date <- deals %>%
  select(index, one, boolean)

#working with rows that have dates
#filter those rows with dates
has_date <- deals %>%
  select(index, boolean, one, two, three, four, five, six, seven,
         eight, nine) %>%
  filter(boolean == TRUE)

#filter transfers deals
transfers <- has_date %>%
  filter(index < 570)

#identify columns to remove
unique(transfers$two)
unique(transfers$four)
unique(transfers$six)
unique(transfers$eight)

#all are blank or NA
#removing unneeded columns
transfers <- transfers %>%
  select(index, boolean, one, three, five, seven, nine)

#filter loan deals
loans <- has_date %>%
  filter(index > 570)

#adding boolean to use for reshaping data
loans$boolean <- loans$three == ''

#filter using boolean
loans_one <- loans %>%
  filter(boolean == FALSE)

#identify columns to remove
unique(loans_one$two)
unique(loans_one$five)
unique(loans_one$seven)
unique(loans_one$nine)

#all are blank or NA
#removing unneeded columns
loans_one <- loans_one %>%
  select(index, boolean, one, four, six, eight)

#working with remaining rows
loans_two <- loans %>%
  filter(boolean == TRUE)

#identify columns to remove
unique(loans_two$three)
unique(loans_two$five)
unique(loans_two$seven)
unique(loans_two$eight)
unique(loans_two$nine)

#all are blank or NA
#removing unneeded columns
loans_two <- loans_two %>%
  select(index, boolean, two, four, six)

#working with rows that have no dates
no_date <- deals %>%
  select(index, boolean, one, two, three, four, five, six, seven,
         eight, nine) %>%
  filter(boolean == FALSE)

#removing unneeded rows
no_date <- no_date[2:458,]

#identify columns to remove
unique(no_date$two)
unique(no_date$four)
unique(no_date$six)
unique(no_date$eight)
unique(no_date$nine)

#removing unneeded columns
no_date <- no_date %>%
  select(index, boolean, one, three, five, seven)

#clean up work space
remove(deals, loans, mainUrl)

#select players from objects to make final table
#players
players1 <- transfers %>%
  select(index, three) %>%
  rename(player = three)

players2 <- loans_two %>%
  select(index, two) %>%
  rename(player = two)

players3 <- loans_one %>%
  select(index, four) %>%
  rename(player = four)

players4 <- no_date %>%
  select(index, one) %>%
  rename(player = one)

#combining player objects
player <- as_tibble(rbind(players1, players2,
                          players3, players4)) %>%
  arrange(index)

#clean up work space
remove(players1, players2, players3, players4)

#select clubs from objects to make final table
#previous clubs
old_clubs1 <- transfers %>%
  select(index, five) %>%
  rename(old_club = five)

old_clubs2 <- loans_two %>%
  select(index, four) %>%
  rename(old_club = four)

old_clubs3 <- loans_one %>%
  select(index, six) %>%
  rename(old_club = six)

old_clubs4 <- no_date %>%
  select(index, three) %>%
  rename(old_club = three)

#combining clubs objects
old_club <- as_tibble(rbind(old_clubs1, old_clubs2,
                            old_clubs3, old_clubs4))

#clean up work space
remove(old_clubs1, old_clubs2, old_clubs3, old_clubs4)

#new clubs
new_clubs1 <- transfers %>%
  select(index, seven) %>%
  rename(new_club = seven)

new_clubs2 <- loans_two %>%
  select(index, six) %>%
  rename(new_club = six)

new_clubs3 <- loans_one %>%
  select(index, eight) %>%
  rename(new_club = eight)

new_clubs4 <- no_date %>%
  select(index, five) %>%
  rename(new_club = five)

#combining clubs objects
new_club <- as_tibble(rbind(new_clubs1, new_clubs2,
                            new_clubs3, new_clubs4))

#cleaning up work space
remove(new_clubs1, new_clubs2, new_clubs3, new_clubs4)

#join tibbles
deals <- inner_join(player, old_club, by = 'index')
deals <- inner_join(deals, new_club, by = 'index')

#clean up work space
remove(player, old_club, new_club)

#select transfer fees from objects to make final table
#transfer fees
transfer_fees1 <- transfers %>%
  select(index, nine) %>%
  rename(fee = nine)

transfer_fees2 <- loans_two %>%
  select(index) %>%
  mutate(fee = 'loan')

transfer_fees3 <- loans_one %>%
  select(index) %>%
  mutate(fee = 'loan')

transfer_fees4 <- no_date %>%
  select(index, seven) %>%
  rename(fee = seven)

#combining transfer fees objects
fees <- as_tibble(rbind(transfer_fees1, transfer_fees2,
                        transfer_fees3, transfer_fees4)) 

#join tibbles
deals <- inner_join(deals, fees, by = 'index') %>%
  arrange(index)

#removing whitespace
deals$player <- trimws(deals$player)
deals$old_club <- trimws(deals$old_club)
deals$new_club <- trimws(deals$new_club)

#clean up work space
remove(fees, loans_one, loans_two, no_date, transfer_fees1,
       transfer_fees2, transfer_fees3, transfer_fees4, transfers)

#cleaning up fee column with regexes
deals$fee <- str_replace(deals$fee, 
                         pattern = '[:punct:][:digit:][:digit:][:punct:]',
                         replacement = '')

deals$fee <- str_replace(deals$fee,
                         pattern = '[:punct:][:digit:][:digit:][:digit:][:punct:]',
                         replacement = '')

deals$fee <- str_replace(deals$fee,
                         pattern = '[:punct:][:digit:][:punct:]',
                         replacement = '')

deals$fee <- str_replace(deals$fee,
                         pattern = '[:punct:][:lower:][:punct:]',
                         replacement = '')

#checking distinct deal fees
unique(deals$fee)

deals <- deals %>%
  mutate(index = seq.int(nrow(deals)))

#adding dates
clean_date <- str_detect(has_date$one,
                         pattern = '\\[')

has_date <- has_date %>%
  select(index, one) %>%
  filter(clean_date == FALSE)

adjusted_index <- has_date$index
adjusted_index <- adjusted_index[-1]
adjusted_index[187] <- 821

has_date <- has_date %>%
  mutate(adjusted = adjusted_index) %>%
  mutate(index_copy = index) %>%
  mutate(diff = adjusted - index_copy) %>%
  mutate(native_index = seq.int(nrow(has_date)))

add_dates <- vector()

for (i in has_date$native_index) {
  add_dates <- append(add_dates,
                      replicate(n = has_date$diff[i],
                                expr = has_date$one[i]))
}

deals <- deals %>%
  mutate(date = add_dates) %>%
  select(index, date, player, old_club, new_club, fee)

#clean up work space
remove(has_date, index_date, add_dates,
       adjusted_index, clean_date, i)

fix_date <- deals %>%
  filter(index >= 570)

deals <- deals %>%
  filter(index < 570)

adjust_date <- fix_date$date
adjust_date <- adjust_date[-1]
adjust_date[250] <- '6 October 2020'

fix_date <- fix_date %>%
  mutate(date = adjust_date)

deals_england <- rbind(deals, fix_date)

#clean up work space
remove(adjust_date, deals, fix_date)

#italy
#pass in url
mainUrl <- read_html("https://en.wikipedia.org/wiki/List_of_Italian_football_transfers_summer_2020")

#pass html nodes into object
deals <- mainUrl %>%
  html_nodes(xpath = '//tr')

#extract text
deals <- deals %>%
  html_text() %>%
  as_tibble()

#add index
deals <- deals %>%
  mutate(index = seq.int(nrow(deals))) %>%
  select(index, value)

#separate text into multiple columns
deals <- deals %>%
  separate(value, into = c('one', 'two', 'three', 'four', 'five',
                           'six', 'seven', 'eight', 'nine'), sep = '\n')

#identify columns to remove
unique(deals$two)
unique(deals$four)
unique(deals$six)
unique(deals$eight)

#removing unneeded columns
deals <- deals %>%
  select(index, one, three, five, seven, nine) %>%
  as_tibble()

#boolean for rows with dates
deals <- deals %>%
  mutate(boolean_date = 
           str_detect(one,
                      pattern =
                        '[:digit:][:digit:][:digit:][:digit:]'))

#filter dates / no-dates
#without dates
without_date <- deals %>%
  filter(boolean_date == FALSE)

#identify columns to remove
#unique values in selected columns
unique(without_date$nine)

#returns rows where column nine != NA
without_date %>%
  filter(is.na(nine) == FALSE)

#removing columns
without_date <- without_date %>%
  select(-nine, -boolean_date)

#removing metadata rows
without_date <- without_date[-1,]
without_date <- without_date[-639,]
without_date <- without_date[-638,]
without_date <- without_date[-637,]
without_date <- without_date[-30,]

#removing whitespace
without_date$one <- trimws(without_date$one)
without_date$three <- trimws(without_date$three)
without_date$five <- trimws(without_date$five)
without_date$seven <- trimws(without_date$seven)

#cleaning fees column
without_date$seven <- str_split_fixed(without_date$seven,
                                      pattern = '\\[',
                                      n = 2)

#filter dates / no-dates
#with dates
with_date <- deals %>%
  filter(boolean_date == TRUE) %>%
  select(-boolean_date)

#removing whitespace
with_date$one <- trimws(with_date$one)
with_date$three <- trimws(with_date$three)
with_date$five <- trimws(with_date$five)
with_date$seven <- trimws(with_date$seven)
with_date$nine <- trimws(with_date$nine)

#cleaning fees column
with_date$nine <- str_split_fixed(with_date$nine,
                                  pattern = '\\[',
                                  n = 2)

this_index <- with_date$index
this_index <- this_index[-1]
this_index[90] <- 0

with_date <- with_date %>%
  mutate(new_index = this_index) %>%
  mutate(old_index = index) %>%
  mutate(diff = new_index - old_index) %>%
  select(-new_index, -old_index)

date_list <- vector()

for (date in seq.int(with_date$index)) {
  date_list <- append(date_list, (replicate(with_date$diff[date],
                                            with_date$one[date])))
}

#clean up work space
remove(date, this_index)

date_list <- as.data.frame(date_list)

#index, date, player, old_club, new_club, fee
with_date$fee <- with_date$nine[,1]

with_date <- with_date %>%
  select(index, three, five, seven, fee) %>%
  rename(player = three, old_club = five,
         new_club = seven)

without_date$fee <- without_date$seven[,1]

without_date <- without_date %>%
  select(index, one, three, five, fee) %>%
  rename(player = one, old_club = three,
         new_club = five)

deals <- rbind(with_date, without_date)

dates <- date_list$date_list

deals_italy <- deals %>%
  arrange(index) %>%
  mutate(date = dates) %>%
  select(index, date, player, old_club,
         new_club, fee)

#clean up work space
remove(dates, deals, date_list, mainUrl, with_date, without_date)


#spain
#pass in url
main_url <- read_html('https://en.wikipedia.org/wiki/List_of_Spanish_football_transfers_summer_2020')

rows <- html_nodes(main_url, xpath = '//tr') %>%
  html_text() %>%
  as_tibble()

rows <- rows %>%
  mutate(index = seq.int(nrow(rows)))

header_row <- str_detect(rows$value, pattern = '^D')

indexes <- rows %>%
  filter(header_row == TRUE)

#clean up work space
remove(header_row)

teams <- html_nodes(main_url, xpath = '//h2//*[contains(concat( " ", @class, " " ), concat( " ", "mw-headline", " " ))]') %>%
  html_text() %>%
  as_tibble()

teams <- as_tibble(teams$value[-22])
teams <- as_tibble(teams$value[-21])

indexes <- indexes %>%
  filter(index < 545)

adjusted_index <- indexes$index
adjusted_index <- adjusted_index[-1]
adjusted_index[40] <- 545
indexes$adjusted <- adjusted_index

#clean up work space
remove(adjusted_index)

indexes <- indexes %>%
  select(value, adjusted, index) %>%
  mutate(diff = adjusted - index - 1)

team_list <- vector()

for (team in teams$value) {
  team_list <- append(team_list, values = replicate(n = 2, expr = team))
}

indexes <- indexes %>%
  mutate(teams = team_list) %>%
  mutate(new_index = seq.int(nrow(indexes)))

#clean up work space
remove(team, team_list)

transfers_table <- vector()

for (i in indexes$new_index) {
  transfers_table <- append(transfers_table, 
                            replicate(n = indexes$diff[i],
                                      expr = indexes$teams[i]))
}

indexes <- indexes %>%
  mutate(odd_even = new_index %% 2 == 1) %>%
  mutate(old_new = if_else(odd_even == TRUE, 'new_club', 'old_club'))

transfers_table <- as_tibble(transfers_table)

club <- vector()

for (i in indexes$new_index) {
  club <- append(club, replicate(n = indexes$diff[i],
                                 expr = indexes$old_new[i]))
}

transfers_table <- transfers_table %>%
  mutate(club = club)

#clean up work space
remove(i, club)

dates <- html_nodes(main_url, xpath = '//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>%
  html_text()

other_club <- html_nodes(main_url, xpath = '//td[(((count(preceding-sibling::*) + 1) = 3) and parent::*)]') %>%
  html_text()

players <- html_nodes(main_url, xpath = '//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]') %>%
  html_text()

dates[509]
dates[505]
dates[504]
dates <- dates[1:504]

players[505]
players[504]
players <- players[1:504]

transfers_table <- transfers_table %>%
  mutate(date = dates) %>%
  mutate(other_team = other_club) %>%
  mutate(player = players)

#clean up work space
remove(dates, other_club, players)

transfers_table <- transfers_table %>%
  mutate(new_club = ifelse(club == 'new_club', value, other_team)) %>%
  mutate(old_club = ifelse(club == 'old_club', value, other_team)) %>%
  select(date, player, old_club, new_club)

#clean up work space
remove(indexes, teams)


rows <- rows %>%
  select(index, value) %>%
  separate(value, into = c('date', 'player', 'other_team',
                           'transfer', 'type', 'ref'),
           sep = '\n') %>%
  filter(index < 545) %>%
  arrange(index)

rows <- rows %>%
  mutate(boolean = ifelse(transfer == '', 1, 0)) %>%
  filter(boolean == 0) %>%
  select(transfer, type)


fee_boolean <- str_detect(rows$type, pattern = '[:digit:][:upper:]')

rows <- rows %>%
  mutate(fee = fee_boolean) %>%
  mutate(fee = ifelse(fee == TRUE, type, 'other'))

#clean up work space
remove(fee_boolean)

retired <- str_detect(rows$transfer, pattern = '[:digit:][:digit:]')

rows <- rows %>%
  mutate(retire = retired) %>%
  mutate(transfer = ifelse(retire == TRUE, 'Retired', transfer)) %>%
  select(transfer, fee) %>%
  mutate(fee = ifelse(transfer == 'Retired', 'Retired', fee)) %>%
  unite('transfer', transfer:fee, sep = '-')

deals_spain <- transfers_table %>%
  mutate(fee = rows$transfer)

deals_spain <- deals_spain %>%
  mutate(index = seq.int(nrow(deals_spain))) %>%
  select(index, date, player, old_club, new_club, fee)

#clean up work space
remove(main_url, retired, rows, transfers_table)

#combining all deals objects
deals <- rbind(deals_england, deals_italy, deals_spain)

#reindexing deals
deals <- deals %>%
  mutate(index = seq.int(nrow(deals)))

#clean up work space
remove(deals_england, deals_italy, deals_spain)

#date col as dates
deals <- deals %>%
  mutate(date = dmy(deals$date)) %>%
  arrange(date)

#getting transfer fees
#change 2-year to Two-year
deals <- deals %>%
  mutate(fee2 = if_else(fee == '2-year loan', 'Two-year loan', fee))

#verifying it worked
deals %>%
  filter(fee2 == '2-year loan')

#keeping updated column
deals <- deals %>%
  select(-fee) %>%
  rename(fee = fee2)

has_fee <- str_detect(deals$fee, pattern = '[:digit:]')

fees <- deals %>%
  filter(has_fee == TRUE)

remove(has_fee)

deals$player <- trimws(deals$player)


#working with currencies
fees <- fees %>%
  separate(fee, into = c('transfer', 'fee'), sep = '-')

fees <- fees %>% 
  mutate(fee_two = if_else(
    str_detect(transfer, pattern = '[:digit:]'), transfer, fee))

fees <- fees %>%
  select(-fee, -transfer) %>%
  rename(fee = fee_two)

fees <- fees %>% 
  mutate(currency = if_else(
    str_detect(fees$fee, pattern = '^£'), 'pound', 'euro'))


fees$fee <- str_replace(fees$fee, pattern = '£',
                        replacement = '')

fees$fee <- str_replace(fees$fee, pattern = '€',
                        replacement = '')

fees$fee <- tolower(fees$fee)

fees <- fees %>%
  mutate(millions = if_else(str_detect(
    fees$fee, pattern = 'm$'), 'millions', 'thousands'))
    
fees <- fees %>%
  mutate(point = if_else(str_detect(
    fees$fee, pattern = '[:digit:][:punct:]'), 'point', 'n/a'))

fees_two <- fees %>%
  filter(point == 'point')

fees_two <- fees_two %>%
  mutate(four_zeros = if_else(str_detect(
    fees_two$fee, pattern = '[:punct:][:digit:][:digit:]m'), 
    '0000', 'na'))

fees_three <- fees_two %>%
  filter(four_zeros == '0000')

fees_three$fee <- str_replace(fees_three$fee,
                              pattern = '[:punct:]',
                              replacement = '')

fees_three$fee <- str_replace(fees_three$fee,
                              pattern = 'm',
                              replacement = '')



fees_three$fee <- paste(as.character(fees_three$fee),
                        fees_three$four_zeros)

fees_three$fee <- str_replace(fees_three$fee,
                              pattern = ' ',
                              replacement = '')

fees_three$fee <- as.numeric(fees_three$fee)

fees_three <- fees_three %>%
  select(-millions, -point, -four_zeros)

fees_two <- fees_two %>%
  filter(four_zeros == 'na')

fees_two$fee <- str_replace(fees_two$fee,
                            pattern = '[:punct:]',
                            replacement = '')

fees_two$fee <- str_replace(fees_two$fee,
                            pattern = 'm',
                            replacement = '')

fees_two$five_zeros <- '00000'

fees_two$fee <- paste(as.character(fees_two$fee),
                      fees_two$five_zeros)

fees_two$fee <- str_replace(fees_two$fee,
                            pattern = ' ',
                            replacement = '')

fees_two$fee <- as.numeric(fees_two$fee)

fees_two <- fees_two %>%
  select(-millions, -point, -four_zeros, -five_zeros)

fees_two <- rbind(fees_two, fees_three)

remove(fees_three)

fees_four <- fees %>%
  filter(point == 'n/a')

fees_thousands <- fees_four %>%
  filter(millions == 'thousands')

fees_thousands$fee <- str_replace(fees_thousands$fee,
                                  pattern = 'k',
                                  replacement = '000')

fees_thousands$fee <- as.numeric(fees_thousands$fee)

fees_thousands <- fees_thousands %>%
  select(-millions, -point)

fees_four <- fees_four %>%
  filter(millions == 'millions')


fees_four$fee <- str_replace(fees_four$fee,
                             pattern = 'm',
                             replacement = '000000')

fees_four$fee <- as.numeric(fees_four$fee)

fees_four <- fees_four %>%
  select(-millions, -point)

fees <- rbind(fees_four, fees_thousands, fees_two)

remove(fees_four, fees_thousands, fees_two)

fees$player <- trimws(fees$player)

fees <- fees %>%
  distinct(player, .keep_all = TRUE)

euro_to_usd <- 1.19

pound_to_usd <- 1.33

fees <- fees %>%
  mutate(ex_rate = if_else(fees$currency == 'euro', 
                           euro_to_usd, pound_to_usd)) %>%
  mutate(fee_usd = fee * ex_rate) %>%
  select(-fee, -currency, -ex_rate)

remove(euro_to_usd, pound_to_usd)

fees <- fees %>%
  filter(player != 'Arthur')

write.csv(deals, 'deals.csv')
write.csv(fees, 'deals_fees.csv')

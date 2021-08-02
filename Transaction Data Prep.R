#### SETUP ####

#Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(tibble)
library(rvest)
library(kableExtra)

#Source the scraping and analysis functions
source("Transaction Scraper Functions.R")

#Read in team information
teams <- read_csv("team_info.csv")

#Create a list of team names that can be extracted from Bref transactions
team_list <- paste0("(", paste0(teams$full_name, collapse="|"), ")")

#Create a list of alternate team names that can be cleaned up in the Bref transactions
team_alt_name <- paste0(teams$city, "|", teams$nickname)
#Create a list of team abbreviations to match (and replace) the alternate team names above
team_abbrevs <- teams$abbrev

#### SCRAPE THE DATA ####

#adv_stats <- scrape_adv_stats(2010, 2021)
#write_csv(adv_stats, "adv_stats.csv")

#bref_trans <- scrape_bref_transactions(2010, 2021)
#write_rds(bref_trans, "bref_transactions.rds")

#realgm_trans <- scrape_realgm_transactions(2010, 2021)
#write_rds(realgm_trans, "realgm_transactions.rds")

#execs <- scrape_execs(team_abbrevs[1:30])
#write_csv(execs, "execs.csv")

#### READ IN THE RAW DATA ####

#adv_stats <- read_csv("adv_stats.csv")
#bref_trans <- read_rds("bref_transactions.rds")
#realgm_trans <- read_rds("realgm_transactions.rds")
#execs <- read_csv("execs.csv")

#### CLEAN BREF TRADES ####

bref_details <- bref_trans[[2]] %>%
  #Get rid of non-ASCII characters
  mutate(name = textclean::replace_non_ascii(name))

bref_trades_final <- bref_trans[[1]] %>%
  #Keep trades only
  filter(str_detect(description, "trade")) %>%
  #Convert dates
  mutate(date = lubridate::mdy(date)) %>%
  #Get rid of non-ASCII characters
  mutate(transaction = textclean::replace_non_ascii(description),
         #Remove "In a X-team trade, "
         transaction = str_remove(transaction, "^In a \\d-team trade, "),
         #Remove some things in parentheses
         transaction = str_remove(transaction, "(?<=(?<![JS\\.])[a-zKM\\)\\. ]\\. ).*")) %>%
  #Split the transactions into multiple rows if they have semi-colons
  separate_rows(transaction, sep = ";") %>%
  #Remove "and" and "The" from the end or beginning of transactions
  mutate(transaction = str_remove(transaction, "^ and "),
         transaction = str_remove(transaction, "^ ?[Tt]he "),
         #Extract the teams and the assets they receive from the transaction
         team_a = str_extract(transaction, paste0("^", team_list)),
         team_b_receives = str_extract(transaction, "(?<=traded )(.*)(?= to the)"),
         team_b = str_extract(transaction, paste0("(?<= to the )", team_list)),
         team_a_receives = str_remove(str_extract(transaction, "(?<=for )(.*)$"), "\\. ?$")) %>%
  #Replace "and" with comma
  mutate_at(vars(team_a_receives, team_b_receives), ~str_replace_all(., " and", ",")) %>%
  #Remove commas within numbers
  mutate_at(vars(team_a_receives, team_b_receives), ~str_remove_all(., "(?<=\\d),(?=\\d)")) %>%
  #Join the team abbreviations in
  left_join(teams %>%
              rename(team_a_abbrev = abbrev),
            by = c("team_a" = "full_name")) %>%
  left_join(teams %>%
              rename(team_b_abbrev = abbrev),
            by = c("team_b" = "full_name")) %>%
  select(date, trans_id, description, team_a = team_a_abbrev, team_a_receives, team_b = team_b_abbrev, team_b_receives) %>%
  #Pivot the assets long
  pivot_longer(col = c("team_a_receives", "team_b_receives"),
               names_to = "receiver",
               values_to = "asset") %>%
  #Clarify the sending and receiving team for each set of assets
  mutate(sending_team = if_else(receiver == "team_a_receives", team_b, team_a),
         receiving_team = if_else(receiver == "team_a_receives", team_a, team_b)) %>%
  select(date, trans_id, description, sending_team, receiving_team, asset) %>%
  #Split the sets of assets into individual rows using commas
  separate_rows(asset, sep = ",") %>%
  #Trim the assets
  mutate(asset = str_trim(asset)) %>%
  #Identify picks
  mutate(future_pick = str_extract(asset, "(?<=\\().*(?= was later selected)"),
         name = if_else(is.na(future_pick), asset, future_pick)) %>%
  #Join in player IDs
  left_join(bref_details,
            by = c("trans_id", "name")) %>%
  #Filter out NAs and blanks
  filter(!is.na(asset) & asset != "") %>%
  #Remove * at end of some assets
  mutate(asset = str_remove(asset, "\\*$")) %>%
  select(date, trans_id, description, notes, sending_team, receiving_team, asset, bref_id) %>%
  #Correct an issue with Terence Davis
  mutate(bref_id = if_else(asset == "Terence Davis", "daviste02", bref_id)) %>% 
  #Correct a series of trades
  mutate(sending_team = case_when(
    trans_id == "2014-235-005" & asset == "Cory Jefferson" ~ "PHI",
    TRUE ~ sending_team
  )) %>%
  mutate(receiving_team = case_when(
    trans_id == "2014-235-005" & asset == "cash" ~ "PHI",
    trans_id == "2021-130-012" & asset %in% c("Robert Woodard", "a 2022 2nd round draft pick") ~ "SAC",
    TRUE ~ receiving_team
  ))  %>%
  mutate(asset = case_when(
    trans_id == "2014-236-008" & asset == "Alonzo Gee" ~ "Scotty Hopson",
    TRUE ~ asset
  )) %>%
  mutate(bref_id = case_when(
    trans_id == "2014-236-008" & bref_id == "geeal01" ~ "hopsosc01",
    TRUE ~ bref_id
  )) %>%
  #Replace "cash considerations" with "cash"
  mutate(asset = if_else(asset == "cash considerations", "cash", asset))

#### CLEAN REALGM TRADES ####

#Filter to trades and extract the appropriate data
realgm_trades <- realgm_trans[[1]] %>%
  mutate(date = mdy(str_sub(date, end = 12))) %>%
  #Filter to trades
  filter(str_detect(description, " acquired ")) %>%
  #Replace non-Sr. Jr. .. periods with semi-colons and then separate the rows on semi-colons
  #(This parses multi-team trades into multiple rows)
  mutate(transaction = str_replace_all(description, "(?<=(?<![JS\\.])[A-z\\)\\. ]\\. )", ";")) %>%
  separate_rows(transaction, sep = ";") %>%
  #Extract the teams and what they receive
  mutate(team_a_receives = str_extract(transaction, ".*(?= (was|were) acquired)"),
         team_a = str_extract(transaction, "(?<=acquired by the ).*(?= from)"),
         team_a = if_else(is.na(team_a),
                          str_extract(transaction, "(?<=acquired by the ).*(?= in exchange)"),
                          team_a),
         team_b = str_extract(transaction, "(?<= from the ).*(?= in exchange)"),
         team_b_receives = str_extract(transaction, "(?<=in exchange for:? ).*")) %>%
  #Replace commmas with semi-colons (unless the comma is before Sr. or Jr.)
  mutate_at(vars(team_a_receives, team_b_receives), ~str_replace_all(., ",(?! (S|J)r\\.)", ";")) %>%
  #Replace " and" with a semi-colon
  mutate_at(vars(team_a_receives, team_b_receives), ~str_replace_all(., " and ", "; ")) %>%
  #Pivot the data long
  pivot_longer(col = c("team_a_receives", "team_b_receives"),
               names_to = "receiver",
               values_to = "asset") %>%
  #Determine which team is the sender or receiver for each set of assets
  mutate(sending_team = if_else(receiver == "team_a_receives", team_b, team_a),
         receiving_team = if_else(receiver == "team_a_receives", team_a, team_b)) %>%
  #Clean up the columns
  select(date, trans_id, description, sending_team, receiving_team, asset) %>%
  #Break each asset into its own row
  separate_rows(asset, sep = ";") %>%
  #Clean up the asset and remove any periods at the end (unless preceded by Sr or Jr)
  mutate(asset = str_remove(str_squish(str_trim(asset)), "(?<! (S|J)r)\\.$")) %>%
  #Change "Sixers" to "76ers"
  mutate_at(vars(sending_team, receiving_team), ~str_replace(., "Sixers", "76ers")) %>%
  #Join in the team abbreviations
  left_join(teams %>%
              select(sending_team = full_name, sending_abbrev = abbrev),
            by = "sending_team") %>%
  left_join(teams %>%
              select(receiving_team = full_name, receiving_abbrev = abbrev),
            by = "receiving_team") %>%
  #Clean up the columns
  select(date, trans_id, description, sending_team = sending_abbrev, receiving_team = receiving_abbrev, asset) %>%
  #Convert RealGM's abbrevs to match Bref's
  mutate(asset = str_replace_all(asset, "CHA", "CHO"),
         asset = str_replace_all(asset, "GOS", "GSW"),
         asset = str_replace_all(asset, "PHL", "PHI"),
         asset = str_replace_all(asset, "PHX", "PHO"),
         asset = str_replace_all(asset, "SAN", "SAS"),
         asset = str_replace_all(asset, "UTH", "UTA")) %>%
  #REMOVE VOIDED TRADE
  filter(trans_id != "2016-071-001")

#Merge trades (from multi-team trades) that are missing a sending team or receiving team
missing_parts <- full_join(
  realgm_trades %>%
    filter(is.na(receiving_team)) %>%
    select(-receiving_team) %>%
    mutate(asset = str_replace(asset, "^A ", "a "),
           asset = str_replace(asset, "^The ", "the ")),
  realgm_trades %>%
    filter(is.na(sending_team)) %>%
    select(-sending_team) %>%
    mutate(asset = str_replace(asset, "^A ", "a "),
           asset = str_replace(asset, "^The ", "the ")),
  by = c("trans_id", "date", "asset", "description")
)

#Add in the corrected missing parts and clean up the asset column
realgm_trades_final <- realgm_trades %>%
  filter(!is.na(receiving_team) & !is.na(sending_team)) %>%
  bind_rows(missing_parts) %>%
  mutate(asset = case_when(
    str_detect(asset, "^(A|a) [0-9]{4}") ~ str_remove(str_remove(asset, "^(A|a) "), " own"),
    str_detect(asset, "^(A|a) conditional [0-9]{4}") ~ str_remove(str_replace(asset, "^(A|a) c", "C"), " own"),
    str_detect(asset, "^(A|a) (Least|least|More|more) (F|f)avorable [0-9]{4}") ~ str_replace(str_remove(asset, "^(A|a) "), "Favorable", "favorable"),
    str_detect(asset, "^(A|a) Swap [0-9]{4}") ~ paste0(str_remove(str_remove(asset, "^(A|a) Swap"), "s$"), " swap"),
    asset == "cash" ~ "Cash",
    str_detect(asset, "(A|a) TPE") ~ "Traded Player Exception",
    asset == "multiple TPEs" ~ "Multiple Traded Player Exceptions",
    TRUE ~ asset
  )) %>%
  #Create a column that has the player name clean
  mutate(asset_clean = case_when(
    str_detect(asset, "(T|t)he draft rights") ~ str_extract(asset, "(?<=(T|t)he draft rights to ).*"),
    TRUE ~ asset
  )) %>%
  #Correct two assets' receiving_teams
  mutate(receiving_team = case_when(
    trans_id == "2012-139-031" & asset_clean %in% c("Ahmad Nivins","Georgios Printezis") ~ "NYK",
    TRUE ~ receiving_team
  )) %>%
  #Correct two assets to match Bref
  mutate(asset_clean = case_when(
    trans_id == "2019-283-048" & asset == "The draft rights to Isaac Bonga" ~ "2018 2nd round pick (NYK)",
    trans_id == "2020-001-005" & asset == "Leandro Bolmaro" ~ "2020 1st round pick (UTA)",
    TRUE ~ asset_clean
  )) 

#### CLEAN EXEC DATA ####

execs_final <- execs %>%
  janitor::clean_names("snake") %>%
  #Set present GMs' end date to today
  mutate(end = if_else(end == "present", as.character(Sys.Date()), end)) %>%
  #Convert to dates
  mutate_at(vars(start, end), ~as_date(.)) %>%
  #Filter out stints that ended before 2009
  filter(end >= "2009-01-01") %>%
  #If necessary et end dates back one day (to avoid overlap)
  group_by(team) %>%
  mutate(end = if_else(start == lag(end) & !is.na(lag(end)),
                       end - 1,
                       end)) %>%
  #Look for gaps in dates or multiple executives
  mutate(flag = if_else(start != lag(end) + 1, 1, 0)) %>%
  ungroup() %>%
  #Remove problematic rows
  filter(!(team == "PHO" & executive == "Trevor Bukstein") &
           !(team == "LAC" & executive %in% c("Vinny Del Negro", "Andy Roeser"))) %>%
  #Fix a few rows
  mutate(start = case_when(
    (team == "LAC" & executive == "Gary Sacks") ~ as_date("2012-09-04"),
    (team == "PHO" & executive == "James Jones") ~ as_date("2019-04-11"),
    TRUE ~ start
    )) %>%
  #Combine a few rows and re-add them
  bind_rows(
    tibble(
      team = c("PHO", "LAC", "PHO"),
      executive = c("James Jones, Trevor Bukstein", "Vinny Del Negro, Andy Roeser, Gary Sacks", "Lon Babby"),
      start = c("2018-10-08", "2012-06-04", "2010-07-01"),
      end = c("2019-04-10", "2012-09-03", "2010-08-04")
    ) %>%
      mutate_at(vars(start, end), ~as_date(.))
  ) %>%
  arrange(team, start) %>%
  select(team, executive, start, end)
  

#### DETERMINE PLAYER TENURE ####

#Join the players (and their RealGM IDs) to the transactions
realgm_details <- realgm_trans[[2]] %>%
  filter(str_detect(link, "player")) %>%
  right_join(realgm_trans[[1]] %>%
               mutate(date = mdy(str_sub(date, end = 12))),
             by = "trans_id")

#Remove the player and team names from them descriptions to identify any patterns
realgm_details %>%
  group_by(trans_id, date, description) %>%
  summarize(names = paste0(name, collapse = "|")) %>%
  ungroup() %>%
  mutate(clean_description = str_remove_all(str_remove_all(description, names), team_list)) %>%
  mutate(clean_description = str_remove_all(clean_description, "Philadelphia Sixers")) %>%
  distinct(clean_description) %>%
  arrange(clean_description) %>%
  #Remove trades (since they include various combos of picks, swaps, etc.)
  filter(!str_detect(clean_description, "acquired")) %>%
  #Get rid of draft selections
  filter(!str_detect(clean_description, "in Round [12] with Pick")) %>%
  #Get rid of G-League assignments/recalls
  filter(!str_detect(clean_description, "assigned  to|recalled  from"))

#Create vectors of strings for joining teams and exiting teams
joining_terms <- "selected .* in Round [12]|signed a contract with|signed a multi-year contract with|signed a two-way contract with|converted .* to a Two-Way Contract|made a successful amnesty waiver claim|made a successful waiver claim|signed .* to a Substitute Contract"
exiting_terms <- "exercised an Early Termination Option|became a free agent|ended the two-way contract of|placed the contract of .* on waivers|renounced their Draft Rights|terminated the 10 day contract|utilized the amnesty provision on the contract|withdrew their Qualifying Offer|contract of .* was voided"

#Identify all the (non-trade) transactions where players join or exit teams
joins_exits <- realgm_details %>%
  mutate(join_team = if_else(str_detect(description, joining_terms), 1, 0),
         exit_team = if_else(str_detect(description, exiting_terms), 1, 0)) %>%
  #Filter to such transactions
  filter(join_team == 1 | exit_team == 1) %>% 
  #Extract the team name and join in the team abbreviation
  mutate(description = str_replace_all(description, "Philadelphia Sixers", "Philadelphia 76ers"),
         full_name = str_extract(description, team_list)) %>%
  left_join(teams %>%
              select(full_name, team = abbrev),
            by = "full_name") %>%
  select(-full_name) %>%
  #Remove sign-and-trade signings
  filter(!str_detect(description, "eventual sign and trade"))

#Now find all the trades
trades <- realgm_trades_final %>%
  select(trans_id, asset_clean, sending_team, receiving_team) %>%
  full_join(realgm_details %>%
               inner_join(realgm_trades_final %>%
                            distinct(trans_id),
                          by = "trans_id"),
             by = c("trans_id", "asset_clean" = "name")) %>%
  filter(!is.na(link)) %>%
  pivot_longer(cols = c("sending_team", "receiving_team"),
               names_to = "sender_receiver",
               values_to = "team") %>%
  mutate(join_team = if_else(sender_receiver == "receiving_team", 1, 0),
         exit_team = if_else(sender_receiver == "sending_team", 1, 0)) %>%
  select(link, name = asset_clean, trans_id, date, description, join_team, exit_team, team)

#Combine the joins_exits and trades
joins_exits_final <- bind_rows(
  joins_exits,
  trades
) %>%
  #Convert join_team and exit_team to one column
  mutate(join_exit = if_else(join_team == 1, "join", "exit")) %>%
  #Need to identify draft picks to sort them first
  mutate(draft_pick = if_else(str_detect(description, "selected .* in Round"), 1, NA_real_)) %>%
  #Get rid of rows where player becomes free agent after being waived or released
  arrange(link, team, date) %>%
  group_by(link, team) %>%
  mutate(waived = if_else(str_detect(description, "became a free agent") & str_detect(lag(description), "(on waivers|utilized the amnesty provision|ended the two-way contract of|terminated the 10 day contract)"),
                          1, NA_real_)) %>%
  filter(is.na(waived)) %>%
  ungroup() %>%
  arrange(link, date, draft_pick, desc(trans_id), join_exit) %>%
  #Fill down team abbreviation for transactions missing team abbrev
  group_by(link) %>%
  fill(team, .direction = "down") %>%
  #Now identify the stints players are with a given team
  mutate(stint_num = if_else(row_number() == 1 | team != lag(team),
                             row_number(),
                             NA_integer_)) %>%
  fill(stint_num, .direction = "down") %>%
  ungroup() %>%
  arrange(link, stint_num, date, trans_id) %>% #, desc(join_exit), date, trans_id) %>%
  #Filter to joins and exits only
  group_by(link, stint_num) %>%
  filter((row_number() == 1 & join_exit == "join") | (row_number() == n() & join_exit == "exit")) %>%
  ungroup() %>%
  #Pivot the joins and exits wider
  pivot_wider(id_cols = c("link", "name", "team", "stint_num"),
              names_from = "join_exit",
              values_from = c("date", "description")) %>%
  #Determine the exit type
  mutate(exit_type = case_when(
    str_detect(description_exit, "acquired") ~ "Traded",
    str_detect(description_exit, "exercised an Early Termination Option|became a free agent|withdrew their Qualifying Offer") ~ "Free Agency",
    str_detect(description_exit, "ended the two-way contract of|placed the contract of .* on waivers|terminated the 10 day contract") ~ "Waived/Released",
    str_detect(description_exit, "renounced their Draft Rights") ~ "Renounced",
    str_detect(description_exit, "utilized the amnesty provision on the contract") ~ "Amnestied",
    str_detect(description_exit, "contract of .* was voided") ~ "Contract Voided",
    TRUE ~ NA_character_
  )) %>%
  select(name, link, team, join_date = date_join, exit_date = date_exit, 
         join_description = description_join, exit_description = description_exit, exit_type)

#Check for errors
joins_exits_final %>%
  group_by(link) %>%
  mutate(missing_join = sum(is.na(join_date)),
         missing_exit = sum(is.na(exit_date))) %>%
  ungroup() %>%
  filter(missing_join > 1 | missing_exit > 1) %>%
  as.data.frame()

#### CREATE A MAPPING BETWEEN BREF AND REALGM IDS ####

#Extract only the IDs and assets from the bref data
bref_ids <- bref_trades_final %>%
  filter(!is.na(bref_id)) %>%
  mutate(asset = case_when(
    str_detect(asset, "was later selected") ~ str_extract(asset, "(?<=\\().*(?= was later selected)"),
    TRUE ~ asset
    )) %>%
  distinct(bref_name = asset, bref_id)

#Extract only the IDs and assets from the RealGM data
realgm_ids <- realgm_trades_final %>%
  filter(asset_clean != "Cash" & 
           !str_detect(asset_clean, "Exception") & 
           !str_detect(asset_clean, " pick") &
           asset_clean != "a DPE" &
           asset_clean != "") %>%
  distinct(trans_id, realgm_name = asset_clean) %>%
  left_join(
    realgm_details %>%
      distinct(trans_id, realgm_name = name, realgm_id = link),
    by = c("trans_id", "realgm_name")
  ) %>%
  distinct(realgm_name, realgm_id)

#Do an initial matching
id_matching <- full_join(
  bref_ids,
  realgm_ids,
  by = c("bref_name" = "realgm_name")
)

#Find trades that have multiple matches or no matches
id_matching_issues <- id_matching %>%
  group_by(bref_name) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count > 1 | is.na(bref_id) | is.na(realgm_id)) %>%
  left_join(realgm_details %>%
              distinct(realgm_name = name, realgm_id_2 = link),
            by = c("bref_name" = "realgm_name")) %>%
  mutate(realgm_id = if_else(is.na(realgm_id), realgm_id_2, realgm_id)) %>%
  select(-realgm_id_2)

#Write to a CSV to do some manual matching
#write_csv(id_matching_issues, "id_matching_issues.csv")

#Read in manual matches
id_matching_issues <- read_csv("id_matching_issues_CLEAN.csv")

#Clean it up and keep only matched values and non-NAs
id_matching_final <- id_matching %>%
  group_by(bref_name) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count == 1 & !is.na(bref_id) & !is.na(realgm_id)) %>%
  select(-count) %>%
  bind_rows(
    id_matching_issues
  ) %>%
  select(-bref_name)

#### MERGE THE BREF AND REALGM TRADE DATA ####

#Do an initial matching of trades
trade_matching <- full_join(
  realgm_trades_final %>%
    distinct(date, trans_id, sending_team, receiving_team) %>%
    pivot_longer(cols = c("sending_team", "receiving_team"),
                 names_to = "sending_receiving",
                 values_to = "teams") %>%
    select(-sending_receiving) %>%
    distinct() %>%
    filter(!is.na(teams)) %>%
    arrange(date, trans_id, teams) %>%
    group_by(date, trans_id) %>%
    summarize(teams = paste0(teams, collapse = ",")) %>%
    ungroup() %>%
    rename(realgm_trans_id = trans_id),
  bref_trades_final %>%
    distinct(date, trans_id, sending_team, receiving_team) %>%
    pivot_longer(cols = c("sending_team", "receiving_team"),
                 names_to = "sending_receiving",
                 values_to = "teams") %>%
    select(-sending_receiving) %>%
    distinct() %>%
    filter(!is.na(teams)) %>%
    arrange(date, trans_id, teams) %>%
    group_by(date, trans_id) %>%
    summarize(teams = paste0(teams, collapse = ",")) %>%
    ungroup() %>%
    rename(bref_trans_id = trans_id),
  by = c("date", "teams")
)

#Look trades missing matches and manual map them to their match
missing_matches <- full_join(
  trade_matching %>%
    filter(is.na(realgm_trans_id)) %>%
    select(-realgm_trans_id) %>%
    mutate(date = make_date(year = year(date), month = month(date), day = 1)),
  trade_matching %>%
    filter(is.na(bref_trans_id)) %>%
    select(-bref_trans_id) %>%
    mutate(date = make_date(year = year(date), month = month(date), day = 1)),
  by = c("date", "teams")
) %>% 
  #Manual mapping
  mutate(realgm_trans_id = case_when(
    bref_trans_id == "2021-131-012" ~ "2020-002-044",
    bref_trans_id == "2010-178-001" ~ "2011-237-019",
    bref_trans_id == "2010-178-003" ~ "2011-237-022",
    bref_trans_id == "2014-236-008" ~ "2015-272-005",
    TRUE ~ realgm_trans_id)) %>% 
  mutate(bref_trans_id = case_when(
    realgm_trans_id %in% c("2015-076-011", "2015-076-021") ~ "2015-199-005",
    realgm_trans_id %in% c("2015-004-004", "2015-003-016") ~ "2015-269-002",
    realgm_trans_id %in% c("2016-071-010", "2016-071-012") ~ "2016-202-004",
    TRUE ~ bref_trans_id)) %>%
  #Examine which ones need to be matched using the logic above
  #filter(is.na(bref_trans_id) | is.na(realgm_trans_id)) %>%
  filter(!(realgm_trans_id %in% c("2020-002-044", "2011-237-019", "2011-237-022", "2015-272-005") & is.na(bref_trans_id))) %>%
  filter(!(bref_trans_id %in% c("2015-199-005", "2015-269-002", "2016-202-004") & is.na(realgm_trans_id)))

#Remove trades without a match
trade_matching_final <- trade_matching %>%
  filter(!is.na(realgm_trans_id) & !is.na(bref_trans_id)) %>%
  bind_rows(missing_matches) %>%
  #Find any potential duplicates (same teams trading on same day)
  #group_by(date, teams) %>%
  #mutate(n = n()) %>%
  #arrange(desc(n))
  select(bref_trans_id, realgm_trans_id) %>%
  #Filter out duplicate trades
  filter(!(bref_trans_id == "2017-206-004" & realgm_trans_id == "2017-079-021")) %>%
  filter(!(bref_trans_id == "2017-206-008" & realgm_trans_id == "2017-079-012")) %>%
  filter(!(bref_trans_id == "2011-188-004" & realgm_trans_id == "2011-007-006")) %>%
  filter(!(bref_trans_id == "2011-188-013" & realgm_trans_id == "2011-007-007"))

#Bring the trades together in a piece-meal fashion: Players, Past Draft Picks, Future Draft Picks, and TPEs/Cash
merged_trades <- bind_rows(
  #Players
  full_join(
    #RealGM trades
    realgm_trades_final %>%
      rename(realgm_trans_id = trans_id, realgm_description = description, 
             realgm_asset = asset, asset_match = asset_clean) %>%
      #Keep players only
      filter(!str_detect(asset_match, " ?(C|c)ash| pick|Exception")) %>%
      #Remove commmas before Jr and Sr
      mutate(asset_match = str_remove(asset_match, ",(?= (S|J)r\\.)")) %>%
      #Correct inconsistent player names
      mutate(asset_match = case_when(
        asset_match == "Bruce Brown Jr." ~ "Bruce Brown",
        asset_match == "Cady Lalanne" ~ "Cady Lalane",
        asset_match == "Chu Maduabum" ~ "Chukwudiebere Maduabum",
        asset_match == "Dennis Smith" ~ "Dennis Smith Jr.",
        asset_match == "Derrick Walton Jr." ~ "Derrick Walton",
        asset_match == "Didi Louzada" ~ "Marcos Louzada Silva",
        asset_match == "Isaac Bonga" ~ "Isaac Bonga",
        asset_match == "Isaiah Stewart II" ~ "Isaiah Stewart",
        asset_match == "Jakob Poeltl" ~ "Jakob Poltl",
        asset_match == "Jayden Scrubb" ~ "Jay Scrubb",
        asset_match == "Juancho Hernangomez" ~ "Juan Hernangomez",
        asset_match == "K.J. Martin" ~ "Kenyon Martin Jr.",
        asset_match == "Leandro Bolmaro" ~ "Leandro Bolmaro",
        asset_match == "Marcelinho Huertas" ~ "Marcelo Huertas",
        asset_match == "Matt Dellavedova" ~ "Matthew Dellavedova",
        asset_match == "Maybyner Nene" ~ "Nene",
        asset_match == "Moe Wagner" ~ "Moritz Wagner",
        asset_match == "Raymond Spalding" ~ "Ray Spalding",
        asset_match == "Robert Woodard II" ~ "Robert Woodard",
        asset_match == "Sergii Gladyr" ~ "Sergiy Gladyr",
        asset_match == "Svi Mykhailiuk" ~ "Sviatoslav Mykhailiuk",
        asset_match == "Vasilje Micic" ~ "Vasilije Micic",
        asset_match == "Wade Baldwin IV" ~ "Wade Baldwin",
        asset_match == "Yusuf Sanon" ~ "Issuf Sanon",
        asset_match == "Flip Murray" ~ "Ronald Murray",
        asset_match == "Raulzinho Neto" ~ "Raul Neto",
        asset_match == "Daniel Diez" ~ "Dani Diez",
        asset_match == "Juan Vaulet" ~ "Juan Pablo Vaulet",
        asset_match == "John Lucas" ~ "John Lucas III",
        asset_match == "Ahmad Nivins" ~ "Ahman Nivins",
        TRUE ~ asset_match
      )) %>%
      #Get the bref trade ID for joining later
      left_join(trade_matching_final,
                by = "realgm_trans_id"),
    #BRef trades
    bref_trades_final %>%
      rename(bref_trans_id = trans_id, bref_description = description, bref_asset = asset) %>%
      #Keep players only
      filter(!str_detect(bref_asset, " ?(C|c)ash| pick|Exception" )) %>%
      mutate(asset_match = case_when(
        bref_asset == "cash" | str_detect(bref_asset, "\\$") ~ "Cash",
        TRUE ~ bref_asset
      )) %>% 
      #Correct certain inconsistent player names
      mutate(asset_match = case_when(
        asset_match == "Dennis Smith" ~ "Dennis Smith Jr.",
        asset_match == "Caris Levert" ~ "Caris LeVert",
        asset_match == "Yi Jianlian" ~ "Jianlian Yi",
        asset_match == "Hamady N'Diaye" ~ "Hamady Ndiaye",
        asset_match == "Hamady NDiaye" ~ "Hamady Ndiaye",
        asset_match == "Glen Rice Jr." ~ "Glen Rice",
        asset_match == "Georgios Papagiannis" ~ "George Papagiannis",
        TRUE ~ asset_match
      )) %>% 
      select(-date),
    #Join by the bref_trans_id, asset name, sending, and receiving team
    by = c("bref_trans_id", "asset_match", "sending_team", "receiving_team")
  ),
  #Draft Picks <= 2020
  #Use draft picks <= 2020 from basketball reference (because it lists the player selected)
  bref_trades_final %>%
    rename(bref_trans_id = trans_id, bref_description = description, bref_asset = asset) %>%
    filter(str_detect(bref_asset, "pick")) %>%
    mutate(draft_year = as.numeric(str_extract(bref_asset, "[0-9]{4}"))) %>%
    filter(draft_year <= 2020) %>%
    select(-draft_year, -date) %>%
    left_join(trade_matching_final,
              by = "bref_trans_id") %>%
    left_join(realgm_trades_final %>%
                select(realgm_trans_id = trans_id, realgm_description = description, date) %>%
                distinct(),
              by = "realgm_trans_id"),
  #Draft Picks >= 2021
  #Use draft picks >= 2021 from RealGM (because it specifies the original team)
  realgm_trades_final %>%
    rename(realgm_trans_id = trans_id, realgm_description = description, realgm_asset = asset) %>%
    filter(str_detect(realgm_asset, "pick")) %>%
    mutate(draft_year = as.numeric(str_extract(realgm_asset, "[0-9]{4}"))) %>%
    filter(draft_year >= 2021) %>%
    select(-draft_year),
  #TPEs and Cash form RealGM
  realgm_trades_final %>%
    rename(realgm_trans_id = trans_id, realgm_description = description, realgm_asset = asset) %>%
    filter(str_detect(realgm_asset, "Cash|Exception"))
) %>%
  #Correct missing player IDs
  mutate(bref_id = case_when(
    asset_match == "Tibor Pleiss" ~ "pleisti01",
    asset_match == "Dominique Jones" ~ "jonesdo02",
    asset_match == "David Andersen" ~ "anderda03",
    asset_match == "Jerome Jordan" ~ "jordaje01",
    asset_match == "Russ Smith" ~ "smithru01",
    asset_match == "Pierre Jackson" ~ "jackspi01",
    asset_match == "Semaj Christon" ~ "chrisse01",
    asset_match == "Cory Jefferson" ~ "jeffeco01",
    asset_match == "Willy Hernangomez" ~ "hernawi01",
    asset_match == "Rakeem Christmas" ~ "chrisra01",
    asset_match == "Cedi Osman" ~ "osmande01",
    asset_match == "Jerian Grant" ~ "grantje02",
    asset_match == "Scotty Hopson" ~ "hopsosc01",
    asset_match == "Alonzo Gee" ~ "geeal01",
    asset_match == "Donatas Motiejunas" ~ "motiedo01",
    asset_match == "Marcus Thornton" ~ "thornma01",
    asset_match == "Joel Anthony" ~ "anthojo01",
    asset_match == "Chukwudiebere Maduabum" ~ "maduach01",
    TRUE ~ bref_id
  )) %>%
  filter(asset_match != "" | is.na(asset_match))

merged_trades %>%
  filter(is.na(bref_id) & !is.na(asset_match)) %>% view()

#Finalize the dataframe
master_trades <- merged_trades %>%
  #Get Bref date (in case it's missing from RealGM)
  left_join(bref_trades_final %>%
              select(bref_trans_id = trans_id, bref_date = date) %>%
              distinct(),
            by = "bref_trans_id") %>%
  #Get Bref sending and receiving teams (in case they're missing from RealGM)
  left_join(bref_trades_final %>%
              select(bref_trans_id = trans_id, bref_asset = asset, 
                     bref_sending_team = sending_team, bref_receiving_team = receiving_team) %>%
              distinct(),
            by = c("bref_trans_id", "bref_asset")) %>%
  #If the RealGM value is missing for date, sending_team, receiving_team, use the Bref version
  mutate(date = if_else(is.na(date), bref_date, date),
         sending_team = if_else(is.na(sending_team), bref_sending_team, sending_team),
         receiving_team = if_else(is.na(receiving_team), bref_receiving_team, receiving_team)) %>%
  #If RealGM trans ID is missing, use BRef ID and add a "-B" to it.
  mutate(realgm_trans_id = if_else(is.na(realgm_trans_id), paste0(bref_trans_id, "-B"), realgm_trans_id)) %>%
  #If the RealGM asset is missing, use BRef
  mutate(asset = if_else(is.na(realgm_asset), bref_asset, realgm_asset)) %>%
  #Clean up the assets a little
  mutate(asset = case_when(
    str_detect(asset, "was later selected") ~ str_remove(str_remove(asset, "draft "), "^(A|a) "),
    str_detect(asset, "(T|t)he draft rights to") ~ str_replace(asset, "^the", "The"),
    TRUE ~ asset
  ))  %>%
  #Correct/append past draft picks that did not/have not conveyed
  mutate(asset = case_when(
    bref_trans_id == "2017-003-002" & asset == "a 2019 2nd round draft pick" ~ "Conditional 2019 2nd round pick (DAL) (did not convey)",
    bref_trans_id == "2017-161-002" & asset == "a 2019 1st round draft pick" ~ "Conditional 2019 1st round pick (CLE) (did not convey)",
    bref_trans_id == "2017-198-002" & asset == "a 2017 2nd round draft pick" ~ "Conditional 2017 2nd round pick (CHA) (did not convey)",
    bref_trans_id == "2017-206-003" & asset == "a 2017 2nd round draft pick" ~ "Conditional 2017 2nd round pick (PHO) (did not convey)",
    bref_trans_id == "2018-069-002" & asset == "a 2018 2nd round draft pick" ~ "Conditional 2018 2nd round pick (PHO) (did not convey)",
    bref_trans_id == "2018-071-003" & asset == "a 2018 2nd round draft pick" ~ "Conditional 2018 2nd round pick (ATL) (did not convey)",
    bref_trans_id == "2018-089-002" & asset == "a 2020 2nd round draft pick" ~ "Conditional 2020 2nd round pick (POR) (did not convey)",
    bref_trans_id == "2018-112-001" & asset == "a 2018 2nd round draft pick" ~ "Conditional 2018 2nd round pick (MIL) (did not convey)",
    bref_trans_id == "2018-204-009" & asset == "a 2019 2nd round draft pick" ~ "Conditional 2019 2nd round pick (ATL) (did not convey)",
    bref_trans_id == "2019-013-010" & asset == "a 2020 2nd round draft pick" ~ "Conditional 2020 2nd round pick (POR) (did not convey)",
    bref_trans_id == "2019-031-001" & asset == "a 2020 2nd round draft pick" ~ "Conditional 2020 2nd round pick swap",
    bref_trans_id == "2019-170-003" & asset == "a 2020 2nd round draft pick" ~ "Conditional 2020 2nd round pick (MEM) (did not convey)",
    bref_trans_id == "2019-194-002" & asset == "a 2020 2nd round draft pick" ~ "Conditional 2020 2nd round pick (CHI) (did not convey)",
    bref_trans_id == "2020-007-011" & asset == "a 2020 1st round draft pick" ~ "Conditional 2020 1st round pick (UTA) (has not yet conveyed)",
    bref_trans_id == "2020-008-008" & asset == "a 2020 1st round draft pick" ~ "Conditional 2020 1st round pick (CLE) (did not convey; becomes 2021 and 2022 2nd round picks)",
    bref_trans_id == "2020-008-019" & asset == "a 2020 1st round draft pick" ~ "Conditional 2020 1st round pick (GSW) (did not convey)",
    bref_trans_id == "2010-003-002" & asset == "a 2016 2nd round draft pick" ~ "2016 2nd round pick (Wang Zhelin was later selected)",
    bref_trans_id == "2010-026-003" & asset == "a 2016 2nd round draft pick" ~ "Conditional 2016 2nd round pick (LAC) (did not convey)",
    bref_trans_id == "2010-058-003" & asset == "a 2012 2nd round draft pick" ~ "Conditional 2012 2nd round pick (MIN) (did not convey)",
    bref_trans_id == "2010-094-002" & asset == "a 2012 2nd round draft pick" ~ "Conditional 2012 2nd round pick (MIA) (did not convey)",
    bref_trans_id == "2010-099-002" & asset == "a 2016 2nd round draft pick" ~ "Conditional 2016 2nd round pick (SAC) (did not convey)",
    bref_trans_id == "2010-123-001" & asset == "a 2014 2nd round draft pick" ~ "Conditional 2014 2nd round pick (SAC) (did not convey)",
    bref_trans_id == "2010-123-005" & asset == "a 2010 2nd round draft pick" ~ "Conditional 2010 2nd round pick (SAC) (did not convey)",
    bref_trans_id == "2010-123-007" & asset == "a 2016 2nd round draft pick" ~ "Conditional 2016 2nd round pick (CHO) (did not convey)",
    bref_trans_id == "2011-004-008" & asset == "a 2012 2nd round draft pick" ~ "Conditional 2012 2nd round pick (NYK) (did not convey)",
    bref_trans_id == "2011-009-007" & asset == "a 2011 2nd round draft pick" ~ "Conditional 2011 2nd round pick (BRK) (did not convey)",
    bref_trans_id == "2011-021-001" & asset == "a 2015 2nd round draft pick" ~ "Conditional 2015 2nd round pick (LAC) (did not convey)",
    bref_trans_id == "2011-096-002" & asset == "a 2011 2nd round draft pick" ~ "Conditional 2011 2nd round pick (SAC) (did not convey)",
    bref_trans_id == "2011-143-011" & asset == "a 2017 2nd round draft pick" ~ "Conditional 2017 2nd round pick (SAC) (did not convey)",
    bref_trans_id == "2012-002-012" & asset == "a 2013 2nd round draft pick" ~ "Conditional 2013 2nd round pick (MIL) (did not convey)",
    bref_trans_id == "2012-003-001" & asset == "a 2012 2nd round draft pick" ~ "Conditional 2012 2nd round pick (WAS) (did not convey)",
    bref_trans_id == "2013-005-001" & asset == "a 2017 2nd round draft pick" ~ "Conditional 2017 2nd round pick (NOP) (did not convey)",
    bref_trans_id == "2014-009-004" & asset == "a 2014 2nd round draft pick" ~ "Conditional 2014 2nd round pick (PHI) (did not convey)",
    bref_trans_id == "2014-017-003" & asset == "a 2016 2nd round draft pick" ~ "Removal of protection on conditional 2016 2nd round pick (MEM)",
    bref_trans_id == "2014-039-001" & asset == "a 2014 2nd round draft pick" ~ "Conditional 2014 2nd round pick (PHI) (did not convey)",
    bref_trans_id == "2014-123-004" & asset == "a 2014 2nd round draft pick" ~ "Conditional 2014 2nd round pick (PHI) (did not convey)",
    bref_trans_id == "2014-164-001" & asset == "a 2014 2nd round draft pick" ~ "Conditional 2014 2nd round pick (PHI) (did not convey)",
    bref_trans_id == "2014-164-003" & asset == "a 2015 2nd round draft pick" ~ "Conditional 2015 2nd round pick (SAC) (did not convey)",
    bref_trans_id == "2015-014-015" & asset == "a 2015 2nd round draft pick" ~ "Conditional 2015 2nd round pick (LAC) (did not convey)",
    bref_trans_id == "2015-018-007" & asset == "a 2015 2nd round draft pick" ~ "Conditional 2015 2nd round pick (WAS) (did not convey)",
    bref_trans_id == "2015-044-003" & asset == "a 2015 2nd round draft pick" ~ "Conditional 2015 2nd round pick (PHI) (did not convey)",
    bref_trans_id == "2015-067-001" & asset == "a 2015 2nd round draft pick" ~ "Conditional 2015 2nd round pick (SAC) (did not convey)",
    bref_trans_id == "2015-067-001" & asset == "a 2017 2nd round draft pick" ~ "Conditional 2017 2nd round pick (SAC) (did not convey)",
    bref_trans_id == "2015-069-001" & asset == "a 2015 2nd round draft pick" ~ "Conditional 2015 2nd round pick (PHI) (did not convey)",
    bref_trans_id == "2015-141-003" & asset == "a 2016 2nd round draft pick" ~ "Conditional 2016 2nd round pick (HOU) (did not convey)",
    bref_trans_id == "2015-199-002" & asset == "a 2017 1st round draft pick" ~ "2017 1st round pick (Zhaire Smith was later selected)",
    bref_trans_id == "2015-199-011" & asset == "a 2016 2nd round draft pick" ~ "Conditional 2016 2nd round pick (SAC) (did not convey)",
    bref_trans_id == "2015-199-011" & asset == "a 2015 2nd round draft pick" ~ "Conditional 2015 2nd round pick (PHI) (did not convey)",
    bref_trans_id == "2016-007-039" & asset == "a 2018 2nd round draft pick" ~ "Conditional 2018 2nd round pick (DAL) (did not convey)",
    bref_trans_id == "2016-007-040" & asset == "a 2017 2nd round draft pick" ~ "Conditional 2017 2nd round pick (ATL) (did not convey)",
    bref_trans_id == "2016-012-011" & asset == "a 2020 2nd round draft pick" ~ "Conditional 2020 2nd round pick (POR) (did not convey)",
    bref_trans_id == "2016-024-006" & asset == "a 2019 2nd round draft pick" ~ "Conditional 2019 2nd round pick (BOS) (did not convey)",
    bref_trans_id == "2016-024-007" & asset == "a 2016 2nd round draft pick" ~ "Conditional 2016 2nd round pick (ORL) (did not convey)",
    bref_trans_id == "2016-168-001" & asset == "a 2020 2nd round draft pick" ~ "Conditional 2020 2nd round pick (POR) (did not convey)",
    bref_trans_id == "2016-168-001" & asset == "a 2017 2nd round draft pick" ~ "Conditional 2017 2nd round pick (SAC) (did not convey)",
    bref_trans_id == "2016-200-001" & asset == "a 2019 2nd round draft pick" ~ "Conditional 2019 2nd round pick (BOS) (did not convey)",
    TRUE ~ asset
  )) %>%
  #Join the bref_description back in to cover as many trades as possible
  left_join(
    left_join(bref_trades_final %>%
                distinct(bref_trans_id = trans_id, bref_description_2 = description),
              trade_matching_final,
              by = "bref_trans_id"),
    by = "realgm_trans_id"
  ) %>%
  #Set description to bref_description_2 where available, bref_description where it's not, and realgm_description where there's no bref_description
  mutate(description = case_when(
    is.na(bref_description_2) & is.na(bref_description) ~ realgm_description,
    is.na(bref_description_2) ~ bref_description,
    TRUE ~ bref_description_2
  )) %>%
  #Create an asset ID for each asset (equal to the bref_id or pick ID)
  mutate(asset_id = case_when(
    !is.na(bref_id) ~ bref_id,
    str_detect(asset, "round pick") & !str_detect(asset, "later selected|swap|favorable") ~ str_extract(asset, "[0-9]{4} (1st|2nd) round pick \\([A-Z]{3}\\)"),
    asset == "2017 1st round pick (Zhaire Smith was later selected)" ~ "smithzh01",
    TRUE ~ asset
  )) %>%
  #Correct an issue
  mutate(bref_id = case_when(
    asset == "2017 1st round pick (Zhaire Smith was later selected)" ~ "smithzh01",
    TRUE ~ bref_id
  )) %>%
  #Set an order among the assets (used for ranking later)
  mutate(asset_order = case_when(
    !is.na(bref_id) | str_detect(asset, "The draft rights to") ~ 1,
    str_detect(asset, "round pick") ~ 2,
    str_detect(asset, "Exception") ~ 3,
    TRUE ~ 4
  )) %>%
  select(date, realgm_trans_id, description, sending_team, receiving_team, 
         asset, asset_id, asset_order) %>%
  #Remove duplicates
  distinct() %>%
  #Get rid of rows with no receiving_team (mostly cash and TPEs)
  filter(!is.na(receiving_team))

#### JOIN IN PLAYER TENURE ####

#Join the players' tenure to the trades
trades_tenure_execs <- master_trades %>%
  left_join(
    #Join the match between the player's RealGM info and their BRef info
    joins_exits_final %>%
      left_join(id_matching_final,
                by = c("link" = "realgm_id")) %>%
      select(bref_id, team, join_date, exit_date, exit_type, exit_description) %>%
      mutate(join_date = if_else(is.na(join_date), as_date("2009-07-01"), join_date),
             exit_date = if_else(is.na(exit_date), Sys.Date(), exit_date)),
    by = c("asset_id" = "bref_id", "receiving_team" = "team")
  ) %>%
  #Determine the number of days between the transaction and each stint/tenure's join_date
  mutate(days_diff = as.numeric(abs(date - join_date)),
         #Replace NAs (for rows without join and exit dates) with 0
         days_diff = if_else(is.na(days_diff), 0, days_diff)) %>%
  #For each trade and asset combo, take the one with the smallest date difference
  group_by(realgm_trans_id, asset_id) %>%
  top_n(n = 1, wt = -days_diff) %>%
  ungroup() %>%
  select(-join_date, -days_diff) %>%

#### JOIN IN EXECUTIVE(S) ####

  fuzzyjoin::fuzzy_left_join(execs_final,
                             by = c("receiving_team" = "team",
                                    "date" = "start",
                                    "date" = "end"),
                             match_fun = list(`==`, `>=`, `<=`)) %>%
  select(-team, -start, -end)

#### CALCULATE TRADES' WIN SHARE DIFFERENTIALS ####

#Determine how many win shares each trade has generated for its teams
ws_prep <- trades_tenure_execs %>%
  #Join in the advanced stats
  left_join(adv_stats %>%
              select(asset_id = bref_id, season, team, ws),
            by = c("asset_id", "receiving_team" = "team")) %>%
  #Determine the season of the transaction
  identify_season_by_date(date, trans_season) %>%
  #Determine the player's exit season from the team he was traded to
  identify_season_by_date(exit_date, exit_season) %>%
  #Keep only stats seasons matching/after the transaction season or before/matching the player's exit season (or just NA)
  filter((season >= trans_season &  season <= exit_season) | is.na(season)) %>%
  #Flag stats from the transaction's first season 
  mutate(first_season = if_else(season == trans_season, 1, 0)) %>%
  #Calculate win shares from the first season
  mutate(first_season_ws = ws * first_season) %>%
  #Sum up win shares (to date and in the first season) for each team/trade
  group_by(realgm_trans_id, receiving_team) %>%
  summarize(to_date_ws = sum(ws, na.rm = TRUE),
            first_season_ws = sum(first_season_ws, na.rm = TRUE)) %>%
  ungroup()

#Set remaining_trades marker to 1
remaining_trades <- 1
#Create a copy of the trades df to use as the starting point for the running tally of trade assets
running_tally <- trades_tenure_execs %>%
  #Keep only the ID, receiving team, asset ID, and trade_date
  select(initial_trans_id = realgm_trans_id, team = receiving_team, asset_id, initial_trade_date = date) %>%
  mutate(sub_trans_id = initial_trans_id)
#Create an empty prev_sub_trades_common_only tibble for use in first loop iteration
prev_sub_trades_common_only <- tibble(initial_trans_id = character(),
                                      sub_trans_id = character(),
                                      team = character())

#Run this loop as long as remaining_trades is greater than 1
#The loop will create a running tally of assets from subsequent trades
while (remaining_trades > 0) {
  
  #Identify all subsequent trades that use assets from an initial trade
  sub_trades_all <- running_tally %>%
    #Hold out the sub_trans_id for now
    select(-sub_trans_id) %>%
    inner_join(trades_tenure_execs %>%
                 select(sub_trans_id = realgm_trans_id, team = sending_team, asset_id, sub_trade_date = date) %>%
                 filter(!str_detect(asset_id, "^Cash$|Exception|\\$|DPE")),
               by = c("team", "asset_id")) %>%
    #Keep only subsequent trades that follow the initial trade
    filter(sub_trade_date >= initial_trade_date) %>%
    #Keep only the IDs and teams
    distinct(initial_trans_id, sub_trans_id, team) %>%
    #Add in the running tally's values
    bind_rows(running_tally %>%
                distinct(initial_trans_id, sub_trans_id, team)) %>%
    distinct() %>%
    #Filter out sub trans that are just the initial trans id
    filter(initial_trans_id != sub_trans_id)
  
  #Filter those trades down to those that use only common assets from the initial trade (and subsequent trades)
  sub_trades_common_only <- sub_trades_all %>%
    #Get the subsequent trades' outgoing assets (minus cash and exceptions)
    left_join(trades_tenure_execs %>%
                select(sub_trans_id = realgm_trans_id, team = sending_team, asset_id) %>%
                filter(!str_detect(asset_id, "^Cash$|Exception|\\$|DPE")),
              by = c("sub_trans_id", "team")) %>%
    #Get the initial trades' incoming assets (minus cash and exceptions) from the running tally
    left_join(running_tally %>%
                select(initial_trans_id, team, asset_id) %>%
                filter(!str_detect(asset_id, "^Cash$|Exception|\\$|DPE")) %>%
                #Add flag for assets that are common with initial trade
                mutate(common_asset = 1),
               by = c("initial_trans_id", "team", "asset_id")) %>%
    #Group by the trade and add up the common asset variables. If any are NA, the sum will be NA
    group_by(sub_trans_id, team) %>%
    mutate(common_assets_only = sum(common_asset)) %>%
    ungroup() %>%
    #Filter out the NA trades (i.e. those with asset that are not common with the initial trade)
    filter(!is.na(common_assets_only)) %>%
    #Keep only the mapping (and teams) for subsequent trades that use common assets only
    distinct(initial_trans_id, sub_trans_id, team)
  
  #Determine how many additional trades there are (compared to the previous iteration of this loop)
  #If there are none left, the loop ends
  remaining_trades <- anti_join(sub_trades_common_only,
                                prev_sub_trades_common_only,
                                by = c("initial_trans_id", "sub_trans_id", "team")) %>%
    nrow()
  
  #Save another copy of the sub_trades_common_only df to compare to the next loop
  prev_sub_trades_common_only <- sub_trades_common_only
  
  #Update the running tally of assets accrued from a trade and its subsequent trades
  running_tally <- sub_trades_common_only %>%
    rename(receiving_team = team) %>%
    #Join in the assets received from the subsequent trades
    left_join(trades_tenure_execs %>%
                rename(sub_trans_id = realgm_trans_id),
              by = c("sub_trans_id", "receiving_team")) %>%
    #Bind in the assets received from the initial trade
    bind_rows(trades_tenure_execs %>%
                rename(initial_trans_id = realgm_trans_id) %>%
                mutate(sub_trans_id = initial_trans_id)) %>%
    #Keep only the ids, teams, assets, and trade dates
    select(initial_trans_id, sub_trans_id, team = receiving_team, asset_id, initial_trade_date = date)
  
}

#Now add the initial trades' assets to the assets received from the subsequent trades and add in the win shares
sub_ws_prep <- bind_rows(
  #Initial trades
  trades_tenure_execs %>%
    select(realgm_trans_id, initial_trade_date = date, team = receiving_team, asset_id, exit_date),
  #Subsequent trades
  sub_trades_common_only %>%
    rename(realgm_trans_id = initial_trans_id) %>%
    left_join(trades_tenure_execs %>%
                select(sub_trans_id = realgm_trans_id, sub_trade_date = date, team = receiving_team, asset_id, exit_date),
              by = c("sub_trans_id", "team"))
) %>%
  #Join in the advanced stats
  left_join(adv_stats %>%
              select(asset_id = bref_id, season, team, ws),
            by = c("asset_id", "team")) %>%
  #Identify the date on which the asset came to the team (via the initial or subsequent trade date)
  mutate(trans_date = if_else(is.na(initial_trade_date), sub_trade_date, initial_trade_date)) %>%
  #Determine the season of the transaction
  identify_season_by_date(trans_date, trans_season) %>%
  #Determine the player's exit season from the team he was traded to
  identify_season_by_date(exit_date, exit_season) %>%
  #Keep only stats seasons matching/after the transaction season or before/matching the player's exit season (or just NA)
  filter((season >= trans_season &  season <= exit_season) | is.na(season)) %>%
  #Sum up win shares (to date) for each team/trade
  group_by(realgm_trans_id, team) %>%
  summarize(to_date_sub_ws = sum(ws, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(receiving_team = team)

#Combine the two win share summaries and calculate the differential among the teams
ws_summary <- full_join(
  ws_prep,
  sub_ws_prep,
  by = c("realgm_trans_id", "receiving_team")
) %>%
  #For each trade, sum the total WS generated
  group_by(realgm_trans_id) %>%
  mutate(total_to_date_ws = sum(to_date_ws, na.rm = TRUE),
         total_first_season_ws = sum(first_season_ws, na.rm = TRUE),
         total_to_date_sub_ws = sum(to_date_sub_ws, na.rm = TRUE)) %>%
  ungroup() %>%
  #For each trade and team, determine how many WS the other team(s) generated
  mutate(other_to_date_ws = total_to_date_ws - to_date_ws,
         other_first_season_ws = total_first_season_ws - first_season_ws,
         other_to_date_sub_ws = total_to_date_sub_ws - to_date_sub_ws) %>%
  #For each trade and team, calculate the differential between the win shares generated by the team and the other team(s)
  mutate(to_date_ws_diff = to_date_ws - other_to_date_ws,
         first_season_ws_diff = first_season_ws - other_first_season_ws,
         to_date_sub_ws_diff = to_date_sub_ws - other_to_date_sub_ws) %>%
  select(realgm_trans_id, receiving_team, first_season_ws_diff, to_date_ws_diff, to_date_sub_ws_diff)

#### JOIN WIN SHARE DIFFS TO TRADES ####

master_df <- trades_tenure_execs %>%
  left_join(ws_summary,
            by = c("realgm_trans_id", "receiving_team")) %>%
  #Calculate number of teams involved in trade
  group_by(realgm_trans_id) %>%
  mutate(num_teams = n_distinct(receiving_team)) %>%
  ungroup()

#write_rds(master_df, "master_data.rds")

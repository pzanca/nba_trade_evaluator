#Identify the season given a date column from a data frame
identify_season_by_date <- function(df, date_col, new_date_col) {
  
  date_col <- enquo(date_col)
  new_date_col <- enquo(new_date_col)
  new_date_name <- as_label(new_date_col)
  
  df %>%
    mutate(!!new_date_name := case_when(
      !!date_col >= as_date("2009-06-15") & !!date_col <= as_date("2010-06-17") ~ 2010,
      !!date_col >= as_date("2010-06-18") & !!date_col <= as_date("2011-06-12") ~ 2011,
      !!date_col >= as_date("2011-06-13") & !!date_col <= as_date("2012-06-21") ~ 2012,
      !!date_col >= as_date("2012-06-22") & !!date_col <= as_date("2013-06-20") ~ 2013,
      !!date_col >= as_date("2013-06-21") & !!date_col <= as_date("2014-06-15") ~ 2014,
      !!date_col >= as_date("2014-06-16") & !!date_col <= as_date("2015-06-16") ~ 2015,
      !!date_col >= as_date("2015-06-17") & !!date_col <= as_date("2016-06-19") ~ 2016,
      !!date_col >= as_date("2016-06-20") & !!date_col <= as_date("2017-06-12") ~ 2017,
      !!date_col >= as_date("2017-06-13") & !!date_col <= as_date("2018-06-08") ~ 2018,
      !!date_col >= as_date("2018-06-09") & !!date_col <= as_date("2019-06-13") ~ 2019,
      !!date_col >= as_date("2019-06-14") & !!date_col <= as_date("2020-10-11") ~ 2020,
      !!date_col >= as_date("2020-10-12") & !!date_col <= as_date("2021-07-22") ~ 2021,
      TRUE ~ NA_real_
    ))
  
}

#Scrape the advanced stats tables from Basketball Reference over a series of years
scrape_adv_stats <- function(start_year, end_year) {
  
  #Create a running tibble to capture the data
  adv_stats_data <- tibble()
  
  #Loop through the years
  for (year in start_year:end_year) {
    
    #REGULAR SEASON:
    #Read the HTML
    html <- read_html(paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_advanced.html"))
    
    #Extract the table
    rs_adv_stats <- html %>%
      html_table() %>%
      .[[1]] %>%
      janitor::clean_names("snake") %>%
      filter(rk != "Rk") %>%
      mutate(season = year) %>%
      bind_cols(
        #Bind the players' URLs (they serve as a player ID)
        html %>%
          html_nodes("table") %>%
          .[[1]] %>%
          html_nodes("a") %>%
          html_attr("href") %>%
          tibble::enframe(name = NULL) %>%
          filter(str_detect(value, "players")) %>%
          mutate(bref_id = str_sub(value, start = 12, end = -6)) %>%
          select(bref_id)
      ) %>%
      select(bref_id, season, team = tm, g, mp, ws)
    
    #PLAYOFFS:
    #Read the HTML
    html <- read_html(paste0("https://www.basketball-reference.com/playoffs/NBA_", year, "_advanced.html"))
    
    #Extract the table
    po_adv_stats <- html %>%
      html_table() %>%
      .[[1]] %>%
      janitor::clean_names("snake") %>%
      filter(rk != "Rk") %>%
      mutate(season = year) %>%
      bind_cols(
        #Bind the players' URLs (they serve as a player ID)
        html %>%
          html_nodes("table") %>%
          .[[1]] %>%
          html_nodes("a") %>%
          html_attr("href") %>%
          tibble::enframe(name = NULL) %>%
          filter(str_detect(value, "players")) %>%
          mutate(bref_id = str_sub(value, start = 12, end = -6)) %>%
          select(bref_id)
      ) %>%
      select(bref_id, season, team = tm, g, mp, ws)
    
    #Bind all the data together
    adv_stats_data <- bind_rows(adv_stats_data,
                                rs_adv_stats,
                                po_adv_stats)
    
  }
  
  #Clean up the data
  adv_stats_data <- adv_stats_data %>%
    #Add up the regular season and playoff stats
    group_by(bref_id, season, team) %>%
    mutate_at(vars(g, mp, ws), as.numeric) %>%
    summarize_at(vars(g, mp, ws), ~sum(.)) %>%
    ungroup() %>%
    #Clean up team abbreviations
    mutate(team = case_when(
      team %in% c("NJN") ~ "BRK",
      team %in% c("CHA") ~ "CHO",
      team %in% c("NOH") ~ "NOP",
      TRUE ~ team
    ))
  
  return(adv_stats_data)
  
}

#Scrape the transactions from Basketball-Reference for a given set of years
scrape_bref_transactions <- function(start_year, end_year) {
  
  #Create running tibbles
  trans_list <- tibble()
  trans_details <- tibble()
  
  #Loop through the years
  for (year in start_year:end_year) {
    
    #Read HTML for transaction page
    html <- read_html(paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_transactions.html"))
    
    #Pull the transaction dates
    trans <- html %>%
      html_nodes(".page_index") %>%
      html_nodes("li") 
    
    #Determine number of dates
    num_dates <- trans %>%
      length()
    
    #Loop through dates
    for (d in 1:num_dates) {
      
      #Determine number of transactions on that date
      num_trans <- trans %>%
        .[[d]] %>%
        html_nodes("p") %>%
        length()
      
      #Go through each transaction
      for (t in 1:num_trans) {
        
        #Identify the date of the transaction
        date <- trans %>%
          .[[d]] %>%
          html_node("span") %>%
          html_text()
        
        #Get the transaction description
        transaction <- trans %>%
          .[[d]] %>%
          html_nodes("p") %>%
          .[[t]] %>%
          html_text()
        
        #Extract the hyperlinks from the description (particularly the player links)
        transaction_detail <- trans %>%
          .[[d]] %>%
          html_nodes("p") %>%
          .[[t]] %>%
          html_nodes("a") %>%
          html_attr("href") %>%
          enframe(name = NULL) %>%
          rename(link = value) %>%
          mutate(trans_id = paste0(year, "-", 
                                   str_pad(d, 3, "left", "0"), "-", 
                                   str_pad(t, 3, "left", "0"))) %>%
          bind_cols(
            #Get the player URLs (player IDs)
            trans %>%
              .[[d]] %>%
              html_nodes("p") %>%
              .[[t]] %>%
              html_nodes("a") %>%
              html_text() %>%
              enframe(name = NULL) %>%
              rename(name = value)
          ) %>%
          filter(str_detect(link, "players")) %>%
          mutate(bref_id = str_sub(link, start = 12, end = -6)) %>%
          select(trans_id, name, bref_id)
        
        #Bind the transaction info together
        trans_list <- bind_rows(trans_list,
                                tibble(date = date,
                                       description = transaction,
                                       trans_id = paste0(year, "-", 
                                                         str_pad(d, 3, "left", "0"), "-", 
                                                         str_pad(t, 3, "left", "0"))))
        
        #Bind the transaction details together
        trans_details <- bind_rows(trans_details,
                                   transaction_detail)
        
      }
      
    }
    
  }
  
  #Bind together the list of transactions and the details from those transactions into a list
  transactions <- list(trans_list,
                       trans_details)
  
  return(transactions)
  
}

#Scrape the transactions from Basketball-Reference for a given set of years
scrape_realgm_transactions <- function(start_year, end_year) {
  
  #Create running tibbles
  trans_list <- tibble()
  trans_details <- tibble()
  
  #Loop through each year
  for (year in start_year:end_year) {
    
    #Read the HTML
    html <- read_html(paste0("https://basketball.realgm.com/nba/transactions/league/", year))
    
    #Extract the months
    num_months <- html %>%
      html_nodes("#site-takeover > div.main-container > div > div.large-column-left") %>%
      html_nodes("h2") %>%
      html_text() %>%
      length()
    
    day_num <- 1
    
    #Loop through each month
    for (m in 1:num_months) {
      
      #Determine the number of days in the month
      num_days <- html %>%
        html_nodes(paste0("#site-takeover > div.main-container > div > div.large-column-left > div:nth-child(", m * 2 + 2,") > div")) %>%
        html_nodes("h3") %>%
        length()
      
      #Loop through each day in the month
      for (d in 1:num_days) {
        
        #Capture the date
        date <- html %>%
          html_nodes(paste0("#site-takeover > div.main-container > div > div.large-column-left > div:nth-child(", m * 2 + 2,") > div:nth-child(", d, ")")) %>%
          html_nodes("h3") %>%
          html_text()
        
        #Capture the transactions
        trans <- html %>%
          html_nodes(paste0("#site-takeover > div.main-container > div > div.large-column-left > div:nth-child(", m * 2 + 2,") > div:nth-child(", d, ")")) %>%
          html_nodes("li")
        
        #Determine the number of transactions
        num_trans <- trans %>%
          length()
        
        #Loop through each transaction
        for (t in 1:num_trans) {
          
          #Extract the transaction
          transaction <- trans %>%
            .[[t]] %>%
            html_text()
          
          #Extract the details (including the player IDs)
          transaction_detail <- trans %>%
            .[[t]] %>%
            html_nodes("a") %>%
            html_attr("href") %>%
            enframe(name = NULL) %>%
            rename(link = value) %>%
            bind_cols(
              trans %>%
                .[[t]] %>%
                html_nodes("a") %>%
                html_text() %>%
                enframe(name = NULL) %>%
                rename(name = value)
            ) %>%
            mutate(trans_id = paste0(year, "-",
                                     str_pad(day_num, 3, "left", "0"), "-",
                                     str_pad(t, 3, "left", "0")))
          
          #Bind the transaction info together
          trans_list <- bind_rows(trans_list,
                                  tibble(date = date,
                                         description = transaction,
                                         trans_id = paste0(year, "-", 
                                                           str_pad(day_num, 3, "left", "0"), "-", 
                                                           str_pad(t, 3, "left", "0"))))
          
          #Bind the transaction details together
          trans_details <- bind_rows(trans_details,
                                     transaction_detail)
          
          
        }
        
        day_num <- day_num + 1
        
      }
      
    }
    
  }
  
  #Bind together the list of transactions and the details from those transactions into a list
  transactions <- list(trans_list,
                       trans_details)
  
  return(transactions)
  
}

#Scrape the executive information from Basketball-Reference
scrape_execs <- function(team_abbrevs) {
  
  #Create a running tibble
  execs_all <- tibble()
  
  #Loop through the team abbreviations
  for (team_abbrev in team_abbrevs) {
    
    #Convert certain abbreviations
    if (team_abbrev == "BRK") {
      t <- "NJN"
    } else if (team_abbrev == "CHO") {
      t <- "CHA"
    } else if (team_abbrev == "NOP") {
      t <- "NOH"
    } else {
      t <- team_abbrev
    }
    
    #Read the HTML
    html <- read_html(paste0("https://www.basketball-reference.com/teams/", t, "/executives.html"))
    
    #Extract the table
    execs <- html %>%
      html_table() %>%
      .[[1]] %>%
      filter(Rk != "Rk") %>%
      mutate(team = team_abbrev) %>%
      select(-Rk)
    
    #Bind to the running tibble
    execs_all <- bind_rows(execs_all,
                           execs)
    
  }
  
  return(execs_all)
  
}

#Analyze trade
analyze_trade <- function(trade_id, subsequent_trades = FALSE, colorScheme = teamSolidColor,
                          trade_df = master_df, stats_df = adv_stats) {
  
  #Determine what season the trade originated in:
  trade_season <- identify_season_by_date(
    df = trade_df %>%
      filter(realgm_trans_id == trade_id),
    date,
    trans_season
  ) %>%
    pull(trans_season) %>%
    unique()
  
  #Determine the original teams involved in the trade
  teams_involved <- trade_df %>%
    #Filter to the trade
    filter(realgm_trans_id == trade_id) %>%
    #Get rid of items without a receiving team
    filter(!is.na(receiving_team)) %>%
    #Pull the receiving teams
    pull(receiving_team) %>%
    c(
      #Do the same for sending teams
      trade_df %>%
        filter(realgm_trans_id == trade_id) %>%
        filter(!is.na(sending_team)) %>%
        pull(sending_team)
    ) %>%
    #Get the unique items
    unique() %>%
    #Sort them alphabetically
    str_sort()
  
  #What assets were received by each team?
  assets_received <- trade_df %>%
    #Filter to the assets received for each team in the given trade
    filter(realgm_trans_id == trade_id & receiving_team %in% teams_involved)
  
  #Pivot the table to show the assets each team received over the course of related trades, and how many WS they produced
  ws_received <- assets_received %>%
    select(date, team = receiving_team, asset, asset_id, asset_order, description, 
           exit_date, exit_type, exit_description) %>%
    #Give each asset a number (used in pivot_wider later)
    mutate(asset_num = 1:n()) %>%
    #Join in the advanced stats
    left_join(stats_df %>%
                select(asset_id = bref_id, season, team, ws),
              by = c("asset_id", "team")) %>%
    #Determine the season of the transaction
    identify_season_by_date(date, trans_season) %>%
    #Determine the player's exit season from the team he was traded to
    identify_season_by_date(exit_date, exit_season) %>%
    #Keep only stats seasons matching/after the transaction season or before/matching the player's exit season (or just NA)
    filter((season >= trans_season &  season <= exit_season) | is.na(season)) %>%
    #Arrange by season
    arrange(season) %>%
    #Pivot the win shares into season columns
    pivot_wider(id_cols = c("asset_num", "team", "asset", "asset_id", "asset_order", "description", 
                            "exit_date", "exit_type", "exit_description"),
                names_from = "season",
                values_from = "ws") %>%
    #Arrange by team and then asset type
    arrange(team, asset_order) %>%
    #Set up dummy values for trade sequence (these aren't really necessary but keep columns consistent with subsequent trades approach below)
    mutate(trade_seq_in = 1,
           trade_seq_out = 1) %>%
    #Select the necessary columns for the table
    select(team, asset, asset_id, asset_order, trade_seq_in, trade_seq_out, description_in = description,
           exit_date, exit_type, exit_description,
           starts_with("20"))
  
  #If including subsequent trades...
  if (subsequent_trades == TRUE) {
    
    #Set remaining_trades marker to 1
    remaining_trades <- 1
    #Create a copy of the assets received to use as the starting point for the running tally of trade assets
    running_tally <- assets_received %>%
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
        inner_join(trade_df %>%
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
        left_join(trade_df %>%
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
        left_join(trade_df %>%
                    rename(sub_trans_id = realgm_trans_id),
                  by = c("sub_trans_id", "receiving_team")) %>%
        #Bind in the assets received from the initial trade
        bind_rows(assets_received %>%
                    rename(initial_trans_id = realgm_trans_id) %>%
                    mutate(sub_trans_id = initial_trans_id)) %>%
        #Keep only the ids, teams, assets, and trade dates
        select(initial_trans_id, sub_trans_id, team = receiving_team, asset_id, initial_trade_date = date)
      
    }
    
    #Now add the initial trades' assets to the assets received from the subsequent trades and add in the win shares
    assets_received <- bind_rows(
      #Initial trades
      assets_received %>%
        select(realgm_trans_id,  initial_trade_date = date, team = receiving_team, asset, asset_id, asset_order, 
               description, exit_date, exit_type, exit_description),
      #Subsequent trades
      sub_trades_common_only %>%
        rename(realgm_trans_id = initial_trans_id) %>%
        left_join(trade_df %>%
                    select(sub_trans_id = realgm_trans_id, sub_trade_date = date, team = receiving_team, asset, asset_id, asset_order, 
                           description, exit_date, exit_type, exit_description),
                  by = c("sub_trans_id", "team"))
    ) %>%
      #Create clean transaction ID and date columns
      mutate(trans_id = if_else(is.na(sub_trans_id), realgm_trans_id, sub_trans_id),
             trade_date = if_else(is.na(sub_trade_date), initial_trade_date, sub_trade_date)) %>%
      #Join in each player's exit trade (if applicable)
      left_join(trade_df %>%
                  inner_join(
                    bind_rows(select(sub_trades_common_only, realgm_trans_id = initial_trans_id),
                              select(sub_trades_common_only, realgm_trans_id = sub_trans_id)) %>%
                      distinct(),
                    by = "realgm_trans_id"
                  ) %>%
                  filter(!str_detect(asset_id, "^Cash$|Exception|\\$|DPE")) %>%
                  select(trans_id_out = realgm_trans_id, team = sending_team, asset_id, description_out = description),
                by = c("team", "asset_id"))
    
    #Determine the trade sequence for each team in the trade
    trade_sequence <- assets_received %>%
      distinct(trans_id, trade_date, team) %>%
      arrange(team, trade_date, trans_id) %>%
      group_by(team) %>%
      mutate(trade_seq = 1:n()) %>%
      ungroup()
    
    #Add the trade sequence number to the assets_received df (for incoming and outgoing assets in each trade)
    assets_received <- assets_received %>%
      left_join(trade_sequence %>%
                  rename(trade_seq_in = trade_seq),
                by = c("trans_id", "trade_date", "team")) %>%
      left_join(trade_sequence %>%
                  select(trade_seq_out = trade_seq, trans_id_out = trans_id, team),
                by = c("trans_id_out", "team")) %>% 
      arrange(team, trade_date)
    
    #Pivot the table to show the assets each team received over the course of related trades, and how many WS they produced
    ws_received <- assets_received %>%
      select(trade_date, team, asset, asset_id, asset_order, 
             trade_seq_in, description_in = description, trade_seq_out, description_out,
             exit_date, exit_type, exit_description) %>%
      #Give each asset a number (used in pivot_wider later)
      mutate(asset_num = 1:n()) %>%
      #Join in the advanced stats
      left_join(stats_df %>%
                  select(asset_id = bref_id, season, team, ws),
                by = c("asset_id", "team")) %>%
      #Determine the season of the transaction
      identify_season_by_date(trade_date, trans_season) %>%
      #Determine the player's exit season from the team he was traded to
      identify_season_by_date(exit_date, exit_season) %>%
      #Keep only stats seasons matching/after the transaction season or before/matching the player's exit season (or just NA)
      filter((season >= trans_season &  season <= exit_season) | is.na(season)) %>%
      #Arrange by season
      arrange(season) %>%
      #Pivot the win shares into season columns
      pivot_wider(id_cols = c("asset_num", "team", "asset", "asset_id", "asset_order", 
                              "trade_seq_in", "description_in", "trade_seq_out", "description_out",
                              "exit_date", "exit_type", "exit_description"),
                  names_from = "season",
                  values_from = "ws") %>%
      #Select the necessary columns for the table
      select(team, asset, asset_id, asset_order, trade_seq_in, description_in, 
             trade_seq_out, description_out, 
             exit_date, exit_type, exit_description,
             starts_with("20")) %>%
      #Arrange by team, trade sequence (in), and then asset type
      arrange(team, trade_seq_in, asset_order)
    
  }
  
  #Create total rows for each team in the trade and prep for kableExtra
  table <- bind_rows(
    #Bind the the existing ws df with...
    ws_received,
    #A version of the df grouped and summed by team
    ws_received %>%
      group_by(team) %>%
      summarize_at(vars(starts_with("20")), ~sum(., na.rm = TRUE)) %>%
      ungroup() %>%
      #Input appropriate values to assist with binding and sorting
      mutate(asset = "TOTAL",
             asset_order = 5,
             trade_seq_in = NA_integer_,
             trade_seq_out = NA_integer_,
             description_in = "",
             description_out = "",
             exit_date = as_date(""),
             exit_type = "",
             exit_description = "")
  ) %>%
    #Sum the win shares for each row
    mutate(TOTALS = select(., starts_with("20")) %>% rowSums(na.rm = TRUE)) %>%
    #Arrange the table by team, trade_seq_in, asset_order, and WS Totals
    arrange(team, trade_seq_in, asset_order, desc(TOTALS)) %>%
    #If exit_type is NA (still with team), set it to blank
    mutate(exit_type = if_else(is.na(exit_type), "", exit_type)) %>%
    #Unless exit_type is blank, concat the exit date and description
    mutate(exit_info = if_else(exit_type == "", 
                               "",
                               paste0(format(exit_date, "%m/%d/%y"), ": ", exit_description))) %>%
    #Keep/rename the columns
    select(Team = team, `Trade In` = trade_seq_in, Description_In = description_in, 
           `Trade Out` = trade_seq_out, Description_Out = description_out, 
           Asset = asset, `Still with Team?` = exit_type, exit_info,
           starts_with("20"), TOTALS)
  
  #Prep table and variables for final table depending on whether subsequent trades are included
  if (subsequent_trades == FALSE) {
    table <- table %>%
      select(Team, Asset, `Still with Team?`, exit_info, starts_with("20"), TOTALS) %>%
      mutate_at(vars(starts_with("20"), TOTALS), ~str_replace_all(format(., trim = FALSE, nsmall = 1), "NA", "")) %>%
      mutate(Team = cell_spec(Team, "html",
                              color = colorScheme[Team])) %>%
      mutate(`Still with Team?` = cell_spec(`Still with Team?`, "html",
                                            tooltip = exit_info,
                                            color = if_else(table$Asset[row_number()] != "TOTAL",
                                                            colorScheme[table$Team[row_number()]],
                                                            "#FFFFFF"))) %>%
      mutate_at(vars(-Team, -`Still with Team?`), ~cell_spec(., "html",
                                                             color = if_else(table$Asset[row_number()] != "TOTAL",
                                                                             colorScheme[table$Team[row_number()]],
                                                                             "#FFFFFF"))) %>%
      select(-exit_info)
    asset_col <- 2
    first_year_col <- 4
    alignment <- "clc"
  } else if (subsequent_trades == TRUE) {
    table <- table %>%
      select(Team, `Trade In`, Description_In, `Trade Out`, Description_Out, Asset, 
             `Still with Team?`, exit_info, starts_with("20"), TOTALS) %>%
      mutate_at(vars(`Trade In`, `Trade Out`), ~if_else(is.na(.), "", as.character(.))) %>%
      mutate_at(vars(starts_with("20"), TOTALS), ~str_replace_all(format(., trim = FALSE, nsmall = 1), "NA", "")) %>%
      mutate(Team = cell_spec(Team, "html",
                              color = colorScheme[Team])) %>%
      mutate(`Trade In` = cell_spec(`Trade In`, "html", 
                                    tooltip = Description_In,
                                    color = if_else(table$Asset[row_number()] != "TOTAL",
                                                    colorScheme[table$Team[row_number()]],
                                                    "#FFFFFF"))) %>%
      mutate(`Trade Out` = text_spec(`Trade Out`, "html", 
                                     tooltip = Description_Out,
                                     color = if_else(table$Asset[row_number()] != "TOTAL",
                                                     colorScheme[table$Team[row_number()]],
                                                     "#FFFFFF"))) %>%
      mutate(`Still with Team?` = cell_spec(`Still with Team?`, "html",
                                            tooltip = exit_info,
                                            color = if_else(table$Asset[row_number()] != "TOTAL",
                                                            colorScheme[table$Team[row_number()]],
                                                            "#FFFFFF"))) %>%
      mutate_at(vars(-Team, -`Trade In`, -`Trade Out`, -`Still with Team?`), 
                ~cell_spec(., "html",
                           color = if_else(table$Asset[row_number()] != "TOTAL",
                                           colorScheme[table$Team[row_number()]],
                                           "#FFFFFF"))) %>%
      select(-Description_In, -Description_Out, -exit_info) 
    asset_col <- 4
    first_year_col <- 6
    alignment <- "ccclc"
  }
  
  num_teams <- length(teams_involved)
  num_cols <- dim(table)[2]
  total_rows <- which(str_detect(table$Asset,"TOTAL"))
  alignment <- paste0(alignment, strrep("c", num_cols))
  
  options(knitr.kable.NA = '')
  
  final_table <- table %>%
    kbl(escape = FALSE,
        align = alignment) %>%
    kable_styling(html_font = "Helvetica",
                  full_width = FALSE) %>%
    column_spec(column = 1, width = "50px", bold = TRUE) %>%
    column_spec(column = asset_col, width = "300px") %>%
    column_spec(column = first_year_col:num_cols, width = "65px") %>%
    column_spec(column = num_cols, background = "#e0e0e0") %>%
    collapse_rows(columns = 1, valign = "top") %>%
    row_spec(row = 0, background = "black", color = "white") %>%
    row_spec(row = 0:tail(total_rows, n=1), extra_css = 'border: 1px solid gray;') %>%
    row_spec(row = total_rows[1], bold = TRUE, color = "white",
             background = colorScheme[teams_involved[1]]) %>%
    row_spec(row = total_rows[2], bold = TRUE, color = "white",
             background = colorScheme[teams_involved[2]])
  
  if (num_teams >= 3) {
    
    final_table <- final_table %>%
      row_spec(row = total_rows[3], bold = TRUE, color = "white",
               background = colorScheme[teams_involved[3]])
    
  }
  
  if (num_teams >= 4) {
    
    final_table <- final_table %>%
      row_spec(row = total_rows[4], bold = TRUE, color = "white",
               background = colorScheme[teams_involved[4]])
    
  }
  
  return(final_table)
  
}


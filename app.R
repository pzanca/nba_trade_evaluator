#### SETUP ####
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(tibble)
library(kableExtra)
library(shiny)
library(ggplot2)
library(plotly)

source("Transaction Scraper Functions.R")

master_df <- read_rds("master_data.rds") %>%
  mutate(description = paste0(format(date, "%m/%d/%y"), ": ", description))
adv_stats <- read_csv("adv_stats.csv")
teams <- read_csv("team_info.csv")

teamSolidColor <- teams$solid_color
names(teamSolidColor) <- teams$abbrev

trades_by_player <- master_df %>%
  filter(asset_order == 1) %>%
  mutate(player = case_when(
    str_detect(asset, "later selected") ~ str_extract(asset, "(?<=\\().*(?= was later selected)"),
    str_detect(asset, "draft rights") ~ str_extract(asset, "(?<=The draft rights to ).*"),
    TRUE ~ asset
  )) %>%
  select(date, realgm_trans_id, player, asset_id, description) %>%
  arrange(player, date, realgm_trans_id)

player_list <- trades_by_player %>%
  distinct(player, asset_id)

team_list <- teams$full_name[1:30]

exec_list <- master_df %>%
  filter(!is.na(executive)) %>%
  group_by(executive) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  pull(executive)

max_trades_by_exec <- master_df %>%
  distinct(realgm_trans_id, executive) %>%
  group_by(executive) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pull(count) %>%
  max()

ui <- fluidPage(
  title = "NBA Trade Evaluator",
  h1("NBA Trade Evaluator"),
  p("App by ", a(href = "https://twitter.com/Peter_Zanca", target="_blank", "Peter Zanca"), " | Data from ", 
    a(href = "https://basketball.realgm.com/nba/transactions/league/2021", target="_blank", "RealGM"), " and ",
    a(href = "https://www.basketball-reference.com/leagues/NBA_2021_transactions.html", target="_blank", "Basketball-Reference.com"), " | Trades from 07-09-2009 to 06-27-2021"),
  tabsetPanel(
    tabPanel(
      "Introduction",
      h3("Introduction"),
      p("Whenever NBA teams make a trade, every sports media outlet rushes to grade the trade and declare winners and losers before any of the players even hit the court for their new teams. Of course, it takes time to find out how players fit on their new teams, how draft picks pan out, and whether players stick around with their new team in the long term. Sometimes it takes years or even decades to know which team really got the better end of a trade."),
      p("This app aims to assist in evaluating past trades by aggregating the ", a(href = "https://www.basketball-reference.com/about/ws.html", target="_blank", "win shares"), " accrued by the players (and draft picks) each team acquired in a trade. While win shares (WS) is far from a perfect metric, it gives us a decent feel for how much players contributed to winning games."),
      h3("Initial Trades & Subsequent Trades"),
      p("When assessing the outcome of a trade, it is important to also consider how subsequent trades impact the outcome of an original trade."),
      p("For example, in 2016 the Thunder traded Serge Ibaka to the Magic for Victor Oladipo, Domantas Sabonis, and Ersan Ilyasova. Each of those players played only one season for their new teams. Orlando flipped Ibaka for Terrence Ross, while Oklahoma City turned Oladipo and Sabonis into Paul George. Two years later, they turned George into Shai Gilgeous-Alexander, Danilo Gallinari, and a whole slew of picks and swaps. If we evaluate the trade based solely on the assets exchanged in the initial trade, we might fail to account for all of the value created from the assets exchanged in the initial trade."),
      p("This app allows you to look at both the initial trade (the players and assets exchanged for each other) and the result of any 'subsequent trades' involving the same assets. When we talk about subsequent trades, we're including only those trades where outgoing assets weren't aggregated with other players or picks from outside the trade."),
      p("For example, a team trades player A for B. A year later, the team trades player B for players C and D. We would include that trade (and player C and D's win shares) in our analysis. If they then traded player D and some other player Z for player E, we would not include that trade, since extends outside of the original exchange of assets."),
      h3("A Few Notes"),
      tags$ul(
        tags$li("You can search for trades by player, by team, or by executive."),
        tags$li("The win shares displayed in the app include both regular season and playoffs."),
        tags$li("The data is sourced from RealGM and Basketball-Reference, dating back to the 2009-10 season. (I'll add more seasons at a later date."),
        tags$li("I cleaned the data as best as I could without going through 600+ trades line by line. There may be some errors. DM me ",
                a(href = "https://twitter.com/Peter_Zanca", target="_blank", "@Peter_Zanca"),
                " if you see something strange or something missing.")
      ),
      h3("Limitations"),
      p("Because generating wins is not always a team's immediate objective in executing a trade, this analysis has some blind spots. Here are some limitations of this analysis:"),
      tags$ul(
        tags$li(strong("Future Picks & Performance: "), "As noted above, it takes time to evaluate trades. Many trades, particularly those in the past few years, involve players who are still rounding into form. Many others involve future draft picks. Who knows whether those picks will convey, where they'll fall, whom teams will pick, or how those players will pan out? This analysis assesses only the value gained by teams to date."),
        tags$li(strong("Cap Space: "), "Some trades are salary dumps to free up cap space or avoid the luxury tax. This analysis does not account for the benefits of cap space (free agent signings, etc.) or avoiding the tax (revenue sharing, etc.)."),
        tags$li(strong("Cash: "), "Some trades generate much needed cash for an organization (like when ", 
                a(href = "https://ph.news.yahoo.com/nba-star-recalls-being-traded-201007761.html", target="_blank", "Kyle Korver was traded for cash to buy a copy machine"), 
                "). This analysis does not account for what teams were able to do with cash gained from trades."),
        tags$li(strong("Trade Exceptions: "), "Some trades generate Traded Player Exceptions (TPEs). These exceptions may allow teams to execute future trades without impacting their cap and luxury-tax liabilities. This analysis does not account for if/how TPEs are utilized.")
      )
    ),
    tabPanel(
      "Search by Player",
      fluidRow(
        column(width = 2,
               offset = 0,
               selectInput(inputId = "player",
                           label = "Select Player",
                           choices = pull(player_list, player),
                           selected = "James Harden")),
        column(width = 6,
               offset = 0,
               selectInput(inputId = "trade_player",
                           label = "Select Trade",
                           choices = character(0),
                           width = '100%')),
        column(width = 2,
               offset = 0,
               selectInput(inputId = "sub_trades_player",
                           label = "Include subsequent trades*?",
                           choices = c("Yes", "No"),
                           selected = "No"),
               p("*Subsequent trades are included only if the outgoing assets were not aggregated with other players or picks.",
                 style = "font-size:x-small;"))
      ),
      fluidRow(
        tableOutput(outputId = "table_player")
      )
    ),
    tabPanel(
      "Search by Exec",
      fluidRow(
        column(width = 2,
               offset = 0,
               selectInput(inputId = "exec",
                           label = "Select Executive",
                           choices = exec_list)),
        column(width = 6,
               offset = 0,
               selectInput(inputId = "trade_exec",
                           label = "Select Trade",
                           choices = character(0),
                           width = '100%')),
        column(width = 2,
               offset = 0,
               selectInput(inputId = "sub_trades_exec",
                           label = "Include subsequent trades*?",
                           choices = c("Yes", "No"),
                           selected = "No"),
               p("*Subsequent trades are included only if the outgoing assets were not aggregated with other players or picks.",
                 style = "font-size:x-small;"))
      ),
      fluidRow(
        tableOutput(outputId = "table_exec")
      )
    ),
    tabPanel(
      "Search by Team",
      fluidRow(
        column(width = 2,
               offset = 0,
               dateRangeInput(inputId = "date_range",
                              label = "Select Date Range",
                              start = min(master_df$date),
                              end = Sys.Date(),
                              min = min(master_df$date),
                              max = Sys.Date(),
                              format = "mm-dd-yyyy",
                              startview = "year")),
        column(width = 2,
               offset = 0,
               selectInput(inputId = "team",
                           label = "Select Team",
                           choices = team_list)),
        column(width = 6,
               offset = 0,
               selectInput(inputId = "trade_team",
                           label = "Select Trade",
                           choices = character(0),
                           width = '100%')),
        column(width = 2,
               offset = 0,
               selectInput(inputId = "sub_trades_team",
                           label = "Include subsequent trades*?",
                           choices = c("Yes", "No"),
                           selected = "No"),
               p("*Subsequent trades are included only if the outgoing assets were not aggregated with other players or picks.",
                 style = "font-size:x-small;"))
      ),
      fluidRow(
        tableOutput(outputId = "table_team")
      )
    ),
    tabPanel(
      "Exec Comparison",
      fluidRow(
        column(width = 2,
               offset = 0,
               dateRangeInput(inputId = "date_range_comparison",
                              label = "Select Date Range",
                              start = min(master_df$date),
                              end = Sys.Date(),
                              min = min(master_df$date),
                              max = Sys.Date(),
                              format = "mm-dd-yyyy",
                              startview = "year")),
        column(width = 2,
               offset = 0,
               sliderInput(inputId = "min_trades",
                           label = "Min. Number of Trades",
                           value = 1,
                           min = 1,
                           max = max_trades_by_exec,
                           step = 1,
                           ticks = FALSE)),
        column(width = 3,
               offset = 0,
               selectInput(inputId = "multi_team",
                           label = "Include 3+ Team Trades?",
                           choices = c("Yes", "No"),
                           selected = "Yes")),
        column(width = 3,
               offset = 0,
               selectInput(inputId = "sort_execs",
                           label = "Sort By",
                           choices = c("Number of Trades" = "exec_num_trades",
                                       "Cumulative WS Diff." = "cum_ws",
                                       "Average WS Diff." = "avg_ws",
                                       "Median WS Diff." = "med_ws",
                                       "Cumulative WS Diff. (Including Subsequent Trades)" = "cum_sub_ws",
                                       "Average WS Diff. (Including Subsequent Trades)" = "avg_sub_ws",
                                       "Median WS Diff. (Including Subsequent Trades" = "med_sub_ws"),
                           selected = "exec_num_trades"))
      ),
      fluidRow(
        plotlyOutput(outputId = "table_exec_comp")
      )
    )
  )
)

server <- function (input, output, session) {
  
  #Update trade dropdown list based on player selected
  observe({
    
    #Identify selected player's asset_id
    player_asset_id <- player_list %>%
      filter(player == input$player) %>%
      pull(asset_id)
    
    #Refresh the trade list based on the selected player
    player_trade_list <- trades_by_player %>%
      filter(asset_id == player_asset_id) %>%
      pull(description)
    
    #Make the list empty if necessary
    if (is.null(player_trade_list)) {
      player_trade_list <- character(0)
    }
    
    #Update the select input
    updateSelectInput(session, "trade_player",
                      choices = player_trade_list)
    
  })
  
  #Update trade dropdown list based on exec selected
  observe({
    
    #Refresh the trade list based on the selected exec
    exec_trade_list <- master_df %>%
      filter(executive == input$exec) %>%
      arrange(date) %>%
      distinct(description) %>%
      pull()
    
    #Make the list empty if necessary
    if (is.null(exec_trade_list)) {
      exec_trade_list <- character(0)
    }
    
    #Update the select input
    updateSelectInput(session, "trade_exec",
                      choices = exec_trade_list)
    
  })
  
  #Update trade dropdown list based on team selected
  observe({
    
    #Identify selected team's abbrev
    team_abbrev <- teams %>%
      filter(full_name == input$team) %>%
      pull(abbrev)
    
    #Refresh the trade list based on the selected team
    team_trade_list <- master_df %>%
      filter(receiving_team == team_abbrev) %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      arrange(date) %>%
      distinct(description) %>%
      pull()
    
    #Make the list empty if necessary
    if (is.null(team_trade_list)) {
      team_trade_list <- character(0)
    }
    
    #Update the select input
    updateSelectInput(session, "trade_team",
                      choices = team_trade_list)
    
  })
  
  #Create/update the table for the player tab
  output$table_player <- function() {
    
    #Identify the trade
    selected_trade_player <- trades_by_player %>%
      filter(description == input$trade_player) %>%
      distinct(realgm_trans_id) %>%
      pull()
    
    #Determine whether to include subsequent trades
    if (input$sub_trades_player == "Yes") {
      include_subs = TRUE
    } else {
      include_subs = FALSE
    }
    
    #Create/update the table using the analyze_trade function
    analyze_trade(trade_id = selected_trade_player,
                  subsequent_trades = include_subs,
                  trade_df = master_df,
                  stats_df = adv_stats,
                  colorScheme = teamSolidColor)
    
  }
  
  #Create/update the table for the exec tab
  output$table_exec <- function() {
    
    #Identify the trade
    selected_trade_exec <- master_df %>%
      filter(description == input$trade_exec) %>%
      distinct(realgm_trans_id) %>%
      pull()
    
    #Determine whether to include subsequent trades
    if (input$sub_trades_exec == "Yes") {
      include_subs = TRUE
    } else {
      include_subs = FALSE
    }
    
    #Create/update the table using the analyze_trade function
    analyze_trade(trade_id = selected_trade_exec,
                  subsequent_trades = include_subs,
                  trade_df = master_df,
                  stats_df = adv_stats,
                  colorScheme = teamSolidColor)
    
  }
  
  #Create/update the table for the team tab
  output$table_team <- function() {
    
    #Identify the trade
    selected_trade_team <- master_df %>%
      filter(description == input$trade_team) %>%
      distinct(realgm_trans_id) %>%
      pull()
    
    #Determine whether to include subsequent trades
    if (input$sub_trades_team == "Yes") {
      include_subs = TRUE
    } else {
      include_subs = FALSE
    }
    
    #Create/update the table using the analyze_trade function
    analyze_trade(trade_id = selected_trade_team,
                  subsequent_trades = include_subs,
                  trade_df = master_df,
                  stats_df = adv_stats,
                  colorScheme = teamSolidColor)
    
  }
  
  #Create executive comparison plot
  output$table_exec_comp = renderPlotly({
    
    ggplotly(
      master_df %>%
        #Reduce df to distinct exec-related elements for each trade
        distinct(realgm_trans_id, date, num_teams, executive, team = receiving_team, description,
                 to_date_ws_diff, to_date_sub_ws_diff) %>%
        #Count the number of trades the executive executed
        group_by(executive) %>%
        mutate(exec_num_trades = n(),
               cum_ws = sum(to_date_ws_diff, na.rm = TRUE),
               avg_ws = mean(to_date_ws_diff, na.rm = TRUE),
               med_ws = median(to_date_ws_diff, na.rm = TRUE),
               cum_sub_ws = sum(to_date_sub_ws_diff, na.rm = TRUE),
               avg_sub_ws = mean(to_date_sub_ws_diff, na.rm = TRUE),
               med_sub_ws = median(to_date_sub_ws_diff, na.rm = TRUE)) %>%
        ungroup() %>%
        #Filter out trades and executives who don't meet the filter requirements
        filter(if (input$multi_team == "No") num_teams == 2 else num_teams > 0) %>%
        filter(exec_num_trades >= input$min_trades) %>%
        filter(date >= input$date_range_comparison[1] & date <= input$date_range_comparison[2]) %>%
        #Pivot ws_diff metrics long so that we can do a facet_grid
        rename(initial = to_date_ws_diff, subsequent = to_date_sub_ws_diff) %>%
        pivot_longer(cols = c("initial", "subsequent"),
                     names_to = "initial_sub",
                     values_to = "ws_diff") %>%
        #Create tooltip with HTML formatting and limited width
        mutate(description = str_wrap(description, width = 80),
               tooltip = paste0("<b>", executive, " (", team, ")</b><br>",
                                "<b>WS Diff: </b>", ws_diff, "<br>",
                                description)) %>%
        ggplot(aes(x = ws_diff, y = reorder(executive, !!sym(input$sort_execs)), fill = team, color = team, text = tooltip)) +
        geom_point(alpha = 0.7, size = 3, stroke = 0.25) +
        #stat_summary(aes(group = executive), fun = "mean", geom = "point", fill = "black", size = 3, shape = 15) +
        #stat_summary(aes(group = executive), fun = "median", geom = "point", fill = "black", size = 3, shape = 2) +
        scale_fill_manual(values = teamSolidColor) +
        scale_color_manual(values = teamSolidColor) +
        facet_grid(
          ~ initial_sub,
          labeller = as_labeller( c(`initial` = "Win Share Differential (Initial Trade Only)",
                                    `subsequent` = "Win Share Differential (Including Subsequent Trades*)"))
        ) +
        theme_bw() +
        theme(
          text = element_text(family = "Helvetica"),
          legend.position = "none",
          axis.title.x = element_blank(),
          strip.background = element_rect(fill = "black"),
          strip.text.x = element_text(color = "white")
        ),
      height = exec_length() * 25 + 100,
      tooltip = "text"
    ) %>%
      layout(
        font = list(family = "Helvetica")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #Determine number of execs within filter (so we can set the height appropriately)
  exec_length <- reactive({
    
    master_df %>%
      #Reduce df to distinct exec-related elements for each trade
      distinct(realgm_trans_id, date, num_teams, executive, team = receiving_team, description,
               to_date_ws_diff, to_date_sub_ws_diff) %>%
      #Count the number of trades the executive executed
      group_by(executive) %>%
      mutate(exec_num_trades = n()) %>%
      #Filter out trades and executives who don't meet the filter requirements
      filter(if (input$multi_team == "No") num_teams == 2 else num_teams > 0) %>%
      filter(exec_num_trades >= input$min_trades) %>%
      filter(date >= input$date_range_comparison[1] & date <= input$date_range_comparison[2]) %>%
      distinct(executive) %>%
      nrow()
    
  })
  
}

shinyApp(ui = ui, server = server)


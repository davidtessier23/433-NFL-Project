install.packages("tidyverse", type = "binary")
install.packages("ggrepel", type = "binary")
install.packages("ggimage", type = "binary")
install.packages("nflfastR", type = "binary")
install.packages("gsisdecoder")
install.packages("ggplot2")
install.packages("patchwork")

library(patchwork)
library(tidyverse, warn.conflicts = FALSE)
library(ggrepel, warn.conflicts = FALSE)
library(ggimage, warn.conflicts = FALSE)
library(nflfastR, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(nflreadr, warn.conflicts = FALSE)
library(nflscrapR, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(shiny)

data1 <- load_pbp(2010:2020) %>% 
  filter(season_type == "REG")

fd_dropbacks1 <- data1 %>%
  filter(down==1 & ydstogo==10 & yardline_100<=80 & yardline_100>=20 & qb_dropback==1 & wp>= .20 & wp<= .80 & half_seconds_remaining>120 & !is.na(epa)) %>%
  select(posteam, season, week, play_id, down, ydstogo, epa, wpa, success, yards_gained)

fd_runs1 <- data1 %>%
  filter(down==1 & ydstogo==10 & yardline_100<=80 & yardline_100>=20 & 
           qb_dropback==0 & play_type == "run" & wp>= .20 & wp<= .80 & 
           half_seconds_remaining>120 & !is.na(epa)) %>%
  select(posteam, season, week, play_id, down, ydstogo, epa, wpa, success, yards_gained)

### Data for QB drop backs on 1st and 10, grouped by season
fd_dropbacks_by_year <- fd_dropbacks1 %>%
  group_by(posteam, season)%>% 
  summarize(
    epa_fd_dropback = mean(epa),
    wpa_fd_dropback = mean(wpa),
    success_rate_fd_dropback = mean(success),
    yards_per_fd_dropback_play = mean(yards_gained), 
    fd_dropback_plays = n()
  ) %>%
  ungroup()
fd_dropbacks_by_year

### Data for runs on 1st and 10, grouped by season
fd_runs_by_year <- fd_runs1 %>%
  group_by(posteam, season)%>% 
  summarize(
    epa_fd_run = mean(epa),
    wpa_fd_run = mean(wpa),
    success_rate_fd_run = mean(success),
    yards_per_fd_run_play = mean(yards_gained),
    fd_run_plays = n()
  ) %>%
  ungroup()
fd_runs_by_year

### Data for both QB dropbacks and run plays on 1st and 10, grouped by season
data3 <- fd_runs_by_year%>%
  left_join(fd_dropbacks_by_year , by = c('posteam' = 'posteam','season' = 'season')) %>%
  mutate(tot_fd_plays = fd_run_plays + fd_dropback_plays,
         prop_fd_run_plays = fd_run_plays/tot_fd_plays,
         prop_fd_QBD_plays = fd_dropback_plays/tot_fd_plays)




ui <- fluidPage(
  
  # App title ----
  titlePanel("NFL 1st Down Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      conditionalPanel(
        'input.dataset === "data3"',
        checkboxGroupInput("show_vars", "Selected columns from data:",
                           names(data3), selected = names(data3))
      ),
      width=2
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("data3", DT::dataTableOutput("mytable1"))
      )
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
data4 = data3[sample(nrow(data3), 300),]
output$mytable1 <- DT::renderDataTable({
  DT::datatable(data4[,input$show_vars, drop = FALSE])
})
  
}

shinyApp(ui = ui, server = server)

# Libraries used

library(ggplot2)
library(stringr)
library(dplyr)
library(rvest)
library(scales)
library(ggradar)

# URLs from teams that played the USA during the FIFA Woman's World Cup

# USA FIFA Woman's World Cup 2023 Matches
wurl1 <- "https://fbref.com/en/matches/8c6ffabe/United-States-Vietnam-July-22-2023-FIFA-Womens-World-Cup"
wurl2 <- "https://fbref.com/en/matches/128ee0d1/Portugal-United-States-August-1-2023-FIFA-Womens-World-Cup"
wurl3 <- "https://fbref.com/en/matches/420048e7/United-States-Netherlands-July-27-2023-FIFA-Womens-World-Cup"
wurl4 <- "https://fbref.com/en/matches/4e49aebf/Sweden-United-States-August-6-2023-FIFA-Womens-World-Cup"

# Vietnam FIFA Woman's World Cup 2023 Matches
wurl5 <- "https://fbref.com/en/matches/c8a51af1/Portugal-Vietnam-July-27-2023-FIFA-Womens-World-Cup"
wurl6 <- "https://fbref.com/en/matches/0b30d47a/Vietnam-Netherlands-August-1-2023-FIFA-Womens-World-Cup"

# Portugal FIFA Woman's World Cup 2023 Matches
wurl7 <- "https://fbref.com/en/matches/fb1330c0/Netherlands-Portugal-July-23-2023-FIFA-Womens-World-Cup"

# Netherlands FIFA Woman's World Cup 2023 Matches
wurl8 <- "https://fbref.com/en/matches/b476327d/Netherlands-South-Africa-August-6-2023-FIFA-Womens-World-Cup"
wurl9 <- "https://fbref.com/en/matches/f90d5fcc/Spain-Netherlands-August-11-2023-FIFA-Womens-World-Cup"

# Sweden FIFA Woman's World Cup 2023 Matches
wurl10 <- "https://fbref.com/en/matches/50e9fc5d/Sweden-South-Africa-July-23-2023-FIFA-Womens-World-Cup"
wurl11 <- "https://fbref.com/en/matches/68c2de9d/Sweden-Italy-July-29-2023-FIFA-Womens-World-Cup"
wurl12 <- "https://fbref.com/en/matches/33643599/Argentina-Sweden-August-2-2023-FIFA-Womens-World-Cup"
wurl13 <- "https://fbref.com/en/matches/7afa7945/Japan-Sweden-August-11-2023-FIFA-Womens-World-Cup"
wurl14 <- "https://fbref.com/en/matches/8063eb54/Spain-Sweden-August-15-2023-FIFA-Womens-World-Cup"
wurl15 <- "https://fbref.com/en/matches/4c7493fc/Sweden-Australia-August-19-2023-FIFA-Womens-World-Cup"

# Read in tables from the linked urls

selected_urls <- rbind(wurl1, wurl2, wurl3, wurl4, wurl5, wurl6, wurl7, wurl8, 
                       wurl9, wurl10, wurl11, wurl12, wurl13, wurl14, wurl15)

full_stat <- NULL

for (g in 1:length(selected_urls)) {
  game_data <- substr(selected_urls[g], 39, nchar(selected_urls[g]) - 22)
  game_data <- str_replace(game_data, "United-States", "United States")
  game_data <- str_replace(game_data, "South-Africa", "South Africa")
  parts <- strsplit(game_data, "-")[[1]]
  teamA <- parts[1]
  teamB <- parts[2]
  date <- paste(parts[3:5], collapse = "-")
  
  statA = read_html(selected_urls[g],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[4]] %>% html_table(fill=TRUE)
  statA <- head(statA, -1)
  colnames(statA) <- as.character(unlist(statA[1, ]))
  statA = statA[-1, ]
  statA = cbind(date, Team = teamA, Opponent = teamB, statA)
  statB = read_html(selected_urls[g],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[11]] %>% html_table(fill=TRUE)
  statB <- head(statB, -1)
  colnames(statB) <- as.character(unlist(statB[1, ]))
  statB = statB[-1, ]
  statB = cbind(date, Team = teamB, Opponent = teamA, statB)
  stat_both <- rbind(statA, statB)
  
  full_stat <- rbind(full_stat, stat_both)
  Sys.sleep(1)
}

all_stat_full <- unique(full_stat)
write.csv(all_stat_full, "FifaWomen2023.csv")

# summary data frame

summaryDF <- read.csv("FifaWomen2023.csv")
summaryDF$Pos <-NULL
summaryDF$Age <- NULL
summaryDF$X <- NULL
summaryDF$X. <- NULL

summaryDF <- summaryDF %>%
  group_by(Player) %>%
  summarise_if(is.numeric, function(x) sum(as.numeric(x), na.rm = TRUE))

# Select Players - Focusing on comparing Sophia Smith to other women who play 
# her position she was marked as a LW and FW so I selected a few around her age 
# and skill level/ playing time to fairly compare her to

selected_LW_players <- subset(summaryDF, Player == "Sophia Smith" | 
                              Player == "Esmee Brugts" | 
                              Player == "Fridolina Rolfö" | 
                              Player == "Hildah Magaia" | 
                              Player == "Mariona Caldentey" | 
                              Player == "Lina Hurtig" | 
                              Player == "Olivia Schough" | 
                              Player == "Esther González")

selected_FW_players <- subset(summaryDF, Player == "Sophia Smith" | 
                                Player == "Stina Blackstenius" | 
                                Player == "Lieke Martens" | 
                                Player == "Alex Morgan" | 
                                Player == "Lineth Beerensteyn" | 
                                Player == "Rebecka Blomqvist" | 
                                Player == "Salma Paralluelo" | 
                                Player == "Sam Kerr")

# Create Radar Plots

LW_subset <- selected_LW_players %>%
  select(Player, xG, Cmp, Sh, SoT, Touches, Succ)

FW_subset <- selected_FW_players %>%
  select(Player, xG, Cmp, Sh, SoT, Touches, Succ)

create_radar <- function(subset, indices, name) {
  result <- subset %>%
    mutate(across(-Player, ~ rescale(.))) %>%
    slice(indices) %>%
    select(1:7)
  
  assign(paste0(name, "_radar"), result, envir = .GlobalEnv)
}

create_radar(LW_subset, c(6, 8), "LW_MCSS")
create_radar(LW_subset, c(1, 8), "LW_EBSS")
create_radar(LW_subset, c(3, 8), "LW_FRSS")
create_radar(FW_subset, c(1, 7), "FW_AMSS")
create_radar(FW_subset, c(2, 7), "FW_LMSS")
create_radar(FW_subset, c(3, 7), "FW_LBSS")


# Fix Aesthetics of Graphs

create_radar_plot <- function(data, font, group_colors, title, filename) {
  plot <- data %>%
    ggradar(
      font.radar = font,
      grid.label.size = 4,
      axis.label.size = 3, 
      group.point.size = 2,  
      grid.line.width = 0.5,
      group.colours = group_colors,
      gridline.mid.colour = "#8b8c8c",
      group.line.width = 1
    ) +
    theme(
      legend.position = c(1, 0),  
      legend.justification = c(1, 0),
      legend.text = element_text(size = 8, family = font),
      legend.key = element_rect(fill = NA, color = NA),
      legend.background = element_blank()
    ) +
    labs(
      title = title
    ) + 
    theme(
      plot.title.position = "panel", 
      plot.title = element_text(
        family = font, 
        size = 12, 
        hjust = 0.5
      )
    )
  
  ggsave(
    filename = filename,
    plot = plot,
    width = 5.7,
    height = 5,
    device = "png"
  )
}

create_radar_plot(FW_AMSS_radar, "sans", c("#BB2533", "#1F2742"), "Radar Plot Comparing Alex Morgan and Sophia Smith Statistics", "AlexMorgan_SophiaSmith.png")
create_radar_plot(FW_LMSS_radar, "sans", c("#F36C21", "#1F2742"), "Radar Plot Comparing Lieke Martens and Sophia Smith Statistics", "LiekeMartens_SophiaSmith.png")
create_radar_plot(FW_LBSS_radar, "sans", c("#F36C21", "#1F2742"), "Radar Plot Comparing Lineth Beerensteyn and Sophia Smith Statistics", "LinethBeerensteyn_SophiaSmith.png")

create_radar_plot(LW_MCSS_radar, "sans", c("#FCB507", "#1F2742"), "Radar Plot Comparing Mariona Caldentey and Sophia Smith Statistics", "MarionaCaldentey_SophiaSmith.png")
create_radar_plot(LW_EBSS_radar, "sans", c("#F36C21", "#1F2742"), "Radar Plot Comparing Esmee Brugts and Sophia Smith Statistics", "EsmeeBrugts_SophiaSmith.png")
create_radar_plot(LW_FRSS_radar, "sans", c("#FFEC00", "#1F2742"), "Radar Plot Comparing Fridolina Rolfö and Sophia Smith Statistics", "FridolinaRolfö_SophiaSmith.png")

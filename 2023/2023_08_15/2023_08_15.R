# Load data and packages --------------------------------------------------

spam <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv')

library(tidyverse)

head(spam)

# Data processing ---------------------------------------------------------

spam <- spam |> 
  janitor::clean_names()

spam_1 <- spam |> 
  mutate(yesno = factor(yesno, levels = c("y", "n"), labels = c("yes", "no")),
         dollar = dollar * 100,
         bang = bang * 100,
         money = money * 100,
         n000 = n000 * 100,
         make = make * 100)

spam_plot <- spam_1 |>
  group_by(yesno) |> 
  summarize(dollar = mean(dollar),
            bang = mean(bang),
            money = mean(money),
            n000 = mean(n000),
            make = mean(make)) |> 
  pivot_longer(
    cols = dollar:make,
    names_to = "Spam_signs",
    values_to = "value"
  ) |> 
  pivot_wider(
    names_from = yesno,
    values_from = value
  ) |> 
  ungroup() |> 
  arrange(desc(yes))

spam_plot$Spam_signs[1] <- "!\nsymbol"
spam_plot$Spam_signs[2] <- "000\nstring"
spam_plot$Spam_signs[3] <- "Money\nword"
spam_plot$Spam_signs[4] <- "Dollar\nsign"
spam_plot$Spam_signs[5] <- "Make\nword"

# Plot --------------------------------------------------------------------

# Save to png
jpeg(file = "~/Documents/GitHub/TidyTuesday/2023/2023_08_15/plot.jpeg",
    width = 1800,
    height = 1200,
    res = 300)

# Background set
par(bg = rgb(50, 50, 50, maxColorValue = 255),
    mar = c(6, 6, 4, 3))

plot(1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "", bty = "n",
     type = "n", ylim = c(0, 60), xlim = c(.25, 6))

# Gridlines
abline(h = seq(0, 60, 10),
       lty = 3,
       col = gray(0.95), lwd = 1)

# y-axis labels
mtext(text = seq(10, 60, 10),
      side = 2, at = seq(10, 60, 10),
      las = 1, line = 1, col = gray(0.95))

# Spam sign labels
mtext(text = spam_plot$Spam_signs,
      side = 1, at = 1:5, las = 1,
      line = 1, col = gray(0.95))

# Yes bars
rect(xleft = 1:5 - .35 - .04 / 2,
     ybottom = rep(0, 5),
     xright = 1:5 - .04 / 2,
     ytop = spam_plot$yes,
     col = "lightgreen", border = NA)

# No bars
rect(xleft = 1:5 + .04 / 2,
     ybottom = rep(0,5),
     xright = 1:5 + .35 + .04 / 2,
     ytop = spam_plot$no,
     col = "#FF7470", border = NA)

legend(5, 60, c("Yes", "No"), col = c("lightgreen", "#FF7470"),
       pch = rep(15, 2), bty = "n", pt.cex = 1.5, text.col = "white")

# Text
mtext("Percentage of different symbols, words\nand string at spam emails",
      side = 3, cex = 1.5, col = "white")

mtext("Percentage", side = 2, col = "white", line = 3.5)

mtext("TidyTuesday 2023-08-15", side = 1, at = 0, adj = 0, line = 3, font = 3,
      col = "white")

dev.off()

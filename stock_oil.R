library(ggplot2)
library(magrittr)
library(gganimate)
library(reshape2)

stock_oil <- read.csv('OSL_OIL.csv')
  
stock_oil$Date <- as.Date(as.character(stock_oil$Date), format = "%m/%d/%y")

stock_oil$Brent.Crude.Oil <- round(stock_oil$Brent.Crude.Oil, 1)
stock_oil$Crude.Oil.WTI <- round(stock_oil$Crude.Oil.WTI, 1)
stock_oil$OSEAX <- round(stock_oil$OSEAX)

stock_oil_mlt <- melt(stock_oil, id.vars = c('Date'))
stock_oil_mlt$variable2 <- as.character(stock_oil_mlt$variable)
stock_oil_mlt$variable2[stock_oil_mlt$variable %in% c("Brent.Crude.Oil", "Crude.Oil.WTI")] <- "Brent/Crude"
stock_oil_mlt$variable2 <- as.factor(stock_oil_mlt$variable2)

lst_color <- c("#009E73" ,"#D55E00", "#0072B2")

oilPlot <- ggplot(data = stock_oil_mlt, aes(x = Date, y = value, color = variable, group = variable )) +
  geom_line()+
  geom_segment(aes(xend = max(Date), yend = value), linetype = 2, colour = 'grey') +
  geom_point(size = 1) +
  geom_text(aes(x = max(Date), label = value), hjust = 0) + 
  scale_x_date(date_breaks = "years", date_labels = "%Y", limits = c(min(stock_oil_mlt$Date), max(stock_oil_mlt$Date) + 120)) +
  transition_reveal(Date) +
  labs(title = 'Date {frame_along} ', subtitle = "From May 2015 to April 2020", 
       colour = 'Index', y = 'Points                                     Price (USD)') +
  scale_colour_manual(values = lst_color) + 
  theme_bw()+
  facet_grid(rows = vars(variable2), scales = "free_y")

animate(oilPlot, renderer = gifski_renderer(), width = 550, 
        height = 550, nframes = 250, duration = 20)
anim_save("OIL_OSEAX.gif")

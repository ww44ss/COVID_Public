

library(xkcd)
library(ggplot2)
library(tidyverse)
library(extrafont)
library(extrafontdb)




download.file("http://simonsoftware.se/other/xkcd.ttf",
              dest="xkcd.ttf", mode="wb")

system("mkdir ~/.fonts")

system("cp xkcd.ttf ~/.fonts")

font_import(pattern = "[X/x]kcd", prompt=FALSE)

fonts()

fonttable()

if(.Platform$OS.type != "unix") {
   ## Register fonts for Windows bitmap output
    loadfonts(device="win")
  } else {
    loadfonts()
    }




fontb <- tibble(log2payload = 4:20)




bw1 = 2^4
latency1 = 2^4

bw2 = 2^8
latency2  = 2^8

b <- b %>%
  mutate(payload = 2^log2payload) %>%
  mutate(t1 = payload/bw1 + latency1) %>%
  mutate(t2 = payload/bw2 + latency2) %>%
  mutate(log2t1 = log2(t1), log2t2 = log2(t2))


xrange <- range(b$log2payload)
yrange <- range(b$log2t1)


ggplot(data = b, aes(x = log2payload, y = log2t1)) + geom_line(color = "red") + 
  geom_line(aes(y = log2t2), color = "blue") + 
  labs(title = "Transmission Time", subtitle = "latency / bandwidth crosssover", x = "log(total data size)", y = "log(time)") + 
  theme_xkcd() + 
  xkcdaxis(xrange, yrange) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())







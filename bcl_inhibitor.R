library(tidyverse)
library(magrittr)
library(readxl)
library(ggplot2)
library(ggbeeswarm)

#read in data
df <- read_excel("BM1244_mouse_tissue_tidydata.xlsx", skip=2, sheet=7)

#clean up data types and names
df$Rx <- as.factor(df$Rx)
df$Route <- as.factor(df$Route)
df$`Timepoint (hr)` <- as.factor(df$`Timepoint (hr)`)
df$Organ <- as.factor(df$Organ)
names(df) <- c("organ", "rx", "route", "hr", "conc")
df<-  slice(df, 1:216)
df$logconc<- log(df$conc)
df$logconc[df$logconc == -Inf]<-0
#reorder levels of organ
df$organ <- factor(df$organ, levels(df$organ)[c(6,3,1,2,4,5)])


#exploratory data analysis
# how does IP compare to PO?
ggplot(data= df, aes(x=route, y=logconc)) +
  geom_boxplot() + geom_quasirandom(varwidth = T, cex=1.5) +   
  ggtitle("BM1244 by Route") +
  labs(x="Route") + labs(y="BM1244 natural log of ng/mL") +
  theme(plot.title = element_text(family = "Arial", color = "black", face="bold", size=20, hjust=0.5)) +
  theme(axis.title = element_text(family = "Arial", color = "black", face="bold", size=16)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=15, color="red", alpha= 0.5)

#what is effect by organ, IP only in healthy mice?
# how does IP ?
df %>% 
  filter(route=="IP") %>% 
  filter(rx=="HBSS") %>% 
  ggplot(aes(x=route, y=logconc, color=hr)) + facet_grid(. ~ organ) +
  geom_quasirandom(dodge.width = 1) +   ggtitle("BM1244 by IP Route in healthy mice, horizontal line is effective dose in vitro") +
  labs(x="Route") + labs(y="BM1244 natural log of ng/mL") + guides(color=guide_legend(title="hours after administration")) +
  geom_hline(yintercept = log(637)) +theme(legend.position = "top")

#what is effect by organ, IP only in diseased mice?
# how does IP ?
df %>% 
  filter(route=="IP") %>% 
  filter(rx=="S. typhi") %>% 
  ggplot(aes(x=route, y=logconc, color=hr)) + facet_grid(. ~ organ) +
  geom_quasirandom(dodge.width = 1) +   ggtitle("BM1244 by IP Route in diseased mice, horizontal line is effective dose in vitro") +
  labs(x="Route") + labs(y="BM1244 natural log of ng/mL") + guides(color=guide_legend(title="hours after administration")) +
  geom_hline(yintercept = log(637)) +theme(legend.position = "top")

#what is effect by organ, IP only in both healthy and diseased mice?
# how does IP ?
df %>% 
  filter(route=="IP") %>% 
  ggplot(aes(x=route, y=logconc, color=hr)) + facet_grid(rx ~ organ) +
  geom_quasirandom(dodge.width = 1) +   ggtitle("BM1244 by IP Route in all mice, horizontal line is effective dose in vitro") +
  labs(x="Route") + labs(y="BM1244 natural log of ng/mL") + guides(color=guide_legend(title="hours after administration")) +
  geom_hline(yintercept = log(637)) +theme(legend.position = "top")

#what is effect by organ, PO only in healthy mice?
# how does IP ?
df %>% 
  filter(route=="PO") %>% 
  filter(rx=="HBSS") %>% 
  ggplot(aes(x=route, y=logconc, color=hr)) + facet_grid(. ~ organ) +
  geom_quasirandom(dodge.width = 1) +   ggtitle("BM1244 by PO Route in healthy mice, horizontal line is effective dose in vitro") +
  labs(x="Route") + labs(y="BM1244 natural log of ng/mL") + guides(color=guide_legend(title="hours after administration")) +
  geom_hline(yintercept = log(637)) +theme(legend.position = "top")

#what is effect by organ, PO only in diseased mice?
# how does IP ?
df %>% 
  filter(route=="PO") %>% 
  filter(rx=="S. typhi") %>% 
  ggplot(aes(x=route, y=logconc, color=hr)) + facet_grid(. ~ organ) +
  geom_quasirandom(dodge.width = 1) +   ggtitle("BM1244 by PO Route in diseased mice, horizontal line is effective dose in vitro") +
  labs(x="Route") + labs(y="BM1244 natural log of ng/mL") + guides(color=guide_legend(title="hours after administration")) +
  geom_hline(yintercept = log(637)) +theme(legend.position = "top")

#what is effect by organ, PO only in both healthy and diseased mice?
# how does IP ?
df %>% 
  filter(route=="PO") %>% 
  ggplot(aes(x=route, y=logconc, color=hr)) + facet_grid(rx ~ organ) +
  geom_quasirandom(dodge.width = 1) +   ggtitle("BM1244 by PO Route in all mice, horizontal line is effective dose in vitro") +
  labs(x="Route") + labs(y="BM1244 natural log of ng/mL") + guides(color=guide_legend(title="hours after administration")) +
  geom_hline(yintercept = log(637)) +theme(legend.position = "top")

#what is effect by organ, IP only?
# how does IP ?
df %>% 
  filter(route=="IP") %>% 
ggplot(aes(x=route, y=logconc, color=hr)) + facet_grid(rx ~ organ) +
  geom_quasirandom(dodge.width = 1) +   ggtitle("BM1244 by IP Route, horizontal line is effective dose in vitro") +
  labs(x="Route") + labs(y="BM1244 natural log of ng/mL") + guides(color=guide_legend(title="hours after administration")) +
  geom_hline(yintercept = log(637)) +theme(legend.position = "top")

#what is effect by organ, PO only?
# how does PO ?
df %>% 
  filter(route=="PO") %>% 
  ggplot(aes(x=route, y=logconc, color=hr)) + facet_grid(rx ~ organ) +
  geom_quasirandom(dodge.width = 1) +   ggtitle("BM1244 by PO Route, horizontal line is effective dose in vitro") +
  labs(x="Route") + labs(y="BM1244 natural log of ng/mL") + guides(color=guide_legend(title="hours after administration")) +
  geom_hline(yintercept = log(637)) +theme(legend.position = "top")


#what is effect by organ, IP vs. PO?
# how does IP compare to PO?
g<- ggplot(data= df, aes(x=route, y=logconc, color=hr)) + facet_grid(rx ~ organ) +
   geom_quasirandom(dodge.width = 1) +   ggtitle("APG1244 by Route, horizontal line is effective dose in vitro") +
  labs(x="Route") + labs(y="APG1244 natural log of ng/mL") + guides(color=guide_legend(title="hours after administration")) +
  geom_hline(yintercept = log(637)) +theme(legend.position = "top")

g
ggsave("grid.png")

library(officer)
my_pres <- read_pptx()
layout_summary(my_pres)
my_pres <-my_pres %>% add_slide(layout = "Blank", master = "Office Theme")
my_pres <-my_pres %>%
  ph_with_img_at(src="grid.png", left=0.1, top=0.1,
                 width = 4, height=4.6)

print(my_pres, target="my_pres.pptx") %>% invisible()

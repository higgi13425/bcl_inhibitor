library(tidyverse)
library(magrittr)
library(readxl)
library(ggplot2)
library(ggbeeswarm)

#read in data
stf <- read_excel("ESTF-131.xls", sheet=4)

stf$gene[stf$gene=="col1a1"]<- "Collagen COL1A1"
stf$gene[stf$gene=="fn1"]<- "Fibronectin FN1"
stf$gene[stf$gene=="ACTA2"]<- "Alpha Smooth Muscle Actin ACTA2"
stf$gene[stf$gene=="MYLK"]<- "Myosin Light Chain Kinase MYLK"
stf$rx <- as.factor(stf$rx)
stf$gene <- as.factor(stf$gene)
stf$micromolar<- as.factor(stf$micromolar)



#exploratory data analysis

#what is effect by gene, by dose for BM1197?
stf %>%
  filter(rx=="BM1197") %>%
  ggplot(aes(x=micromolar, y=dct, color=rx)) +
  facet_grid(rx ~ gene) +
  geom_quasirandom(dodge.width = 1, color=2) +  
  ggtitle("BM1197 in vitro vs. Stiffness Induction of Fibrogenic Genes") +
  labs(x="Dose (micromolar)") + labs(y="dCt") +
  guides(color=guide_legend(title="Drug")) +
  theme(legend.position = "top")


#what is effect by gene, by dose for BM1244?
stf %>%
  filter(rx=="BM1244") %>%
  ggplot(aes(x=micromolar, y=dct, color=rx)) +
  facet_grid(rx ~ gene) +
  geom_quasirandom(dodge.width = 1, color=3) +  
  ggtitle("BM1244 in vitro vs. Stiffness Induction of Fibrogenic Genes") +
  labs(x="Dose (micromolar)") + labs(y="dCt") +
  guides(color=guide_legend(title="Drug")) +
  theme(legend.position = "top")

#what is effect by gene, by dose for BM1252?
stf %>%
  filter(rx=="BM1252") %>%
  ggplot(aes(x=micromolar, y=dct, color=rx)) +
  facet_grid(rx ~ gene) +
  geom_quasirandom(dodge.width = 1, color=4) +  
  ggtitle("BM1252 in vitro vs. Stiffness Induction of Fibrogenic Genes") +
  labs(x="Dose (micromolar)") + labs(y="dCt") +
  guides(color=guide_legend(title="Drug")) +
  theme(legend.position = "top")

#what is effect by gene, by drug, by dose - All drugs?
ggplot(data= stf, aes(x=micromolar, y=dct, color=rx)) +
  facet_grid(rx ~ gene) +
  geom_quasirandom(dodge.width = 1) +  
  ggtitle("Bcl inhibitors in vitro vs. Stiffness Induction of Fibrogenic Genes") +
  labs(x="Dose (micromolar)") + labs(y="dCt") +
  guides(color=guide_legend(title="Drug")) +
  theme(legend.position = "top")

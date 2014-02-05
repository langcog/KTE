#setwd("~/Dropbox/Stanford/2012-2013 Q2 Winter/Psych 254 Lab/Ong/Analysis")
rm(list=ls())
source("mcf.useful.R")

#### --- Preprocessing --- ####
### full analysis
s1a <- read.csv("study1a.csv") #direct replication 1
s1b <- read.csv("study1b.csv") #direct replication 2
s1c <- read.csv("study1c.csv") #direct replication 3 (in lab)

s2a <- read.csv("study2a.csv") # 2AFC online
s2aCR <- read.csv("study2aCR.csv") # 2AFC online Correct Rejections
s2b <- read.csv("study2b.csv") # 2AFC lab
s2bCR <- read.csv("study2bCR.csv") # 2AFC lab CR

s3 <- read.csv("study3.csv") # answer on absent instead of present

s4 <- read.csv("study4.csv") # Occluder
s5 <- read.csv("study5.csv") # Standard, with no attention check
s6 <- read.csv("study6.csv") # Standard; attention check at agent re-enter (19s for all movies)
s7 <- read.csv("study7.csv") # No agent; attention check on lightbulb instead of agent.
s8 <- read.csv("study8.csv") # Standard; attention check on lightbulb at various timings


s1a$expt <- "1a:Replication1"
s1b$expt <- "1b:Replication2"
s1c$expt <- "1c:LabReplication"
s2a$expt <- "2a:2AFC"
s2aCR$expt <- "2a:2AFC,CR"
s2b$expt <- "2b:Lab2AFC"
s2bCR$expt <- "2b:Lab2AFC,CR"
s3$expt <- "3:Absent"

s4$expt <- "4:Occluder"
s5$expt <- "5:NoAttn"
s6$expt <- "6:Attn19s"
s7$expt <- "7:NoAgent,LB"
s8$expt <- "8:LBtimes"

s1c$workerid = factor(s1c$workerid)
s2b$workerid = factor(s2b$workerid)
s2bCR$workerid = factor(s2bCR$workerid)

d <- rbind(s1a,s1b,s1c,s2a,s2aCR,s2b,s2bCR,s3,s4,s5,s6,s7)
# add "attentionCheckTimings"
d$attentionTime = (16.7*(d$PArray==1 & d$AArray==1) + 
                     13.2*(d$PArray==1 & d$AArray==0) + 
                     10.8*(d$PArray==0 & d$AArray==1) +
                     16.7*(d$PArray==0 & d$AArray==0))*(
                       d$expt!='attn19s' & d$expt!='noAgent' & d$expt!='noAttn') +
  (d$expt=='attn19s')*19
d <- rbind(d, s8)

d$expt <- factor(d$expt, levels=c("1a:Replication1", "1b:Replication2", "1c:LabReplication", 
                                  "2a:2AFC", "2a:2AFC,CR", "2b:Lab2AFC", "2b:Lab2AFC,CR",
                                  "3:Absent", "4:Occluder", 
                                  "5:NoAttn", "6:Attn19s", "7:NoAgent,LB", "8:LBtimes"))
d$participant <- factor(c("Absent","Present")[d$PArray+1])
d$agent <- factor(c("Absent","Present")[d$AArray+1])

length(unique(d$workerid))




#### --- t-test for replications --- ####

# see: http://easycalculation.com/statistics/effect-size-t-test.php
# r = sqrt( ( t2 ) / ( ( t2 ) + ( df * 1) ) )
# d = ( t*2 ) / ( sqrt(df) )
library(lsr)
# Simple t-tests to replicate KTE:
# P-A- - P+A+
# P-A- - P+A-
# P-A- - P-A+
# P-A+ - P+A-

# P-A- - P+A+
comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Present")
comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Present")
comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Present")

# P-A- - P+A-
comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Absent")
comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Absent")
comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Absent")

# P-A- - P-A+
comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Present")
comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Present")
comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Present")

# P-A+ - P+A-
comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Absent")
comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Absent")
comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Absent")

c1aggregate <- aggregate(reactionTime ~ workerid + participant + agent, comparison1, mean)
c1aggregate = c1aggregate[order(c1aggregate$workerid),]
c2aggregate <- aggregate(reactionTime ~ workerid + participant + agent, comparison2, mean)
c2aggregate = c2aggregate[order(c2aggregate$workerid),]
c1aggregate$workerid == c2aggregate$workerid
t.test(c1aggregate$reactionTime, c2aggregate$reactionTime, paired=TRUE)

manualDiff <- c1aggregate$reactionTime - c2aggregate$reactionTime
stanDev <- sd(manualDiff)
tStat <- mean(manualDiff)/sd(manualDiff) * sqrt(length(manualDiff))
cohenD <- mean(manualDiff)/sd(manualDiff); cohenD

# Calculating KTE's Cohen's d's
#tValue <- 3.47
#cohenD <- (tValue / sqrt(23))
#cohenD

#########
# --- Crossover interaction section comparisons --- ####
# Additional comparisons: P+A+>P+A- and P+A+>P-A+
  comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Absent")
  comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Absent")
  comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Absent")

  comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Present")
  comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Present")
  comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Present")

# for Studies 2a-b, 3 : P-A+ - P-A-
  comparison1 = subset(d, d$expt=="2a:2AFC,CR" & d$participant=="Absent" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="2a:2AFC,CR" & d$participant=="Absent" & d$agent=="Absent")
  comparison1 = subset(d, d$expt=="2b:Lab2AFC,CR" & d$participant=="Absent" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="2b:Lab2AFC,CR" & d$participant=="Absent" & d$agent=="Absent")
  comparison1 = subset(d, d$expt=="3:Absent" & d$participant=="Absent" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="3:Absent" & d$participant=="Absent" & d$agent=="Absent")


## for Studies 2a-b, 3 : P-A- - P+A-
#comparison1 = subset(d, d$expt=="2a:2AFC,CR" & d$participant=="Absent" & d$agent=="Absent")
#comparison2 = subset(d, d$expt=="2a:2AFC,CR" & d$participant=="Present" & d$agent=="Absent")
#comparison1 = subset(d, d$expt=="2b:Lab2AFC,CR" & d$participant=="Absent" & d$agent=="Absent")
#comparison2 = subset(d, d$expt=="2b:Lab2AFC,CR" & d$participant=="Present" & d$agent=="Absent")
#comparison1 = subset(d, d$expt=="3:Absent" & d$participant=="Absent" & d$agent=="Absent")
#comparison2 = subset(d, d$expt=="3:Absent" & d$participant=="Present" & d$agent=="Absent")

# lmer for Study 1a
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="1a:Replication1")))
2*(1-pnorm(4.304)) # print p-value.
# lmer for Study 1b
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="1b:Replication2")))
2*(1-pnorm(4.259)) # print p-value.
# lmer for Study 1c
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="1c:LabReplication")))
2*(1-pnorm(2.478)) # print p-value.
## lmer for Study 2a: 2AFC
#summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="2a:2AFC")))
#2*(1-pnorm(4.453)) # print p-value.
# lmer for Study 2a: 2AFC CR
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="2a:2AFC,CR")))
2*(1-pnorm(3.848)) # print p-value.
# lmer for Study 2b: Lab 2AFC
#summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="2b:Lab2AFC")))
#2*(1-pnorm(4.281)) # print p-value.
# lmer for Study 2b: Lab 2AFC CR
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="2b:Lab2AFC,CR")))
2*(1-pnorm(4.847)) # print p-value.
# lmer for Study 3: Absent
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="3:Absent")))
2*(1-pnorm(6.0653)) # print p-value.
# lmer for Study 4: Occluder
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="4:Occluder")))
2*(1-pnorm(3.473)) # print p-value.

# --- End Crossover interaction section comparisons ---
#########

### BEGIN
#d$first.half <- d$trialNum < 20 
mss <- aggregate(reactionTime ~ workerid + participant + agent + expt , d,mean)
ms <- aggregate(reactionTime ~  participant + agent + expt , mss,mean)
ms$ci.h <- aggregate(reactionTime ~  participant + agent + expt , mss, ci.high)$reactionTime
ms$ci.l <- aggregate(reactionTime ~  participant + agent + expt , mss, ci.low)$reactionTime
ms$n <- aggregate(workerid ~  participant + agent + expt , mss, n.unique)$workerid

KTE = data.frame(participant = factor(c("Absent", "Present", "Absent", "Present")),
                 agent = factor(c("Absent", "Absent", "Present", "Present")),
                 expt = factor(c("KTE", "KTE", "KTE", "KTE")),
                 #attentionTime = c(16.7, 13.2, 10.8, 16.7),
                 reactionTime = c(360, 320, 328, 313),
                 ci.h = c(14, 10, 12, 10)*1.96,
                 ci.l = c(14, 10, 12, 10)*1.96,
                 n = rep(24,4))
ms = rbind(KTE, ms) # to put KTE as the first factor


# ------------------ Plot Fig2: KTE + Conceptual Figure ####

conceptualFigDF = data.frame(panel = factor(c(rep("KTE",4), 
                                              rep("Ball Present",4),
                                              rep("Ball Absent",4), 
                                              rep("Occluder",4)),
                                            levels = c("KTE", "Ball Present", "Ball Absent", "Occluder"),
                                            labels = c("KTE", "Ball Present", "Ball Absent", "Occluder")),
                             participant = factor(rep(c("Absent","Present"),2*4)),
                             agent = factor(rep(c("Absent", "Absent", "Present", "Present"),4)),
                             reactionTime = c( c(360,320,328,313), #KTE
                                               c(350,340,320,310), #Ball Present prediction
                                               c(310,320,340,350), #Ball Absent prediction
                                               c(350,340,346,336)), #Occluder prediction
                             ci.h = c(c(14, 10, 12, 10)*1.96, rep(NA,12)),
                             ci.l = c(c(14, 10, 12, 10)*1.96, rep(NA,12)),
                             illustrationData = factor(c(rep(0,4),rep(1,12)))
                             )

blackGreyPalette <- c("#000000", "#999999")  

ggplot(conceptualFigDF, aes(x=participant, y=reactionTime, colour=agent, group=agent, 
                            shape=illustrationData)) + 
  ylab("Reaction Time (ms)") + xlab("Participant Belief") +
  guides(color=guide_legend(title="Agent Belief")) +
  geom_line(position=position_dodge(width=.1),stat="identity") +
  geom_point(aes(size=illustrationData), position=position_dodge(width=.1)) +
  scale_colour_manual(values=blackGreyPalette) +
  scale_size_discrete(guide=FALSE) +
  scale_shape_manual(values=c(20,1), guide=FALSE) +
  geom_linerange(aes(ymin=reactionTime - ci.l, ymax=reactionTime + ci.h),
                 position=position_dodge(width=.1)) +
  facet_wrap( ~ panel, ncol=4, as.table=TRUE, drop=FALSE) +
  theme(strip.background = element_rect(fill="#FFFFFF"))
# Saved as a pdf. 7 x 3

# did not add error bars on the KTE because error bars on the other panels don't make sense
  

# ------------------ Plot Fig3: Studies 1-4 -- 9 panels in 2 rows ####
mss <- aggregate(reactionTime ~ workerid + participant + agent + expt , d,mean)
ms <- aggregate(reactionTime ~  participant + agent + expt , mss,mean)
ms$ci.h <- aggregate(reactionTime ~  participant + agent + expt , mss, ci.high)$reactionTime
ms$ci.l <- aggregate(reactionTime ~  participant + agent + expt , mss, ci.low)$reactionTime
ms$n <- aggregate(workerid ~  participant + agent + expt , mss, n.unique)$workerid

# with 2 rows:
#1a, 1b, 1c, 2a Hit, 2b Hit
#2a CR, 2b CR, Absent, <BLANK>, 4:Occluder
msSub <- subset(ms,expt=="1a:Replication1"|expt=="1b:Replication2"|expt=="1c:LabReplication"|
                  expt=="2a:2AFC"|expt=="2a:2AFC,CR"|expt=="2b:Lab2AFC"|expt=="2b:Lab2AFC,CR" | expt=="3:Absent" |
                  expt=="4:Occluder")
msSub$expt <- factor(msSub$expt)
levels(msSub$expt) <- c("1a: Web 1","1b: Web 2","1c: Lab",
                        "2a: Web 2AFC", "2a: Web 2AFC, CR","2b: Lab 2AFC", "2b: Lab 2AFC, CR", "3:Absent", "4:Occluder")
msSub$expt <- factor(msSub$expt, levels = c("1a: Web 1","1b: Web 2","1c: Lab", "2a: Web 2AFC", "2b: Lab 2AFC", 
                                            "2a: Web 2AFC, CR","2b: Lab 2AFC, CR", "3:Absent", "blank", "4:Occluder"))

blackGreyPalette <- c("#000000", "#999999")  

ggplot(msSub, aes(x=participant, y=reactionTime, colour=agent, group=agent)) + 
  ylab("Reaction Time (ms)") + ylim(390,1100) +
  xlab("Participant Belief") +
  guides(color=guide_legend(title="Agent Belief")) +
  geom_line(position=position_dodge(width=.1),stat="identity") +
  scale_colour_manual(values=blackGreyPalette) +
  facet_wrap( ~ expt, ncol=5, as.table=TRUE, drop=FALSE) +
  geom_linerange(aes(ymin=reactionTime - ci.l, ymax=reactionTime + ci.h),
                 position=position_dodge(width=.1)) +
  theme(strip.background = element_rect(fill="#FFFFFF"))
# pdf 9 by 6
# square version: 9 by 4.
# have to edit out the blank.
# ------------------ End Plot Fig 3


# ------------------ Plot Fig4: Studies 5-8 only. Fig 5: Vs. Time ####
msSub2 <- subset(ms,expt=="5:NoAttn"|expt=="6:Attn19s"|expt=="7:NoAgent,LB"|expt=="8:LBtimes")
msSub2$expt <- factor(msSub2$expt)
levels(msSub2$expt) <- c("5: No Attention Check", "6: Check at 19s", "7: No Agent, Lightbulb", "8: Lightbulb timing")
msSub2$expt <- factor(msSub2$expt, c("5: No Attention Check", "6: Check at 19s", "7: No Agent, Lightbulb", "8: Lightbulb timing"))

## Keeping all of "attentionTime", "participant", "agent" as variables:
#mssAll <- aggregate(reactionTime ~ workerid + participant + agent + attentionTime, 
#                    subset(d, expt=="8a:LBtimes"|expt=="8b:LBtimes"),mean)
mssAll <- aggregate(reactionTime ~ workerid + participant + agent + attentionTime, subset(d, expt=="8:LBtimes"),mean)
msAll <- aggregate(reactionTime ~ participant + agent + attentionTime, mssAll,mean)
msAll$ci.h <- aggregate(reactionTime ~ participant + agent + attentionTime, mssAll, ci.high)$reactionTime
msAll$ci.l <- aggregate(reactionTime ~ participant + agent + attentionTime, mssAll, ci.low)$reactionTime
msAll$n <- aggregate(workerid ~ participant + agent + attentionTime, mssAll, n.unique)$workerid

msAll$condition = (msAll$participant=="Absent" & msAll$agent=="Absent")*1 +
  (msAll$participant=="Absent" & msAll$agent=="Present")*2 +
  (msAll$participant=="Present" & msAll$agent=="Absent")*3 +
  (msAll$participant=="Present" & msAll$agent=="Present")*4
msAll$condition = factor(msAll$condition, levels = c(1:4), labels = c("P-A-", "P-A+", "P+A-", "P+A+"))
msAll$attentionTimeFactor = factor(msAll$attentionTime)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

blackGreyPalette <- c("#000000", "#999999")

## Fig 4, with lines:
ggplot(msSub2, aes(x=participant, y=reactionTime, colour=agent, group=agent)) + 
  ylab("Reaction Time (ms)") + ylim(390,1100) +
  xlab("Participant Belief") +
  guides(color=guide_legend(title="Agent Belief")) +
  geom_line(position=position_dodge(width=.1),stat="identity") +
  scale_colour_manual(values=blackGreyPalette) +
  facet_wrap( ~ expt, nrow=1) +
  geom_linerange(aes(ymin=reactionTime - ci.l, ymax=reactionTime + ci.h), position=position_dodge(width=.1)) +
  theme(strip.background = element_rect(fill="#FFFFFF"))
# Save as 8 by 5 pdf
# square: 10 by 3

## Fig 5, MCF 12/12/13
ggplot(aes(x=attentionTime,y=reactionTime,
                 ymin=reactionTime-ci.l,ymax=reactionTime+ci.h,
                 colour=condition,group=condition),
             data=msAll) +
  geom_line() + 
  geom_pointrange(position=position_dodge(width=.2)) + 
  scale_colour_manual(values=cbPalette) + 
  guides(color=guide_legend(title="Video Condition")) +
  ggtitle("Study 8: Lightbulb timing") +
  ylab("Reaction Time (ms)") + 
  xlab("Attention Check Time (s)") + ylim(390,1100)
# Save as 5 by 5 pdf
# square: 7 by 5

# ------------------ End Plot Fig 4 and 5



#### ---- Stats for Studies 5-8 ---- ####

# lmer for Study 5: No Attention
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="5:NoAttn")))
2*(1-pnorm(0.616)) # print p-value.
# lmer for Study 6: Attention at 19s
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="6:Attn19s")))
2*(1-pnorm(0.528)) # print p-value.
# lmer for Study 7: No Agent, with lightbulb
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="7:NoAgent,LB")))
2*(1-pnorm(3.34)) # print p-value.
# lmer for Study 8: Lightbulb x Times
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="8:LBtimes")))
2*(1-pnorm(0.23)) # print p-value.

# for Study 8, comparing with attentioncheck
summary(lmer(reactionTime ~ participant*agent + attentionTime + (1 + participant*agent|workerid), mssAll))
2*(1-pnorm(4.528)) # print p-value.

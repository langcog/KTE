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
s5a <- read.csv("study5a.csv") # Standard, with no attention check
s5b <- read.csv("study5b.csv") # Standard, with and without attention check, counterbalanced
s6 <- read.csv("study6.csv") # Standard; attention check at agent re-enter (19s for all movies)
s7 <- read.csv("study7.csv") # No agent; attention check on lightbulb instead of agent.
s8a <- read.csv("study8a.csv") # Standard; attention check on lightbulb at various timings
s8b <- read.csv("study8b") # Standard; attention check on lightbulb at various timings

s1a$expt <- "1a:Replication1"
s1b$expt <- "1b:Replication2"
s1c$expt <- "1c:LabReplication"
s2a$expt <- "2a:2AFC"
s2aCR$expt <- "2a:2AFC,CR"
s2b$expt <- "2b:Lab2AFC"
s2bCR$expt <- "2b:Lab2AFC,CR"
s3$expt <- "3:Absent"

s4$expt <- "4:Occluder"
s5a$expt <- "5a:NoAttn"
# splitting up study 5b into two blocks
s5b_noAttn = subset(s5b, (attentionCheckFirst==0 & trialNum<25) | (attentionCheckFirst==1 & trialNum>24))
s5b_attn = subset(s5b, (attentionCheckFirst==1 & trialNum<25) | (attentionCheckFirst==0 & trialNum>24))

s5b_noAttn <- s5b_noAttn[,-c(2)]
s5b_attn <- s5b_attn[,-c(2)]

s5b_noAttn$expt <- "5b:NoAttn"
s5b_attn$expt <- "5b:Attn"

s6$expt <- "6:Attn19s"
s7$expt <- "7:NoAgent,LB"
s8a$expt <- "8a:LBtimes"
s8b$expt <- "8b:LBtimes"

s1c$workerid = factor(s1c$workerid)
s2b$workerid = factor(s2b$workerid)
s2bCR$workerid = factor(s2bCR$workerid)

d <- rbind(s1a,s1b,s1c,s2a,s2aCR,s2b,s2bCR,s3,s4,s5a,s5b_noAttn,s5b_attn,s6,s7)
# add "attentionCheckTimings"
d$attentionTime = (16.7*(d$PArray==1 & d$AArray==1) + 
                     13.2*(d$PArray==1 & d$AArray==0) + 
                     10.8*(d$PArray==0 & d$AArray==1) +
                     16.7*(d$PArray==0 & d$AArray==0))*(
                       d$expt!='attn19s' & d$expt!='noAgent' & d$expt!='noAttn') +
  (d$expt=='attn19s')*19
d <- rbind(d, s8a, s8b)

d$expt <- factor(d$expt, levels=c("1a:Replication1", "1b:Replication2", "1c:LabReplication", 
                                  "2a:2AFC", "2a:2AFC,CR", "2b:Lab2AFC", "2b:Lab2AFC,CR",
                                  "3:Absent", "4:Occluder", 
                                  "5a:NoAttn", "5b:NoAttn", "5b:Attn",
                                  "6:Attn19s", "7:NoAgent,LB", "8a:LBtimes", "8b:LBtimes"))
d$participant <- factor(c("Absent","Present")[d$PArray+1])
d$agent <- factor(c("Absent","Present")[d$AArray+1])

length(unique(d$workerid))




#### --- t-test for replications --- ####

# Calculating KTE's Cohen's d's
#tValue <- 3.47
#cohenD <- (tValue / sqrt(23))
#cohenD
library(lsr)

# helper functions
myCalculateCohensD <- function (comparison1, comparison2) {
  c1aggregate <- aggregate(reactionTime ~ workerid + participant + agent, comparison1, mean)
  c1aggregate = c1aggregate[order(c1aggregate$workerid),]
  c2aggregate <- aggregate(reactionTime ~ workerid + participant + agent, comparison2, mean)
  c2aggregate = c2aggregate[order(c2aggregate$workerid),]
  #c1aggregate$workerid == c2aggregate$workerid # to check that the ordering are the same.
  
  manualDiff <- c1aggregate$reactionTime - c2aggregate$reactionTime
  #stanDev <- sd(manualDiff)
  #tStat <- mean(manualDiff)/sd(manualDiff) * sqrt(length(manualDiff))
  cohenD <- mean(manualDiff)/sd(manualDiff); 
  return(cohenD)
}

myCalculateTTest <- function (comparison1, comparison2) {
  c1aggregate <- aggregate(reactionTime ~ workerid + participant + agent, comparison1, mean)
  c1aggregate = c1aggregate[order(c1aggregate$workerid),]
  c2aggregate <- aggregate(reactionTime ~ workerid + participant + agent, comparison2, mean)
  c2aggregate = c2aggregate[order(c2aggregate$workerid),]
  c1aggregate$workerid == c2aggregate$workerid
  return(t.test(c1aggregate$reactionTime, c2aggregate$reactionTime, paired=TRUE))
}





# Simple t-tests to replicate KTE:
# P-A- - P+A+
# P-A- - P+A-
# P-A- - P-A+
# P-A+ - P+A-

### for Studies 1a,b,c ###
#---- P-A- - P+A+ ----#
comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)


#---- P-A- - P+A- ----#
comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)


#---- P-A- - P-A+ ---- #
comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)



#---- P-A+ - P+A- ---- #
comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)



#########
# --- Crossover interaction section comparisons --- ####
# Additional comparisons: P+A+>P+A- and P+A+>P-A+
  comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Absent")
  myCalculateCohensD(comparison1, comparison2)
  myCalculateTTest(comparison1, comparison2)

  comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Absent")
  myCalculateCohensD(comparison1, comparison2)
  myCalculateTTest(comparison1, comparison2)

  comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Absent")
  myCalculateCohensD(comparison1, comparison2)
  myCalculateTTest(comparison1, comparison2)



  comparison1 = subset(d, d$expt=="1a:Replication1" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1a:Replication1" & d$participant=="Absent" & d$agent=="Present")
  myCalculateCohensD(comparison1, comparison2)
  myCalculateTTest(comparison1, comparison2)

  comparison1 = subset(d, d$expt=="1b:Replication2" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1b:Replication2" & d$participant=="Absent" & d$agent=="Present")
  myCalculateCohensD(comparison1, comparison2)
  myCalculateTTest(comparison1, comparison2)

  comparison1 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Present" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="1c:LabReplication" & d$participant=="Absent" & d$agent=="Present")
  myCalculateCohensD(comparison1, comparison2)
  myCalculateTTest(comparison1, comparison2)


# for Studies 2a-b, 3 : P-A+ - P-A-
  comparison1 = subset(d, d$expt=="2a:2AFC,CR" & d$participant=="Absent" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="2a:2AFC,CR" & d$participant=="Absent" & d$agent=="Absent")
  myCalculateCohensD(comparison1, comparison2)
  myCalculateTTest(comparison1, comparison2)

  comparison1 = subset(d, d$expt=="2b:Lab2AFC,CR" & d$participant=="Absent" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="2b:Lab2AFC,CR" & d$participant=="Absent" & d$agent=="Absent")
  myCalculateCohensD(comparison1, comparison2)
  myCalculateTTest(comparison1, comparison2)

  comparison1 = subset(d, d$expt=="3:Absent" & d$participant=="Absent" & d$agent=="Present")
  comparison2 = subset(d, d$expt=="3:Absent" & d$participant=="Absent" & d$agent=="Absent")
  myCalculateCohensD(comparison1, comparison2)
  myCalculateTTest(comparison1, comparison2)


## for Studies 2a-b, 3 : P-A- - P+A-
#comparison1 = subset(d, d$expt=="2a:2AFC,CR" & d$participant=="Absent" & d$agent=="Absent")
#comparison2 = subset(d, d$expt=="2a:2AFC,CR" & d$participant=="Present" & d$agent=="Absent")
#comparison1 = subset(d, d$expt=="2b:Lab2AFC,CR" & d$participant=="Absent" & d$agent=="Absent")
#comparison2 = subset(d, d$expt=="2b:Lab2AFC,CR" & d$participant=="Present" & d$agent=="Absent")
#comparison1 = subset(d, d$expt=="3:Absent" & d$participant=="Absent" & d$agent=="Absent")
#comparison2 = subset(d, d$expt=="3:Absent" & d$participant=="Present" & d$agent=="Absent")


#### ---- Mixed model analysis for studies 1-4 ---- ####

mss <- aggregate(reactionTime ~ workerid + participant + agent + expt , d,mean)

# lmer for Study 1a
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="1a:Replication1")))
ciUpper = 172.65 + 40.11 * 1.96; ciUpper # print CI
ciLower = 172.65 - 40.11 * 1.96; ciLower # print CI
2*(1-pnorm(4.304)) # print p-value.

# lmer for Study 1b
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="1b:Replication2")))
ciUpper = 121.81 + 28.6 * 1.96; ciUpper # print CI
ciLower = 121.81 - 28.6 * 1.96; ciLower # print CI
2*(1-pnorm(4.259)) # print p-value.

# lmer for Study 1c
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="1c:LabReplication")))
ciUpper = 65.71 + 26.52 * 1.96; ciUpper # print CI
ciLower = 65.71 - 26.52 * 1.96; ciLower # print CI
2*(1-pnorm(2.478)) # print p-value.

## lmer for Study 2a: 2AFC
#summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="2a:2AFC")))
#2*(1-pnorm(4.453)) # print p-value.

# lmer for Study 2a: 2AFC CR
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="2a:2AFC,CR")))
ciUpper = 213.48 + 55.47 * 1.96; ciUpper # print CI
ciLower = 213.48 - 55.47 * 1.96; ciLower # print CI
2*(1-pnorm(3.848)) # print p-value.

# lmer for Study 2b: Lab 2AFC
#summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="2b:Lab2AFC")))
#2*(1-pnorm(4.281)) # print p-value.

# lmer for Study 2b: Lab 2AFC CR
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="2b:Lab2AFC,CR")))
ciUpper = 152.80 + 31.52 * 1.96; ciUpper # print CI
ciLower = 152.80 - 31.52 * 1.96; ciLower # print CI
2*(1-pnorm(4.847)) # print p-value.

# lmer for Study 3: Absent
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="3:Absent")))
ciUpper = 171.47 + 28.33 * 1.96; ciUpper # print CI
ciLower = 171.47 - 28.33 * 1.96; ciLower # print CI
2*(1-pnorm(6.0653)) # print p-value.

# lmer for Study 4: Occluder
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="4:Occluder")))
ciUpper = 113.71 + 32.74 * 1.96; ciUpper # print CI
ciLower = 113.71 - 32.74 * 1.96; ciLower # print CI
2*(1-pnorm(3.473)) # print p-value.


# Tests for differences by response type ("Present" vs "Absent") across Studies1-3
allStudies1to3 = subset(d, d$expt=="1a:Replication1" | d$expt=="1b:Replication2" | d$expt=="2a:2AFC" |
                             d$expt=="2a:2AFC,CR" | d$expt=="3:Absent" | d$expt=="1c:LabReplication"  | 
                          d$expt=="2b:Lab2AFC" | d$expt=="2b:Lab2AFC,CR" )

allStudies1to3$Absent <- allStudies1to3$expt == "2a:2AFC,CR" | allStudies1to3$expt=="2b:Lab2AFC,CR" | 
  allStudies1to3$expt=="3:Absent"

# this tests whether there is a difference in CR/Absent conditions
crmod <- lmer(reactionTime ~ participant*agent*Absent +  
               (participant*agent|workerid), 
             data=allStudies1to3)
summary(crmod)
# cr coefficient
fixef(crmod)[4]
fixef(crmod)[4] + summary(crmod)$coefficients[4,2] * 1.96
fixef(crmod)[4] - summary(crmod)$coefficients[4,2] * 1.96

2*(1-pnorm(1.68)) # for the interaction t

fixef(crmod)[5]
fixef(crmod)[5] + summary(crmod)$coefficients[5,2] * 1.96
fixef(crmod)[5] - summary(crmod)$coefficients[5,2] * 1.96


#### ---- End Crossover interaction section comparisons for Studies1-4 ----####
#########

#### ---- Start Fig sections ---- ####

### Preprocessing for Figures
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
                                              rep("Occluder",4),
                                              rep("Crossover",4)),
                                            levels = c("KTE", "Ball Present", "Ball Absent", "Occluder", "Crossover"),
                                            labels = c("KTE", "Ball Present", "Ball Absent", "Occluder", "Crossover")),
                             participant = factor(rep(c("Absent","Present"),2*5)),
                             agent = factor(rep(c("Absent", "Absent", "Present", "Present"),5)),
                             reactionTime = c( c(360,320,328,313), #KTE
                                               c(360,330,340,310), #Ball Present prediction
                                               c(310,340,330,360), #Ball Absent prediction
                                               c(360,330,356,326), #Occluder prediction
                                               c(360,342,330,360)), # Crossover prediction
                             ci.h = c(c(14, 10, 12, 10)*1.96, rep(NA,16)),
                             ci.l = c(c(14, 10, 12, 10)*1.96, rep(NA,16)),
                             illustrationData = factor(c(rep(0,4),rep(1,16)))
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
  facet_wrap( ~ panel, ncol=3, as.table=TRUE, drop=FALSE) +
  theme(strip.background = element_rect(fill="#FFFFFF"), 
        strip.text = element_text(size=12), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(size=16, vjust=-0.10),
        axis.title.y = element_text(size=16, vjust=0.35),
        legend.text = element_text(size=12),
        title = element_text(size=16),
        panel.grid = element_blank())
# Saved as a pdf. 8 x 5

# did not add error bars on the KTE because error bars on the other panels don't make sense
  

# ------------------ Plot Fig3: Studies 1-4 -- 9 panels in 3 rows ####
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
                                            "2a: Web 2AFC, CR","2b: Lab 2AFC, CR", "3:Absent", "4:Occluder"))

blackGreyPalette <- c("#000000", "#999999")  

ggplot(msSub, aes(x=participant, y=reactionTime, colour=agent, group=agent)) + 
  ylab("Reaction Time (ms)") + ylim(390,1100) +
  xlab("Participant Belief") +
  guides(color=guide_legend(title="Agent Belief")) +
  geom_line(position=position_dodge(width=.1),stat="identity") +
  scale_colour_manual(values=blackGreyPalette) +
  facet_wrap( ~ expt, ncol=3, as.table=TRUE, drop=FALSE) + # ncol = 5 or 3
  geom_linerange(aes(ymin=reactionTime - ci.l, ymax=reactionTime + ci.h),
                 position=position_dodge(width=.1)) +
  theme(strip.background = element_rect(fill="#FFFFFF"), 
        strip.text = element_text(size=12), 
        axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        legend.text = element_text(size=12),
        title = element_text(size=16))
# pdf 10 by 5
# ------------------ End Plot Fig 3


# ------------------ Plot Fig4: Studies 5-8 only. ####
msSub2 <- subset(ms,expt=="5a:NoAttn"|expt=="5b:NoAttn"|expt=="5b:Attn"|
                   expt=="6:Attn19s"|expt=="7:NoAgent,LB"|expt=="8a:LBtimes"|expt=="8b:LBtimes")
msSub2$expt <- factor(msSub2$expt)
levels(msSub2$expt) <- c("5a: No Check", "5b: No Check", "5b: Attention Check", 
                         "6: Check at 19s", "7: No Agent, Lightbulb", "8a: Lightbulb timing", "8b: Lightbulb timing")
msSub2$expt <- factor(msSub2$expt, c("5a: No Check", "5b: No Check", "5b: Attention Check", 
                                     "6: Check at 19s", "7: No Agent, Lightbulb", "8a: Lightbulb timing", "8b: Lightbulb timing"))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

blackGreyPalette <- c("#000000", "#999999")

## Fig 4, with lines:
ggplot(msSub2, aes(x=participant, y=reactionTime, colour=agent, group=agent)) + 
  ylab("Reaction Time (ms)") + ylim(390,1100) +
  xlab("Participant Belief") +
  guides(color=guide_legend(title="Agent Belief")) +
  geom_line(position=position_dodge(width=.1),stat="identity") +
  scale_colour_manual(values=blackGreyPalette) +
  facet_wrap( ~ expt, nrow=2) +
  geom_linerange(aes(ymin=reactionTime - ci.l, ymax=reactionTime + ci.h), position=position_dodge(width=.1)) +
  theme(strip.background = element_rect(fill="#FFFFFF"), 
        strip.text = element_text(size=12), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(size=14, vjust=-0.10),
        axis.title.y = element_text(size=14, vjust=0.35),
        legend.text = element_text(size=12),
        title = element_text(size=18),
        panel.grid = element_blank())
# square: 10 by 5

# ------------------ End Plot Fig 4

# ------------------ Plot Fig5: Vs. Time ####


## Keeping all of "attentionTime", "participant", "agent" as variables:
mssAll <- aggregate(reactionTime ~ workerid + participant + agent + attentionTime + expt, subset(d, expt=="8a:LBtimes" | expt=="8b:LBtimes"),mean)
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

msAll$expt = factor(msAll$expt)
levels(msAll$expt) <- c("8a: Lightbulb timing", "8b: Lightbulb timing")
msAll$expt <- factor(msAll$expt, c("8a: Lightbulb timing", "8b: Lightbulb timing"))

## Fig 5, MCF 12/12/13
ggplot(aes(x=attentionTime,y=reactionTime,
           ymin=reactionTime-ci.l,ymax=reactionTime+ci.h,
           colour=condition,group=condition),
       data=msAll) +
  geom_line() + 
  geom_pointrange(position=position_dodge(width=.2)) + 
  scale_colour_manual(values=cbPalette) + 
  guides(color=guide_legend(title="Video Condition")) +
  ggtitle("Study 8a and 8b: Lightbulb timing") +
  ylab("Reaction Time (ms)") + 
  xlab("Attention Check Time (s)") + ylim(500,900) +
  facet_wrap( ~ expt, nrow=1) + theme(strip.background = element_rect(fill="#FFFFFF"), 
                                      strip.text = element_text(size=14), 
                                      axis.text = element_text(size=14),
                                      axis.title.x = element_text(size=16, vjust=-0.2),
                                      axis.title.y = element_text(size=16, vjust=0.35),
                                      legend.text = element_text(size=14),
                                      title = element_text(size=18, vjust=1),
                                      panel.grid = element_blank())
# Save as 10 by 5 pdf

# ------------------ End Plot Fig 5



#### ---- Mixed Model Stats and Pairwise differences for Studies 5-8 ---- ####


#### ---- Study 5a: No Attention ---- ####
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="5a:NoAttn")))
ciUpper = 21.72 + 35.27 * 1.96; ciUpper # print CI
ciLower = 21.72 - 35.27 * 1.96; ciLower # print CI
2*(1-pnorm(0.616)) # print p-value.

# P-A- - P+A+
comparison1 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A- - P+A-
comparison1 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A- - P-A+
comparison1 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Absent" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A+ - P+A-
comparison1 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# the first 4 were the "Table 1 comparisons", these last 2 are the last 2 comparisons (4C2 = 6)
# P-A+ - P+A+
comparison1 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P+A- - P+A+
comparison1 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Present" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="5a:NoAttn" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

#### ---- Study 5b: With and without Attention check ---- ####

# no attention check
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(d, expt=="5b:NoAttn")))
ciUpper = 62.40 + 29.65 * 1.96; ciUpper # print CI
ciLower = 62.40 - 29.65 * 1.96; ciLower # print CI
2*(1-pnorm(2.10)) # print p-value.

# attention check
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(d, expt=="5b:Attn")))
ciUpper = 139.12 + 26.69 * 1.96; ciUpper # print CI
ciLower = 139.12 - 26.69 * 1.96; ciLower # print CI
2*(1-pnorm(5.21)) # print p-value.




# no attention check and it's the first block
mss5b <- aggregate(reactionTime ~ workerid + participant + agent , subset(d, expt=="5b:NoAttn" & trialNum < 25) ,mean)
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), mss5b))
ciUpper = 77.96 + 49.47 * 1.96; ciUpper # print CI
ciLower = 77.96 - 49.47 * 1.96; ciLower # print CI
2*(1-pnorm(1.576)) # print p-value.

# no attention check and it's the second block
mss5b <- aggregate(reactionTime ~ workerid + participant + agent , subset(d, expt=="5b:NoAttn" & trialNum > 24) ,mean)
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), mss5b))
ciUpper = 50.44 + 39.25 * 1.96; ciUpper # print CI
ciLower = 50.44 - 39.25 * 1.96; ciLower # print CI
2*(1-pnorm(1.285)) # print p-value.

# attention check and it's the first block
mss5b <- aggregate(reactionTime ~ workerid + participant + agent , subset(d, expt=="5b:Attn" & trialNum < 25) ,mean)
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), mss5b))
ciUpper = 84.42 + 34.39 * 1.96; ciUpper # print CI
ciLower = 84.42 - 34.39 * 1.96; ciLower # print CI
2*(1-pnorm(2.455)) # print p-value.

# attention check and it's the second block
mss5b <- aggregate(reactionTime ~ workerid + participant + agent , subset(d, expt=="5b:Attn" & trialNum > 24) ,mean)
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), mss5b))
ciUpper = 183.05 + 38.78 * 1.96; ciUpper # print CI
ciLower = 183.05 - 38.78 * 1.96; ciLower # print CI
2*(1-pnorm(4.720)) # print p-value.



####---- Study 6: Attention at 19s ---- ####
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="6:Attn19s")))
ciUpper = 12.012 + 22.78 * 1.96; ciUpper # print CI
ciLower = 12.012 - 22.78 * 1.96; ciLower # print CI
2*(1-pnorm(0.528)) # print p-value.


# P-A- - P+A+
comparison1 = subset(d, d$expt=="6:Attn19s" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="6:Attn19s" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A- - P+A-
comparison1 = subset(d, d$expt=="6:Attn19s" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="6:Attn19s" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A- - P-A+
comparison1 = subset(d, d$expt=="6:Attn19s" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="6:Attn19s" & d$participant=="Absent" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A+ - P+A-
comparison1 = subset(d, d$expt=="6:Attn19s" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="6:Attn19s" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# the first 4 were the "Table 1 comparisons", these last 2 are the last 2 comparisons (4C2 = 6)
# P-A+ - P+A+
comparison1 = subset(d, d$expt=="6:Attn19s" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="6:Attn19s" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P+A- - P+A+
comparison1 = subset(d, d$expt=="6:Attn19s" & d$participant=="Present" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="6:Attn19s" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)


#### ---- Study 7: No Agent, with lightbulb ---- ####
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="7:NoAgent,LB")))
ciUpper = 90.36 + 27.02 * 1.96; ciUpper # print CI
ciLower = 90.36 - 27.02 * 1.96; ciLower # print CI
2*(1-pnorm(3.34)) # print p-value.

# P-A- - P+A+ n.s.
comparison1 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A- - P+A- sig
comparison1 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A- - P-A+ trending
comparison1 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Absent" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A+ - P+A- n.s.
comparison1 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# the first 4 were the "Table 1 comparisons", these last 2 are the last 2 comparisons (4C2 = 6)
# P-A+ - P+A+ sig
comparison1 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P+A- - P+A+ sig
comparison1 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Present" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="7:NoAgent,LB" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)
# this diff is negative, meaning that RT(P+A+) is longer than RT(P+A-)


#### ---- Study 8a: Lightbulb x Times ---- ####
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="8a:LBtimes")))
ciUpper = 6.299 + 27.37 * 1.96; ciUpper # print CI
ciLower = 6.299 - 27.37 * 1.96; ciLower # print CI
2*(1-pnorm(0.23)) # print p-value.

# P-A- - P+A+
comparison1 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A- - P+A-
comparison1 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A- - P-A+
comparison1 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Absent" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P-A+ - P+A-
comparison1 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# the first 4 were the "Table 1 comparisons", these last 2 are the last 2 comparisons (4C2 = 6)
# P-A+ - P+A+
comparison1 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)

# P+A- - P+A+
comparison1 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Present" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="8a:LBtimes" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2)
myCalculateTTest(comparison1, comparison2)


# for Study 8a, using attentioncheck as a predictor
summary(lmer(reactionTime ~ participant*agent + attentionTime + (1 + participant*agent|workerid), mssAll))
ciUpper = 9.512 + 2.101 * 1.96; ciUpper # print CI
ciLower = 9.512 - 2.101 * 1.96; ciLower # print CI
2*(1-pnorm(4.528)) # print p-value.



#### ---- Study 8b: Lightbulb x Times ---- ####
summary(lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(mss,expt=="8b:LBtimes")))
ciUpper = -31.89 + 17.63 * 1.96; ciUpper # print CI
ciLower = -31.89 - 17.63 * 1.96; ciLower # print CI
2*(1-pnorm(1.81)) # print p-value.

# P-A- - P+A+
comparison1 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2) # .0407
myCalculateTTest(comparison1, comparison2) # .72

# P-A- - P+A-
comparison1 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2) # .0542
myCalculateTTest(comparison1, comparison2) # .49

# P-A- - P-A+
comparison1 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Absent" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Absent" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2) # -0.12
myCalculateTTest(comparison1, comparison2) # .114

# P-A+ - P+A-
comparison1 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Present" & d$agent=="Absent")
myCalculateCohensD(comparison1, comparison2) # .163
myCalculateTTest(comparison1, comparison2) # .03

# the first 4 were the "Table 1 comparisons", these last 2 are the last 2 comparisons (4C2 = 6)
# P-A+ - P+A+
comparison1 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Absent" & d$agent=="Present")
comparison2 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2) # .264
myCalculateTTest(comparison1, comparison2) # <.001

# P+A- - P+A+
comparison1 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Present" & d$agent=="Absent")
comparison2 = subset(d, d$expt=="8b:LBtimes" & d$participant=="Present" & d$agent=="Present")
myCalculateCohensD(comparison1, comparison2) # .06
myCalculateTTest(comparison1, comparison2) # .442

# for Study 8b, using attentioncheck as a predictor
summary(lmer(reactionTime ~ participant*agent + attentionTime + (1 + participant*agent|workerid), subset(mssAll, expt=="8b:LBtimes")))
ciUpper = 12.09 + 1.546 * 1.96; ciUpper # print CI
ciLower = 12.09 - 1.546 * 1.96; ciLower # print CI
2*(1-pnorm(7.818)) # print p-value.


# ------------------ Plot Fig6: Meta Analytic Plot ####

# doing meta analytic calculations from p7 of http://www.meta-analysis.com/downloads/Meta-analysis%20fixed%20effect%20vs%20random%20effects.pdf

# the data for the below DF are from all the calculations in the file above.
metaAnalyticDF = data.frame(study = factor(c("1a", "1b", "1c", "2aHit", "2aCR", "2bHit", "2bCR",
                                             "3", "4", "5bAttn", "7", "5a", "5bNoAttn", "6", "8a", "8b"),
                                           levels = c("1a", "1b", "1c", "2aHit", "2aCR", "2bHit", "2bCR",
                                                      "3", "4", "5bAttn", "7", "5a", "5bNoAttn", "6", "8a", "8b"),
                                           labels = c("1a", "1b", "1c", "2aHit", "2aCR", "2bHit", "2bCR",
                                                      "3", "4", "5bAttn", "7", "5a", "5bNoAttn", "6", "8a", "8b")),
                            crossover = c(172.65, 121.81, 65.71, 169.73, 213.48, 117.09, 152.80, 171.47, 113.71,
                                          139.12, 90.36, 21.72, 62.40, 12.012, 6.299, -31.89),
                            SE = c(40.11, 28.6, 26.52, 38.12, 55.47, 27.35, 31.52, 28.33, 32.74,
                                   26.69, 27.02, 35.27, 29.65, 22.78, 27.37, 17.63),
                            N = c(54, 72, 24, 62, 62, 24, 24, 86, 57,
                                  175, 56, 64, 175, 79, 78, 163),
                            predicted = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
                                          "Yes", "Yes", "No", "No", "No", "No", "No"))
metaAnalyticDF$ci.h = metaAnalyticDF$crossover + metaAnalyticDF$SE*1.96
metaAnalyticDF$ci.l = metaAnalyticDF$crossover - metaAnalyticDF$SE*1.96
metaAnalyticDF$var = (metaAnalyticDF$SE)^2 * metaAnalyticDF$N
metaAnalyticDF$weight = 1/metaAnalyticDF$var

weightedMeanForYes = (sum(metaAnalyticDF$weight*metaAnalyticDF$crossover*1*(metaAnalyticDF$predicted=="Yes")))/(sum(metaAnalyticDF$weight*1*(metaAnalyticDF$predicted=="Yes")))
weightedMeanForNo = (sum(metaAnalyticDF$weight*metaAnalyticDF$crossover*1*(metaAnalyticDF$predicted=="No")))/(sum(metaAnalyticDF$weight*1*(metaAnalyticDF$predicted=="No")))
metaAnalyticDF$weightedMean = weightedMeanForYes * (metaAnalyticDF$predicted=="Yes") + 
  weightedMeanForNo * (metaAnalyticDF$predicted=="No")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# # try flipping axes. flip order of factor labels to have 1a on top
flipMetaAnalyticDF = metaAnalyticDF
flipMetaAnalyticDF$study = factor(flipMetaAnalyticDF$study,
                                  levels = c("8b", "8a", "6", "5bNoAttn", "5a", "5bAttn", "7", 
                                             "4", "3", "2bCR", "2bHit", "2aCR", "2aHit", "1c", "1b", "1a"),
                                  labels = c("8b", "8a", "6", "5bNoAttn", "5a", "5bAttn", "7", 
                                             "4", "3", "2bCR", "2bHit", "2aCR", "2aHit", "1c", "1b", "1a"))
ggplot(flipMetaAnalyticDF, aes(x=study, y=crossover, colour=predicted, group=predicted)) + 
  ylab("Interaction coefficient (ms)") + ylim(-100,400) +
  xlab("Study") +
  guides(color=guide_legend(title="Predicted by\nAttention Check\nHypothesis")) +
  geom_point(shape=1) +
  geom_hline(aes(yintercept=0)) +
  geom_errorbar(aes(y=weightedMean, ymin=weightedMean, ymax=weightedMean), linetype="dashed") +
  scale_colour_manual(values=cbPalette) +
  geom_linerange(aes(ymin=ci.l, ymax=ci.h)) +
  coord_flip() +
  theme(axis.text = element_text(size=12),
        axis.title.x = element_text(size=16, vjust=0.10),
        axis.title.y = element_text(size=16, vjust=0.35),
        legend.text = element_text(size=12),
        title = element_text(size=16),
        panel.grid = element_blank())
# # 8 by 6

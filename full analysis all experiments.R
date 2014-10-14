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
s8b <- read.csv("study8b.csv") # Standard; attention check on lightbulb at various timings

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

expts <- c("1a:Replication1", "1b:Replication2", "1c:LabReplication", 
           "2a:2AFC", "2a:2AFC,CR", "2b:Lab2AFC", "2b:Lab2AFC,CR",
           "3:Absent", "4:Occluder", 
           "5a:NoAttn", "5b:NoAttn", "5b:Attn",
           "6:Attn19s", "7:NoAgent,LB", "8a:LBtimes", "8b:LBtimes")
d$expt <- factor(d$expt, levels=expts)
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

# KTE = data.frame(participant = factor(c("Absent", "Present", "Absent", "Present")),
#                  agent = factor(c("Absent", "Absent", "Present", "Present")),
#                  expt = factor(c("KTE", "KTE", "KTE", "KTE")),
#                  #attentionTime = c(16.7, 13.2, 10.8, 16.7),
#                  reactionTime = c(360, 320, 328, 313),
#                  ci.h = c(14, 10, 12, 10)*1.96,
#                  ci.l = c(14, 10, 12, 10)*1.96,
#                  n = rep(24,4))
# ms = rbind(KTE, ms) # to put KTE as the first factor


# ------------------ Plot Fig2: KTE + Conceptual Figure ####

conceptualFigDF = data.frame(panel = factor(c(rep("KTE",4), 
                                              rep("Response: Ball Present",4),
                                              rep("Response: Ball Absent",4), 
                                              rep("Occluder",4)),
                                            levels = c("KTE", "Response: Ball Present", "Response: Ball Absent", "Occluder"),
                                            labels = c("KTE", "Response: Ball Present", "Response: Ball Absent", "Occluder")),
                             participant = factor(rep(c("Absent","Present"),2*4)),
                             agent = factor(rep(c("Absent", "Absent", "Present", "Present"),4)),
                             reactionTime = c( c(360,320,328,313), #KTE
                                               c(360,330,340,310), #Ball Present prediction
                                               c(310,340,330,360), #Ball Absent prediction
                                               c(360,330,356,326)), #Occluder prediction
                             ci.h = c(c(14, 10, 12, 10)*1.96, rep(NA,12)),
                             ci.l = c(c(14, 10, 12, 10)*1.96, rep(NA,12)),
                             illustrationData = factor(c(rep(0,4),rep(1,12)))
                             )

blackGreyPalette <- c("#000000", "#999999")  

ggplot(conceptualFigDF, aes(x=participant, y=reactionTime, colour=agent, group=agent, shape=illustrationData)) + 
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
  theme(strip.background = element_rect(fill="#FFFFFF"), 
        strip.text = element_text(size=12), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(size=16, vjust=-0.10),
        axis.title.y = element_text(size=16, vjust=0.35),
        legend.text = element_text(size=12),
        title = element_text(size=16),
        panel.grid = element_blank())
# saved as pdf 10x3

# did not add error bars on the KTE because error bars on the other panels don't make sense
  

# ------------------ Plot Fig3: Studies 1-4 -- 9 panels in 3 rows ####
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
        title = element_text(size=16),
        panel.grid = element_blank())
# pdf 10 by 5
# ------------------ End Plot Fig 3


# ------------------ Plot Fig4: Studies 5-8 only. ####
msSub2 <- subset(ms,expt=="5a:NoAttn"|expt=="5b:NoAttn"|expt=="5b:Attn"|
                   expt=="6:Attn19s"|expt=="7:NoAgent,LB"|expt=="8a:LBtimes"|expt=="8b:LBtimes")
msSub2$expt <- factor(msSub2$expt)
levels(msSub2$expt) <- c("5a: No Check", "5b: No Check", "5b: Attention Check", 
                         "6: Check at 19s", "7: No Agent, Lightbulb", "8a: Agent+Lightbulb", "8b: Agent+Lightbulb")
msSub2$expt <- factor(msSub2$expt, c("5a: No Check", "5b: No Check", "5b: Attention Check", 
                                     "6: Check at 19s", "7: No Agent, Lightbulb", "8a: Agent+Lightbulb", "8b: Agent+Lightbulb"))

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

# Edited 9/22/14 to collapse across conditions
mssAll <- aggregate(reactionTime ~ workerid + attentionTime + expt, subset(d, expt=="8a:LBtimes" | expt=="8b:LBtimes"),mean)
msAll <- aggregate(reactionTime ~ attentionTime + expt, mssAll,mean)
msAll$ci.h <- aggregate(reactionTime ~ attentionTime + expt, mssAll, ci.high)$reactionTime
msAll$ci.l <- aggregate(reactionTime ~ attentionTime + expt, mssAll, ci.low)$reactionTime
msAll$n <- aggregate(workerid ~ attentionTime + expt, mssAll, n.unique)$workerid

msAll$expt = factor(msAll$expt)
levels(msAll$expt) <- c("Study 8a", "Study 8b")
msAll$expt <- factor(msAll$expt, c("Study 8a", "Study 8b"))

cbbwPal <- c("#d8b365","#5ab4ac")
ggplot(aes(x=attentionTime,y=reactionTime,
           ymin=reactionTime-ci.l,ymax=reactionTime+ci.h,
           colour=expt,linetype=expt, group=expt),
       data=msAll) +
  geom_line() + geom_pointrange() +
  scale_colour_manual(values=cbbwPal) + 
  guides(color=guide_legend(title="Experiment"),
         linetype=guide_legend(title="Experiment")) +
  scale_x_continuous(breaks=c(10.9, 12.9, 14.9, 16.9, 18.9)) +
  #ggtitle("Dissociation of attention check time from video condition") +
  ylab("Reaction Time (ms)") + 
  xlab("Attention Check Time (s)") + ylim(550,800) +
  theme(strip.background = element_rect(fill="#FFFFFF"), 
        strip.text = element_text(size=14), 
        axis.text = element_text(size=14),
        axis.title.x = element_text(size=16, vjust=-0.2),
        axis.title.y = element_text(size=16, vjust=0.35),
        legend.text = element_text(size=14),
        title = element_text(size=18, vjust=1),
        panel.grid = element_blank())
# 7 by 5 pdf

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

#3 way interaction
summary(lmer(reactionTime ~ participant*agent*expt + (1 + participant*agent|workerid), subset(d, expt=="5b:Attn" | expt == "5b:NoAttn")))

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
summary(lmer(reactionTime ~ participant*agent + attentionTime + (1 + participant*agent|workerid), subset(d, expt=="8a:LBtimes")))
ciUpper = 9.693 + 2.156 * 1.96; ciUpper # print CI
ciLower = 9.693 - 2.156 * 1.96; ciLower # print CI
2*(1-pnorm(4.496)) # print p-value.



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
summary(lmer(reactionTime ~ participant*agent + attentionTime + (1 + participant*agent|workerid), subset(d, expt=="8b:LBtimes")))
ciUpper = 12.086 + 1.546 * 1.96; ciUpper # print CI
ciLower = 12.086 - 1.546 * 1.96; ciLower # print CI
2*(1-pnorm(7.818)) # print p-value.


# ------------------ Plot Fig6: Meta Analytic Plot ####


myCalculateRTCI <- function (comparison1, comparison2) {
  c1aggregate <- aggregate(reactionTime ~ workerid + participant + agent, comparison1, mean)
  c1aggregate = c1aggregate[order(c1aggregate$workerid),]
  c2aggregate <- aggregate(reactionTime ~ workerid + participant + agent, comparison2, mean)
  c2aggregate = c2aggregate[order(c2aggregate$workerid),]
  
  means <- c1aggregate$reactionTime - c2aggregate$reactionTime  
  cis <- list()
  cis$M <- mean(means)
  cis$N <- length(means)
  cis$SE <- sd(means) / sqrt(cis$N)
  
  return(cis)
}


blackGreyPal <- c("#000000", "#999999")
cbPal <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbwPal <- c("#d8b365","#5ab4ac")

# doing meta analytic calculations from p7 of http://www.meta-analysis.com/downloads/Meta-analysis%20fixed%20effect%20vs%20random%20effects.pdf
# updated: http://onlinelibrary.wiley.com/store/10.1002/jrsm.12/asset/12_ftp.pdf?v=1&t=hz05ch4u&s=1e2069ee724b3aad1941cbbd8e713de1bf4312d8
# Borenstein, M., Hedges, L. V., Higgins, J., & Rothstein, H. R. (2010). A basic introduction to fixed‐effect and random‐effects models for meta‐analysis. Research Synthesis Methods, 1(2), 97-111

ordered.expts <- c("1a",   "1b", "1c",  "2aHit", "2aCR", "2bHit", "2bCR", "3",   "4",  "5a", "5bNoAttn", "5bAttn", "6",  "7", "8a", "8b")
predicted     <- c("Yes", "Yes", "Yes",  "Yes",  "Yes",  "Yes",   "Yes",  "Yes", "Yes", "No",     "No",    "Yes",  "No", "Yes", "No", "No")

maAC <- data.frame()
for (i in 1:length(expts)) {
  thisLmer = lmer(reactionTime ~ participant*agent + (1 + participant*agent|workerid), subset(d, expt==expts[i]))
  maAC <- rbind.fill(maAC,
                     data.frame(study=ordered.expts[i],
                                predicted=predicted[i],
                                es= fixef(thisLmer)[4],
                                SE= sqrt(diag(vcov(thisLmer)))[4],
                                N = n.unique(subset(d, expt==expts[i])$workerid)))
}

maAC$ci.h = maAC$es + maAC$SE*1.96
maAC$ci.l = maAC$es - maAC$SE*1.96

maAC$withinStudyVar = (maAC$SE)^2
maAC$withinStudyWeight = 1/maAC$withinStudyVar

maAC_Q_for_yes = with(subset(maAC, maAC$predicted=="Yes"), sum(withinStudyWeight*es*es) - ((sum(withinStudyWeight*es))^2)/sum(withinStudyWeight))
maAC_C_for_yes = with(subset(maAC, maAC$predicted=="Yes"), sum(withinStudyWeight) - sum(withinStudyWeight^2)/sum(withinStudyWeight))
maAC_tauSquared_for_yes = max((maAC_Q_for_yes-(sum(predicted=="Yes")-1))/maAC_C_for_yes,0)
maAC_Q_for_no = with(subset(maAC, maAC$predicted=="No"), sum(withinStudyWeight*es*es) - ((sum(withinStudyWeight*es))^2)/sum(withinStudyWeight))
maAC_C_for_no = with(subset(maAC, maAC$predicted=="No"), sum(withinStudyWeight) - sum(withinStudyWeight^2)/sum(withinStudyWeight))
maAC_tauSquared_for_no = max((maAC_Q_for_no-(sum(predicted=="No")-1))/maAC_C_for_no,0)

maAC$tauSquared = maAC_tauSquared_for_yes * (maAC$predicted=="Yes") + maAC_tauSquared_for_no * (maAC$predicted=="No")
maAC$var = (maAC$withinStudyVar + maAC$tauSquared)
maAC$weight = 1/maAC$var # Eqn (5)

# meta analytic weighted means
maACsummary = data.frame(weightedMeanForYes = (sum(maAC$weight*maAC$es*1*(maAC$predicted=="Yes")))/
                           (sum(maAC$weight*1*(maAC$predicted=="Yes"))),
                         weightedMeanForNo = (sum(maAC$weight*maAC$es*1*(maAC$predicted=="No")))/
                           (sum(maAC$weight*1*(maAC$predicted=="No"))),
                         varForYes = 1/(sum(maAC$weight*1*(maAC$predicted=="Yes"))),
                         varForNo = 1/(sum(maAC$weight*1*(maAC$predicted=="No")))
)
maACsummary$ciForYes = sqrt(maACsummary$varForYes) * 1.96
maACsummary$ciForNo = sqrt(maACsummary$varForNo) * 1.96

maAC <- rbind(maAC,
              data.frame(study=c("","Positive Predictions","Null Predictions"),
                         predicted=c("No","Yes","No"),
                         es=c(NA,maACsummary$weightedMeanForYes, maACsummary$weightedMeanForNo),
                         SE=c(NA,sqrt(maACsummary$varForYes),sqrt(maACsummary$varForNo)),
                         N=c(NA,NA,NA),
                         ci.l=c(NA, maACsummary$weightedMeanForYes - maACsummary$ciForYes,
                                maACsummary$weightedMeanForNo - maACsummary$ciForNo),
                         ci.h=c(NA, maACsummary$weightedMeanForYes + maACsummary$ciForYes,
                                maACsummary$weightedMeanForNo + maACsummary$ciForNo),
                         withinStudyVar=c(NA,NA,NA),
                         withinStudyWeight=c(NA,NA,NA),
                         tauSquared=c(NA,NA,NA),
                         var=c(NA,maACsummary$varForYes,maACsummary$varForNo),
                         weight=c(NA,NA,NA))
)                        

#### --- constructing meta analytic figure for the P-A- vs. P-A+ comparison ---- ####

study <-     c("1a",  "1b",  "1c",  "2aHit", "2aCR", "2bHit", "2bCR", "3",  "4",  "5a",  "5bNoAttn", "5bAttn", "6",    "7", "8a",  "8b")
predicted <- c("Yes", "Yes", "Yes", "Yes",   "No",   "Yes",   "No",   "No", "No", "Yes",  "Yes",     "Yes",    "Yes", "No", "Yes", "Yes")

maABE <- data.frame()
for (i in 1:length(expts)) {
  es <- myCalculateRTCI(subset(d, d$expt==expts[i] & d$participant=="Absent" & d$agent=="Absent"),
                        subset(d, d$expt==expts[i] & d$participant=="Absent" & d$agent=="Present"))
  maABE <- rbind.fill(maABE,
                      data.frame(study=study[i],
                                 predicted=predicted[i],
                                 es=es$M,
                                 SE=es$SE,
                                 N = es$N))
}
maABE$ci.h = maABE$es + maABE$SE*1.96
maABE$ci.l = maABE$es - maABE$SE*1.96

#maABE$var = (maABE$SE)^2 *maABE$N
#maABE$weight = 1/maABE$var

maABE$withinStudyVar = (maABE$SE)^2
maABE$withinStudyWeight = 1/maABE$withinStudyVar

maABE_Q_for_yes = with(subset(maABE, maABE$predicted=="Yes"), sum(withinStudyWeight*es*es) - ((sum(withinStudyWeight*es))^2)/sum(withinStudyWeight))
maABE_C_for_yes = with(subset(maABE, maABE$predicted=="Yes"), sum(withinStudyWeight) - sum(withinStudyWeight^2)/sum(withinStudyWeight))
maABE_tauSquared_for_yes = max((maABE_Q_for_yes-(sum(predicted=="Yes")-1))/maABE_C_for_yes,0)
maABE_Q_for_no = with(subset(maABE, maABE$predicted=="No"), sum(withinStudyWeight*es*es) - ((sum(withinStudyWeight*es))^2)/sum(withinStudyWeight))
maABE_C_for_no = with(subset(maABE, maABE$predicted=="No"), sum(withinStudyWeight) - sum(withinStudyWeight^2)/sum(withinStudyWeight))
maABE_tauSquared_for_no = max((maABE_Q_for_no-(sum(predicted=="No")-1))/maABE_C_for_no,0)

maABE$tauSquared = maABE_tauSquared_for_yes * (maABE$predicted=="Yes") + maABE_tauSquared_for_no * (maABE$predicted=="No")
maABE$var = (maABE$withinStudyVar + maABE$tauSquared)
maABE$weight = 1/maABE$var # Eqn (5)

maABEsummary = data.frame(weightedMeanForYes = (sum(maABE$weight*maABE$es*1*(maABE$predicted=="Yes")))/
                            (sum(maABE$weight*1*(maABE$predicted=="Yes"))),
                          varForYes = 1/(sum(maABE$weight*1*(maABE$predicted=="Yes"))),
                          weightedMeanForNo = (sum(maABE$weight*maABE$es*1*(maABE$predicted=="No")))/
                            (sum(maABE$weight*1*(maABE$predicted=="No"))),
                          varForNo = 1/(sum(maABE$weight*1*(maABE$predicted=="No")))
)

maABEsummary$ciForYes = sqrt(maABEsummary$varForYes) * 1.96
maABEsummary$ciForNo = sqrt(maABEsummary$varForNo) * 1.96

maABE <- rbind(maABE,
               data.frame(study=c("","Positive Predictions","Null Predictions"),
                          es=c(NA,maABEsummary$weightedMeanForYes, maABEsummary$weightedMeanForNo),
                          SE=c(NA,sqrt(maABEsummary$varForYes),sqrt(maABEsummary$varForNo)),
                          N=c(NA,NA,NA),
                          predicted=c("No","Yes","No"),
                          ci.l=c(NA,maABEsummary$weightedMeanForYes - maABEsummary$ciForYes,
                                 maABEsummary$weightedMeanForNo - maABEsummary$ciForNo),
                          ci.h=c(NA,maABEsummary$weightedMeanForYes + maABEsummary$ciForYes,
                                 maABEsummary$weightedMeanForNo + maABEsummary$ciForNo),
                          withinStudyVar=c(NA,NA,NA),
                          withinStudyWeight=c(NA,NA,NA),
                          tauSquared=c(NA,NA,NA),
                          var=c(NA,maABEsummary$varForYes,maABEsummary$varForNo),
                          weight=c(NA,NA,NA)))                        


#### now try the combined plot

maAC$hypothesis <- "Attention Check Hypothesis\n(Crossover Interaction)"
maABE$hypothesis <- "Automatic ToM Hypothesis\n(P-A+ > P-A-)"
ma <- rbind.fill(maAC,maABE)

ma$study <- factor(ma$study, 
                   levels=c("Positive Predictions","Null Predictions","","8b", "8a", "7", "6", "5bNoAttn", "5bAttn", "5a",
                            "4", "3", "2bCR", "2bHit", "2aCR", "2aHit", "1c", 
                            "1b", "1a"))
ma$predicted = factor(ma$predicted, levels = c("Yes", "No"),
                      labels = c("Hypothesis predicts\npositive effect\n","Hypothesis predicts\nnull effect\n"))

ma$marker_size = factor(3 + 1*(ma$study=="Positive Predictions") + 1*(ma$study=="Null Predictions"))

ma$study = factor(ma$study, levels = c("Positive Predictions", "Null Predictions", "", "8b", "8a", "6", "5bNoAttn", "5a",
                                       "7", "5bAttn", "4", "3", "2bCR", "2bHit", "2aCR", "2aHit", "1c", "1b", "1a"))
p1 <- ggplot(subset(ma, hypothesis=="Attention Check Hypothesis\n(Crossover Interaction)"), 
             aes(x=study, y=es, ymin=ci.l, ymax=ci.h,colour=predicted, group=predicted, shape=predicted)) + 
  geom_hline(yintercept=0,lty=3) +
  geom_point(aes(size = marker_size)) +
  scale_size_manual(name = "", values=c(3,5), guide=FALSE) +
  geom_linerange() + 
  ylim(-150,301) +
  geom_vline(xintercept=3,lty=2) +
  facet_grid(.~hypothesis, scales="free") + 
  xlab("Study") + 
  ylab("Reaction Time for Key Effect (ms)") +
  scale_colour_manual(name="", values=cbbwPal) +
  scale_shape_discrete(name="") +
  coord_flip() + theme(strip.background = element_rect(fill="#FFFFFF"), 
                       strip.text = element_text(size=14),
                       axis.text = element_text(size=14),
                       axis.title.x = element_text(size=16, vjust=-0.2),
                       axis.title.y = element_text(size=16, vjust=0.35),
                       legend.text = element_text(size=12),
                       panel.grid = element_blank(),
                       legend.position="left")


ma$study = factor(ma$study, levels = c("Positive Predictions", "Null Predictions", "", "7", "4", "3", "2bCR", "2aCR", 
                                       "8b", "8a", "6", "5bNoAttn", "5bAttn", "5a", "2bHit", "2aHit", "1c", "1b", "1a"))
p2 <- ggplot(subset(ma, hypothesis=="Automatic ToM Hypothesis\n(P-A+ > P-A-)"), 
             aes(x=study, y=es, ymin=ci.l, ymax=ci.h, colour=predicted, group=predicted, shape=predicted)) + 
  geom_hline(yintercept=0,lty=3) +
  geom_point(aes(size = marker_size)) +
  scale_size_manual(name = "", values=c(3,5), guide=FALSE) +
  geom_linerange() + 
  ylim(-150,301) +
  geom_vline(xintercept=3,lty=2) +
  facet_grid(.~hypothesis, scales="free") + 
  xlab("Study") + 
  ylab("Reaction Time for Key Effect (ms)") +
  scale_colour_manual(name="", values=cbbwPal) +
  scale_shape_discrete(name="") +
  coord_flip() + theme(strip.background = element_rect(fill="#FFFFFF"), 
                       strip.text = element_text(size=14),
                       axis.text = element_text(size=14),
                       axis.title.x = element_text(size=16, vjust=-0.2),
                       axis.title.y = element_text(size=16, vjust=0.35),
                       legend.text = element_text(size=12),
                       panel.grid = element_blank())

multiplot(p1, p2, cols=2) #15 by 7
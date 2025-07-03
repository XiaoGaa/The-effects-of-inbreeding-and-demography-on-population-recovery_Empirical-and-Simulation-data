library(readxl)
library(MASS)
library(dplyr)
library(ggplot2)
library(cowplot)
require(PMCMR) 
library(multcomp)
library(reshape2)
library(data.table)
library(unpivotr)
library(ggpubr)
library(ggrepel)
library(MetBrewer)
library(ggpubr)

########################Ibis restoration#####################################
data = read.csv("C:\\data\\Population Info_main.csv")


length(unique(data$Repeat))
str(data)
data$Repeat<-as.factor(data$Repeat)
data$SD<-as.numeric(data$SD)

########################Figure 2A #############################
group<- as.data.table(data)
tapply(data$Season, data$Repeat,max)
MaxAll<-group[group[, .I[Season == max(Season)], by=Repeat]$V1]
ExtintRea<-MaxAll[MaxAll$PopulationSize<9000,]
RevivalRea<-MaxAll[MaxAll$PopulationSize>9000,]
RevivalRea<-RevivalRea[1:91,]


# Filter out those rows from data
Extincts <- data[(data$Repeat %in% ExtintRea$Repeat), ]
Extincts$Fate<-"Extinct"
Revivals <- data[(data$Repeat %in% RevivalRea$Repeat), ]
Revivals$Fate<-"Revival"

Popu_recover<-rbind(Revivals, Extincts)
Popu_recover$Fate <- factor(Popu_recover$Fate, levels = c("Revival","Extinct"))

str(Popu_recover)
Popu_recover$Repeat<-as.numeric(Popu_recover$Repeat)
annot_df <- data.frame(
  Season = 70,                         # x position (adjust as needed)
  PopulationSize = 3.8,                # y position (log10-scale is ~0.8 = pop size ~6.3)
  label = "Log10 (9000)"
)



new_data <- expand.grid(
  Season = -1,
  Repeat = unique(Popu_recover$Repeat)
)
new_data$PopulationSize <- 7

Popu_recover <- bind_rows(Popu_recover, new_data)

new_data$Fate <- sapply(new_data$Repeat, function(r) {
  # pull the first Fate matching Repeat
  Popu_recover$Fate[Popu_recover$Repeat == r][1]
})

# Then just append
Popu_recover_extended <- rbind(Popu_recover[,c(1:3,7)], new_data)




r2<-ggplot(Popu_recover_extended, aes(x =Season+1, y = log10(PopulationSize), color = Fate, group = interaction(Repeat,Fate)))+
  geom_step(size=0.8, alpha = 0.4)+theme_bw()+theme(legend.position = "none")+
  scale_x_continuous(limit = c(-1,100))+ylab("Log10 (population size)\n")+ 
  geom_hline(yintercept=log10(9000), linetype="dashed", size = 0.8,color = "grey68")+
  geom_point(x=0,y=log10(7),  size = 1.5,color = "grey25")+
  scale_y_continuous(limit = c(0,4.1), breaks= c(0,0.5,1,1.5,2,2.5,3,3.5,4))+  
  scale_color_manual(values=c("yellowgreen","#68228B"))+xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=14),
        axis.title.x =element_text(size=16, vjust=0.5),strip.text.x = element_text(size = 14),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5))+
  geom_text(data = annot_df,
            aes(x = Season, y = PopulationSize, label = label),
            inherit.aes = FALSE,
            color = "grey50", size = 5, hjust = 0)

r2

####################Figure 2B#############################
ExtintRea<-MaxAll[MaxAll$PopulationSize<9000,]
RevivalRea<-MaxAll[MaxAll$PopulationSize>9000,]
ExtintRea$Fate<-"Extinct"
RevivalRea$Fate<-"Revival"
Max<-rbind(ExtintRea,RevivalRea)
Max$Repeat<-as.numeric(Max$Repeat)
Max$Fate <- factor(Max$Fate, levels = c("Revival","Extinct"))
median(RevivalRea$Season)
median(RevivalRea$AverageIC)

###
r1<-ggplot(Max, aes(x = (Season),fill = Fate))+geom_bar(size = 1,alpha = 0.8,position="identity")+theme_bw()+
  xlab("Time to outcome (year)") + xlim(-1,100)+ylab("Frequency \n")+
  geom_vline(xintercept= 41,linetype="dashed", size = 0.8,color = "grey68")+
  scale_y_continuous(limit = c(0,35), breaks= c(0,5,10,15,20,25,30,35))+guides(fill=guide_legend(title="Outcome"))+
  theme(legend.position = c(0.18,0.85))+ 
  scale_fill_manual(values=c("yellowgreen","#68228B"), labels = c("Restoration", "Extinction"))+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5))+
  theme(axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        legend.text = element_text(size = 12), legend.title=element_text(size = 14),
        strip.text.x = element_text(size = 14))
r1


########Inbreeding trends############
data = read.csv("C:\\data\\Population Info_main.csv")

data_numer<-data    ##needed for making figures
length(unique(data$Repeat))
str(data)
data$Repeat<-as.factor(data$Repeat)
data$SD<-as.numeric(data$SD)

###Generation info######
group<- as.data.table(data)
tapply(data$Season, data$Repeat,max)
MaxAll<-group[group[, .I[Season == max(Season)], by=Repeat]$V1]
ExtintRea<-MaxAll[MaxAll$PopulationSize<9000,]
# Create a composite key (Repeat + Season) to match rows uniquely
Extint_keys <- paste(ExtintRea$Repeat)
MaxAll_keys <- paste(MaxAll$Repeat)
data_keys <- paste(data$Repeat)

# Filter out those rows from data
Revivals_Max <- MaxAll[!(MaxAll_keys %in% Extint_keys), ]


Revivals <- data[!(data_keys %in% Extint_keys), ]
Revivals$Fate<-"Revival"
Extincts <- data[(data_keys %in% Extint_keys), ]
Extincts$Fate<-"Extinct"
Popu_relation<-rbind(Revivals, Extincts)
Popu_relation$Fate <- factor(Popu_relation$Fate, levels = c("Revival","Extinct"))

r3<-ggplot(Popu_relation[Popu_relation$Repeat=="20",], aes(x = Season, y = AverageIC))+
  geom_ribbon(aes(ymin = AverageIC + SD, ymax = AverageIC - SD), fill = "#E0EEE0")+ 
  geom_line(aes(Season), size = 0.7,color = "#6B8E23",alpha = 0.8)+#geom_point(color = "olivedrab4",size = 0.8)+
  scale_x_continuous(limits =c(0,60), breaks = c(0, 20,40,60,80,100,120,140))+
  scale_y_continuous(limits =c(-0.06,0.3), breaks = c(0, 0.1,0.2,0.3,0.4,0.5))+xlab("Year")+
  ylab("Average inbreeding coefficients\n")+theme_bw()+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5))+
  theme(axis.text.y = element_text(size=14),axis.title.y =element_text(size=14, vjust=0.5),
        strip.text.x = element_text(size = 14))
r3


ggarrange(r1,r2,r3,nrow = 1,labels = c("(A)","(B)","(C)"))


################Figure 3. Reintroduction###########################
#########Figue 3B. Stepping-stone strategy##
data_Reintro = read.csv("C:\\data\\Population Info_ReintroSeq_One.csv")
StepStone<-data_Reintro[(data_Reintro$ReintroPairs==3& data_Reintro$Repeat==39)|(data_Reintro$ReintroPairs==11 &data_Reintro$Repeat==229),]
str(StepStone)
StepStone$AverageIC<-as.numeric(StepStone$AverageIC)
StepStone$SD<-as.numeric(StepStone$SD)

table(factor(StepStone$Repeat))
a<-seq(1,263)
b<-seq(1,381)
StepStone$Year<-c(a,b)
colours <- met.brewer("VanGogh1", 7)
colours <- met.brewer("Hokusai3", 7)
StartPoints<-StepStone[StepStone$Season==0,]


r3.2<-ggplot(StepStone, aes(x = Year, y = AverageIC,fill = factor(ReintroPairs)))+
  geom_ribbon(aes(ymin = AverageIC + SD, ymax = AverageIC - SD), alpha = 0.7)+
  theme_cowplot()+theme(legend.position = "none")+scale_x_continuous(limits =c(0,500), breaks = seq(0, 550, by = 100))+scale_y_continuous(limits =c(0.05,0.7), breaks = seq(0,0.8, by= 0.1))+
  geom_line(aes(Year), size = 0.7,color = "#636363",alpha = 0.5)+
  xlab("Year")+ylab(" ")+theme_bw()+
  scale_fill_manual(values=c(colours[2],colours[3]), labels = c("3", "11"))+
  guides(fill=guide_legend(title="Number of pairs"))+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        legend.position = "none",plot.title =  element_text(size=16))+
  geom_point(data = StartPoints,aes(x = Year, y = AverageIC),color = ("#5D478B"))+
  ggtitle("Sequential")+ 
  annotate(geom="text", x=450, y=0.56, label="11 pairs",size = 5,color="black")+
  annotate(geom="text", x=320, y=0.64, label="3 pairs",size = 5,color="black")

r3.2



####Figure 3c Leader-follower approach##

data_FLF_Tran = read.csv("C:\\data\\Population Info_FLF_One.csv")
data_FLF_Rest = read.csv("C:\\data\\Population Info_FLF_Orig.csv")

str(data_FLF_Tran)
Rest<-data_FLF_Rest[data_FLF_Rest$Repeat ==8,]
Tran<-data_FLF_Tran[data_FLF_Tran$Repeat==8,]
Tran$Season<-Tran$Season+28
Tran1<-Tran[,c(1:5)]
Total<-rbind(Rest,Tran1)
Total$Popu<-"Leader"
Tran2<-Tran[,c(1,2,6:8)]
colnames(Tran2)[3:5]<-c("PopuSize","AvgIC","SD")
Tran2$Popu<-"Follower3"
Tran3<-Tran[,c(1,2,9:11)]
colnames(Tran3)[3:5]<-c("PopuSize","AvgIC","SD")
Tran3$Popu<-"Firework3"
Tran4<-Tran[,c(1,2,12:14)]
colnames(Tran4)[3:5]<-c("PopuSize","AvgIC","SD")
Tran4$Popu<-"Follower11"
Tran5<-Tran[,c(1,2,15:17)]
colnames(Tran5)[3:5]<-c("PopuSize","AvgIC","SD")
Tran5$Popu<-"Firework11"

data<-rbind(Total,Tran2,Tran3, Tran4, Tran5)
legend_order <- c( "Follower11", "Follower3", "Firework11", "Firework3","Leader")



##split firework and immigrant##
##Recurrent##
colours <- c(
  "Follower11" = "#FFE4E1",
  "Follower3"  = "#CD96CD",
  "Leader"     = "grey70"
)
legend_order <- c("Leader","Follower11", "Follower3")
df<-data[data$Popu!="Firework11"&data$Popu!="Firework3",]
df$Popu <- factor(df$Popu, levels = c("Leader", "Follower3", "Follower11"))
df <- df[order(df$Popu), ]  # this ensures Follower11 is plotted last

r3.3_re<-ggplot(df, aes(x = Season, y = AvgIC, color = Popu, group = Popu, fill = Popu))+
  geom_ribbon(aes(ymin = AvgIC - SD, ymax = AvgIC + SD), alpha = 0.5, color = NA)+
  theme_cowplot()+scale_x_continuous(limits =c(0,70), breaks = seq(0, 70, by = 10))+ 
  scale_y_continuous(limits =c(-0.06,0.40), breaks = seq(-0.1,0.40, by= 0.1))+
  geom_line(data = subset(df, Popu == "Leader"), aes(color = Popu), size = 1.2) +
  geom_line(data = subset(df, Popu != "Leader"), aes(color = Popu), size = 1.2) + 
  #geom_segment(data = arrows_df,aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.2, "cm")),size = 1,color = "goldenrod4")+
  xlab("Year")+theme_bw()+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=14, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=14, vjust=0.5),
        plot.title =  element_text(size=14)) +ylab(" ")+
  scale_color_manual(values = colours, 
                     breaks = legend_order, 
                     labels = c("Leader","Follower 11 pairs","Follower 3 pairs"))+
  scale_fill_manual(values = colours, 
                    breaks = legend_order, 
                    labels = c("Leader","Follower 11 pairs","Follower 3 pairs"))+theme(legend.position = "none")+
  labs(color = "Reintroduction scenario", fill = "Reintroduction scenario") +  # <- Legend title here
  annotate(geom="text", x=58.3, y=0.223, label="3 pairs",size = 4.5,color="black")+
  annotate(geom="text", x=59, y=0.182, label="11 pairs",size = 4.5,color="black")+
  annotate(geom="text", x=20, y=0.1, label="Source",size = 4.5,color="black")+
  annotate("point", x = 29, y = 0.176, size = 2, shape = 17, color = "black") +
  annotate("point", x = 31, y = 0.179, size = 1.5, color = "#CD69C9")+
  annotate("point", x = 34, y = 0.180, size = 1.5, color = "#CD69C9")+
  annotate("point", x = 37, y = 0.1796, size = 1.5, color = "#CD69C9")+
  annotate("point", x = 40, y = 0.1868, size = 1.5, color = "#CD69C9")+
  annotate("point", x = 43, y = 0.1856, size = 1.5, color = "#CD69C9")+
  annotate("point", x = 31, y = 0.21, size = 1.5, color = "#7A378B")+
  annotate("point", x = 34, y = 0.208, size = 1.5, color = "#7A378B")+
  annotate("point", x = 37, y = 0.202, size = 1.5, color =  "#7A378B")+
  annotate("point", x = 40, y = 0.2168, size = 1.5, color = "#7A378B")+
  annotate("point", x = 43, y = 0.207, size = 1.5, color = "#7A378B")+
  ggtitle("Firework (with immigrants)")
r3.3_re




##One time
colours <- c(
  "Firework11" = "#FFA500",
  "Firework3"  = "#CD8500",
  "Leader"     = "grey70"
)
legend_order <- c("Leader", "Firework11", "Firework3")
df1<-data[data$Popu!="Follower11"&data$Popu!="Follower3",]

r3.3_One<-ggplot(df1, aes(x = Season, y = AvgIC, color = Popu, group = Popu, fill = Popu))+
  geom_ribbon(aes(ymin = AvgIC - SD, ymax = AvgIC + SD), alpha = 0.5, color = NA)+
  theme_cowplot()+scale_x_continuous(limits =c(0,70), breaks = seq(0, 70, by = 10))+ 
  scale_y_continuous(limits =c(-0.06,0.40), breaks = seq(-0.1,0.40, by= 0.1))+
  geom_line(data = subset(df1, Popu == "Leader"), aes(color = Popu), size = 1.2) +
  geom_line(data = subset(df1, Popu != "Leader"), aes(color = Popu), size = 1.2) + 
  #geom_segment(data = arrows_df,aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.2, "cm")),size = 1,color = "goldenrod4")+
  xlab("Year")+theme_bw()+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=14, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=14, vjust=0.5),
        plot.title =  element_text(size=14)) +ylab(" ")+
  scale_color_manual(values = colours, 
                     breaks = legend_order, 
                     labels = c("Leader","Firework 11 pairs","Firework 3 pairs"))+
  scale_fill_manual(values = colours, 
                    breaks = legend_order, 
                    labels = c("Leader","Firework 11 pairs","Firework 3 pairs"))+theme(legend.position = "none")+
  labs(color = "Reintroduction scenario", fill = "Reintroduction scenario") + ylab("Average inbreeding coefficients")+ # <- Legend title here
  annotate("point", x = 29, y = 0.176, size = 2, shape = 17, color = "black") +
  annotate(geom="text", x=57.9, y=0.26, label="3 pairs",size = 4.5,color="black")+
  annotate(geom="text", x=59, y=0.228, label="11 pairs",size = 4.5,color="black")+
  annotate(geom="text", x=20, y=0.1, label="Source",size = 4.5,color="black")+
  ggtitle("Firework (no immigrant)")
r3.3_One


ggarrange(r3.3_One,r3.3_re,r3.2, nrow = 1,labels = c("(A1)","(A2)","(B)"))


##############Figure 4. Generalized Demography#####################

#####With inbreeding effect##
data_general_diver3 = read.csv("C:\\data\\Population Info_all.csv")
data_general_diver3<-data_general_diver3[data_general_diver3$ClutchSize!= 4,]
data_general_diver4 = read.csv("C:\\data\\Population Info_5-6.csv")
data_general_diver_With<-rbind(data_general_diver3,data_general_diver4)
#####Without inbreeding effect##
data_general_diver1 = read.csv("C:\\data\\Population Info_NOIC_all.csv")
data_general_diver2 = read.csv("C:\\data\\Population Info_NOIC_5-6.csv")
data_general_diver_Without<-rbind(data_general_diver1,data_general_diver2)

### dataset of species mortality####
data_spe = read.csv("C:\\data\\analysis_Species_1.csv")
data_points<-data_spe[data_spe$ClutchSize<7,2:7]
colnames(data_points)[2:3]<-c("Para_j","Para_a")
data_points$ClutchSize<-as.factor(data_points$ClutchSize)
data_points$difference<-1




###Figrue 4A. without inbreeding##
data_general_diver_Without$ParaMorJuv<-format(round(data_general_diver_Without$ParaMorJu,2),nsmall=2)
data_general_diver_Without$ParaMorPar<-format(round(data_general_diver_Without$ParaMorPa,2),nsmall=2)
data_general_diver_Without$Repeat<-as.factor(data_general_diver_Without$Repeat)
data_general_diver_Without$ParaMorJuv<-as.factor(data_general_diver_Without$ParaMorJuv)
data_general_diver_Without$ParaMorPar<-as.factor(data_general_diver_Without$ParaMorPar)

data_general_diver_Without<-as.data.table(data_general_diver_Without)

MaxAll<-data_general_diver_Without[data_general_diver_Without[, .I[Season == max(Season)], by=list(data_general_diver_Without$ParaMorPar,data_general_diver_Without$ParaMorJuv,data_general_diver_Without$ClutchSize,data_general_diver_Without$Repeat)]$V1]
ExtintRea<-MaxAll[MaxAll$PopulationSize<5000,]
ExtintRea$Remove<-1
RemoveCol<-ExtintRea[,c(1,2,3,4,10)]

join_df_diver<-merge(data_general_diver_Without,RemoveCol, by.x = c( "ParaMorJuv", "ParaMorPar","ClutchSize","Repeat"), by.y = c( "ParaMorJuv", "ParaMorPar","ClutchSize","Repeat"),
                     all.x=TRUE,all.y = TRUE)
join_df_diver$Remove[is.na(join_df_diver$Remove)]<-0

##probability of revive and extinct
join_df_diver<-as.data.table(join_df_diver)
MaxGen<-join_df_diver[join_df_diver[, .I[Season == max(Season)], by=list(join_df_diver$ParaMorPar,join_df_diver$ParaMorJuv,join_df_diver$ClutchSize,join_df_diver$Repeat)]$V1]

MaxGen$Remove<-as.factor(MaxGen$Remove)
MaxGen$Fate[MaxGen$Remove==0]<-"Revival"
MaxGen$Fate[MaxGen$Remove==1]<-"EXtinct"
MaxGen$Comb<- paste(MaxGen$ParaMorJuv, MaxGen$ParaMorPar, sep="_")

A<-table(MaxGen$Comb, MaxGen$Fate,MaxGen$ClutchSize)
percent<-data.frame(A)
colnames(percent)<-c("Para","Fate","ClutchSize","Freq")
percent$Para_j<-substr(percent$Para,1,4)
percent$Para_a<-substr(percent$Para,6,9)
str(percent)
success<-percent[percent$Para_a!="0.55" & percent$Para_a!="0.60",]
success<-success[success$Fate=="Revival",]

Clutch.labs<-c("1 egg","2 eggs","3 eggs","4 eggs","5 eggs","6 eggs")
names(Clutch.labs) <- c(1,2,3,4,5,6)
str(data_points)
success$Para_j<-as.numeric(success$Para_j)
success$Para_a<-as.numeric(success$Para_a)
data_points$Freq<-1


ZoomSuccess<-success[success$ClutchSize=="3",]
data_points_zoom<-data_points[data_points$ClutchSize=="3",]
Clutch.labs_zoom<-c("3 eggs")
names(Clutch.labs_zoom) <- c(3)
Name<-c( "A","B","C")
JMor<-c(0.22,0.55, 0.7)
AMor<-c(0.05, 0.30,0.4)
Spot<-data.frame (Name, JMor, AMor)
P1<-ggplot(ZoomSuccess, aes(x=Para_j, y=Para_a, fill= Freq/100)) +
  geom_tile()+scale_fill_gradient(high="yellowgreen", low="#8B7B8B") +theme_cowplot()+
  xlab("Juvenile mortality")+ylab("Adult mortality")+guides(fill=guide_legend(title="Restoration\nprobability"))+
  scale_x_continuous(limit = c(-0.025,0.825), breaks= c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), expand = c(0,0))+
  scale_y_continuous(limit = c(-0.025,0.525), breaks= c(0,0.1,0.2,0.3,0.4,0.5), expand = c(0,0))+
  geom_point(data = Spot, aes(JMor, AMor),size = 1.5, inherit.aes = FALSE)+
  geom_text(data = Spot, aes(x = JMor, y = AMor, label = as.character(as.roman(1:3)),family = "serif"), inherit.aes = FALSE, 
            hjust = 0.5, vjust = -1, size = 5, color = "black")+
  ##annotate("text", x = 0.2, y = 0.05, label = "(Nipponia nippon)",fontface="italic", size = 4, color = "black", hjust = 0.3, vjust = 1.3)+
  ##theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        strip.text.x = element_text(size=14),plot.title = element_text(hjust = 0.5))+ggtitle("No inbreeding effect")




##Figure 4B. with inbreeding##
data_general_diver_With$ParaMorJuv<-format(round(data_general_diver_With$ParaMorJu,2),nsmall=2)
data_general_diver_With$ParaMorPar<-format(round(data_general_diver_With$ParaMorPa,2),nsmall=2)
data_general_diver_With$Repeat<-as.factor(data_general_diver_With$Repeat)
data_general_diver_With$ParaMorJuv<-as.factor(data_general_diver_With$ParaMorJuv)
data_general_diver_With$ParaMorPar<-as.factor(data_general_diver_With$ParaMorPar)

data_general_diver_With<-as.data.table(data_general_diver_With)

MaxAll<-data_general_diver_With[data_general_diver_With[, .I[Season == max(Season)], by=list(data_general_diver_With$ParaMorPar,data_general_diver_With$ParaMorJuv,data_general_diver_With$ClutchSize,data_general_diver_With$Repeat)]$V1]
ExtintRea<-MaxAll[MaxAll$PopulationSize<5000,]
ExtintRea$Remove<-1
RemoveCol<-ExtintRea[,c(1,2,3,4,10)]

join_df_diver<-merge(data_general_diver_With,RemoveCol, by.x = c( "ParaMorJuv", "ParaMorPar","ClutchSize","Repeat"), by.y = c( "ParaMorJuv", "ParaMorPar","ClutchSize","Repeat"),
                     all.x=TRUE,all.y = TRUE)
join_df_diver$Remove[is.na(join_df_diver$Remove)]<-0

##probability of revive and extinct
join_df_diver<-as.data.table(join_df_diver)
MaxGen<-join_df_diver[join_df_diver[, .I[Season == max(Season)], by=list(join_df_diver$ParaMorPar,join_df_diver$ParaMorJuv,join_df_diver$ClutchSize,join_df_diver$Repeat)]$V1]

MaxGen$Remove<-as.factor(MaxGen$Remove)
MaxGen$Fate[MaxGen$Remove==0]<-"Revival"
MaxGen$Fate[MaxGen$Remove==1]<-"EXtinct"
MaxGen$Comb<- paste(MaxGen$ParaMorJuv, MaxGen$ParaMorPar, sep="_")

A<-table(MaxGen$Comb, MaxGen$Fate,MaxGen$ClutchSize)
percent<-data.frame(A)
colnames(percent)<-c("Para","Fate","ClutchSize","Freq")
percent$Para_j<-substr(percent$Para,1,4)
percent$Para_a<-substr(percent$Para,6,9)
str(percent)
success<-percent[percent$Para_a!="0.55" & percent$Para_a!="0.60",]
success<-success[success$Fate=="Revival",]

Clutch.labs<-c("1 egg","2 eggs","3 eggs","4 eggs","5 eggs","6 eggs")
names(Clutch.labs) <- c(1,2,3,4,5,6)
str(data_points)
success$Para_j<-as.numeric(success$Para_j)
success$Para_a<-as.numeric(success$Para_a)
data_points$Freq<-1


ZoomSuccess<-success[success$ClutchSize=="3",]
data_points_zoom<-data_points[data_points$ClutchSize=="3",]
Clutch.labs_zoom<-c("3 eggs")
names(Clutch.labs_zoom) <- c(3)
Name<-c( "A","B","C")
JMor<-c(0.22,0.55, 0.7)
AMor<-c(0.05, 0.30,0.4)
Spot<-data.frame (Name, JMor, AMor)
P2<-ggplot(ZoomSuccess, aes(x=Para_j, y=Para_a, fill= Freq/100)) +
  geom_tile()+scale_fill_gradient(high="yellowgreen", low="#8B7B8B") +theme_cowplot()+
  xlab("Juvenile mortality")+ylab("Adult mortality")+guides(fill=guide_legend(title="Restoration\nprobability"))+
  scale_x_continuous(limit = c(-0.025,0.825), breaks= c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), expand = c(0,0))+
  scale_y_continuous(limit = c(-0.025,0.525), breaks= c(0,0.1,0.2,0.3,0.4,0.5), expand = c(0,0))+
  geom_point(data = Spot, aes(JMor, AMor),size =1.5, inherit.aes = FALSE)+
  geom_text(data = Spot, aes(x = JMor, y = AMor, label = as.character(as.roman(1:3)),family = "serif"), inherit.aes = FALSE, 
            hjust = 0.5, vjust = -1, size = 5, color = "black")+
  ##annotate("text", x = 0.2, y = 0.05, label = "(Nipponia nippon)",fontface="italic", size = 4, color = "black", hjust = 0.3, vjust = 1.3)+
  ##theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        strip.text.x = element_text(size=14),plot.title = element_text(hjust = 0.5))+ggtitle("Inbreeding depression")


###Figure 4C##
data_general_diver1 = read.csv("C:\\data\\Population Info_NOIC_all.csv")
data_general_diver2 = read.csv("C:\\data\\Population Info_NOIC_5-6.csv")
data_general_diver_NoIC<-rbind(data_general_diver1,data_general_diver2)


data_general_diver_NoIC$ParaMorJuv<-format(round(data_general_diver_NoIC$ParaMorJu,2),nsmall=2)
data_general_diver_NoIC$ParaMorPar<-format(round(data_general_diver_NoIC$ParaMorPa,2),nsmall=2)
data_general_diver_NoIC$Repeat<-as.factor(data_general_diver_NoIC$Repeat)
data_general_diver_NoIC$ParaMorJuv<-as.factor(data_general_diver_NoIC$ParaMorJuv)
data_general_diver_NoIC$ParaMorPar<-as.factor(data_general_diver_NoIC$ParaMorPar)

data_general_diver_NoIC<-as.data.table(data_general_diver_NoIC)

MaxAll_NoIC<-data_general_diver_NoIC[data_general_diver_NoIC[, .I[Season == max(Season)], by=list(data_general_diver_NoIC$ParaMorPar,data_general_diver_NoIC$ParaMorJuv,data_general_diver_NoIC$ClutchSize,data_general_diver_NoIC$Repeat)]$V1]
ExtintRea<-MaxAll_NoIC[MaxAll_NoIC$PopulationSize<5000,]
ExtintRea$Remove<-1
RemoveCol<-ExtintRea[,c(1,2,3,4,10)]

join_df_diver_NoIC<-merge(data_general_diver_NoIC,RemoveCol, by.x = c( "ParaMorJuv", "ParaMorPar","ClutchSize","Repeat"), by.y = c( "ParaMorJuv", "ParaMorPar","ClutchSize","Repeat"),
                          all.x=TRUE,all.y = TRUE)
join_df_diver_NoIC$Remove[is.na(join_df_diver_NoIC$Remove)]<-0
join_df_diver_NoIC<-as.data.table(join_df_diver_NoIC)
MaxGen<-join_df_diver_NoIC[join_df_diver_NoIC[, .I[Season == max(Season)], by=list(join_df_diver_NoIC$ParaMorPar,join_df_diver_NoIC$ParaMorJuv,join_df_diver_NoIC$ClutchSize,join_df_diver_NoIC$Repeat)]$V1]

MaxGen$Remove<-as.factor(MaxGen$Remove)
MaxGen$Fate[MaxGen$Remove==0]<-"Revival"
MaxGen$Fate[MaxGen$Remove==1]<-"EXtinct"
MaxGen$Comb<- paste(MaxGen$ParaMorJuv, MaxGen$ParaMorPar, sep="_")

A<-table(MaxGen$Comb, MaxGen$Fate,MaxGen$ClutchSize)
percent<-data.frame(A)
colnames(percent)<-c("Para","Fate","ClutchSize","Freq")
percent$Para_j<-substr(percent$Para,1,4)
percent$Para_a<-substr(percent$Para,6,9)
str(percent)
success_NoIC<-percent[percent$Para_a!="0.55" & percent$Para_a!="0.60",]
success_NoIC<-success_NoIC[success_NoIC$Fate=="Revival",]
NoIC<-success_NoIC[,4]
Diff<-cbind(success,NoIC)
Diff$difference<-(Diff$NoIC-Diff$Freq)/100
str(Diff)
Clutch.labs<-c("1 egg", "2 eggs","3 eggs","4 eggs","5 eggs","6 eggs")
names(Clutch.labs) <- c(1,2,3,4,5,6)
Diff$Para_j<-as.numeric(Diff$Para_j)
Diff$Para_a<-as.numeric(Diff$Para_a)
Diff1<-Diff[Diff$NoIC!=0,]
Diff1$difference[Diff1$difference<0]<-0.00

DiffPlot<-Diff1[Diff1$ClutchSize=="3",]

str(DiffPlot)

P3<-ggplot(DiffPlot, aes(x=Para_j, y=Para_a, fill= difference))+
  geom_tile()+scale_fill_gradient(high="darkorange2", low="cornsilk1") +theme_cowplot()+
  xlab("Juvenile mortality")+ylab("Adult mortality")+guides(fill=guide_legend(title="Net effects"))+
  scale_x_continuous(limit = c(-0.025,0.825), breaks= c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), expand = c(0,0))+
  scale_y_continuous(limit = c(-0.025,0.525), breaks= c(0,0.1,0.2,0.3,0.4,0.5), expand = c(0,0))+
  geom_point(data = Spot, aes(JMor, AMor),size = 1.5, inherit.aes = FALSE)+
  geom_text(data = Spot, aes(x = JMor, y = AMor, label = as.character(as.roman(1:3)),family = "serif"), inherit.aes = FALSE, 
            hjust = 0.5, vjust = -1, size = 5, color = "black")+
  ##annotate("text", x = 0.2, y = 0.05, label = "(Nipponia nippon)",fontface="italic", size = 4, color = "black", hjust = 0.3, vjust = 1.3)+
  ##theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        strip.text.x = element_text(size=14),plot.title = element_text(hjust = 0.5))+ggtitle("Net effects of inbreeding")


legend1 <- get_legend(P1)
P1_noleg <- P1 + theme(legend.position = "none") +xlab("") 
P2_noleg <- P2 + theme(legend.position = "none") +ylab("")
legend3 <- get_legend(P3)
P3_noleg <- P3 + theme(legend.position = "none") +xlab("")+ylab("")
#P4 <- P4 + ylab("")
last_two <- plot_grid(legend1, legend3, ncol = 1)

final<-plot_grid(P1_noleg, P2_noleg, P3_noleg,last_two, ncol = 4, rel_widths = c(1,1,1,0.25),labels = c("(A)","(B)","(C)",""),
                 label_x = 0.05)




####Figure 5###
##plot all the species_in supplementary
ggplot(Diff1, aes(x=Para_j, y=Para_a, fill= difference)) + facet_wrap(.~ClutchSize,nrow=2,labeller = labeller(ClutchSize = Clutch.labs))+
  geom_tile()+scale_fill_gradient(high="darkorange2", low="cornsilk1") +theme_cowplot()+
  xlab("Juvenile Mortality")+ylab("Adult Mortality")+guides(fill=guide_legend(title="Net effect of \ninbreeding depression"))+
  scale_x_continuous(limit = c(-0.05,0.85), breaks= c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8))+
  scale_y_continuous(limit = c(-0.05,0.55), breaks= c(0,0.1,0.2,0.3,0.4,0.5))+
  geom_point(data = data_points,colour="black",size = 2)+
  geom_text_repel(data = data_points, aes(x = Para_j, y = Para_a, label = ShortLatinName), color = "black",vjust = 0.5, fontface="italic",size = 3.5,
                  box.padding = 0.5)





##########################Supplementary materials################################
###Supplement 1#####
data = read.csv("C:\\data\\Population Info_main.csv")

data_numer<-data    ##needed for making figures
length(unique(data$Repeat))
str(data)
data$Repeat<-as.factor(data$Repeat)
data$SD<-as.numeric(data$SD)

###Generation info######
group<- as.data.table(data)
tapply(data$Season, data$Repeat,max)
MaxAll<-group[group[, .I[Season == max(Season)], by=Repeat]$V1]
ExtintRea<-MaxAll[MaxAll$PopulationSize<9000,]
mean(ExtintRea$Season)
median(ExtintRea$Season)
sd(ExtintRea$Season)
# Create a composite key (Repeat + Season) to match rows uniquely
Extint_keys <- paste(ExtintRea$Repeat)
MaxAll_keys <- paste(MaxAll$Repeat)
data_keys <- paste(data$Repeat)

# Filter out those rows from data
Revivals_Max <- MaxAll[!(MaxAll_keys %in% Extint_keys), ]


Revivals <- data[!(data_keys %in% Extint_keys), ]
Revivals$Fate<-"Restoration"
Extincts <- data[(data_keys %in% Extint_keys), ]
Extincts$Fate<-"Extinction"
Popu_relation<-rbind(Revivals, Extincts)
Popu_relation$Fate <- factor(Popu_relation$Fate, levels = c("Restoration","Extinction"))

new_data <- expand.grid(
  Season = -1,
  Repeat = unique(Popu_relation$Repeat)
)
new_data$PopulationSize <- 7

Popu_relation <- bind_rows(Popu_relation, new_data)

new_data$Fate <- sapply(new_data$Repeat, function(r) {
  # pull the first Fate matching Repeat
  Popu_relation$Fate[Popu_relation$Repeat == r][1]
})

# Then just append
Popu_relation_extended <- rbind(Popu_relation[,c(1:3,7)], new_data)


mean(Revivals_Max$Season)
median(Revivals_Max$Season)
sd(Revivals_Max$Season)
mean(Revivals_Max$AverageIC)
sd(Revivals_Max$AverageIC)
max(Revivals_Max$Season)
min(Revivals_Max$Season)
str(Popu_relation_extended)


S2_1<-ggplot(Popu_relation_extended[!is.na(Popu_relation_extended$Fate),], aes(x =Season+1, y = PopulationSize, color = Fate, group = interaction(Repeat,Fate)))+geom_step(size=0.8, alpha = 0.4)+
  theme_bw()+theme(legend.position = "none")+ 
  scale_x_continuous(limit = c(0,140),,breaks = c(0, 20,40,60,80,100,120,140))+ylab("Population size\n")+geom_hline(yintercept=9000, linetype="dashed", size = 0.8,color = "grey68")+
  scale_y_continuous(limit = c(0,11000), breaks= c(0,2000,4000,6000,8000,10000))+  scale_color_manual(values=c("yellowgreen","#68228B"))+xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5))+
  theme(axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5))
  
S2_1



annot_df1 <- data.frame(
  Season = 100,                         # x position (adjust as needed)
  PopulationSize = 3.8,                # y position (log10-scale is ~0.8 = pop size ~6.3)
  label = "Log10 (9000)"
)

S2_2<-ggplot(Popu_relation_extended[!is.na(Popu_relation_extended$Fate),], aes(x =Season+1, y = log10(PopulationSize), color = Fate, group = interaction(Repeat,Fate)))+geom_step(size=0.8, alpha = 0.4)+theme_bw()+
  scale_x_continuous(limit = c(0,140),breaks = c(0, 20,40,60,80,100,120,140))+ylab("Log10 (population size)\n")+geom_hline(yintercept=log10(9000), linetype="dashed", size = 0.8,color = "grey68")+
  scale_y_continuous(limit = c(0,4.1), breaks= c(0,0.5,1,1.5,2,2.5,3,3.5,4))+  scale_color_manual(values=c("yellowgreen","#68228B"),  labels = c("Restoration", "Extinction"))+
  xlab("Year")+guides(color=guide_legend(title="Outcome"))+
  geom_point(x=0,y=log10(7),  size = 1.5,color = "grey25")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5))+
  theme(axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        legend.text = element_text(size = 12), legend.title=element_text(size = 14))
  
S2_2
legend1 <- get_legend(S2_2)
S2_2noleg <- S2_2 + theme(legend.position = "none")

S2<-plot_grid(S2_1,S2_2noleg,legend1,labels = c("(A)","(B)"),nrow =1, rel_widths = c(1,1,0.3))
S2

###Supplement 3###
Median_Re<-median(Revivals_Max$AverageIC)
Mean_Re<-mean(Revivals_Max$AverageIC)


LineIC <- c(
  "Restoration" = "#A2CD5A",
  "Extinction"  = "#473C8B"
)
RibonIC <- c(
  "Restoration"="#E0EEE0",
  "Extinction"  = "#EEE0E5"
)
Popu_relation$Fate <- factor(Popu_relation$Fate, levels = c("Restoration", "Extinction"))
hline_df <- data.frame(Fate = factor("Restoration", levels = c("Restoration", "Extinction")),
                       Median_Re = Median_Re)

S3_1<-ggplot(Popu_relation, aes(x = Season, y = AverageIC, group = Repeat, color = Fate, fill = Fate))+
  geom_ribbon(aes(ymin = AverageIC + SD, ymax = AverageIC - SD), alpha = 0.6, color = NA)+ 
  geom_line(aes(Season), size = 0.4,alpha = 0.9)+#geom_point(color = "olivedrab4",size = 0.8)+
  scale_x_continuous(limits =c(0,140), breaks = c(0, 20,40,60,80,100,120,140))+xlab("Year")+
  ylab("Average inbreeding coefficients")+theme_bw()+geom_hline(data = hline_df,aes(yintercept=0.17), linetype="dashed", size = 0.8,color = "grey68")+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5))+
  theme(axis.text.y = element_text(size=14),axis.title.y =element_text(size=14, vjust=0.5),
        strip.text.x = element_text(size = 14))+
  scale_color_manual(values = LineIC)+scale_fill_manual(values = RibonIC)+
  facet_wrap(.~Fate)+
  theme(legend.position = "none")
  
S3_1


S3_2<-ggplot(Popu_relation, aes(x = Season, y = SD,group = Repeat, color = Fate))+
  geom_line(aes(Season), size = 0.4,alpha = 0.9)+#geom_point(color = "olivedrab4",size = 0.8)+
  scale_x_continuous(limits =c(0,140), breaks = c(0, 20,40,60,80,100,120,140))+xlab("Year")+
  ylab("Standard deviation of \n inbreeding coefficients")+theme_bw()+ylim(0,0.3)+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5))+
  theme(axis.text.y = element_text(size=14),axis.title.y =element_text(size=14, vjust=0.5),
        strip.text.x = element_text(size = 14))+scale_color_manual(values = LineIC)+
  facet_wrap(.~Fate)+theme(legend.position = "none")
S3_2
S3<-plot_grid(S3_1,S3_2,labels = c("(A)","(B)"),nrow =2, rel_widths = c(1,1),
              label_x = 0.0,          # Adjust horizontal position of labels
              label_y = 0.99  )
S3

##Supplement 4##
data_Reintro = read.csv("C:\\data\\Population Info_Reintro_One.csv")

Group<-rep(1:6, each=100)
data_Reintro<-as.data.table(data_Reintro)
MaxAll<-data_Reintro[data_Reintro[, .I[Season == max(Season)], by=list(data_Reintro$Repeat,data_Reintro$ReintroNo)]$V1]
ExtintRea<-MaxAll[MaxAll$HaveBorn<1000,]
Extint_keys <- paste(ExtintRea$Repeat, ExtintRea$Season, sep = "_")
MaxAll_keys <- paste(MaxAll$Repeat, MaxAll$Season, sep = "_")


Revivals <- MaxAll[!(MaxAll_keys %in% Extint_keys), ]
Revivals$Fate<-"Revival"
Extincts <- MaxAll[(MaxAll_keys %in% Extint_keys), ]
Extincts$Fate<-"Extinct"
Popu_Firework<-rbind(Revivals, Extincts)
Popu_Firework$Fate <- factor(Popu_Firework$Fate, levels = c("Revival","Extinct"))



pair.labs<-c("3 pairs","5 pairs","7 pairs","9 pairs","11 pairs","13 pairs")
names(pair.labs) <- c(3,5,7,9,11,13)
S4_1<-ggplot(data =Popu_Firework, aes(x = Season, y = AverageIC,color = Fate))+geom_point()+facet_wrap(.~ReNumPair,labeller = labeller(ReNumPair = pair.labs),nrow=1 )+
  ylim(0,0.8)+xlim(0,200)+theme_bw()+xlab("Time to outcome (year)")+ylab("Average inbreeding coefficients")+
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),axis.title.x =element_text(size=14, vjust=0.5), axis.title.y=element_text(size=14))+
  theme(strip.text.x = element_text(size=14))+theme(legend.text = element_text(size = 12),legend.title=element_text(size=14))+
  scale_color_manual(values = c("yellowgreen","#68228B"), labels = c("Extinction","Restoration"))+
  guides(color=guide_legend(title="Outcome"))+
  ggtitle("Firework approach (no immigrant)")+
  theme(plot.title = element_text(size = 16, face = "bold"))




data_Reintro = read.csv("C:\\data\\Population Info_ReintroSeq_One.csv")

data_Reintro<-as.data.table(data_Reintro)
MaxAll_Gen<-data_Reintro[data_Reintro[, .I[Season == max(Season)], by=list(data_Reintro$Repeat,data_Reintro$ReintroNo)]$V1]
R<-MaxAll_Gen[MaxAll_Gen$HaveBorn>1000,]
R$Fate<-"Restored"
E<-MaxAll_Gen[MaxAll_Gen$HaveBorn<1000,]
E$Fate<-"Extinct"
MaxAll_Res<-rbind(R,E)

MaxAll_Gen<-aggregate(Season ~ReintroPairs+ReintroNo, MaxAll_Res[MaxAll_Res$Fate=="Restored",], mean)

##calculated revival frequency##
Res<-MaxAll_Res[MaxAll_Res$Fate=="Restored",]  
Res$AverageIC<-as.numeric(Res$AverageIC)
Res$SD<-as.numeric(Res$SD)
  CountRes<-aggregate( Repeat~ReintroNo+ReintroPairs, Res, FUN = function(x) length(unique(x)))
colnames(CountRes)[3]<-"Frequency"
Count_Season<-merge(CountRes,MaxAll_Gen,by = c("ReintroNo","ReintroPairs"),all=TRUE) 
AvgIC<-aggregate( AverageIC~ReintroNo+ReintroPairs, Res, FUN = mean)
Count_Season_IC<-merge(Count_Season,AvgIC,by = c("ReintroNo","ReintroPairs"),all=TRUE) 

pair.labs<-c("3 pairs","5 pairs","7 pairs","9 pairs","11 pairs","13 pairs")
names(pair.labs) <- c(3,5,7,9,11,13)
colours  <- scales::seq_gradient_pal(low = "darkgoldenrod1", high = "dodgerblue3", space = "Lab")(1:10/10)
plot<-Count_Season_IC[Count_Season_IC$ReintroPairs<15,]
S4_2<-ggplot(data = plot, aes(x = Season, y=AverageIC,color = as.factor(ReintroNo)))+geom_point(size = sqrt(plot$Frequency)/2)+
  facet_wrap(.~ReintroPairs,nrow=1,labeller = labeller(ReintroPairs = pair.labs))+
  scale_color_manual(values = colours)+theme_bw()+ylim(0,0.8)+xlim(0,200)+xlab("Time to outcome (year)")+ylab("Average inbreeding coefficients")+
  guides(color=guide_legend(title="The round of\nreintroduction"))+
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),axis.title.x =element_text(size=14, vjust=0.5), axis.title.y=element_text(size=14))+
  theme(strip.text.x = element_text(size=14))+theme(legend.text = element_text(size = 12),legend.title=element_text(size=14))+
  ggtitle("Sequential approach")+
  theme(plot.title = element_text(size = 16, face = "bold"))

S4<-plot_grid(S4_1,S4_2, nrow = 2,labels = c("(A)","(B)"))
S4

#######################################Nature Communications: Major revision##########################################################
###############################Supplements session 5: simulate relatedness of the initial families##
data1 = read.csv("C:\\data\\Population Info_S1.1.csv")
data2 = read.csv("C:\\data\\Population Info_S1.2.csv")
data3 = read.csv("C:\\data\\Population Info_main.csv")
data4 = read.csv("C:\\data\\Population Info_no relatedness.csv")

data1$Type<-"Adults are full siblings"
data2$Type<-"Adults are half siblings"
data3$Type<-"Adults are not related"
data4$Type<-"All founders are not related"
data<-rbind(data1, data2, data3,data4)

length(unique(data$Repeat))
str(data)
data$Repeat<-as.factor(data$Repeat)
data$SD<-as.numeric(data$SD)

########################Figure S1.1 #############################
group<- as.data.table(data)
tapply(data$Season, list(data$Repeat,data$Type),max)
MaxAll<-group[group[, .I[Season == max(Season)], by=list(Repeat, Type)]$V1]
ExtintRea<-MaxAll[MaxAll$PopulationSize<9000,]
ExtintRea$Fate<-"Extinct"
RevivalRea<-MaxAll[MaxAll$PopulationSize>8999,]
RevivalRea$Fate<-"Revival"
MaxCount<-rbind(ExtintRea,RevivalRea)


# Create a composite key (Repeat + Season) to match rows uniquely
Extint_keys <- paste(ExtintRea$Repeat, ExtintRea$Type, sep = "_")
MaxAll_keys <- paste(MaxAll$Repeat, MaxAll$Type, sep = "_")
data_keys <- paste(data$Repeat, data$Type, sep = "_")

# Filter out those rows from data
Revivals_Max <- MaxAll[!(MaxAll_keys %in% Extint_keys), ]


Revivals <- data[!(data_keys %in% Extint_keys), ]
Revivals$Fate<-"Revival"
Extincts <- data[(data_keys %in% Extint_keys), ]
Extincts$Fate<-"Extinct"
Popu_relation<-rbind(Revivals, Extincts)
Popu_relation$Fate <- factor(Popu_relation$Fate, levels = c("Revival","Extinct"))

table_counts<-table(MaxCount$Fate,MaxCount$Type)
Revival_percent <- prop.table(table_counts, margin = 2)["Revival", ] * 100
data.frame()
Type<-c("Adults are full siblings", "Adults are half siblings", "Adults are not related","All founders are not related")
Percent<-data.frame(Revival_percent)[,1]
c<-cbind(Type,Percent)
Popu_relation<-merge(c, Popu_relation)



annot_df <- data.frame(
  Season = 160,                         # x position (adjust as needed)
  PopulationSize = 3.8,                # y position (log10-scale is ~0.8 = pop size ~6.3)
  label = "Log10 (9000)",
  Type = "All founders are not related"
)

label_df <- unique(Popu_relation[, c("Type", "Percent")])
label_df$Percent <- as.numeric(as.character(label_df$Percent))  # ensure numeric
label_df$Season <- 150  # or any x position you want
label_df$y <- 1.8       # or any y position you want

r2<-ggplot(Popu_relation, aes(x =Season, y = log10(PopulationSize), color = Fate, group = interaction(Repeat,Fate)))+geom_step(size=0.8, alpha = 0.4)+theme_bw()+
  theme(legend.position = "none")+ 
  scale_x_continuous(limit = c(0,300))+ylab("Log10 (population size)\n")+geom_hline(yintercept=log10(9000), linetype="dashed", size = 0.8,color = "grey68")+
  scale_y_continuous(limit = c(0,4.1), breaks= c(0,0.5,1,1.5,2,2.5,3,3.5,4))+  scale_color_manual(values=c("yellowgreen","#68228B"))+xlab("Year")+
  facet_wrap(.~Type, nrow = 1 )+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=14),
        axis.title.x =element_text(size=16, vjust=0.5),strip.background = element_blank(),strip.text = element_blank(),#strip.text.x = element_text(size = 14),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5))+
  geom_text(data = annot_df,
    aes(x = Season, y = PopulationSize, label = label),
    inherit.aes = FALSE,
    color = "grey50", size = 5, hjust = 0) +
  geom_text(data = label_df,
    aes(x = Season, y = y, label = paste(sprintf("%.1f", Percent), "%")),
    inherit.aes = FALSE,
    color = "grey50", size = 5, hjust = -0.2)
r2










#######################Figure S1.2###
maxGen<-tapply(Popu_relation$Season, list(Popu_relation$Repeat,Popu_relation$Type),max)
group<- as.data.table(Popu_relation)
Max<-group[group[, .I[Season == max(Season)], by=list(Repeat, Type)]$V1]
Max$Repeat<-as.numeric(Max$Repeat)
Max$PopuFate <- factor(Max$PopuFate, levels = c("Revival","Extinct"))

#average and sd of restoration time and IC

Avg<-aggregate(data = Revivals_Max, cbind(Season, AverageIC) ~ Type, FUN = mean)
Median<-aggregate(data = Revivals_Max, cbind(Season, AverageIC) ~ Type, FUN = median)

colnames(Avg)[3]<-"PopuAvgIC"
colnames(Avg)[2]<-"RestoreT"

colnames(Median)[3]<-"PopuAvgMed"
colnames(Median)[2]<-"RestoreT"

#merge it to Max
Max<-merge(Median, Max)

###
r1<-ggplot(Max, aes(x = (Season-1),fill = Fate))+geom_bar(size = 1,alpha = 0.8,position="identity")+theme_bw()+
  xlab("Time to outcome (year)") + xlim(0,300)+ylab("Frequency \n")+geom_vline(aes(xintercept= RestoreT),linetype="dashed", size = 0.8,color = "grey68")+
  scale_y_continuous(limit = c(0,30), breaks= c(0,5,10,15,20,25,30))+
  scale_fill_manual(values=c("yellowgreen","#68228B"), labels = c("Restoration", "Extinction"))+theme(legend.position = c(0.93,0.79))+ 
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5))+
  theme(axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        legend.position = "none",
        strip.background = element_blank(),strip.text = element_blank(),#strip.text.x = element_text(size = 14)
        )+facet_wrap(.~Type, nrow = 1)+
  geom_text(data = Max, aes(x = (RestoreT-1), y = 20, label = paste(round((RestoreT-1), 0), "years")),  # Adjust y position and label as needed
            angle = 0, vjust = -0.5, size = 5, color = "grey50", hjust = -0.2)
r1


##################Figure S1.3###
#merge it to Max
Popu_relation<-merge(Median, Popu_relation)
r3<-ggplot(Popu_relation[Popu_relation$Fate=="Revival",], aes(x = Season, y = AverageIC,group = Repeat))+
  geom_ribbon(aes(ymin = AverageIC + SD, ymax = AverageIC - SD),fill = "#E0EEE0")+ 
  geom_line(aes(Season), size = 0.4,color = "yellowgreen", alpha = 0.8)+#geom_point(color = "olivedrab4",size = 0.8)+
  scale_x_continuous(limits =c(0,300))+xlab("Year")+
  ylab("Average inbreeding coefficients\n")+theme_bw()+geom_hline(aes(yintercept= PopuAvgMed),linetype="dashed", size = 0.8,color = "grey68")+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=14, vjust=0.5), 
        strip.background = element_blank(),strip.text = element_blank(),# strip.text.x = element_text(size = 14)
        )+
  facet_wrap(.~Type, nrow = 1)+
  geom_text(data = Max, 
            aes(x = 100, y =PopuAvgMed , label = paste("Median F = ",sprintf("%.2f", PopuAvgMed))),  # Adjust y position and label as needed
            angle = 0, vjust = -0.5, size = 5, color = "grey50", hjust = -0.2)
r3

R_all<-plot_grid(r1,r2,r3,nrow =3, rel_widths = c(1,1),
              label_x = 0.00,          # Adjust horizontal position of labels
              label_y = 1.02)
R_all




  #####################################Supplement 6: sensitivity analysis##
data = read.csv("C:\\data\\Population Info_Sensitivity analysis.csv")   ## Population Info_check: from the sensitivity analysis

length(unique(data$Repeat))
str(data)
data$Repeat<-as.factor(data$Repeat)
data$SD<-as.numeric(data$SD)

group<- as.data.table(data)
tapply(data$Season, list(data$Repeat, data$S),max)
MaxAll<-group[group[, .I[Season == max(Season)], by=list(Repeat,S)]$V1]
ExtintRea<-MaxAll[MaxAll$PopulationSize<9000,]
ExtintRea$Fate<-"Extinct"
RevivalRea<-MaxAll[MaxAll$PopulationSize>8999,]
RevivalRea$Fate<-"Revival"
MaxCount<-rbind(ExtintRea,RevivalRea)


# Create a composite key (Repeat + Season) to match rows uniquely
Extint_keys <- paste(ExtintRea$Repeat, ExtintRea$Season, sep = "_")
MaxAll_keys <- paste(MaxAll$Repeat, MaxAll$Season, sep = "_")

# Filter out those rows from datas
str(Revivals_Max)
Revivals_Max <- MaxAll[!(MaxAll_keys %in% Extint_keys), ]

aggregate(data = Revivals_Max, cbind(Season, AverageIC) ~S, FUN = mean)
mean(Revivals_Max$Season)
sd(Revivals_Max$Season)
mean(Revivals_Max$AverageIC)
mean(Revivals_Max$SD)
max(Revivals_Max$Season)
min(Revivals_Max$Season)

Extint_keys <- paste(ExtintRea$Repeat, ExtintRea$S, sep = "_")
data_keys <- paste(data$Repeat, data$S, sep = "_")


Revivals <- data[!(data_keys %in% Extint_keys), ]
Revivals$Fate<-"Revival"
Extincts <- data[(data_keys %in% Extint_keys), ]
Extincts$Fate<-"Extinct"
Popu_Sensi<-rbind(Revivals, Extincts)
Popu_Sensi$Fate <- factor(Popu_Sensi$Fate, levels = c("Revival","Extinct"))
str(Popu_Sensi)
S_labels <- c(`-2` = "s[0] == 2", `-1.5` = "s[0] == 1.5", `-1` = "s[0] == 1",`-0.5` = "s[0] == 0.5")
Popu_Sensi$S<-factor(Popu_Sensi$S, levels = c(-0.5,-1,-1.5,-2))



table_counts<-table(MaxCount$Fate,MaxCount$S)
Revival_percent <- prop.table(table_counts, margin = 2)["Revival", ] * 100
S<-c(-2,-1.5,-1,-0.5)
Percent<-data.frame(Revival_percent)[,1]
c<-cbind(S,Percent)
Popu_Sensi<-merge(c, Popu_Sensi)
Popu_Sensi$S<-factor(Popu_Sensi$S, levels = c(-0.5,-1,-1.5,-2))

annot_df <- data.frame(
  Season = 160,                         # x position (adjust as needed)
  PopulationSize = 3.8,                # y position (log10-scale is ~0.8 = pop size ~6.3)
  label = "Log10 (9000)",
  S = -2
)


label_df <- unique(Popu_Sensi[, c("S", "Percent")])
label_df$Percent <- as.numeric(as.character(label_df$Percent))  # ensure numeric
label_df$Season <- 150  # or any x position you want
label_df$y <- 1.8       # or any y position you want


r2<-ggplot(Popu_Sensi, aes(x =Season, y = log10(PopulationSize), color = Fate, group = interaction(Repeat,Fate)))+geom_step(size=0.8, alpha = 0.4)+theme_bw()+theme(legend.position = "none")+ 
  scale_x_continuous(limit = c(0,300))+ylab("Log10 (population size)\n")+geom_hline(yintercept=log10(9000), linetype="dashed", size = 0.8,color = "grey68")+
  scale_y_continuous(limit = c(0,4.1), breaks= c(0,0.5,1,1.5,2,2.5,3,3.5,4))+  scale_color_manual(values=c("yellowgreen","#68228B"))+xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5))+
  theme(axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        strip.text.x = element_text(size = 14))+
  facet_wrap(.~S, labeller = labeller(S = as_labeller(S_labels, label_parsed)), nrow = 1)+
  geom_text(data = annot_df,aes(x = Season, y = PopulationSize, label = label), inherit.aes = FALSE,color = "grey50", size = 5, hjust = 0)+
  geom_text(data = label_df,
            aes(x = Season, y = y, label = paste(Percent, "%")),
            inherit.aes = FALSE,
            color = "grey50", size = 5, hjust = -0.2)

r2

#######################Figure S2.2###
maxGen<-tapply(Popu_Sensi$Season, list(Popu_Sensi$S, Popu_Sensi$Repeat),max)
mean(maxGen)
sd(maxGen)
group<- as.data.table(Popu_Sensi)
Max<-group[group[, .I[Season == max(Season)], by=list(Repeat,S)]$V1]
Max$Repeat<-as.numeric(Max$Repeat)
mean(Max$AverageIC)
mean(Max$SD)
str(Max)
Max$Fate <- factor(Max$Fate, levels = c("Revival","Extinct"))

Max$S<-factor(Max$S, levels = c(-0.5,-1,-1.5,-2))
a<-aggregate(data = Max[Max$Fate=="Revival",], cbind(Season, AverageIC) ~S, FUN = median)
colnames(a)[2:3]<-c("RestoreT","PopuMedian")
Max<-merge(a,Max)


r1<-ggplot(Max, aes(x = Season, fill = Fate)) +
  geom_bar(size = 1, alpha = 0.8, position = "identity") +
  facet_wrap(. ~ S, labeller = labeller(S = as_labeller(S_labels, label_parsed)), nrow = 1) +
  theme_bw() +
  xlab("Time to outcome (year)") + xlim(0, 300) + ylab("Frequency \n") +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 30, 5)) +
  scale_fill_manual(values = c("yellowgreen", "#68228B"),
                    labels = c("Restoration", "Extinction")) +
  geom_vline(aes(xintercept= RestoreT-1),linetype="dashed", size = 0.8,color = "grey68")+
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16, vjust = 0.5),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, vjust = 0.5),
    legend.title = element_text(size = 14),
    strip.text.x = element_text(size = 14)
  )+
  geom_text(data = Max, 
            aes(x = (RestoreT-1), y = 8, label = paste(round((RestoreT-1), 0), "years")),  # Adjust y position and label as needed
            angle = 0, vjust = -0.5, size = 5, color = "grey50", hjust = -0.2)
r1


##################Figure S2.3###
Popu_Sensi<-merge(a, Popu_Sensi)
r3<-ggplot(Popu_Sensi[Popu_Sensi$Fate=="Revival",], aes(x = Season, y = AverageIC,group = Repeat))+
  geom_ribbon(aes(ymin = AverageIC + SD, ymax = AverageIC - SD), fill = "#E0EEE0")+ 
  facet_wrap(.~S,labeller = labeller(S = as_labeller(S_labels, label_parsed)), nrow = 1)+
  geom_line(aes(Season), size = 0.4,color = "yellowgreen",alpha = 0.8)+#geom_point(color = "olivedrab4",size = 0.8)+
  scale_x_continuous(limits =c(0,300))+xlab("Year")+
  ylab("Average inbreeding coefficients\n")+theme_bw()+geom_hline(aes(yintercept= PopuMedian),linetype="dashed", size = 0.8,color = "grey68")+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=14, vjust=0.5),
        strip.text.x = element_text(size = 14))+ylim(0, 0.6)+
  geom_text(data = Popu_Sensi, 
            aes(x = 80, y =PopuMedian , label = paste("Median F = ",sprintf("%.2f", PopuMedian))),  # Adjust y position and label as needed
            angle = 0, vjust = -0.5, size = 5, color = "grey50", hjust = -0.2)
r3

plot_grid(r1,r2,r3,labels = c("(A)","(B)","(C)"),nrow =3, rel_widths = c(1,1),
          label_x = 0.00,          # Adjust horizontal position of labels
          label_y = 1.02)


################################S7 stage dependent inbreeding effects############################
data = read.csv("C:\\data\\Population Info_main.csv")
data1 = read.csv("C:\\data\\Population Info_S3.1.csv")
data2 = read.csv("C:\\data\\Population Info_S3.2.csv")
data3 = read.csv("C:\\data\\Population Info_S3.3.csv")
data4 = read.csv("C:\\data\\Population Info_S3.4.csv")
data5 = read.csv("C:\\data\\Population Info_S3.5.csv")



data<-data[data$Repeat<201 &data$Repeat>100,]
data$Scena<-"s0"
data1$Scena<-"s1"
data2$Scena<-"s2"
data3$Scena<-"s0+s1"
data4$Scena<-"s0+s2"
data5$Scena<-"s0+s1+s2"

data<-rbind(data,data1,data2,data3,data4,data5)


data$Repeat<-as.factor(data$Repeat)
data$SD<-as.numeric(data$SD)

group<- as.data.table(data)
tapply(data$Season, list(data$Repeat, data$Scena),max)
MaxAll<-group[group[, .I[Season == max(Season)], by=list(Repeat, Scena)]$V1]
ExtintRea<-MaxAll[MaxAll$PopulationSize<9000,]
ExtintRea$Fate<-"Extinct"
RevivalRea<-MaxAll[MaxAll$PopulationSize>8999,]
RevivalRea$Fate<-"Revival"
MaxCount<-rbind(ExtintRea,RevivalRea)
# Create a composite key (Repeat + Season) to match rows uniquely
Extint_keys <- paste(ExtintRea$Repeat, ExtintRea$Season, sep = "_")
MaxAll_keys <- paste(MaxAll$Repeat, MaxAll$Season, sep = "_")

# Filter out those rows from data
Revivals_Max <- MaxAll[!(MaxAll_keys %in% Extint_keys), ]
aggregate(data = Revivals_Max, cbind(Season, AverageIC) ~ Scena, FUN = mean)


Extint_keys <- paste(ExtintRea$Repeat, ExtintRea$Scena, sep = "_")
data_keys <- paste(data$Repeat, data$Scena, sep = "_")


Revivals <- data[!(data_keys %in% Extint_keys), ]
Revivals$Fate<-"Revival"
Extincts <- data[(data_keys %in% Extint_keys), ]
Extincts$Fate<-"Extinct"
Popu_Stage<-rbind(Revivals, Extincts)
Popu_Stage$Fate <- factor(Popu_Stage$Fate, levels = c("Revival","Extinct"))
Popu_Stage$Scena <- factor(Popu_Stage$Scena, levels = c("s0","s1","s2","s0+s1","s0+s2","s0+s1+s2"))


table_counts<-table(MaxCount$Fate,MaxCount$Scena)
Revival_percent <- prop.table(table_counts, margin = 2)["Revival", ] * 100
Scena<-c("s0","s0+s1","s0+s1+s2","s0+s2","s1","s2")
Percent<-data.frame(Revival_percent)[,1]
c<-cbind(Scena,Percent)
Popu_Stage<-merge(c, Popu_Stage)
Popu_Stage$Scena <- factor(Popu_Stage$Scena, levels = c("s0","s1","s2","s0+s1","s0+s2","s0+s1+s2"))


vnames <-list(
  bquote(S[0]),
  bquote(S[1]),
  bquote(S[2]),
  bquote(S[0]*" + "*S[1]),
  bquote(S[0]*" + "*S[2]),
  bquote(S[0]*" + "*S[1]*" + "*S[2]))

vlabeller <- function(variable,value){
  return(vnames[value])
}



annot_df <- data.frame(
  Season = 160,                         # x position (adjust as needed)
  PopulationSize = 3.8,                # y position (log10-scale is ~0.8 = pop size ~6.3)
  label = "Log10 (9000)",
  Scena = factor("s0+s1+s2", levels = levels(Popu_Stage$Scena))
)

label_df <- unique(Popu_Stage[, c("Scena", "Percent")])
label_df$Percent <- as.numeric(as.character(label_df$Percent))  # ensure numeric
label_df$Season <- 270  # or any x position you want
label_df$y <- 1.8       # or any y position you want


p2 <-ggplot(Popu_Stage, aes(x =Season, y = log10(PopulationSize), color = Fate, group = interaction(Repeat,Fate)))+geom_step(size=0.8, alpha = 0.4)+theme_bw()+theme(legend.position = "none")+ 
  scale_x_continuous(limit = c(0,500))+ylab("Log10 (population size)\n")+geom_hline(yintercept=log10(9000), linetype="dashed", size = 0.8,color = "grey68")+
  scale_y_continuous(limit = c(0,4.1), breaks= c(0,0.5,1,1.5,2,2.5,3,3.5,4))+  scale_color_manual(values=c("yellowgreen","#68228B"))+xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        strip.text.x = element_text(size = 14))+facet_wrap(.~Scena, nrow =1,labeller = vlabeller)+
  geom_text(data = annot_df,
            aes(x = Season, y = PopulationSize, label = label),
            inherit.aes = FALSE,
            color = "grey50", size = 5, hjust = 0)+
  geom_text(data = label_df,
            aes(x = Season, y = y, label = paste(Percent, "%")),
            inherit.aes = FALSE,
            color = "grey50", size = 5, hjust = -0.2)
  
p2 

maxGen<-tapply(Popu_Stage$Season, list(Popu_Stage$Scena, Popu_Stage$Repeat),max)
mean(maxGen)
sd(maxGen)
group<- as.data.table(Popu_Stage)
Max<-group[group[, .I[Season == max(Season)], by=list(Repeat,Scena)]$V1]
Max$Repeat<-as.numeric(Max$Repeat)
mean(Max$AverageIC)
mean(Max$SD)
str(Max)
Max$Fate <- factor(Max$Fate, levels = c("Revival","Extinct"))

b<-aggregate(data = Max[Max$Fate=="Revival",], cbind(Season, AverageIC) ~Scena, FUN = median)
colnames(b)[2:3]<-c("RestoreT","PopuMedian")
Max<-merge(b,Max)


p1<-ggplot(Max, aes(x = Season, fill = Fate)) +
  geom_bar(size = 1, alpha = 0.8, position = "identity") +
  facet_wrap(. ~ Scena, nrow = 1,labeller = vlabeller) +
  theme_bw() +
  xlab("Time to outcome (year)") + xlim(0, 500) + ylab("Frequency \n") +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 30, 5)) +
  guides(fill = guide_legend(title = "Outcome")) +
  scale_fill_manual(values = c("yellowgreen", "#68228B"),
                    labels = c("Restoration", "Extinction")) +
  geom_vline(aes(xintercept= RestoreT-1),linetype="dashed", size = 0.8,color = "grey68")+
  theme(
    legend.position ="none",
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16, vjust = 0.5),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, vjust = 0.5),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    strip.text.x = element_text(size = 14)
  )+
  geom_text(data = Max, 
            aes(x = (RestoreT-1), y = 8, label = paste(round((RestoreT-1), 0), "years")),  # Adjust y position and label as needed
            angle = 0, vjust = -0.5, size = 5, color = "grey50", hjust = -0.2)
p1



Popu_Stage<-merge(b,Popu_Stage)
p3<-ggplot(Popu_Stage[Popu_Stage$Fate!="Extinct",], aes(x = Season, y = AverageIC,group = Repeat))+
  geom_ribbon(aes(ymin = AverageIC + SD, ymax = AverageIC - SD), fill = "#E0EEE0")+ 
  facet_wrap(.~Scena, nrow = 1,labeller = vlabeller)+geom_line(aes(Season), size = 0.4,color = "yellowgreen",alpha = 0.8)+#geom_point(color = "olivedrab4",size = 0.8)+
  scale_x_continuous(limits =c(0,500))+xlab("Year")+geom_hline(aes(yintercept= PopuMedian),linetype="dashed", size = 0.8,color = "grey68")+
  ylab("Average inbreeding coefficients\n")+theme_bw()+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=14, vjust=0.5),
        strip.text.x = element_text(size = 14))+
  geom_text(data = Popu_Stage, aes(x = 60, y =PopuMedian , label = paste("Median F = ",sprintf("%.2f", PopuMedian))),  # Adjust y position and label as needed
            angle = 0, vjust = -0.5, size = 5, color = "grey50", hjust = -0.2)
p3

plot_grid(p1,p2,p3, labels = c("(A)","(B)","(C)"),nrow =3, rel_widths = c(1,1),
          label_x = 0.00,          # Adjust horizontal position of labels
          label_y = 1.02)

##################################Supplement 4 density dependent###################################
data1 = read.csv("C:\\data\\Population Info_Density_15000.csv")
data2 = read.csv("C:\\data\\Population Info_Density_10000.csv")
data3 = read.csv("C:\\data\\Population Info_Density_5000.csv")
data4 = read.csv("C:\\data\\Population Info_Density_3000.csv")


data1$Type<-"m = m0 * (1 + N / 15000)"
data2$Type<-"m = m0 * (1 + N / 10000)"
data3$Type<-"m = m0 * (1 + N / (N+5000))"
data4$Type<-"m = m0 * (1 + N / (N+3000))"

data<-rbind(data1,data2,data3,data4)


length(unique(data$Repeat))
str(data)
data$Repeat<-as.factor(data$Repeat)
data$SD<-as.numeric(data$SD)

########################Figure S4.1 #############################
group<- as.data.table(data)
tapply(data$Season, list(data$Repeat,data$Type),max)
MaxAll<-group[group[, .I[Season == max(Season)], by=list(Repeat, Type)]$V1]
ExtintRea<-MaxAll[MaxAll$PopulationSize<9000,]
ExtintRea$Fate<-"Extinct"
RevivalRea<-MaxAll[MaxAll$PopulationSize>8999,]
RevivalRea$Fate<-"Revival"
MaxCount<-rbind(ExtintRea,RevivalRea)
MaxCount<-MaxCount[MaxCount$Repeat!="101",]
# Create a composite key (Repeat + Season) to match rows uniquely
Extint_keys <- paste(ExtintRea$Repeat, ExtintRea$Type, sep = "_")
MaxAll_keys <- paste(MaxAll$Repeat, MaxAll$Type, sep = "_")
data_keys <- paste(data$Repeat, data$Type, sep = "_")

# Filter out those rows from data
Revivals_Max <- MaxAll[!(MaxAll_keys %in% Extint_keys), ]


Revivals <- data[!(data_keys %in% Extint_keys), ]
Revivals$Fate<-"Revival"
Extincts <- data[(data_keys %in% Extint_keys), ]
Extincts$Fate<-"Extinct"
Popu_Den<-rbind(Revivals, Extincts)
Popu_Den$Fate <- factor(Popu_Den$Fate, levels = c("Revival","Extinct"))

table_counts<-table(MaxCount$Fate,MaxCount$Type)
Revival_percent <- prop.table(table_counts, margin = 2)["Revival", ] * 100
data.frame()
Type<-c("m = m0 * (1 + N / (N+3000))","m = m0 * (1 + N / (N+5000))", "m = m0 * (1 + N / 10000)","m = m0 * (1 + N / 15000)")
Percent<-data.frame(Revival_percent)[,1]
c<-cbind(Type,Percent)
Popu_Den<-merge(c, Popu_Den)

annot_df <- data.frame(
  Season = 80,                         # x position (adjust as needed)
  PopulationSize = 3.8,                # y position (log10-scale is ~0.8 = pop size ~6.3)
  label = "Log10 (9000)",
  Type = factor("m = m0 * (1 + N / 10000)", levels = levels(Popu_Den$Type))
)

label_df <- unique(Popu_Den[, c("Type", "Percent")])
label_df$Percent <- as.numeric(as.character(label_df$Percent))  # ensure numeric
label_df$Season <- 50  # or any x position you want
label_df$y <- 1.8       # or any y position you want


str(Popu_Den)
Popu_Den$Repeat<-as.numeric(Popu_Den$Repeat)
Popu_Den$Type<-factor(Popu_Den$Type, levels = c("m = m0 * (1 + N / 15000)","m = m0 * (1 + N / 10000)","m = m0 * (1 + N / (N+5000))","m = m0 * (1 + N / (N+3000))"))



S4.1.1<-ggplot(Popu_Den[Popu_Den$Repeat<101 & (Popu_Den$Type=="m = m0 * (1 + N / 15000)" | Popu_Den$Type=="m = m0 * (1 + N / 10000)"),], aes(x =Season, y = log10(PopulationSize), color = Fate, group = interaction(Repeat,Fate)))+
  geom_step(size=0.8, alpha = 0.4)+theme_bw()+theme(legend.position = "none")+ 
  scale_x_continuous(limit = c(0,150))+ylab("Log10 (population size)\n")+geom_hline(yintercept=log10(9000), linetype="dashed", size = 0.8,color = "grey68")+
  scale_y_continuous(limit = c(0,4.1), breaks= c(0,0.5,1,1.5,2,2.5,3,3.5,4))+  scale_color_manual(values=c("yellowgreen","#68228B"))+xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+facet_wrap(.~Type, nrow = 1)+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        strip.background = element_blank(),strip.text = element_blank())+
  geom_text(data = label_df[label_df$Type=="m = m0 * (1 + N / 15000)" |label_df$Type=="m = m0 * (1 + N / 10000)",],
            aes(x = Season, y = y, label = paste(sprintf("%.0f", Percent), "%")),
            inherit.aes = FALSE,
            color = "grey50", size = 5, hjust = -0.2)+
  geom_text(data = annot_df,
            aes(x = Season, y = PopulationSize, label = label),
            inherit.aes = FALSE,
            color = "grey50", size = 5, hjust = 0)
S4.1.1

annot_df <- data.frame(
  Season = 80,                         # x position (adjust as needed)
  PopulationSize = 3.8,                # y position (log10-scale is ~0.8 = pop size ~6.3)
  label = "Log10 (9000)",
  Type = factor("m = m0 * (1 + N / (N+3000))", levels = levels(Popu_Den$Type))
)

S4.1.2<-ggplot(Popu_Den[Popu_Den$Repeat<101 & (Popu_Den$Type=="m = m0 * (1 + N / (N+5000))" | Popu_Den$Type=="m = m0 * (1 + N / (N+3000))"),], aes(x =Season, y = log10(PopulationSize), color = Fate, group = interaction(Repeat,Fate)))+
  geom_step(size=0.8, alpha = 0.4)+theme_bw()+theme(legend.position = "none")+ 
  scale_x_continuous(limit = c(0,150))+ylab("Log10 (population size)\n")+geom_hline(yintercept=log10(9000), linetype="dashed", size = 0.8,color = "grey68")+
  scale_y_continuous(limit = c(0,4.1), breaks= c(0,0.5,1,1.5,2,2.5,3,3.5,4))+  scale_color_manual(values=c("yellowgreen","#68228B"))+xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+facet_wrap(.~Type, nrow = 1)+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        strip.background = element_blank(),strip.text = element_blank())+
  geom_text(data = annot_df,
            aes(x = Season, y = PopulationSize, label = label),
            inherit.aes = FALSE,
            color = "grey50", size = 5, hjust = 0)+
  geom_text(data = label_df[c(1:2),],
            aes(x = Season, y = y, label = paste(sprintf("%.0f", Percent), "%")),
            inherit.aes = FALSE,
            color = "grey50", size = 5, hjust = -0.2)
S4.1.2

##############S4.2##
group<- as.data.table(Popu_Den)
Max<-group[group[, .I[Season == max(Season)], by=list(Repeat, Type)]$V1]
Max$Repeat<-as.numeric(Max$Repeat)
mean(Max$AverageIC)
mean(Max$SD)

Median<-aggregate(data = Max[Max$Repeat<101,], cbind(Season, AverageIC) ~ Type, FUN = median)
colnames(Median)[3]<-"PopuMedian"
colnames(Median)[2]<-"RestoreT"

#merge it to Max
Max<-merge(Median, Max)
Max<-Max[Max$Repeat<101,]

Max$Fate <- factor(Max$Fate, levels = c("Revival","Extinct"))


S4.2.1<-ggplot(Max[Max$Type=="m = m0 * (1 + N / 15000)" | Max$Type=="m = m0 * (1 + N / 10000)",],aes(x = Season,fill = Fate))+geom_bar(size = 1,alpha = 0.8,position="identity")+theme_bw()+
  xlab("Time to outcome (year)") + xlim(0,150)+ylab("Frequency \n")+geom_vline(aes(xintercept=(RestoreT-1)),linetype="dashed", size = 0.8,color = "grey68")+
  scale_y_continuous(limit = c(0,15), breaks= c(0,5,10,15,20,25,30))+guides(fill=guide_legend(title="Outcome"))+
  scale_fill_manual(values=c("yellowgreen","#68228B"), labels = c("Restoration", "Extinction"))+ theme(legend.position = c(0.92,0.83))+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        legend.text = element_text(size = 12), legend.title=element_text(size = 14),
        strip.background = element_blank(),strip.text = element_blank(),legend.position = "none")+
  facet_wrap(.~Type, nrow = 1)+
  geom_text(data = Max[Max$Type=="m = m0 * (1 + N / 15000)" | Max$Type=="m = m0 * (1 + N / 10000)",], 
            aes(x = (RestoreT-1), y = 10, label = paste(round((RestoreT-1), 0), "years")),  # Adjust y position and label as needed
            angle = 0, vjust = -0.5, size = 5, color = "grey50", hjust = -0.2)

S4.2.1

S4.2.2<-ggplot(Max[Max$Type=="m = m0 * (1 + N / (N+5000))" | Max$Type=="m = m0 * (1 + N / (N+3000))",],aes(x = Season,fill = Fate))+geom_bar(size = 1,alpha = 0.8,position="identity")+theme_bw()+
  xlab("Time to outcome (year)") + xlim(0,150)+ylab("Frequency \n")+geom_vline(aes(xintercept=(RestoreT-1)),linetype="dashed", size = 0.8,color = "grey68")+
  scale_y_continuous(limit = c(0,15), breaks= c(0,5,10,15,20,25,30))+guides(fill=guide_legend(title="Outcome"))+
  scale_fill_manual(values=c("yellowgreen","#68228B"), labels = c("Restoration", "Extinction"))+ theme(legend.position = c(0.88,0.81))+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        legend.position = "none",
        strip.background = element_blank(),strip.text = element_blank())+
  facet_wrap(.~Type, nrow = 1)+
  geom_text(data = Max[Max$Type=="m = m0 * (1 + N / (N+5000))" | Max$Type=="m = m0 * (1 + N / (N+3000))",], 
            aes(x = (RestoreT-1), y = 10, label = paste(round((RestoreT-1), 0), "years")),  # Adjust y position and label as needed
            angle = 0, vjust = -0.5, size = 5, color = "grey50", hjust = -0.2)

S4.2.2




Popu_Den<-merge(Median, Popu_Den[Popu_Den$Repeat<101,])
S4.3.1<-ggplot(Popu_Den[Popu_Den$Fate=="Revival" & Popu_Den$Repeat<101 & (Popu_Den$Type=="m = m0 * (1 + N / 15000)" | Popu_Den$Type=="m = m0 * (1 + N / 10000)"),], aes(x = Season, y = AverageIC,group = Repeat))+
  geom_ribbon(aes(ymin = AverageIC + SD, ymax = AverageIC - SD), fill = "#E0EEE0")+ geom_hline(aes(yintercept= PopuMedian),linetype="dashed", size = 0.8,color = "grey68")+
  geom_line(aes(Season), size = 0.4,color = "yellowgreen",alpha = 0.8)+#geom_point(color = "olivedrab4",size = 0.8)+
  scale_y_continuous(limit = c(0,0.6), breaks= c(0,0.1,0.2,0.3,0.4,0.5,0.6))+
  scale_x_continuous(limits =c(0,160))+xlab("Year")+
  ylab("Average inbreeding coefficients\n")+theme_bw()+##geom_hline(yintercept=0.20,linetype="dashed", size = 0.8,color = "grey68")+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5))+
  theme(axis.text.y = element_text(size=14),axis.title.y =element_text(size=14, vjust=0.5),
        strip.background = element_blank(),strip.text = element_blank())+
  facet_wrap(.~Type, nrow = 1)+
  geom_text(data = Popu_Den[Popu_Den$Type=="m = m0 * (1 + N / 15000)" | Popu_Den$Type=="m = m0 * (1 + N / 10000)",], aes(x = 50, y =PopuMedian , label = paste("Median F = ",sprintf("%.2f", PopuMedian))),  # Adjust y position and label as needed
            angle = 0, vjust = -0.5, size = 5, color = "grey50", hjust = -0.2)
S4.3.1


S4.3.2<-ggplot(Popu_Den[Popu_Den$Fate=="Revival" & Popu_Den$Repeat<101 & (Popu_Den$Type=="m = m0 * (1 + N / (N+5000))" | Popu_Den$Type=="m = m0 * (1 + N / (N+3000))"),], aes(x = Season, y = AverageIC,group = Repeat))+
  geom_ribbon(aes(ymin = AverageIC + SD, ymax = AverageIC - SD), fill = "#E0EEE0")+ geom_hline(aes(yintercept= PopuMedian),linetype="dashed", size = 0.8,color = "grey68")+
  geom_line(aes(Season), size = 0.4,color = "yellowgreen",alpha = 0.8)+#geom_point(color = "olivedrab4",size = 0.8)+
  scale_y_continuous(limit = c(0,0.6), breaks= c(0,0.1,0.2,0.3,0.4,0.5,0.6))+
  scale_x_continuous(limits =c(0,160))+xlab("Year")+
  ylab("Average inbreeding coefficients\n")+theme_bw()+##geom_hline(yintercept=0.20,linetype="dashed", size = 0.8,color = "grey68")+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5))+
  theme(axis.text.y = element_text(size=14),axis.title.y =element_text(size=14, vjust=0.5),
        strip.background = element_blank(),strip.text = element_blank())+
  facet_wrap(.~Type, nrow = 1)+
  geom_text(data = Popu_Den[Popu_Den$Type=="m = m0 * (1 + N / (N+5000))" | Popu_Den$Type=="m = m0 * (1 + N / (N+3000))",], aes(x = 50, y =PopuMedian , label = paste("Median F = ",sprintf("%.2f", PopuMedian))),  # Adjust y position and label as needed
            angle = 0, vjust = -0.5, size = 5, color = "grey50", hjust = -0.2)
S4.3.2


#15000 and 10000
plot_grid(S4.2.1,S4.1.1, S4.3.1, nrow = 3,labels = c("(A)","(B)","(C)" ),
          rel_widths = c(1,1),
          label_x = 0.00,          # Adjust horizontal position of labels
          label_y = 1.02)

#5000 and 3000
plot_grid(S4.2.2,S4.1.2, S4.3.2, nrow = 3,labels = c("(A)","(B)","(C)"),
          rel_widths = c(1,1),
          label_x = -.01,          # Adjust horizontal position of labels
          label_y = 1.02)


#######################Figure S5 Change threshold of Stepping-stone (S9) ######################
data_Reintro = read.csv("C:\\data\\Population Info_ReintroSeq_One_T500.csv")
StepStone<-data_Reintro[(data_Reintro$ReintroPairs==3& data_Reintro$Repeat==38)|(data_Reintro$ReintroPairs==11 &data_Reintro$Repeat==203),]

table(factor(StepStone$Repeat))
a<-seq(1,261)
b<-seq(1,265)
StepStone$Year<-c(a,b)
colours <- met.brewer("VanGogh1", 7)
colours <- met.brewer("Hokusai3", 7)
StartPoints<-StepStone[StepStone$Season==0,]
str(StepStone)
StepStone$SD<-as.numeric(StepStone$SD)

S5.1<-ggplot(StepStone, aes(x = Year, y = AverageIC,fill = factor(ReintroPairs)))+
  geom_ribbon(aes(ymin = AverageIC + SD, ymax = AverageIC - SD), alpha = 0.7)+ 
  theme_cowplot()+theme(legend.position = "none")+scale_x_continuous(limits =c(0,450), breaks = seq(0, 550, by = 100))+
  scale_y_continuous(limits =c(0.05,0.75), breaks = seq(0,0.8, by= 0.1))+
  geom_line(aes(Year), size = 0.9,color = "#636363",alpha = 0.5)+
  xlab("Year")+ylab(" ")+theme_bw()+
  scale_fill_manual(values=c(colours[2],colours[3]), labels = c("3", "11"))+
  guides(fill=guide_legend(title="Number of pairs"))+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        legend.position = "none",plot.title =  element_text(size=16))+
  geom_point(data = StartPoints,aes(x = Year, y = AverageIC),color = ("#5D478B"))+
  ggtitle("Sequential_recover until 500")+ 
  annotate(geom="text", x=330, y=0.50, label="11 pairs",size = 5,color="black")+
  annotate(geom="text", x=280, y=0.62, label="3 pairs",size = 5,color="black")
S5.1



data_Reintro = read.csv("C:\\data\\Population Info_ReintroSeq_One_T1000.csv")
StepStone<-data_Reintro[(data_Reintro$ReintroPairs==3& data_Reintro$Repeat==47)|(data_Reintro$ReintroPairs==11 &data_Reintro$Repeat==251),]
StepStone$SD<-as.numeric(StepStone$SD)
StepStone$AverageIC<-as.numeric(StepStone$AverageIC)

table(factor(StepStone$Repeat))
a<-seq(1,214)
b<-seq(1,332)
StepStone$Year<-c(a,b)
colours <- met.brewer("VanGogh1", 7)
colours <- met.brewer("Hokusai3", 7)
StartPoints<-StepStone[StepStone$Season==0,]
str(StepStone)
str(StartPoints)



S5.2<-ggplot(StepStone, aes(x = as.numeric(Year), y = AverageIC,fill = factor(ReintroPairs)))+
  geom_ribbon(aes(ymin = AverageIC + SD, ymax = AverageIC - SD), alpha = 0.7)+ 
  theme_cowplot()+theme(legend.position = "none")+scale_x_continuous(limits =c(0,400), breaks = seq(0, 550, by = 100))+
  scale_y_continuous(limits =c(0.05,0.75), breaks = seq(0,0.8, by= 0.1))+
  geom_line(aes(Year), size = 0.9,color = "#636363",alpha = 0.5)+
  xlab("Year")+ylab(" ")+theme_bw()+
  scale_fill_manual(values=c(colours[2],colours[3]), labels = c("3", "11"))+
  guides(fill=guide_legend(title="Number of pairs"))+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        legend.position = "none",plot.title =  element_text(size=16))+
  geom_point(data = StartPoints,aes(x = Year, y = AverageIC),color = ("#5D478B"))+
  ggtitle("Sequential_recover until 1000")+ 
  annotate(geom="text", x=350, y=0.45, label="11 pairs",size = 5,color="black")+
  annotate(geom="text", x=280, y=0.60, label="3 pairs",size = 5,color="black")
S5.2



data_Reintro = read.csv("C:\\data\\Population Info_ReintroSeq_OneT200.csv")
StepStone<-data_Reintro[(data_Reintro$ReintroPairs==3& data_Reintro$Repeat==35)|(data_Reintro$ReintroPairs==11 &data_Reintro$Repeat==203),]
StepStone$SD<-as.numeric(StepStone$SD)
StepStone$AverageIC<-as.numeric(StepStone$AverageIC)
table(factor(StepStone$Repeat))
a<-seq(1,179)
b<-seq(1,256)
StepStone$Year<-c(a,b)
colours <- met.brewer("VanGogh1", 7)
colours <- met.brewer("Hokusai3", 7)
StartPoints<-StepStone[StepStone$Season==0,]
str(StepStone)
StepStone$SD<-as.numeric(StepStone$SD)
StepStone$AverageIC<-as.numeric(StepStone$AverageIC)

S5.3<-ggplot(StepStone, aes(x = Year, y = AverageIC,fill = factor(ReintroPairs)))+
  geom_ribbon(aes(ymin = AverageIC + SD, ymax = AverageIC - SD), alpha = 0.7)+ 
  theme_cowplot()+theme(legend.position = "none")+scale_x_continuous(limits =c(0,450), breaks = seq(0, 550, by = 100))+
  scale_y_continuous(limits =c(0.05,0.75), breaks = seq(0,0.8, by= 0.1))+
  geom_line(aes(Year), size = 0.9,color = "#636363",alpha = 0.5)+
  xlab("Year")+ylab("Average inbreeding coefficients")+theme_bw()+
  scale_fill_manual(values=c(colours[2],colours[3]), labels = c("3", "11"))+
  guides(fill=guide_legend(title="Number of pairs"))+
  theme(axis.text.x = element_text(size=14),axis.title.x =element_text(size=16, vjust=0.5),
        axis.text.y = element_text(size=14),axis.title.y =element_text(size=16, vjust=0.5),
        legend.position = "none",plot.title =  element_text(size=16))+
  geom_point(data = StartPoints,aes(x = Year, y = AverageIC),color = ("#5D478B"))+
  ggtitle("Sequential_recover until 200")+ 
  annotate(geom="text", x=330, y=0.53, label="11 pairs",size = 5,color="black")+
  annotate(geom="text", x=260, y=0.62, label="3 pairs",size = 5,color="black")
S5.3
library(ggpubr)
ggarrange(S5.3,S5.1,S5.2, nrow = 1,labels = c("(A)","(B)"))

#####Supplement 4 Population size of immigrant and no immigirants ###
data_FLF_Tran = read.csv("C:\\data\\Population Info_FLF_One.csv")
data_FLF_Rest = read.csv("C:\\data\\Population Info_FLF_Orig.csv")

str(data_FLF_Tran)
Rest<-data_FLF_Rest[data_FLF_Rest$Repeat ==8,]
Tran<-data_FLF_Tran[data_FLF_Tran$Repeat==8,]
Tran$Season<-Tran$Season+28
Tran1<-Tran[,c(1:5)]
Total<-rbind(Rest,Tran1)
Total$Popu<-"Leader"
Tran2<-Tran[,c(1,2,6:8)]
colnames(Tran2)[3:5]<-c("PopuSize","AvgIC","SD")
Tran2$Popu<-"Follower3"
Tran3<-Tran[,c(1,2,9:11)]
colnames(Tran3)[3:5]<-c("PopuSize","AvgIC","SD")
Tran3$Popu<-"Firework3"
Tran4<-Tran[,c(1,2,12:14)]
colnames(Tran4)[3:5]<-c("PopuSize","AvgIC","SD")
Tran4$Popu<-"Follower11"
Tran5<-Tran[,c(1,2,15:17)]
colnames(Tran5)[3:5]<-c("PopuSize","AvgIC","SD")
Tran5$Popu<-"Firework11"

data<-rbind(Total,Tran2,Tran3, Tran4, Tran5)
str(data)

Dynamic<-data[,c(2,3,6)]
a<-c(28,6,'Follower3')
b<-c(28,6,'Firework3')
c<-c(28,22,'Follower11')
d<-c(28,22,'Firework11')
add<-rbind(a,b,c,d)
colnames(add) <- colnames(Dynamic)
Dynamic<-rbind(Dynamic,add)

colours <- c(
  "Firework11" = "#FFA500",
  "Firework3"  = "#CD8500",
  "Follower11" = "#8A5D86",
  "Follower3"  = "#DEB0D0",
  "Leader"     = "grey70"
)



colours <- c(
  "Firework11" = "#FFA500",
  "Firework3"  = "#CD8500",
  "Follower11" = "#DEB0D0",
  "Follower3"  = "#8A5D86",
  "Leader"     = "grey70"
)
legend_order <- c( "Follower11", "Follower3", "Firework11", "Firework3","Leader")

annot_df <- data.frame(
  Season = 55,                         # x position (adjust as needed)
  PopulationSize = 3.2,                # y position (log10-scale is ~0.8 = pop size ~6.3)
  label = "Log10(1000)"
)


PopuDynamic<- ggplot(Dynamic, aes(x = as.numeric(Season), y = log10(as.numeric(PopuSize)), color = factor(Popu)))+
  geom_step(size=0.8, alpha = 0.8)+theme_bw()+scale_x_continuous(limit = c(0,70))+
  scale_color_manual(values = colours)+ylab("Log10 (population size)")+
  xlab("Season")+  theme(legend.position = "none") +
  annotate(geom="text", x=25, y=2.2, label="The source",size = 4,color="black")+
  annotate(geom="text", x=53, y=2.8, label="11 pairs",size = 4,color="black")+
  annotate(geom="text", x=53, y=2.5, label="11 pairs",size = 4,color="black")+
  annotate(geom="text", x=48, y=1.8, label="3 pairs",size = 4,color="black")+
  annotate(geom="text", x=53, y=1.4, label="3 pairs",size = 4,color="black")+
  geom_hline(yintercept=log10(1000), linetype="dashed", size = 0.8,color = "grey30")+
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14, vjust = 0.5),
        plot.title = element_text(size = 14))+
  ylim(0,4.5)+ geom_text(data = annot_df,
                       aes(x = Season, y = PopulationSize, label = label),
                       inherit.aes = FALSE,
                       color = "grey50", size = 4, hjust = 0)

PopuDynamic




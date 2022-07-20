setwd("E:/Working Files/PhD/Conferences/IAHR 2019/R")

library("tidyverse")
library("NORMT3") #calculate erf()
library("gridExtra")
library("cowplot")

hyd_data <- read_csv("hyd_data.csv")
hyd_data <- mutate(hyd_data, u_s = sqrt(shear/1000))
hyd_data <- mutate(hyd_data, ey = 0.6*u_s*d)

summary <- hyd_data%>%
  group_by(site)%>%
  summarise(d = mean(d), u = mean(u), shear = mean(shear), ey = mean(ey), u_s = mean(u_s))


C <- tibble(y = seq(0,100,1))

C0 <-148

ey_p <- filter(summary, site == "P")
ey_p <- as.numeric(select(ey_p, ey))
u_p <- filter(summary, site == "P")
u_p <- as.numeric(select(u_p, u))
d_p <- filter(summary, site == "P")
d_p <- as.numeric(select(d_p, d))

x <- 1000

C <- mutate(C, C2 = Re(C0/2*(erf((y+d_p/2)/sqrt(4*ey_p*x/u_p))-erf((y-d_p/2)/sqrt(4*ey_p*x/u_p))))+Re(C0/2*(erf((y+d_p/2+d_p/2)/sqrt(4*ey_p*x/u_p))-erf((y+d_p/2-d_p/2)/sqrt(4*ey_p*x/u_p)))))
cmax <- as.numeric(C%>%summarise(C2 = max(C2)))


ggplot(data=C) + 
  geom_path(mapping = aes(y=C2, x=y))

###Analysis just considering concentration at y=0####

vol_g <- 7000  #volume of gravel m3
dens_g <- 1600  #bulk density of gravel  kg/m3
mass_g <- vol_g*dens_g  #kg
u <- u_p  #m/s
d <- d_p  #average depth m
A <- d^2  #area of entrainment (multiply by u*t to get volume)
x <- 1000  #monitoring location
ey <- ey_p  #m2/s
y <- 0 #release next to bank
max_dur <- 25

dozer <- 100*dens_g  #maximum gravel movement with dozer kg/hour



dur_daily <- 12  #daily duration of 12 hours

work_12 <- tibble(dur = seq(1, dur_daily*max_dur))
work_12 <- mutate(work_12, dur_days = dur/dur_daily)
work_12 <- mutate(work_12, C0 = 0.013*mass_g/A/u/dur/3600*10^3)
work_12 <- mutate(work_12, 
                   C1000 = Re(C0/2*(erf((y+d/2)/sqrt(4*ey*x/u))-erf((y-d/2)/sqrt(4*ey*x/u))))+
                     Re(C0/2*(erf((y+d/2+d/2)/sqrt(4*ey*x/u))-erf((y+d/2-d/2)/sqrt(4*ey*x/u)))))  #Concentration at monitoring location (mg/L)
work_12 <- mutate(work_12, SEV = 1.0684+0.6068*log(dur)+0.7384*log(C1000)) 
work_12 <- mutate(work_12, SEV_daily = 1.0684+0.6068*log(ifelse(dur>dur_daily, dur_daily,dur))+0.7384*log(C1000))  #Average daily SEV score
work_12 <- mutate(work_12, SEV_ave = SEV_daily*ceiling(dur_days)/30)
work_12 <- mutate(work_12, rate = mass_g/dur)  # kg/hour 
work_12 <- mutate(work_12, num_dozer = ceiling(rate/dozer))
work_12 <- mutate(work_12, cost = num_dozer*230*dur)

  
  
  
dur_daily <- 9  #daily duration of 8 hours

work_9 <- tibble(dur = seq(1, dur_daily*max_dur))
work_9 <- mutate(work_9, dur_days = dur/dur_daily)
work_9 <- mutate(work_9, C0 = 0.013*mass_g/A/u/dur/3600*10^3)
work_9 <- mutate(work_9, 
                  C1000 = Re(C0/2*(erf((y+d/2)/sqrt(4*ey*x/u))-erf((y-d/2)/sqrt(4*ey*x/u))))+
                    Re(C0/2*(erf((y+d/2+d/2)/sqrt(4*ey*x/u))-erf((y+d/2-d/2)/sqrt(4*ey*x/u)))))  #Concentration at monitoring location (mg/L)
work_9 <- mutate(work_9, SEV = 1.0684+0.6068*log(dur)+0.7384*log(C1000)) 
work_9 <- mutate(work_9, SEV_daily = 1.0684+0.6068*log(ifelse(dur>dur_daily, dur_daily,dur))+0.7384*log(C1000))  #Average daily SEV score
work_9 <- mutate(work_9, SEV_ave = SEV_daily*ceiling(dur_days)/30)
work_9 <- mutate(work_9, rate = mass_g/dur)  # kg/hour 
work_9 <- mutate(work_9, num_dozer = ceiling(rate/dozer))
work_9 <- mutate(work_9, cost = num_dozer*230*dur)

dur_daily <- 6  #daily duration of 6 hours

work_6 <- tibble(dur = seq(1, dur_daily*max_dur))
work_6 <- mutate(work_6, dur_days = dur/dur_daily)
work_6 <- mutate(work_6, C0 = 0.013*mass_g/A/u/dur/3600*10^3)
work_6 <- mutate(work_6, 
                 C1000 = Re(C0/2*(erf((y+d/2)/sqrt(4*ey*x/u))-erf((y-d/2)/sqrt(4*ey*x/u))))+
                   Re(C0/2*(erf((y+d/2+d/2)/sqrt(4*ey*x/u))-erf((y+d/2-d/2)/sqrt(4*ey*x/u)))))  #Concentration at monitoring location (mg/L)
work_6 <- mutate(work_6, SEV = 1.0684+0.6068*log(dur)+0.7384*log(C1000)) 
work_6 <- mutate(work_6, SEV_daily = 1.0684+0.6068*log(ifelse(dur>dur_daily, dur_daily,dur))+0.7384*log(C1000))  #Average daily SEV score, drops at day turnover because the small amount on next day drops the average noticeably
work_6 <- mutate(work_6, SEV_ave = SEV_daily*ceiling(dur_days)/30)
work_6 <- mutate(work_6, rate = mass_g/dur)  # kg/hour 
work_6 <- mutate(work_6, num_dozer = ceiling(rate/dozer))
work_6 <- mutate(work_6, cost = num_dozer*230*dur)

SEV_COST <- ggplot()+
  geom_path(work_6, mapping = aes(x = SEV_daily, y = cost, color = "6"))+
  geom_path(work_9, mapping = aes(x = SEV_daily, y = cost, color = "9"))+
  geom_path(work_12, mapping = aes(x = SEV_daily, y = cost, color = "12"))

plot(SEV_COST)

COST_df12 <- data.frame(x1 = 146/12, x2 = 146/12, y1 = 0, y2 = 33580)
COST_df9 <- data.frame(x1 = 146/9, x2 = 146/9, y1 = 0, y2 = 33580)
COST_df6 <- data.frame(x1 = 146/6, x2 = 146/6, y1 = 0, y2 = 33580)
COST_df_annot <- data.frame(x1 = 13, x2 = 146/12, x3 = 146/9, x4 = 146/6, y1 = 55000, y2 = 33580)
COST2_df12 <- data.frame(x1 = 6.92, x2 = 6.92, y1 = 0, y2 = 19090)
COST2_df9 <- data.frame(x1 = 7.29, x2 = 7.29, y1 = 0, y2 = 30000)
COST2_df6 <- data.frame(x1 = 7.83, x2 = 7.83, y1 = 0, y2 = 21620)
COST2_df_annot <- data.frame(x1 = 3, x2 = 6.92, x3 = 7.29, x4 = 7.83, y1 = 47000, y2 = 19090, y3 = 30000, y4 = 21620)

DUR_COST <- ggplot()+
  geom_path(work_6, mapping = aes(x = dur_days, y = cost, linetype = "6 hr/day"), size=1)+
  geom_path(work_9, mapping = aes(x = dur_days, y = cost, linetype = "9 hr/day"), size=1)+
  geom_path(work_12, mapping = aes(x = dur_days, y = cost, linetype = "12 hr/day"), size=1)+
  #geom_vline(xintercept = 146/12, linetype = "solid", color = "red")+
  #geom_vline(xintercept = 146/9, linetype = "dashed", color = "red")+
  #geom_vline(xintercept = 146/6, linetype = "dotted", color = "red")+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), COST_df12)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = "dashed"), COST_df9)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = "dotted"), COST_df6)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), COST_df_annot)+
  geom_segment(aes(x = x1, xend = x3, y = y1, yend = y2, linetype = "dashed"), COST_df_annot)+
  geom_segment(aes(x = x1, xend = x4, y = y1, yend = y2, linetype = "dotted"), COST_df_annot)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), COST2_df12)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = "dashed"), COST2_df9)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = "dotted"), COST2_df6)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), COST2_df_annot)+
  geom_segment(aes(x = x1, xend = x3, y = y1, yend = y3, linetype = "dashed"), COST2_df_annot)+
  geom_segment(aes(x = x1, xend = x4, y = y1, yend = y4, linetype = "dotted"), COST2_df_annot)+
  annotate("pointrange", x = 7.78, y = 16100, ymin = 16100, ymax = 16100, colour = "red", size = 1)+
  annotate("pointrange", x = 11.67, y = 16100, ymin = 16100, ymax = 16100, colour = "red", size = 1)+
  annotate("pointrange", x = 1, y = 90000, ymin = 75000, ymax = 75000, colour = "red", size = 1)+
  annotate("pointrange", x = 1, y = 83000, ymin = 70000, ymax = 70000, size = 0.7)+
  annotate("pointrange", x = 6.9, y = 19090, ymin = 19090, ymax = 19090, size = 0.7)+
  annotate("pointrange", x = 7.2, y = 30000, ymin = 30000, ymax = 30000, size = 0.7)+
  annotate("pointrange", x = 7.8, y = 21620, ymin = 21620, ymax = 21620, size = 0.7)+
  annotate("pointrange", x = 12.2, y = 33580, ymin = 33580, ymax = 33580, size = 0.7)+
  annotate("pointrange", x = 16.2, y = 33580, ymin = 33580, ymax = 33580, size = 0.7)+
  annotate("pointrange", x = 24.3, y = 33580, ymin = 33580, ymax = 33580, size = 0.7)+
  #geom_hline(yintercept = 33580, color = "red")+
  #geom_hline(yintercept = 16100, color = "red")+
  ylab("Cost ($)  ")+
  xlab("Construction Duration (Days)")+
  ggtitle(" ")+
  theme(axis.text = element_text(size = 17), text = element_text(family = "TT Arial"), axis.title.y = element_text(angle = 0, vjust = 0.5 ), axis.title = element_text(size = 17), legend.title = element_text(size = 17), legend.text = element_text(size = 17))+
  guides(linetype=guide_legend(title="Daily Work Duration"))+
  scale_linetype_discrete(breaks=c("12 hr/day","9 hr/day","6 hr/day"))+
  scale_y_continuous(breaks = seq(0, 90000, 10000), limits = c(0,95000), expand = c(0,0))+
  annotate("text", x = 3.4, y = 58000, size=5.5, label = c("SEV-Based Compliance \n Cost at Minimum Duration \n 12 hr/day = $19.090 \n 9 hr/day = $30,000 \n 6 hr/day = $21,620"))+
  annotate("text", x = 13, y = 62000, size=5.5, label = c("SSC-Based Compliance \n Minimum Cost \n ($33,580)"))+
  annotate("text", x = 11, y = 90000, size=5.5, label = c("Optimum Environmentally-Acceptable Solutions ($16,100)"))+
  annotate("text", x = 10, y = 83000, size=5.5, label = c("Minimum Duration, Based on Compliance Approach"))+
  annotate("text", x = 25, y = 93000, size = 5.5, label = c("(b)"))
  #annotate("text", x = 19, y = 46000, label = c("Minimum SSC-Based \n Compliance Duration"))
  #annotate("text", x = 5, y = 14000, label = c("Duration-Based Compliance \n Minimum Cost ($16,100)"))

plot(DUR_COST)

DUR_RATE <- ggplot()+
  geom_path(work_6, mapping = aes(x = dur_days, y = num_dozer, linetype = "6 hr/day"), size=1)+
  geom_path(work_9, mapping = aes(x = dur_days, y = num_dozer, linetype = "9 hr/day"), size=1)+
  geom_path(work_12, mapping = aes(x = dur_days, y = num_dozer, linetype = "12 hr/day"), size=1)+
  #geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), SEV_df12)+
  #geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), SEV_df9)+
  #geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), SEV_df6)+
  #geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), SEV_df_annot)+
  #geom_segment(aes(x = x1, xend = x3, y = y1, yend = y3), SEV_df_annot)+
  #geom_segment(aes(x = x1, xend = x4, y = y1, yend = y4), SEV_df_annot)+
  #annotate("text", x = 19, y = 6.15, label = c("Minimum SSC-Based \n Compliance Duration"))+
  #geom_vline(xintercept = 146/12, linetype = "solid", color = "red")+
  #geom_vline(xintercept = 146/9, linetype = "dashed", color = "red")+
  #geom_vline(xintercept = 146/6, linetype = "dotted", color = "red")+
  #geom_hline(yintercept = 5.37, linetype = "longdash", color = "red")+
  #ylim(0,10)+
  ylab("Number of \n Bulldozers")+
  xlab(" ")+
  ggtitle(" ")+
  theme(axis.text = element_text(size = 17), text = element_text(family = "TT Arial"), axis.title.y = element_text(angle = 0, vjust = 0.5))+
  theme(axis.text.x = element_blank(), axis.title = element_text(size = 17), legend.title = element_text(size = 17), legend.text = element_text(size = 17))+
  guides(linetype=guide_legend(title="Daily Work Duration"))+
  scale_y_continuous(breaks = round(seq(0, 5)), limits = c(0,5), expand = c(0,0))+
  scale_linetype_discrete(breaks=c("12 hr/day","9 hr/day","6 hr/day"))+
  annotate("text", x = 25, y = 4.5, size = 5.5, label = c("(a)"))
  #annotate("text", x = 3, y = 5.27, label = c("CCME (2003) Guidelines \n (SEV = 5.37)"))

plot(DUR_RATE)

grid.arrange(DUR_RATE, DUR_COST, ncol = 1)

SEV_df12 <- data.frame(x1 = 146/12, x2 = 146/12, y1 = 3.9, y2 = 4.95)
SEV_df9 <- data.frame(x1 = 146/9, x2 = 146/9, y1 = 3.9, y2 = 4.78)
SEV_df6 <- data.frame(x1 = 146/6, x2 = 146/6, y1 = 3.9, y2 = 4.53)
SEV_df_annot <- data.frame(x1 = 19, x2 = 146/12, x3 = 146/9, x4 = 146/6, y1 = 6, y2 = 4.95, y3 = 4.78, y4 = 4.53)
SEV2_df12 <- data.frame(x1 = 6.92, x2 = 6.92, y1 = 3.9, y2 = 5.37)
SEV2_df9 <- data.frame(x1 = 7.29, x2 = 7.29, y1 = 3.9, y2 = 5.37)
SEV2_df6 <- data.frame(x1 = 7.83, x2 = 7.83, y1 = 3.9, y2 = 5.37)
SEV2_df_annot <- data.frame(x1 = 7, x2 = 6.92, x3 = 7.29, x4 = 7.83, y1 = 6, y2 = 5.37)


DUR_SEV <- ggplot()+
  geom_path(work_6, mapping = aes(x = dur_days, y = SEV_daily, linetype = "6 hr/day"), size=1)+
  geom_path(work_9, mapping = aes(x = dur_days, y = SEV_daily, linetype = "9 hr/day"), size=1)+
  geom_path(work_12, mapping = aes(x = dur_days, y = SEV_daily, linetype = "12 hr/day"), size=1)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), SEV_df12)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = "dashed"), SEV_df9)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = "dotted"), SEV_df6)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), SEV_df_annot)+
  geom_segment(aes(x = x1, xend = x3, y = y1, yend = y3, linetype = "dashed"), SEV_df_annot)+
  geom_segment(aes(x = x1, xend = x4, y = y1, yend = y4, linetype = "dotted"), SEV_df_annot)+
  annotate("text", x = 19, y = 6.15, label = c("Minimum SSC-Based \n Compliance Durations"), size=5.5)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), SEV2_df12)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = "dashed"), SEV2_df9)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = "dotted"), SEV2_df6)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), SEV2_df_annot)+
  geom_segment(aes(x = x1, xend = x3, y = y1, yend = y2, linetype = "dashed"), SEV2_df_annot)+
  geom_segment(aes(x = x1, xend = x4, y = y1, yend = y2, linetype = "dotted"), SEV2_df_annot)+
  annotate("text", x = 7, y = 6.15, label = c("Chronic Effect Threshold \n Compliance Durations"), size=5.5)+
  annotate("pointrange", x = 4, y = 6.8, ymin = 6.8, ymax = 6.8, size = 0.7)+
  annotate("pointrange", x = 6.9, y = 5.37, ymin = 6.8, ymax = 6.8, size = 0.7)+
  annotate("pointrange", x = 7.29, y = 5.37, ymin = 6.8, ymax = 6.8, size = 0.7)+
  annotate("pointrange", x = 7.8, y = 5.37, ymin = 6.8, ymax = 6.8, size = 0.7)+
  annotate("pointrange", x = 12.2, y = 4.95, ymin = 6.8, ymax = 6.8, size = 0.7)+
  annotate("pointrange", x = 16.2, y = 4.78, ymin = 6.8, ymax = 6.8, size = 0.7)+
  annotate("pointrange", x = 24.3, y = 4.53, ymin = 6.8, ymax = 6.8, size = 0.7)+
  annotate("text", x = 13, y = 6.8, size=5.5, label = c("Minimum Duration, Based on Compliance Approach"))+
  #geom_vline(xintercept = 146/12, linetype = "solid", color = "red")+
  #geom_vline(xintercept = 146/9, linetype = "dashed", color = "red")+
  #geom_vline(xintercept = 146/6, linetype = "dotted", color = "red")+
  geom_hline(yintercept = 5.37, linetype = "longdash", color = "red")+
  ylim(3.9,7.5)+
  ylab("SEV Score    ")+
  xlab("Construction Duration (Days)")+
  ggtitle(" ")+
  theme(text = element_text(family = "TT Arial"), axis.title.y = element_text(angle = 0, vjust = 0.5), axis.title = element_text(size = 17), legend.text = element_text(size = 17), legend.title = element_text(size = 17))+
  scale_y_continuous(breaks = seq(4, 7, 1), limits = c(3.9,7), expand = c(0,0))+
  guides(linetype=guide_legend(title="Daily Work Duration"))+
  scale_linetype_discrete(breaks=c("12 hr/day","9 hr/day","6 hr/day"))+
  annotate("text", x = 2, y = 5.15, label = c("Chronic Effect \n Threshold \n (SEV = 5.37)"), size=5.5)+
  annotate("text", x = 25, y = 6.8, size = 5.5, label = c("(b)"))

plot(DUR_SEV)

df12 <- data.frame(x1 = 146/12, x2 = 146/12, y1 = 0, y2 = 25)
df9 <- data.frame(x1 = 146/9, x2 = 146/9, y1 = 0, y2 = 25)
df6 <- data.frame(x1 = 146/6, x2 = 146/6, y1 = 0, y2 = 25)
df_annot <- data.frame(x1 = 19, x2 = 146/12, x3 = 146/9, x4 = 146/6, y1 = 60, y2 = 25)

DUR_SSC <- ggplot()+
  geom_path(work_6, mapping = aes(x = dur_days, y = C1000, linetype = "6 hr/day"), size=1)+
  geom_path(work_9, mapping = aes(x = dur_days, y = C1000, linetype = "9 hr/day"), size=1)+
  geom_path(work_12, mapping = aes(x = dur_days, y = C1000, linetype = "12 hr/day"), size=1)+
  ylim(0,100)+
  scale_y_continuous(trans='log10')+
  geom_hline(yintercept = 25, color = "red", linetype = "dashed")+
  ylab("SSC (mg/L)")+
  xlab(" ")+
  ggtitle(" ")+
  theme(text = element_text(family = "TT Arial"), axis.text.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5 ), axis.title = element_text(size = 17), legend.text = element_text(size = 17), legend.title = element_text(size = 17))+
  guides(linetype=guide_legend(title="Daily Work Duration"))+
  annotate("text", x = 5, y = 21, size = 5.5, label = c("SSC-Based Threshold \n (SSC = 25 mg/L)"))+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), df12)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = "dashed"), df9)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = "dotted"), df6)+
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), df_annot)+
  geom_segment(aes(x = x1, xend = x3, y = y1, yend = y2, linetype = "dashed"), df_annot)+
  geom_segment(aes(x = x1, xend = x4, y = y1, yend = y2, linetype = "dotted"), df_annot)+
  annotate("pointrange", x = 7.7, y = 87, ymin = 6.8, ymax = 6.8, size = 0.7)+
  annotate("pointrange", x = 12.2, y = 25, ymin = 6.8, ymax = 6.8, size = 0.7)+
  annotate("pointrange", x = 16.2, y = 25, ymin = 6.8, ymax = 6.8, size = 0.7)+
  annotate("pointrange", x = 24.3, y = 25, ymin = 6.8, ymax = 6.8, size = 0.7)+
  annotate("text", x = 16.7, y = 87, size=5.5, label = c("Minimum Duration, Based on Compliance Approach"))+
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0,100), expand = c(0,0))+
  scale_linetype_discrete(breaks=c("12 hr/day","9 hr/day","6 hr/day"))+
  annotate("text", x = 19, y = 65, size = 5.5, label = c("Minimum SSC-Based \n Compliance Duration"))+
  annotate("text", x = 25, y = 95, size = 5.5, label = c("(a)"))

plot(DUR_SSC)

grid.arrange(DUR_SSC, DUR_SEV, ncol = 1)


DUR_SEV_AVE <- ggplot()+
  geom_path(work_6, mapping = aes(x = dur_days, y = SEV_ave, color = "6 hr/day"))+
  geom_path(work_9, mapping = aes(x = dur_days, y = SEV_ave, color = "9 hr/day"))+
  geom_path(work_12, mapping = aes(x = dur_days, y = SEV_ave, color = "12 hr/day")) 
plot(DUR_SEV_AVE)
DUR_SEV_TOT <- ggplot()+
  geom_path(work_6, mapping = aes(x = dur_days, y = SEV, color = "6 hr/day"))+
  geom_path(work_9, mapping = aes(x = dur_days, y = SEV, color = "9 hr/day"))+
  geom_path(work_12, mapping = aes(x = dur_days, y = SEV, color = "12 hr/day"))+
  ylim(4,7.5)  
plot(DUR_SEV_TOT)

RATE_SEV <- ggplot()+
  geom_path(work_6, mapping = aes(x = rate, y = SEV_daily, color = "6 hr/day"))+
  geom_path(work_9, mapping = aes(x = rate, y = SEV_daily, color = "9 hr/day"))+
  geom_path(work_12, mapping = aes(x = rate, y = SEV_daily, color = "12 hr/day"))+
  ylim(4,7.5)+
  scale_x_continuous(trans='log10')
plot(RATE_SEV)

A <- DUR_SSC
B <- DUR_SEV
C <- DUR_COST



plot_grid(A, B, C, nrow = 3, align = "v")

gA <- ggplotGrob(A)
gB <- ggplotGrob(B)
gC <- ggplotGrob(C)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:7], gC$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:7] <- as.list(maxWidth)
gC$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, gC, ncol=1)

grid.arrange(DUR_SSC, DUR_SEV, DUR_COST, nrow = 3)  



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 01 Importing Jiffy csv files and make data frame----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#install.packages("pacman")
pacman::p_load(tidyverse,readxl,skimr,lubridate,janitor,
               patchwork,cowplot) 

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#INPUT
dTime_0 <- list.files(pattern = "*.csv", full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 
#lists,vectors
v_Project<-dTime_0$Project %>% unique() 
v_NrProject<-dTime_0$Project %>% unique() %>% length()
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#take care that no duplicated files, like "..., 2020 (2).csv"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 02 Default values [can be adjusted]----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#vector of colors with length of number of projects
v_used_color<-rainbow(v_NrProject)
v_used_color<-c("red","blue")

#vector of default working hours per work day 
# in order of appearance of projects in jiffy app
v_used_hours<-c(8,0)# = 8 h for main project, 0 h for side project

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 03 Creating data frames----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#color by project#++++++++++++++++++++++++++
dcolor<-dTime_0 %>% 
  janitor::clean_names() %>% 
  select(project) %>% distinct() %>% 
  add_column(P_color=v_used_color)

#df from original data#++++++++++++++++++++++++++
dTime<-dTime_0 %>%
  janitor::clean_names() %>% 
  select(project:note,-task) %>% 
  mutate(
    #depending on typ of csv: with - or with /
    Start=if_else(str_detect(start_time,"/"),
                  parse_datetime(start_time,"%d/%m/%Y %H:%M"),
                  parse_datetime(start_time)),
    End=if_else(str_detect(stop_time,"/"),
                parse_datetime(stop_time,"%d/%m/%Y %H:%M"),
                parse_datetime(stop_time)), 
    #make "time" eg 06:55:33
    s_time=hms::as_hms(paste(hour(Start),
                             minute(Start),
                             second(Start),sep=":")),
    e_time=hms::as_hms(paste(hour(End),
                             minute(End),
                             second(End),sep=":")),
    #make period
    Day=weekdays(Start) %>% as_factor(),
    Day_num=lubridate::wday(Start, 
                            week_start = getOption("lubridate.week.start", 1)), #weekstart is monday
    Elap=Start%--%End,
    Duration=as.period(Start%--%End),
    Week=isoweek(Start),
    Year=isoyear(Start),
    #add year_week (important if several years)
    Year_Week=str_c(Year,Week,sep = "_"), 
    #hours in dpl
    Start_hour=as.POSIXlt(Start)$hour +
      as.POSIXlt(Start)$min/60 + 
      as.POSIXlt(Start)$sec/3600,
    End_hour=as.POSIXlt(End)$hour +
      as.POSIXlt(End)$min/60 + 
      as.POSIXlt(End)$sec/3600) %>% 
  #order
  select(project,s_time,e_time,Start_hour,End_hour,
         note,Duration,minutes,Year,Week,Year_Week,Day,Day_num,
         Start,End,Elap) %>% 
  arrange(Start)

#dTime<-dTime %>% .[1200:1647,] #-----------------------------------subset for now

#extract aim from notes (e.g.: 8,8,8,8,8) #++++++++++++++++++++++++++
dAim<-dTime %>% 
  select(Year_Week,note,project) %>% distinct() %>% 
  drop_na() %>% 
  #take notes with the numbers
  filter(str_detect(note,paste(seq(0,9),seq(0,9),collapse = '|') %>% 
                      str_replace_all(" ",",")))
#get info from note
dAim2<-dAim %>% 
  bind_cols(dAim %>% 
              pull(note) %>% 
              str_split(",",simplify = TRUE) %>% 
              as.tibble()) %>% 
  #0 for weekend
  mutate(V6=rep(0,nrow(.)),V7=rep(0,nrow(.))) %>% 
  #name days
  rename_with(~weekdays(as.Date(seq(7),origin="2021-02-28")),
              .cols = contains("V")) %>% 
  mutate(across(everything(),type.convert)) %>% 
  pivot_longer(cols=Monday:Sunday,names_to = "Day", values_to = "Aim") %>% 
  select(-note) %>% 
  bind_rows(
    #combine with aim with default hours
    expand_grid(Year_Week=dTime %>% 
                  select(Year_Week,note,project) %>% distinct() %>% 
                  #filter only with no hours from notes (hence bind_rows)
                  filter(!Year_Week %in% dAim$Year_Week) %>% pull(Year_Week),
                project=v_Project,
                Day=weekdays(as.Date(seq(7),origin="2021-02-28")) %>%
                  as_tibble() %>% pull(value)) %>% 
      left_join(dTime %>% 
                  select(project) %>% distinct() %>% 
                  #add default hours per project
                  mutate(Aim=v_used_hours)) %>% 
      #0 for weekend
      mutate(Aim=ifelse(Day %in% c("Saturday","Sunday"),0,Aim)))

#add to df, clean formats, color names #++++++++++++++++++++++++++
dTime2<-dTime %>% 
  left_join(dAim2) %>% 
  mutate(across(c(minutes,Day),type.convert)) %>% 
  #level order for plot
  mutate(Day=factor(Day,
                    levels = weekdays(as.Date(seq(7),origin="2021-02-28")))) %>% 
  arrange(s_time) %>% 
  left_join(dcolor)
#dTime2 %>% glimpse()


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 01.1 alteration for my script----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Aim_days=c(
  5#week 27
  ,5
  ,2
  ,0
  ,0
  ,5#week 32
  ,4
  ,5
  ,5
  ,5#week 36
  ,5
  ,5
  ,5 #23-29 sep
  ,5
  ,2#2 days vacation;1 day traveling
  ,5#helsinki
  ,4# week 43: 1 vacation, 1 traveling
  ,4.5#half day friday
  ,5
  ,5
  ,5
  ,5#week 48 
  ,5
  ,5#week 50
  ,3#vacation: 20 ; still to book: 19
  ,0 #klämdag: 23/27 holly:24/25/26
  #2020-----------------
  ,1#week 1: klämdag: 30; holly:31/1; vacation: 2 (boked 23
  ,3.5#week 2: holly:6; planerings:7; half day: 7
  ,5
  ,5 #week 4
  ,5
  ,5
  ,5 #week 7 (half week iris)
  ,5 #week with iris
  ,5 # last week with iris
  ,5
  ,5
  ,5 #corona week 1 with kids
  ,5 #corona week 2 with kids
  ,5 #corona week 3 with kids
  ,3.5 #corona week 4 without kids (week 15); easter, half day
  ,4
  ,5
  ,3.5 #week18,1 may=holyday, 30 apr = halfday
  ,5
  ,5
  ,3  #thursday: ascension; friday: klämdag
  ,5
  ,4  #week 23: saturday=nationalday-> friday off
  ,4  # 11 of June: vacation for kathi defence!
  ,3.5  # friday midsommer
  ,5 
  ,5  # week 27: last working week
  ,0  #
  ,0
  ,2  # week 30: booked vacation but working 2 days
  ,1  #  booked vacation but working 1 days
  ,2  # hence: working only 2 days
  ,5  #week 33: first full working week
  ,5
  ,5
  ,5  #36
  ,5
  ,5
  ,5
  ,5 #40
  ,5
  ,5
  ,4.5 #43, half day
  ,5
  ,5
  ,5
  ,5 #47
  ,4.5 #half day
  ,5
  ,5
  ,5
  ,1 #only 21 dec, last work day
  ,0 #week 53:28 dec-3 jan
  #2021------------------
  ,3.5#,3.5# 6 is nikolaus, 5 is halfday
  ,4 # monday:booked vacation as planeringsdag
  ,5
  ,5
  ,5
  ,5
  ,5
  ,3# vacation 25/26 feb
  ,5
  ,5 
)

# using final dTime2->overwrite
# getting aim in h from aim in days
#  will be unspecific which day not 8h, but its summarized over week anyway
v_weekdays<-weekdays(as.Date(seq(7),origin="2021-02-28"))
d1<-expand_grid(Year_Week=dTime2$Year_Week %>% unique(),
                Day=v_weekdays,
                project=dTime2$project %>% unique()) %>% 
  left_join(tibble(Year_Week=dTime2$Year_Week %>% unique(),
                   Aim_days=Aim_days,
                   Aim=NA))
#make 8 if Aim_days is 5
d2<-d1 %>% 
  mutate(Aim=ifelse(Aim_days==5,8,Aim)) %>% 
  mutate(Aim=ifelse(Aim_days==0,0,Aim)) %>% 
  #arbitrarily take days in order
  mutate(Aim=ifelse(Aim_days==1 & Day %in% v_weekdays[1],
                    8,Aim)) %>% 
  mutate(Aim=ifelse(Aim_days==2 & Day %in% v_weekdays[1:2],
                    8,Aim)) %>% 
  mutate(Aim=ifelse(Aim_days==3 & Day %in% v_weekdays[1:3],
                    8,Aim)) %>% 
  mutate(Aim=ifelse(Aim_days==3.5 & Day %in% v_weekdays[1:3],
                    8,Aim)) %>% 
  mutate(Aim=ifelse(Aim_days==3.5 & Day %in% v_weekdays[4],
                    4,Aim)) %>% 
  mutate(Aim=ifelse(Aim_days==4 & Day %in% v_weekdays[1:4],
                    8,Aim)) %>% 
  mutate(Aim=ifelse(Day %in% v_weekdays[6:7],
                    0,Aim)) %>% 
  #simply Work old new to 0
  mutate(Aim=ifelse(project == "Work old new",
                    0,Aim)) %>% select(-Aim_days)
#sub Aim with correct one
dTime2<-dTime2 %>% 
  select(-Aim) %>% 
  left_join(d2)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 02 plot and tables----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

l_plots<-list()
l_dday<-list()
Nr_Year_Week<-dTime2$Year_Week %>% unique() %>% length()
v_Year_Week<-dTime2$Year_Week %>% unique() %>% sort()

#1.subset each week
#2.make df for day summary,week summary, plot info, and time for plotting
# (also save week summary for calculating total summary)
#3.make each of these df to a plot
#4.combine the plots-->save in plot list 

for (i in 1:Nr_Year_Week ){#1:Nr_Year_Week
#for (i in 1:4 ){#1:Nr_Year_Week
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #data prep
  #make complete balanced for the specific week+++++++++++++++++++++
  dTime3<-dTime2 %>% filter(Year_Week==v_Year_Week[i])
  dTime3new<-dTime3 %>% 
    select(Year_Week,Year,Week,project,Start_hour,End_hour,Day,Day_num,
           minutes,Start,Aim) %>% 
    #completing missing days with no work hours
    full_join(
      expand_grid(
        Year_WeekX=NA,
        YearX=NA,
        WeekX=NA,
        projectX=v_Project,
        Start_hourX=NA,
        End_hourX=NA,
        minutesX=NA,
        StartX=NA,
        AimX=NA,
        DayX=weekdays(as.Date(seq(7),origin="2021-02-28"))) %>% 
        mutate(Day_num=rep(seq(1:7),2))) %>%
    mutate(Day=coalesce(Day,DayX),
           project=coalesce(project,projectX),
           Start_hour=coalesce(Start_hour,Start_hourX),
           End_hour=coalesce(End_hour,End_hourX),
           minutes=coalesce(minutes,minutesX),
           Start=coalesce(Start,StartX),
           Aim=coalesce(Aim,AimX),
           Year_Week=coalesce(Year_Week,Year_WeekX),
           Week=coalesce(Week,WeekX),
           Year=coalesce(Year,YearX)) %>% 
    select(-contains("X")) %>% 
    distinct() %>% 
    left_join(dcolor) %>% 
    #add Na for each project so each project is kept
    bind_rows(
      expand_grid(
        yy=names(dTime3),
        nan=rep(NA,v_NrProject)) %>% 
        pivot_wider(names_from = yy, values_from = nan) %>% unnest() %>% 
        mutate(Year_Week=dTime3$Year_Week %>% unique(),project=v_Project)) %>% 
    #again fixing order for plotting
    mutate(Day=factor(Day,
                      levels = weekdays(as.Date(seq(7),
                                                origin="2021-02-28"))),
           project=factor(project,levels=v_Project)) %>% 
    #adding Start_hour=8, End_hour=16 so at least this range is shown
    full_join(
      expand_grid(Start_hour=8, End_hour=16,
                 project=v_Project,
                Day=weekdays(as.Date(seq(7),origin="2021-02-28")) %>% type.convert()
                )
            )
  #day summary+++++++++++++++++++++
  l_dday[[i]]<-dTime3new %>% 
    select(Year_Week,project,Day,minutes,Aim,Start) %>% 
    arrange(Start) %>% select(-Start) %>% 
    group_by(project,Day,Aim) %>% 
    summarise(hours=sum(minutes,na.rm = T)/60) %>% 
    drop_na(Day) %>% # NA was from: to have all projects in the plot, can be deleted
    #complete(Day,project,fill = list(hours = 0)) %>% 
    distinct() %>%  
    mutate(hours=hours %>% round(1)) %>% 
    add_column(Day_num=as.numeric(.$Day)) %>% 
    arrange(desc(Day_num)) %>%  ungroup() %>% left_join(dcolor) %>% 
    #if not worked aim is zero
    replace_na(list(Aim=0)) %>% 
    #day project combos also have 0 hours/aim (above: for having all combos always)
    #  summarise to fix it
    group_by(project,Day,Day_num,P_color) %>%  
    summarise(Aim=sum(Aim),hours=sum(hours))
  #week summary+++++++++++++++++++++
  dweek<-l_dday[[i]] %>% 
    group_by(project) %>% 
    summarise(Aim=sum(Aim),
              Done=sum(hours)) %>% 
    add_column(Balance=.$Done-.$Aim) %>% 
    #transpose
    pivot_longer(cols = -project, names_to = "Names") %>% 
    pivot_wider(names_from = project, values_from = value) %>% 
    #total
    mutate(Total=rowSums(.[,-1])) %>% 
    #show as 00:00
    mutate(across(!Names,~as.period(.) %>% "*"(60*60) %>% hms::hms() %>% 
                    as.character() %>% str_extract("^.*(?=(:))")))
  dweek2<-dweek %>% pivot_longer(cols = -Names, names_to = "project") %>% 
    left_join(dcolor) %>% 
    mutate(P_color=P_color %>% replace_na("black")) %>% 
    mutate(across(everything(),type.convert))
  #plot info+++++++++++++++++++++
  dinf<-dTime3new %>% 
    select(Year,Week,project) %>% distinct() %>% 
    fill(c(Year,Week),.direction = "updown") %>% 
    arrange(project) %>% 
    mutate(across(everything(),as.character)) %>% 
    #transpose
    pivot_longer(cols = everything(), names_to = "Names",
                 values_to = "project") %>% 
    distinct() %>% 
    #add dates
    bind_rows(
      tibble(Names="Week",
             project=paste0(dTime3new$Start %>% as_date() %>%
                              format("%m-%d") %>% min(na.rm = T),">",
                            dTime3new$Start %>% as_date() %>% 
                              format("%m-%d" )%>% max(na.rm = T)))) %>%
    #add x axis
    mutate(xx=c(1,1,seq(1:v_NrProject),2)) %>% 
    left_join(dcolor) %>% 
    mutate(P_color=replace_na(P_color,"black")) %>% 
    #projects now in main heading-->remove
    filter(Names!="project")
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #plot prep
  #Plot 1: summary table per day+++++++++++++++++++++
  plotTableDay<-l_dday[[i]] %>% 
    ggplot(aes(x=project,y=Day_num,label=hours,color=P_color))+
    scale_color_identity()+
    geom_blank() +
    geom_text(size=1)+
    scale_x_discrete(name = "")+
    theme_minimal()+
    theme(
      axis.text = element_text(size=3),
      line=element_blank(),
      legend.position = "none",
      axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      panel.background=element_rect(fill="white", linetype=1),
      plot.margin = margin(0, 0, 0, 0))+
    ylim(.3,7.7)
  #Plot 2: summary table per week+++++++++++++++++++++
  plotTableWeek<-dweek2 %>% 
    ggplot(aes(x=fct_inorder(project),y=fct_inorder(Names) %>% fct_rev() ,
               label=value,color=P_color))+
    scale_color_identity()+
    geom_blank() +
    geom_text(size=1)+
    theme_minimal()+
    theme(
      axis.text = element_text(size=3),
      line=element_blank(),
      legend.position = "none",
      axis.text.x=element_blank(),
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      panel.background=element_rect(fill="white", linetype=1),
      plot.margin = margin(0, 0, 0, 2))
  #Plot 3: intervals per day+++++++++++++++++++++
  #min max x scales 
  MIN_DH<-dTime3new$Start_hour %>% min(na.rm = T) %>% floor()
  MAX_DH<-dTime3new$End_hour %>% max(na.rm = T) %>% ceiling()
  #(but at least show 8 to 16 h)
  # MIN_DH<-if_else(MIN_DH<8,MIN_DH,8)
  # MAX_DH<-if_else(MAX_DH<16,16,MAX_DH)
  plotTime<-ggplot(dTime3new,
                   aes(xmin = Start_hour, xmax = End_hour, 
                       ymin = Day_num-1, ymax = Day_num , 
                       fill = P_color)) + 
    scale_fill_identity()+
    geom_rect() +
    scale_y_discrete(name="Weekday",
                     breaks=seq(.5,6.5),
                     labels=levels(dTime3new$Day),
                     limits=seq(.5,11.5))+
    scale_x_discrete(name="Time of day",
                     breaks=seq(MIN_DH,MAX_DH,2),
                     labels=seq(MIN_DH,MAX_DH,2),
                     limits=seq(MIN_DH,MAX_DH,2))+
    labs(fill="Project")+
    theme_minimal()+
    theme(
      axis.text = element_text(size=3),
      panel.border = element_rect(fill = NA),
      # panel.grid.major.x = element_line(colour = "grey"),
      # panel.grid.major.y = element_line(colour = "grey"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      plot.margin = margin(0, 0, 1, 1))
  #Plot 4: Plot info+++++++++++++++++++++
  plotInfo<-ggplot(dinf,aes(x=xx,y=Names,label=project,color=P_color))+
    scale_color_identity()+
    geom_blank() +
    geom_text(size=1)+
    theme_minimal()+
    theme(
      axis.text = element_text(size=3),
      line=element_blank(),
      legend.position = "none",
      axis.text.x=element_blank(),
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      panel.background=element_rect(fill="white", linetype=1),
      plot.margin = margin(0, 0, 0, 0))+
    xlim(.5,v_NrProject+1)
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Combining to one plot
  l_plots[[i]]<-plotInfo+plotTableWeek+plotTime+plotTableDay+
    plot_layout(design = "ABBB
CCCD",
                heights = c(1,4)
                ,widths = c(v_NrProject,1))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#complete summary

#(can not be derived from dTime2: Aim of 0 added for plot,
#  several minutes entries for same hour entry,...)

#name list to get Year_Week
names(l_dday)<-v_Year_Week

dtotsum<-l_dday %>%
  #list to tibble
  cbind() %>% as_tibble(rownames="Year_Week") %>%  
  unnest(!Year_Week) %>% 
  group_by(project,Year_Week) %>% 
  summarise(Aim=sum(Aim),hours=sum(hours)) 
#add total as row
dtotsum2<-dtotsum %>% 
  bind_rows(dtotsum %>% 
  group_by(Year_Week) %>% 
  summarise(Aim=sum(Aim),hours=sum(hours)) %>%  
  mutate(project="Total")) %>% 
  mutate(Balance=hours-Aim)
#final summary table
dtotsum3<-dtotsum2 %>% 
  group_by(project) %>% 
  summarise(
    Aim_sum=sum(Aim),
    hours_sum=sum(hours),
    Balance_mean = Balance %>% 
      qwraps2::mean_sd(denote_sd="paren",digits=1),
    PercentAim = (100/Aim_sum*hours_sum) %>% round(0),
    #show as 00:00
    Balance_h = mean(Balance) %>% as.period(.) %>% "*"(60*60) %>% 
      hms::hms() %>% as.character() %>% str_extract("^.*(?=(:))")) %>% 
  #if project aim is zero percent will be Inf
  mutate(PercentAim=ifelse(is.infinite(PercentAim),NA, PercentAim)) %>% 
  left_join(dcolor) %>% 
  mutate(P_color=P_color %>% replace_na("black")) %>% 
  #transpose (needs characters)
  mutate(across(everything(),as.character)) %>% 
  #add description/project/duration as row (later col)
  rbind(c("Description",
          "Sum aimed hours",
          "Sum worked hours",
          "Week balance [mean (sd)]",
          "% of Aim",
          "Week balance [hh:mm]",
          "black")) %>% 
  rbind(c("Info","Projects",
          v_Project,
          rep(NA,4-v_NrProject),NA)) %>% 
  rbind(c("Info2","Time allocation",
          paste0("during last ",length(v_Year_Week)," weeks"),
          paste0("[",dTime2$Start %>% min() %>% as_date(),"  to  ",
          dTime2$End %>% max() %>% as_date(),"]"),NA,NA,"black")) %>%
  #add 
  pivot_longer(cols = -c(project,P_color), names_to = "Names") %>% 
  # factors and order levels
  mutate(Names=factor(Names,levels=c("Aim_sum",
                                     "hours_sum",
                                     "Balance_mean",
                                     "Balance_h",
                                     "PercentAim")),
         project=factor(project,levels=c("Info","Info2","Description",
                                         v_Project,"Total"))) %>% 
  #add color for projects
  full_join(dcolor %>% rename(value=project)) %>% 
  group_by(value) %>% 
  fill(P_color,.direction = "downup") %>% drop_na(project) %>% 
  mutate(P_color=ifelse(value=="Projects","black",P_color))


plotTotalSummary<-dtotsum3 %>% 
  ggplot(aes(x=project,y=Names %>% fct_rev(),
             label=value,color=P_color))+
  scale_color_identity()+
  geom_blank() +
  geom_text(size=1)+
  theme_minimal()+
  theme(
    axis.text = element_text(size=3),
    line=element_blank(),
    legend.position = "none",
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_blank(),
    panel.background=element_rect(fill="white", linetype=1),
    plot.margin = margin(0, 0, 0, 0))
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 03 combine combined plots----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++
#plot last 6 weeks
i_p=length(l_plots)-6
p_1to4<-plot_grid(l_plots[[i_p]],l_plots[[i_p+1]],
                  l_plots[[i_p+2]],l_plots[[i_p+3]],
                  l_plots[[i_p+4]],l_plots[[i_p+5]],ncol = 2)

p_1to4_summary<-plot_grid(plotTotalSummary,p_1to4,
                          nrow = 2,rel_heights = c(.1,1))
#retrieve 4 week time for label
v_T_1<-l_plots[[i_p]]$patches$plots[[1]]$data$project
v_T_2<-l_plots[[i_p+6]]$patches$plots[[1]]$data$project
v_label<-str_c(v_T_1[1],"_",str_sub(v_T_1[3],1,5),"to",str_sub(v_T_2[3],1,5),
               "_Week_",v_T_1[2],"to",v_T_2[2])
#save
save_plot(paste0("Summary/",v_label,".jpeg"),p_1to4_summary,dpi=1200)
#++++++++++++++++













#++++++++++++++++
#plot last 6 weeks
i_p=length(l_plots)-6
p_1to4<-plot_grid(l_plots[[i_p]],l_plots[[i_p+1]],
                 l_plots[[i_p+2]],l_plots[[i_p+3]],
                 l_plots[[i_p+4]],l_plots[[i_p+5]],ncol = 2)
#retrieve 4 week time for label
v_T_1<-l_plots[[i_p]]$patches$plots[[1]]$data$project
v_T_2<-l_plots[[i_p+6]]$patches$plots[[1]]$data$project
v_label<-str_c(v_T_1[1],"_",str_sub(v_T_1[5],1,5),"to",str_sub(v_T_2[5],1,5),
      "_Week_",v_T_1[2],"to",v_T_2[2])
#save
save_plot(paste0(v_label,".jpeg"),p_1to4,dpi=1200)
#++++++++++++++++

#++++++++++++++++
#5 plots, each 6 weeks
for (i in seq(6,by=6,length.out=5)){
#plot last 6 weeks
i_p=length(l_plots)-i
p_1to4<-plot_grid(l_plots[[i_p]],l_plots[[i_p+1]],
                  l_plots[[i_p+2]],l_plots[[i_p+3]],
                  l_plots[[i_p+4]],l_plots[[i_p+5]],ncol = 2)
#retrieve 4 week time for label
v_T_1<-l_plots[[i_p]]$patches$plots[[1]]$data$project
v_T_2<-l_plots[[i_p+6]]$patches$plots[[1]]$data$project
v_label<-str_c(v_T_1[1],"_",str_sub(v_T_1[5],1,5),"to",str_sub(v_T_2[5],1,5),
               "_Week_",v_T_1[2],"to",v_T_2[2])
#save
save_plot(paste0(v_label,".jpeg"),p_1to4,dpi=1200)}
#++++++++++++++++


























g1<-l_plots[[1]] #%>% as_grob()
g2<-l_plots[[2]] #%>% as_grob()
g3<-l_plots[[3]] #%>% as_grob()
g4<-l_plots[[4]] #%>% as_grob()

g1to4<-plot_grid(g1,g2,g3,g4)
save_plot("test1.jpeg",g1to4)






l_plots_grobs<-list()
for (i in 1:length(l_plots)){
  l_plots_grobs[[i]]<-l_plots[[i]] %>% as_grob()
}









jpeg("12okit.jpg",quality=100,res=100,width=480*2,height = 480*2)
par(mfrow=c(2,2))
for (i in 1:4) {
  l_plots_grop[[i]] 
}
dev.off()

l_plots_grop[[2]] %>% ggplot()



#g1,g2,g3,g4
l_plots_grop<-list()
for(i in 1:12){
  l_plots_grop[[i]]<-l_plots[[i]] %>% cowplot::as_grob()
  
}



pdf("p4.pdf",onefile = T)
for (i in 1:4){
  gridExtra::grid.arrange(l_plots_grop[[i]], ncol = 2, nrow=2)
}
dev.off()



#patchwork
library(patchwork)
g1<-l_plots[[1]] %>% cowplot::as_grob()%>% ggpubr::as_ggplot()
g2<-l_plots[[2]] %>% cowplot::as_grob()%>% ggpubr::as_ggplot()
g3<-l_plots[[3]] %>% cowplot::as_grob()%>% ggpubr::as_ggplot()
g4<-l_plots[[4]] %>% cowplot::as_grob()%>% ggpubr::as_ggplot()
g1to4<-l_plots[[1]]+l_plots[[2]]+l_plots[[3]]+l_plots[[4]]






ggdraw(draw_plot(g1),draw_plot(g1))

hh<-ggpubr::as_ggplot(g14)	

g14<-gridExtra::marrang(g1,g2,g3,g4)
ggsave("4_weeks.jpeg",width = 50, height = 30, units = "cm")


g1+g2

l_p<-l_plots[[1]]+l_plots[[2]]+l_plots[[3]]+l_plots[[4]]+
  plot_layout(design = "AB
CD")
l_p
ggsave("Last_12_weeks.jpeg",width = 50, height = 30, units = "cm")


l_plots[[1]]
l_plots[[4]]
ggsave("Last_12_weeks.jpeg",width = 50, height = 30, units = "cm")
# 

#select last 12
l_plotsNew<-l_plots[seq(l_plots %>% length()-11,l_plots %>% length())]

pdf("4_weeks.pdf")
print(l_plots[[4]])
dev.off()


for (i in 1:12){
  gridExtra::grid.arrange(grobs = l_plotsNew[i], ncol = 3, nrow=4)}
dev.off()


















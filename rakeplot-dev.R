# rakeplot (to be renamed)
# ElPaCo conversation & visualization functions

list.of.packages <- c("utf8","tidyverse","ggthemes","ggrepel","lubridate","stringr","stringi","stringdist","viridis","extrafont","mapproj","cowplot","knitr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Load functions
source("..\\elpaco-lingdiv\\elpaco-lingdiv-0-functions.R")

# Load data
source("..\\elpaco-lingdiv\\elpaco-lingdiv-0-data_load.R")


# Prepare data ------------------------------------------------------------

# add topturns: highly frequent recurring turn formats
# add streaks: successive occurrences of top turns by same participant
# we set a flag whenever a prior or next turn by same participant is also a top turn,
# then set streak=1 for consecutive flags and sum the flags to form streak_pos
d <- d %>%
  ungroup() %>%
  mutate(topturn = ifelse(rank < 21 & n > 20,1,0)) %>%
  group_by(source,participant) %>% 
  mutate(flag = ifelse(topturn==1 & ( lead(topturn)==1 | lag(topturn)==1),T,F)) %>%
  mutate(flag = ifelse(is.na(flag),F,flag)) %>% # set all NA flags to F
  arrange(source,participant) %>% # arrange by source and participant
  group_by(group=cumsum(flag != lag(flag,default=first(flag)))) %>%
  mutate(streak_pos = if(first(flag)) cumsum(flag) else NA,
         streak = ifelse(!is.na(streak_pos),1,0)) %>%
  ungroup() %>%
  select(-c(flag,group)) %>%
  arrange(language,uid)


# add measures for chunkiness
# talk_skew: how skewed is talk in the 10s window preceding and including this turn

d <- d %>%
  mutate(talk_skew = abs(talk_rel - 0.5))

# chunk type 1: flanked by streak items
d <- d %>%
  mutate(chunk = ifelse( lag(streak==1) & lead(streak==1) | streak==1 ,1,0))

# chunk type 2: either highly skewed & dyadic, or flanked by streak items

d <- d %>%
  mutate(chunk = ifelse((talk_skew > 0.4 & participants==2) 
                        | lag(streak==1) & lead(streak==1)
                        | streak==1 ,1,0))




# first let's get metadata at source level

totals_by_source <- d %>% group_by(language,source) %>%
  summarize(firstuid=uid[1],
            start=min.na(begin),
            finish=max.na(end),
            turns=n_distinct(uid),
            totaltime = finish - start,
            notiming = sum(is.na(duration)),
            participants = length(unique(participant)),
            useless = ifelse(notiming==turns,1,0))


# set key variables
extract_length <- 1200000 # 20 min
window_size <- 120000 # 2 min
window_breaks <- as.integer(c(0:round(extract_length/window_size)) * window_size)

# select convos of >extract_length that are dyadic (yes we're losing quite some languages)
long_dyadic_convos <- totals_by_source %>% 
  filter(participants==2,
         totaltime > extract_length, 
         notiming == 0) 

unique(long_dyadic_convos$language)
long_dyadic_convos$firstuid[1]


# create dataframe
extract <- convplot(long_dyadic_convos$firstuid[5],datamode=T,before=0,after=extract_length)

# we make lines by categorizing begin values into intervals the width of window_size
# we drop turns that fall outside the larger interval
extract <- extract %>%
  mutate(end = end - min(begin), # reset timestamps to start from 0
         begin = begin - min(begin),
         line = cut(begin,window_breaks,right=F,labels=F)) %>%
  drop_na(line) %>%
  group_by(line) %>%
  mutate(begin0 = begin - min(begin), # reset timestamps to 0 for each new line
         end0 = end - min(begin))

# and we plot
extract %>% 
  ggplot(aes(y=participant_int,fill=chunk)) +
  theme_tufte() + theme(legend.position = "none",
                        strip.text = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.y = element_blank()) +
  ggtitle(paste0(extract$langfull[1]," (",extract$source[1],")")) +
  ylab("") + xlab("time (ms)") +
  scale_fill_viridis(option="plasma",direction=1,begin=0.2,end=0.8) +
  scale_y_reverse(breaks=c(1:2),
                  labels=LETTERS[1:2]) +
  geom_rect(aes(xmin=begin0,xmax=end0,ymin=participant_int-0.6,ymax=participant_int+0.6),
            size=0.3,colour="white") +
  facet_wrap(~line,ncol=1)

# # can of course add a separate layer at some specified threshold

extract %>% 
  ggplot(aes(y=participant_int,fill=talk_skew)) +
  theme_tufte() + theme(legend.position = "none",
                        strip.text = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.y = element_blank()) +
  ggtitle(paste0(extract$langfull[1]," (",extract$source[1],")")) +
  ylab("") + xlab("time (ms)") +
  scale_fill_viridis(option="plasma",direction=1,begin=0.2,end=0.8) +
  scale_y_reverse(breaks=c(1:2),
                  labels=LETTERS[1:2]) +
  geom_rect(aes(xmin=begin0,xmax=end0,ymin=participant_int-0.6,ymax=participant_int+0.6),
            size=0.2,colour="white") +
  geom_point(data=extract %>% filter(topturn == 1),
             aes(x=begin0+200),colour="white",size=4,shape=21,stroke=1) +
  geom_point(data=extract %>% filter(streak == 1),
             aes(x=begin0+200),fill="white",colour="white",size=2,shape=21,stroke=1) +
  facet_wrap(~line,ncol=1)
filename <- paste0("samples/rakeplot-",extract$uid[1],".png")
ggsave(filename,width=12,height=4,bg="white")

# more creative

extract %>% 
  ggplot(aes(y=participant_int,fill=talk_skew)) +
  theme_tufte() + theme(legend.position = "none",
                        strip.text = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.y = element_blank()) +
  ggtitle(paste0(extract$langfull[1]," (",extract$source[1],")"),
          subtitle = "Mapping load to turn height to show competition") +
  ylab("") + xlab("time (ms)") +
  scale_fill_viridis(option="plasma",direction=1,begin=0.1,end=0.9) +
  scale_y_reverse(breaks=c(1:2),
                  labels=LETTERS[1:2]) +
  geom_rect(aes(xmin=begin0,xmax=end0,
                ymin=participant_int-0.8*load,
                ymax=participant_int+0.8*load),
            colour="white") +
  # geom_point(data=extract %>% filter(topturn == 1),
  #            aes(x=begin0+200),colour="white",size=4,shape=21,stroke=1) +
  # geom_point(data=extract %>% filter(streak == 1),
  #            aes(x=begin0+200),fill="white",colour="white",size=2,shape=21,stroke=1) +
  facet_wrap(~line,ncol=1)
filename <- paste0("samples/rakeplot-",extract$uid[1],"-competition.png")
ggsave(filename,width=12,height=4,bg="white")


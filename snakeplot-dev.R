# snakeplot
# ElPaCo conversation & visualization functions



#get a dataframe with standard convplot function to work with

#>>> does not work well for longer timewindows; returns empty dataframe

#e.g. source /siwu1/Two_men_3 has ~3000 turns; when selecting a uid roughly in the middle:

df_siwu = convplot(uids="siwu-7-1769-3838380", datamode=T, before=30000, after=30000) #get 30sec before and 30sec after
#is working

df_siwu = convplot(uids="siwu-7-1769-3838380", datamode=T, before=300000, after=300000) #get 5 min before and 5 min after
#is not working


#maybe because not dyadic; C talking in between? but dyads not specified as TRUE




#df = convplot(uids="dutch-01-021-38922", datamode=T, lang="dutch", n=1, before=(10000*10)*10, after=(10000*10)*10)
#"dutch-01-021-38922" gives 501 turns


#df = convplot(uids="french-12-0478-985300", datamode=T, lang="french", n=1, before=(10000*10)*10, after=(10000*10)*10)
#"french-12-0478-985300" gives 770 turns; used for (10000*10)*10 as before & after; works well



#korean-014-327-695970, 11 min into conversation, so can sample 10 min before and 10 min after




## succesful example

df = convplot(uids="french-12-0478-985300", datamode=T, lang="french", n=1, before=(10000*10)*50, after=(10000*10)*50)


#set up empty column
df$line <- NA


#settings for plot
nr_of_lines = 10
line_time = 500000


#create vectors with start and end times per line
first_line_start = df[1,]$begin0
first_line_end = df[1,]$begin0 + line_time

line_start <- c()
line_end <- c()

for (n in 0:(nr_of_lines-1)) {
  line_start <- append(line_start, first_line_start+(line_time*n))
  line_end <- append(line_end, first_line_end+(line_time*n))
}

#column-bind vectors together into matrix
array_times <- cbind(line_start, line_end)




#loop to add line numbers

for (r in 1:nrow(df)){
  for (l in 1:nr_of_lines){
    if (df[r,]$begin0 >= array_times[l,][['line_start']] & df[r,]$begin0 <= array_times[l,][['line_end']]){
      df[r,]$line = l
    }
  }
}

#delete whatever does not fit on set nr of lines
df <- df %>% subset(!is.na(line))


#loop to reset begin times to 0 on lines 2 and further

for (r in 1:nrow(df)){
  if (df[r,]$line > 1){
    df[r,]$begin0 = (df[r,]$begin0 - (line_time*(df[r,]$line-1)))
    df[r,]$end0 = (df[r,]$end0 - (line_time*(df[r,]$line-1)))
  }
}


#PLOT

p <- df %>%
  mutate(striplength = case_when(duration < 300 ~ 3,
                                 duration >= 300 ~ round(duration/90)),
         uttshort = ifelse(nchar <= striplength | nchar <= 4, 
                           utterance,
                           paste0(stringx::strtrim(utterance,striplength),'~')))%>%
  ggplot(aes(y=participant_int)) +
  theme_tufte() + theme(legend.position = "none",
                        strip.placement = "outside",
                        strip.text = element_text(hjust=0,color="grey50")) +
  ylab("") + xlab("time (ms)") +
  scale_y_reverse(breaks=c(1:max(df$participant_int)),
                  labels=LETTERS[1:max(df$participant_int)])+
  theme(axis.ticks.y = element_blank()) +
  geom_rect(aes(xmin=begin0,xmax=end0,ymin=participant_int-0.6,ymax=participant_int+0.6),
            size=1,fill="grey90",color="white")


p <- p + geom_rect(data=df %>% filter(talk_rel > 0.8),
                     aes(xmin=begin0,xmax=end0,
                         ymin=participant_int-0.6,ymax=participant_int+0.6),
                     size=1,fill="red",color="white")




p <- p + ggforce::facet_col(facets=vars(line), scales="free_y", space="free")
p

       
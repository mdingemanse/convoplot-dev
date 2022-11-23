for (i in 1:length(long_dyadic_convos$firstuid) ) {
  
# create dataframe
extract <- convplot(long_dyadic_convos$firstuid[i],datamode=T,before=0,after=extract_length)

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
  ggplot(aes(y=participant_int,fill=chunk_incl)) +
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


# add cricles for cotniuers

extract %>% 
  ggplot(aes(y=participant_int,fill=chunk_incl)) +
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
  geom_point(data=extract %>% filter(streak == 0, topturn == 1),
             aes(x=begin0+200),fill="#7F7F7F",colour="white",size=3,shape=21,stroke=1) +
  geom_point(data=extract %>% filter(streak == 1, chunk_incl == 1),
             aes(x=begin0+200),colour="white",size=3,shape=21,stroke=1) +
  facet_wrap(~line,ncol=1)
filename <- paste0("samples/rakeplot-chunkhighlight-circles-",extract$uid[1],".png")
ggsave(filename,width=12,height=4,bg="white")

}

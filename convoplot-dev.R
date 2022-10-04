# convoplot
# ElPaCo conversation & visualization functions


# smal things
`%notin%` <- function(x,y) !(x %in% y) 
mean.na <- function(x) mean(x, na.rm = T)
median.na <- function(x) median(x, na.rm= T)
min.na <- function(x) min(x, na.rm = T)
max.na <- function(x) max(x, na.rm = T)
sd.na <- function(x) sd(x, na.rm = T)
sum.na <- function(x) sum(x, na.rm = T)


# helper functions
finduid <- function(string) {
  d[d$uid %in% string,names(d) %in% c("uid","source","begin","end")]
}

# convplot ----------------------------------------------------------------


# options:

# uids        set of uids to plot (optional; if omitted, n uids are sampled)
# lang        language from which to sample uids (if not supplied)
# n           number of uids to sample
# window      time window in ms (optional; if supplied, window will be split into before and after)
# before      stretch to include before selected turn (default: 10000ms, unless `window` is supplied)
# after       stretch to include after selected turn (default: 0, unless `window` is supplied)

# printuids=T print the sampled uids
# verbose=T   print language and information about selected uids

# dyads=F     if TRUE, select only dyadic interactions for display
# content=F   if TRUE, render annotation content (EXPERIMENTAL)
# highlight=F if TRUE, highlight the uid in focus in the plot
# center=F    if TRUE, center the plot around the uid in focus 

# datamode=F  if TRUE, outputs dataframe instead of plot, for more advanced plotting
# alldata=F   if TRUE, output all data, not just the selected dyads
# debug=F     if TRUE, print the selected data and some other diagnostics
convplot <- function(uids=NULL,lang=NULL,n=10,
                     classictranscript=F,
                     window=NULL,before=10000,after=10000,
                     printuids=T,verbose=T,
                     dyads=F,content=F,highlight=F,center=F,
                     datamode=F,alldata=F,debug=F) {
  #check input
  if(classictranscript & length(uids)>1)
    stop("for printing classic transcripts, please provide a single uid, or no uids (it will randomly select one") #warning message
  
  
  if(!is.null(window)) {
    before <- window / 2
    after <- window / 2
  }
  
  if(classictranscript){
    n <- 1 #if classic transcript but no uid given, sample 1 uid
  }
  
  if(!is.null(uids)) {
    n <- length(uids)
  } else {
    if(verbose) {
      print(paste('No uids given, sampling',n,'random one(s)'))
    }
    if(!is.null(lang)) {
      if(verbose) {
        print(paste('...from',lang))
      }
      d.lg <- d %>% filter(language %in% lang)
      uids <- sample(unique(d.lg[!is.na(d.lg$begin),]$uid),n) #make sure not to sample from annotations without time info
      
    } else {
      uids <- sample(unique(d[!is.na(d$begin),]$uid),n) #make sure not to sample from annotations without time info
    }
    
  }
  
  # print uids when asked
  if(printuids) { dput(sort(uids)) }
  
  # get uid metadata and filter uids that fall in the same window
  theseuids <- finduid(uids) %>% arrange(source,begin)
  theseuids %>% group_by(source) %>% 
    mutate(distance = begin - lag(begin)) %>%
    filter(is.na(distance) | distance > before + after)
  
  # create slim df
  extracts <- d[d$source %in% theseuids$source,]
  extracts <- extracts %>%
    arrange(source,begin) %>%
    group_by(source) %>%
    mutate(focus = ifelse(uid %in% uids,"focus",NA),
           scope = NA)
  
  # set scope (= the uid for which the other turns form the sequential context)
  for (thisuid in theseuids$uid) {
    extracts$scope <- ifelse(extracts$source %in% theseuids[theseuids$uid == thisuid,]$source & 
                               extracts$begin >= theseuids[theseuids$uid == thisuid,]$begin - before &
                               extracts$end < theseuids[theseuids$uid == thisuid,]$end + after,thisuid,extracts$scope)
  }
  
  # drop turns outside scope, add useful metadata, compute relative times for each scope
  extracts <- extracts %>%
    drop_na(scope) %>%
    group_by(scope) %>%
    mutate(participant_int = as.integer(as.factor(participant))) %>%
    mutate(begin0 = begin - min(begin),
           end0 = end - min(begin),
           participation = ifelse(n_distinct(participant) < 3,"dyadic","multiparty"))
  nconv <- length(unique(extracts$scope))
  
  extracts.dyadic <- extracts %>% filter(participation == "dyadic")
  ndyads <- length(unique(extracts.dyadic$scope))
  

    
  
  if(verbose) {
    print(paste('seeing',ndyads,'dyads in ',n,'non-overlapping extracts'))
  }
  
  if (debug) {
    
    #print(dyads)
    dput(uids)
    return(extracts)
  }
  
  if (datamode) {
    
    if(alldata) { return(extracts) }
    
    return(extracts.dyadic)
  }
  
  if (classictranscript&highlight){
    p <- extracts %>%
      mutate(line = 1:nrow(extracts), 
             axis_label=paste0(1:nrow(extracts), "   ", LETTERS[1:max(extracts$participant_int)])) %>%
      ggplot(aes(y=participant_int, fill=focus))+
      scale_y_reverse(breaks=c(1:max(extracts$participant_int)),
                      labels=LETTERS[1:max(extracts$participant_int)])+
      theme_tufte() + theme(legend.position = "none",
                            strip.placement = "outside")+
      #theme(axis.title.y.right = element_text(hjust=-10))+
      ylab("") + xlab("") +
      theme(axis.ticks.y = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
      geom_text(aes(label=utterance, color=focus), x=0, nudge_x=-0.5, hjust=0,size=3)+
      labs(caption=paste0("uid: ", extracts$uid, "\nsource: ",extracts$source, "\nwindow: ", extracts$begin, " ms - ", extracts$end, " ms"))+
      theme(plot.caption = element_text(color="darkgrey"))

    p <- p + ggforce::facet_col(facets=vars(line), scales="free_y", space="free", strip.position="left")+
      theme(strip.text.y.left = element_text(angle=0))

    return(p)}
    
  if (classictranscript&!highlight){
      p <- extracts %>%
        mutate(line = 1:nrow(extracts), 
               axis_label=paste0(1:nrow(extracts), "   ", LETTERS[1:max(extracts$participant_int)])) %>%
        ggplot(aes(y=participant_int))+
        scale_y_reverse(breaks=c(1:max(extracts$participant_int)),
                        labels=LETTERS[1:max(extracts$participant_int)])+
        theme_tufte() + theme(legend.position = "none",
                              strip.placement = "outside")+
        #theme(axis.title.y.right = element_text(hjust=-10))+
        ylab("") + xlab("") +
        theme(axis.ticks.y = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
        geom_text(aes(label=utterance), x=0, color="black", nudge_x=-0.5, hjust=0,size=3)+
        labs(caption=paste0("uid: ", extracts$uid, "\nsource: ",extracts$source, "\nwindow: ", extracts$begin, " ms - ", extracts$end, " ms"))+
        theme(plot.caption = element_text(color="darkgrey"))
      
      p <- p + ggforce::facet_col(facets=vars(line), scales="free_y", space="free", strip.position="left")+
        theme(strip.text.y.left = element_text(angle=0))
      
      return(p)
    
  } else {
    
    if(dyads) { extracts <- extracts.dyadic }
    
    #if(participation=="dyadic") {extract <- extracts %>% filter(participant_int %in% c(1,2))}
    
    p <- extracts %>%
      mutate(striplength = case_when(duration < 300 ~ 3,
                                     duration >= 300 ~ round(duration/90)),
            uttshort = ifelse(nchar <= striplength | nchar <= 4, 
                               utterance,
                               paste0(stringx::strtrim(utterance,striplength),'~')))%>%
            # uttshort = ifelse(length(unlist(str_split(utterance,boundary("character"))))>1 && #if utterance is longer than one character
            #               strwidth(utterance, font = 3, units = 'inches')>duration/893, #AND if utterance is longer than desired width
            #             lapply(utterance, truncate_string(string=utterance, duration=duration)), #then truncate it (function returns a string)
            #             utterance))%>%  #or keep as is
      ggplot(aes(y=participant_int)) +
      theme_tufte() + theme(legend.position = "none",
                            strip.placement = "outside",
                            strip.text = element_text(hjust=0,color="grey50")) +
      ylab("") + xlab("time (ms)") +
      scale_y_reverse(breaks=c(1:max(extracts$participant_int)),
                      labels=LETTERS[1:max(extracts$participant_int)])+
      theme(axis.ticks.y = element_blank()) +
      geom_rect(aes(xmin=begin0,xmax=end0,ymin=participant_int-0.4,ymax=participant_int+0.4),
                size=1,fill="grey90",color="white")
    
      if(highlight) { 
      p <- p + geom_rect(data=extracts %>% filter(focus == "focus"),
                         aes(xmin=begin0,xmax=end0,
                             ymin=participant_int-0.4,ymax=participant_int+0.4),
                         size=1,fill="red",color="white")}
    
    if(content) { 
      p <- p + geom_text(aes(label=uttshort,x=begin0+60),
                          color="black",hjust=0,size=3)
      # p <- p + ggrepel::geom_text_repel(aes(label=uttshort,x=begin0+60),
      #                      color="black",hjust=0,size=3)
      #p <- p + ggfittext::geom_fit_text(aes(xmin=begin0, xmax=end0, label=uttshort), min.size=10, grow=F, padding.x=grid::unit(2, "mm"), padding.y=grid::unit(2, "mm"))
    }
    
    #p <- p + facet_wrap(~ scope, ncol=1, scales="free_y")
    #switch to facet_grid to be able to use 'space' parameters; vary height of panel, such that grey blocks are equal height
    # https://stackoverflow.com/questions/28093412/labels-on-top-with-facet-grid-or-space-option-with-facet-wrap
    #use package ggforce
    p <- p + ggforce::facet_col(facets=vars(scope), scales="free_y", space="free")
    
    return(p)
    
    }
}

# from: https://stackoverflow.com/questions/33629491/dplyr-sample-size-greater-than-population-size
### Custom sampler function to sample min(data, sample) which can't be done with dplyr
### it's a modified copy of sample_n.grouped_df
sample_vals <- function (tbl, size, replace = FALSE, weight = NULL, .env = parent.frame()) 
{
  #assert_that(is.numeric(size), length(size) == 1, size >= 0)
  weight <- substitute(weight)
  index <- attr(tbl, "indices")
  sizes = sapply(index, function(z) min(length(z), size)) # here's my contribution
  sampled <- lapply(1:length(index), function(i) dplyr::sample_group(index[[i]],  frac = FALSE, tbl = tbl, 
                                                                     size = sizes[i], replace = replace, weight = weight, .env = .env))
  idx <- unlist(sampled) + 1
  grouped_df(tbl[idx, , drop = FALSE], vars = groups(tbl))
}





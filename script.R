library(rvest)
library(plyr)
library(ggplot2)


base = 'http://bayesian.org/forums'
forums = c('jobs', 'news', 'conferences')
last_post_number = 37714

# Determine what URLs exist
d = ddply(expand.grid(forum=forums, i=1:last_post_number), .(forum,i), function(x) {
  data.frame(exists=RCurl::url.exists(paste(base,x$forum, x$i, sep='/')))
}, .progress='text')
exists = d[which(d$exists),]

write.csv(exists[,c("forum","i")], file="posts.csv", row.names=FALSE)

# Extract posted-on date from URLs that exist
dates = ddply(exists, .(forum,i), function(x) {
  posted_on = html(paste(base,x$forum, x$i, sep='/')) %>% 
    html_node('.posted-on') %>%
    html_text(trim=TRUE) 
  data.frame(posted_on = posted_on, stringsAsFactors=FALSE)
}, .progress='text')

# Convert to R Date object (incorporate this into the previous for next time)
dates$date = as.POSIXct(strptime(stringr::str_extract(dates$posted_on, "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}"),
                                 format = '%Y-%m-%d %H:%M'))

# Add spam count
dates = dates[order(dates$date),]
dates$spam = c(0, diff(dates$i))

# Aggregate by week, count real posts and spam posts
dates$week = cut(dates$date, 'week')
sm = ddply(dates, .(week), summarize, true_posts = length(i), spam_posts = sum(spam))

ggplot(sm, aes(as.Date(week), spam_posts)) + geom_point() + scale_y_log10() + scale_x_date()

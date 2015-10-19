import ggplot as gg
import ultrasignup as us
import numpy as np

d = us.event_results(299)

p1 = gg.ggplot(
  gg.aes(x='time_hour',fill='gender'),d[(d.distance=='50K')&(d.time_hour>1.0)]) + \
  gg.facet_grid(x='gender') + \
  gg.geom_bar(stat="bin",binwidth=.5,position="dodge",colour="black") + \
  gg.xlab("Time (hours)") + gg.ylab("Number of Finishers") + \
  gg.ggtitle("50K Finishing Times for All Years")

p2 = gg.ggplot(
  gg.aes(x='time_hour',fill='gender'),d[(d.distance=='11 Miler')&(d.time_hour>1.0)]) + \
  gg.facet_grid(x='gender') + \
  gg.geom_bar(stat="bin",binwidth=.5,position="dodge",colour="black") + \
  gg.xlab("Time (hours)") + gg.ylab("Number of Finishers") + \
  gg.ggtitle("11M Finishing Times for All Years")

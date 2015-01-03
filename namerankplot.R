## based on the selected name prints the proportion of children given that name each year by sex 
## and prints a plot of the same data
##data is available here - http://www.calvin.edu/~rpruim/data/baby-names.csv

namerankplot <- function(selectname = "Victoria") {  ##Default selectname is Victoria
  
  require(reshape2)  ## reshape2 required for the melt function
  require(ggplot2)   ## ggplot2 required for the ggplot function
  require(plyr)     ## plyr required for rank
  
  setwd("C:/Users/rr046302/Documents/Bill's Stuff/Coursera/R Programming/Baby Names")
  
  baby<-read.csv("baby-names.csv")  ## read in the names file
 
 boynames <- subset (baby, sex == "boy")
 girlnames <- subset (baby, sex == "girl")
 
 boynames <- ddply(boynames,.(year), transform, rank =  order(prop,decreasing=T))  ##using plyr to add rank 
 girlnames <- ddply(girlnames,.(year), transform, rank =  order(prop,decreasing=T))
 
 boy.results <- subset (boynames, boynames$name == selectname)
 girl.results <- subset (girlnames, girlnames$name == selectname)
 
 results <- merge(boy.results, girl.results, by = "year", all = TRUE)
 
 results$sex.x <- NULL
 results$sex.y <- NULL
 results$name.x <- NULL
 results$name.y <- NULL
 results$prop.x <- NULL
 results$prop.y <- NULL
 
 colnames(results) <- c("year", "boy rank", "girl rank")
 
 print(selectname)
 print(results)
 
 results_long <- melt(results, id="year")  # convert to long format
 
 ggplot(data=results_long,
        aes(x=year, y=value, colour=variable)) +
   geom_line() + ggtitle(selectname) + scale_y_continuous(trans = "reverse") + theme(plot.title = element_text(size=32, face="bold", vjust=1))
}


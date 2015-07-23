setwd("/Users/nancyorgan/Documents/ExploR/R")
library("Hmisc")
library("ggplot2")
library("reshape2")
library("data.table")

########### Set up and scraping ##########
url2012 = paste("cran-logs.rstudio.com/2012/", 
                seq(as.Date("2012/10/1"), as.Date("2012/12/31"), "days"),
                ".csv.gz",sep = "")
url2013 = paste("cran-logs.rstudio.com/2013/", 
                seq(as.Date("2013/1/1"), as.Date("2013/12/31"), "days"),
                ".csv.gz",sep = "")
url2014 = paste("cran-logs.rstudio.com/2014/", 
                seq(as.Date("2014/1/1"), as.Date("2014/12/31"), "days"),
                ".csv.gz",sep = "")
url2015 = paste("cran-logs.rstudio.com/2015/", 
                seq(as.Date("2015/1/1"), as.Date("2015/7/15"), "days"),
                ".csv.gz",sep = "")
url = c(url2012,url2013,url2014,url2015)

# Scraping function that takes the urls as argument
get_cran_data = function(url){
  cran_data = rep(list(data.frame(NULL)),length(url))
  for(i in 1:length(url)){
    temp.file = tempfile()
    data.frame(download.file(url = url[i], destfile = temp.file, method = "curl", quiet = TRUE))
    cran_data[[i]] = read.csv(temp.file)
    unlink(temp.file)
    print(paste("You are ", i, "/", length(url),  " of the way done!", sep = ""))
  }
  cran_data_glued = data.frame(rbindlist(cran_data, fill = TRUE))
  return(cran_data_glued)
}

# Run get_cran_data on dates and SAVE to txt files
# Csv's are too big to handle data this size
cranberry2012 = get_cran_data(url2012)
cranberry2013 = get_cran_data(url2013)
cranberry2014 = get_cran_data(url2014)
cranberry2015 = get_cran_data(url2015)
# Check that all dates came though
unique(cranberry2012$date) # etc...

write.table(cranberry2012, "/Users/nancyorgan/Desktop/cranberry2012.txt", sep = ",")
write.table(cranberry2013, "/Users/nancyorgan/Desktop/cranberry2013.txt", sep = ",")
write.table(cranberry2014, "/Users/nancyorgan/Desktop/cranberry2014.txt", sep = ",")
write.table(cranberry2015, "/Users/nancyorgan/Desktop/cranberry2015.txt", sep = ",")

########### Read in individual years' data sets ########
cranberry2012 = fread("/Users/nancyorgan/Desktop/cranberry2012.txt", sep=",", header=FALSE, select = c(2,8))
cranberry2013 = fread("/Users/nancyorgan/Desktop/cranberry2013.txt", sep=",", header=FALSE, select = c(2,8))
cranberry2014 = fread("/Users/nancyorgan/Desktop/cranberry2014.txt", sep=",", header=FALSE, select = c(2,8))
cranberry2015 = fread("/Users/nancyorgan/Desktop/cranberry2015.txt", sep=",", header=FALSE, select = c(2,8))

cran_all = rbind(cranberry2012, cranberry2013, cranberry2014, cranberry2015)
names(cran_all) = c("date", "package")
write.table(cran_all,"/Users/nancyorgan/Desktop/cran_all.txt", sep = ",")
#read.table("/Users/nancyorgan/Desktop/cran_all.csv", sep = ",", header = FALSE)

########### Find the top 10 packages from the data set #######

package_nums = data.frame(table(cran_all$package))
names(package_nums) = c("package", "freq")
package_nums = package_nums[order(-package_nums$freq),]
heavy_hitters = package_nums$package[1:10]

# Trim less popular packages; this is not entirely neccessary but it's nice
# to have a smaller data set to iterate through. 
crantop = cran_all[cran_all$package %in% heavy_hitters,]

# Run through each day of the year and each of the top 10 packages
# to generate a frequency table for daily downloads. This will be more
# ggplot friendly, though will require melting

heavy_counts = data.frame(matrix(rep(0,365*10), ncol = 10))
names(heavy_counts) = heavy_hitters
for(j in 1:length(heavy_hitters)){
  for(i in 1:length(unique(crantop$date))){
  heavy_counts[i,j] = length(crantop$package[crantop$date == unique(crantop$date)[i] & 
                                               crantop$package == heavy_hitters[j]])
  print(paste("You are ", round(i*100/len(unique(crantop$date)), 2), "% done with ", heavy_hitters[j], "!", sep = ""))
  }
  print(paste("You are ", round(j*100/10, 2), "% done!", sep = ""))
}
heavy_counts = heavy_counts[,order(apply(heavy_counts, 2, max), decreasing = TRUE)]
heavy_counts_with_date = cbind(unique(crantop$date), heavy_counts)
write.table(heavy_counts, "/Users/nancyorgan/Desktop/heavy_counts.txt", sep = ",")
write.table(heavy_counts_with_date, "/Users/nancyorgan/Desktop/heavy_counts_with_date.txt", sep = ",")

########### Plotting ######
heavy_counts = read.table("/Users/nancyorgan/Desktop/heavy_counts.txt", sep = ",")  
heavy_counts = heavy_counts[,order(apply(heavy_counts, 2, max), decreasing = TRUE)]

melted_heavy = melt(heavy_counts)
ggplot(melted_heavy, aes(x=variable, y=value)) + 
  geom_violin(aes(x=variable, y=value, fill = variable)) + 
  theme(axis.text.x = element_text(angle = 45, color = "black",size = 10),
        legend.position = "none",
        axis.text.y=element_text(colour="black"),
        axis.title.x = element_text(vjust=3, size = 14),
        axis.title.y = element_text(vjust=1, size = 14)) + 
  ylab("Number of Downloads") +
  xlab("Package") + 
  ggtitle("Daily Downloads for the Top 10
  Downloaded R Packages 2012-2015") + 
  scale_y_continuous(breaks = seq(0, 24000, by = 2000))
  

test = data.frame(matrix(1:9,ncol = 3))
test[,order(apply(test, 2, max),decreasing = TRUE)]
                  
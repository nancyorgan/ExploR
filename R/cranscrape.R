########## Packages, wd(), and path setting #######

setwd("/Users/nancyorgan/Documents/ExploR/R")
library("Hmisc")
library("ggplot2")
library("reshape2")
library("data.table")
library("demogromatics")

data_path = "/Users/nancyorgan/Documents/ExploR/Data/"

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

########### Run get_cran_data on dates and SAVE to txt files #####
# Csv's are too big to handle data this size
cranberry2012 = get_cran_data(url2012)
cranberry2013 = get_cran_data(url2013)
cranberry2014 = get_cran_data(url2014)
cranberry2015 = get_cran_data(url2015)

# Check that all dates came though 
unique(cranberry2012$date) # etc...

write.table(cranberry2012, paste(data_path, "cranberry2012.txt", sep = ""), sep = ",")
write.table(cranberry2013, paste(data_path, "cranberry2013.txt", sep = ""), sep = ",")
write.table(cranberry2014, paste(data_path, "cranberry2014.txt", sep = ""), sep = ",")
write.table(cranberry2015, paste(data_path, "cranberry2015.txt", sep = ""), sep = ",")

########### Read in individual years' data sets ########
cranberry2012 = fread(paste(data_path, "cranberry2012.txt", sep = ""), sep=",", header=FALSE, select = c(2,8))
cranberry2013 = fread(paste(data_path, "cranberry2013.txt", sep = ""), sep=",", header=FALSE, select = c(2,8))
cranberry2014 = fread(paste(data_path, "cranberry2014.txt", sep = ""), sep=",", header=FALSE, select = c(2,8))
cranberry2015 = fread(paste(data_path, "cranberry2015.txt", sep = ""), sep=",", header=FALSE, select = c(2,8))

cran_all = rbind(cranberry2012, cranberry2013, cranberry2014, cranberry2015)
names(cran_all) = c("date", "package")
write.table(cran_all, paste(data_path, "cran_all.txt", sep = ""), sep = ",")
cran_all = fread(paste(data_path, "cran_all.txt", sep = ""), sep = ",", header = FALSE)

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
write.table(heavy_counts, paste(data_path, "heavy_counts.txt", sep = ""), sep = ",")
write.table(heavy_counts_with_date, paste(data_path, "heavy_counts_with_date.txt", sep = ""), sep = ",")

########### Violin Plots ######
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

########### Function to read select variables #########
# function to take a list of years and fread in country-related variables
select_variables = function(years, variables){
  select_each = rep(list(data.frame(NULL)),length(years))
  for(i in c(1:length(years))){
    file_path = paste("/Users/nancyorgan/Documents/ExploR/Data/cranberry", years[i], ".txt", sep = "")
    scan_names = c("", scan(file_path, "", nlines = 1, sep = ","))
    sel = scan_names %in% variables
    select_each[[i]] <- data.frame(fread(file_path, skip = 1, select = which(sel)))
    print(paste("Starting ", i, "/", length(years), " data sets.", sep = ""))
  }
  select_data = data.frame(rbindlist(select_each, fill = TRUE))
  return(select_data)
}

########### Download mapping shapefiles ################

download_country_shapefiles = function(url,shapefile){
  country.shapes = list(rep(data.frame(NULL), length(url)))
  for (i in 1:length(url)) {
    temp.dir = tempdir()
    temp.file = tempfile()
    download.file(url = url[i], destfile = temp.file)
    data = unzip(zipfile = temp.file, exdir = temp.dir)
    country.shapes[[i]] = process.shapefiles(temp.dir,shapefile)
    unlink(temp.dir)
    unlink(temp.file)
  }
  shapefiles.glued = data.frame(rbindlist(country.shapes))
  return(shapefiles.glued)
}
shapefile = "countries"
url = "http://biogeo.ucdavis.edu/data/world/countries_shp.zip"
world_shapefiles = download_country_shapefiles(url, shapefile)
write.table(world_shapefiles, paste(data_path, "world_shapefiles.txt", sep = ""),
            row.names = FALSE, sep = ",")

world_shapefiles_reduced = subset(world_shapefiles, select = c(long, lat, group, ISO2, FIPS))
write.table(world_shapefiles_reduced, paste(data_path, "world_shapefiles_reduced.txt", sep = ""),
            row.names = FALSE, sep = ",")

# remove Antarctica for prettier rendering
world_shapefiles_no_aq = world_shapefiles_reduced[world_shapefiles_reduced$ISO2 != "AQ",]

########### Reading / crunching country data ########
# This specifies the years and variables that will be used as parameters
# in the select_variables function defined above
years = c(2012:2015)
variables = c("date", "package", "country")
country_data = select_variables(years, variables)
names(country_data) = variables
#write.table(country_data, "/Users/nancyorgan/Documents/ExploR/Data/country_data.txt",
#            sep = ",", row.names = FALSE)

total_downloads_per_country = data.frame(table(country_data$country))
names(total_downloads_per_country) = c("ISO2", "freq")
# ddpc -> daily downloads per country
ddpc = data.frame(ISO2 = total_downloads_per_country$ISO2,
                  freq = total_downloads_per_country$freq/length(unique(country_data$date)))
ddpc$freq = round(ddpc$freq, 2)
ddpc$bin = ifelse(ddpc$freq < 0.1,   "Less than 0.1",
           ifelse(ddpc$freq >= 0.1   & ddpc$freq < 1, "Between 0.1 and 1",
           ifelse(ddpc$freq >= 1     & ddpc$freq < 10, "Between 1 and 10",
           ifelse(ddpc$freq >= 10    & ddpc$freq < 100, "Between 10 and 100",
           ifelse(ddpc$freq >= 100   & ddpc$freq < 1000, "Between 100 and 1000",
           ifelse(ddpc$freq >= 1000  & ddpc$freq < 2000, "Between 1000 and 2000",
           ifelse(ddpc$freq >= 2000  & ddpc$freq < 3000, "Between 2000 and 3000",
           ifelse(ddpc$freq >= 3000  & ddpc$freq,  "More than 3000", NA))))))))

ddpc$bin_number = ifelse(ddpc$bin =="Less than 0.1", 1,
                  ifelse(ddpc$bin == "Between 0.1 and 1", 2, 
                  ifelse(ddpc$bin =="Between 1 and 10", 3, 
                  ifelse(ddpc$bin == "Between 10 and 100", 4, 
                  ifelse(ddpc$bin == "Between 100 and 1000", 5, 
                  ifelse(ddpc$bin =="Between 1000 and 2000", 6, 
                  ifelse(ddpc$bin =="Between 2000 and 3000", 7,
                  ifelse(ddpc$bin == "More than 3000", 8, NA))))))))

########## Making choropleth data set and mapping ########
mycolors <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))
choropleth = join(world_shapefiles_no_aq,ddpc)
choropleth$bin_number = as.factor(choropleth$bin_number)
ggplot(data=choropleth, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=bin_number)) +
  scale_fill_manual(values = c("#0000FF", "#6C00E3", "#9200C9", "#AC00AF",
                               "#C00095", "#D0007C", "#DE0062", "#EA0049",
                               "#F5002D", "#FF0000"),
                    name = "Daily Downloads", 
                    labels = c("<0.1", "0.1-1", "1-10","10-100", "100-1000", 
                               "1000-2000", "2000-3000", ">3000")) + 
  coord_map()

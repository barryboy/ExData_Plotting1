make_plot1 <- function(){
        url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        temp <- tempfile()
        
        message(paste("Downloading data from",url))
        download.file(url, destfile = temp, quiet = TRUE, method = "curl")
        message("Finished downloading")
        
        message("Extracting contents\t", appendLF = FALSE)
        #reading the name of the data file inside the zipped file
        contents <- unzip(temp,list = TRUE)[1]["Name"]
        #reading the first 5 rows to detrermine column classes 
        first5rows <- read.table(unz(temp, contents), header = TRUE, nrows = 5, sep = ";", na.strings = "?")
        classes <- sapply(first5rows, class)
        headers <- names(first5rows)
        firstTimestamp <- strptime(paste(as.character(first5rows$Date[1]),as.character(first5rows$Time[1])),format = "%d/%m/%Y %H:%M:%S")
        beginning <- strptime("2007-02-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
        end <- strptime("2007-02-03 00:00:00", format="%Y-%m-%d %H:%M:%S")
        timeToSkip <- (as.numeric(beginning)-as.numeric(firstTimestamp))/60
        periodInMinutes <- (as.numeric(end)-as.numeric(beginning))/60
        #using colClasses argument speeds up the process of reading table
        data <- read.table(unz(temp, contents), header = FALSE, colClasses = classes, sep = ";", na.strings = "?",skip = timeToSkip+1, nrows = periodInMinutes)
        colnames(data) <- headers
        unlink(temp)
        message("Done\n")
        
        #draw the histogram
        gap <- data[["Global_active_power"]]
        hist(gap[!is.na(gap)], xlab = "Global Active Power (kilowatts)", main = "Global Active Power", col = "red")
}

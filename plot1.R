#The make_plot() function accepts 1 argument: 'quiet'. Setting it to TRUE turns off the messages
make_plot1 <- function(quiet = FALSE){
        #the data url
        url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        #temporary holder for downloaded zip file
        temp <- tempfile()
        
        #determining the operating system to choose a proper download method ('curl' for Unix and Mac)
        if(!quiet) { message(paste("Downloading data from", url)) }
        if( Sys.info()["sysname"] == "Windows") {
                download.file(url, destfile = temp, quiet = TRUE)
        } else {
                download.file(url, destfile = temp, quiet = TRUE, method = "curl")
        }
        if(!quiet) { message("Finished downloading") }
        
        #Preparing the data frame
        if(!quiet) { message("Extracting contents\t", appendLF = FALSE) }
        #extracting the name of the actual data file inside compressed file
        contents <- unzip(temp, list = TRUE)[1]["Name"]
        #reading the first 5 rows to detrermine column classes and headers
        first5rows <- read.table(unz(temp, contents), header = TRUE, nrows = 5, sep = ";", na.strings = "?")
        classes <- sapply(first5rows, class)
        headers <- names(first5rows)
        #preparing line numbers to download, based on the timstamps
        first_line_of_data <- strptime(paste(as.character(first5rows$Date[1]), as.character(first5rows$Time[1])), format = "%d/%m/%Y %H:%M:%S")
        beginning_of_sample <- strptime("2007-02-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
        end_of_sample <- strptime("2007-02-03 00:00:00", format="%Y-%m-%d %H:%M:%S")
        time_to_skip <- (as.numeric(beginning_of_sample) - as.numeric(first_line_of_data)) / 60
        nrows_to_download <- (as.numeric(end_of_sample) - as.numeric(beginning_of_sample)) / 60
        #using colClasses argument speeds up the process of reading table
        data <- read.table(unz(temp, contents), header = FALSE, colClasses = classes, sep = ";", na.strings = "?",skip = time_to_skip+1, nrows = nrows_to_download)
        #adding headers
        colnames(data) <- headers
        #releasing the temporary file
        unlink(temp)
        if(!quiet) { message("Done\n") }
        
        ### PLOT ###
        #subset the data
        gap <- data[["Global_active_power"]]
        #draw the histogram
        if(!quiet) { message("Drawing the histogram") }
        hist(gap[!is.na(gap)], xlab = "Global Active Power (kilowatts)", main = "Global Active Power", col = "red")
        #copy the contents of graphical device
        if(!quiet) { message("Saving .png file") }
        dev.copy(png, file = "plot1.png")
        dev.off()
        if(!quiet) { message("Finished.") }
}

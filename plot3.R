#The make_plot() function accepts 1 argument: 'quiet'. Setting it to TRUE turns off the messages
make_plot3 <- function(quiet = FALSE){
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
        #setting locale, so that the day names are printed in English
        Sys.setlocale("LC_TIME", "English")
        #combining the Date and Time columns
        data$Timestamp <- strptime(paste(as.character(data$Date), as.character(data$Time)), format = "%d/%m/%Y %H:%M:%S")
       
        if(!quiet) { message("Opening png device") }
        png("plot3.png", width = 480, height = 480)
        
        #drawing the plot with type = "n" to make an empty frame first
        plot(data$Timestamp, data$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
        legend("topright", lty = "solid", col = c("black", "blue", "red"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        lines(data$Timestamp, data$Sub_metering_1)
        lines(data$Timestamp, data$Sub_metering_2, col = "red")
        lines(data$Timestamp, data$Sub_metering_3, col = "blue")

        dev.off()
        if(!quiet) { message("Finished.") }
}
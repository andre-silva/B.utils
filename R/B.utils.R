# Reads Workday report into a data frame
getReportFromWorkday <- function(URL, destFile = NULL, authFile = "settings") {
    # First get Workday authentication credentials. From my research I didn't
    # any great way of doing that. Since obviously we don't want those in the
    # code the solution is to have a text file containing username and password.
    # The file name and path should be passed on the authFile argument.
    # Otherwise it will look for a file with the name "settings" in the working
    # directory
    
    # Settings file should have the following format
    # username:jdoe
    # password:Pass123
    
    # Libraries necessary
    suppressWarnings(library(XML))
    suppressWarnings(library(jsonlite))
    suppressWarnings(library(RCurl))
    
    # Check if the file exists and has the correct format
    if(!file.exists(authFile)) {
        stop("Unable to find authentication file")
    }
    else {
        authSettings <- read.table(authFile, header=FALSE, sep=":",
                                   colClasses = "character",
                                   col.names = c("Property", "Value"))
        format <- c("username","password")

        if(all(length(format)==length(authSettings$Property)) &
           all(format == authSettings$Property)) {
            username <- authSettings$Value[1]
            password <- authSettings$Value[2]
        }
        else {
            stop("Authentication settings file format is not the expected")
        }
    }
    
    # Check and validate format
    if (!grepl("&format=", URL)){
        format <- "workdayxml"
    }
    else {
        format <- unlist(strsplit(URL, "&format=", fixed = TRUE))[2]
    }
    
    # Use separate function based on format
    tmpFile <- "workdayFile.tmp"
    switch(format,
           workdayxml = {
               if(is.null(destFile)) {
                   stop(paste("Format not supported as data frame conversion",
                        "for this format needs to be custom. Set the parameter",
                        "destFile to just download the file",sep = " "))
               }
               else {
                   f <- CFILE(tmpFile, mode = "w")
                   a <- curlPerform(url = URL, username = username,
                                    password = password,
                                    writedata = f@ref)
                   close(f)
                   
                   if(a != 0 ) {
                       file.remove(tmpFile)
                       stop(paste("Error downloading the file. Please check",
                                  "the URL is correct as well as the username",
                                  "and password", sep = " "))
                   }
                   
                   result <- TRUE
               }
           },
           simplexml = {
               f <- CFILE(tmpFile, mode = "w")
               a <- curlPerform(url = URL, username = username,
                                password = password,
                                writedata = f@ref)
               close(f)
               
               if(a != 0 ) {
                   file.remove(tmpFile)
                   stop(paste("Error downloading the file. Please check",
                              "the URL is correct as well as the username",
                              "and password", sep = " "))
               }
               
               result <- xmlParse(tmpFile)
               result <- xmlToDataFrame(result)
           },
           csv = {
               f <- CFILE(tmpFile, mode = "w")
               a <- curlPerform(url = URL, username = username,
                                password = password,
                                writedata = f@ref)
               close(f)
               
               if(a != 0 ) {
                   file.remove(tmpFile)
                   stop(paste("Error downloading the file. Please check",
                              "the URL is correct as well as the username",
                              "and password", sep = " "))
               }
               
               result <- read.csv(tmpFile)
           },
           gdata = {
               if(is.null(destFile)) {
                   stop(paste("Format not supported as data frame conversion",
                              "for this format needs to be custom. Set the parameter",
                              "destFile to just download the file",sep = " "))
               }
               else {
                   f <- CFILE(tmpFile, mode = "w")
                   a <- curlPerform(url = URL, username = username,
                                    password = password,
                                    writedata = f@ref)
                   close(f)
                   
                   if(a != 0 ) {
                       file.remove(tmpFile)
                       stop(paste("Error downloading the file. Please check",
                                  "the URL is correct as well as the username",
                                  "and password", sep = " "))
                   }
                   
                   result <- TRUE
               }
           },
           json = {
               f <- CFILE(tmpFile, mode = "w")
               a <- curlPerform(url = URL, username = username,
                                password = password,
                                writedata = f@ref)
               close(f)
               
               if(a != 0 ) {
                   file.remove(tmpFile)
                   stop(paste("Error downloading the file. Please check",
                              "the URL is correct as well as the username",
                              "and password", sep = " "))
               }
               
               result <- fromJSON(tmpFile)
               result <- result$Report_Entry
           },
           {
               file.remove(tmpFile)
               stop(paste("Format not supported: ", format))
           })
    

    if(is.null(destFile)) {
        ## Remove temporary file
        file.remove(tmpFile)
    }
    else {
        ## Save the file locally
        file.copy(tmpFile, destFile, overwrite = TRUE)
        file.remove(tmpFile)
    }
    
    result
}

# B.com color palette - function designed to work in a similar fashion to
# the rainbow function
B.color <- function(color = "darkblue1") {
    # Create color palette as defined in
    # https://wiki.booking.com/display/~rtomasi/Booking.com+color+palette
    # on 2016-2-2
    B.colors <- c(darkblue = "#003580",
                  lightblue = "#0896FF",
                  yellow = "#FEBA02",
                  darkgray = "#7C90A6",
                  green = "#55AF32",
                  red = "#E52923",
                  orange = "#EF6C0A",
                  darkblue2 = "#02214C",
                  lightblue2 = "#155EAB",
                  yellow2 = "#CF812D",
                  darkgray2 = "#3E4853",
                  green2 = "#2C5520",
                  red2 = "#9D2124",
                  orange2 = "#A44C20",
                  darkblue3 = "#355E97",
                  lightblue3 = "#3CB3E7",
                  yellow3 = "#FDCE59",
                  darkgray3 = "#A3B1BF",
                  green3 = "#7BBD65",
                  red3 = "#E96B6B",
                  orange3 = "#F09860",
                  darkblue4 = "#819BBF",
                  lightblue4 = "#72C5F0",
                  yellow4 = "#FEE29E",
                  darkgray4 = "#BEC8D2",
                  green4 = "#9BCD8A",
                  red4 = "#EE9494",
                  orange4 = "#F5B68C",
                  darkblue5 = "#B3C2D8",
                  lightblue5 = "#B4E2F6",
                  yellow5 = "#FFF0CE",
                  darkgray5 = "#E5E9ED",
                  green5 = "#CEE5C3",
                  red5 = "#F5BEBF",
                  orange5 = "#FAE2D0",
                  darkblue6 = "#E2EDF9",
                  lightblue6 = "#ECF7FE",
                  yellow6 = "#FFF8E6",
                  darkgray6 = "#F2F4F6",
                  green6 = "#DEEDD8",
                  red6 = "#FCE9E9",
                  orange6 = "#FDF0E8"
                  )
    
    # If argument is a number then returns vector with colors with length equal
    # to the argument. Otherwise searches for the color name within the vector
    if(is.numeric(color)) {
        B.colors[1:color[1]]
    }
    else {
        B.colors[tolower(color)]
    }
}
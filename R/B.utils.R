#' Get Report from Workday
#'
#' This function allows you to get a web-service enabled report from Workday.
#' If the format is Simple XML, CSV or JSON then the destFile parameter is
#' optional and the function returns a dataframe with the report contents.
#'
#' For other formats, the destFile parameter is mandatory and the only thing
#' this function will do is downloading the report into your hard drive.
#'
#' In any format, it is recommended to set the destFile parameter as the data
#' might be more complex and the simple dataframe conversion in this function
#' might not suit your needs
#'
#' @param URL This is the URL Workday provides for the specific format you're
#' looking to download
#' @param destFile This is the location and file name of the downloaded report
#' @param authFile This is the file containing your authentication settings in
#' Workday. Defaults to NULL where it gets the username and password from the
#' environment variables workday_user and workday_pwd respectively. Alternatively,
#' a filename can be specified and the credentials will be read from it. The file
#' should have the following format:
#'
#' username:jdoe
#' password:Pass123
#' @export
#' @examples
#' workdayURL <- "https://wd3-services1.myworkday.com/ccx/service/customreport2/booking/HSMIT/All_Active_and_Terminated_Workers_with_IDs?Effective_as_of_Date=2016-02-02-08%3A00&format=simplexml"
#' destfile <- "./Data/report.xml"
#' authFile <- "settings.txt"
#' df <- getReportFromWorkday(workdayURL, destFile, authFile)

# suppressWarnings(library(XML))
# suppressWarnings(library(jsonlite))
# suppressWarnings(library(RCurl))

getReportFromWorkday <- function(URL, destFile = NULL, authFile = NULL) {
    # First get Workday authentication credentials. The best way I found to do
    # is to store them as R environment variables. To make that permanent it's
    # only necessary to add them on the  ~./.Renviron file. Another option is to
    # use a text file with the credentials defined in it.
    # The file name and path should be passed on the authFile argument.

    # If using the environment variables, these are the keys that should be set:
    # workday_user
    # workday_pwd

    # Settings file should have the following format
    # username:jdoe
    # password:Pass123

    # Libraries necessary
    suppressWarnings(library(XML))
    suppressWarnings(library(jsonlite))
    suppressWarnings(library(RCurl))

    if(is.null(authFile)) {
        # Read from environment variables
        username <- Sys.getenv("workday_user")
        password <- Sys.getenv("workday_pwd")
    }
    else {
        if(!file.exists(authFile)) {
            stop("Unable to find authentication file")
        }
        else {
            # Read from file
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

#' Returns color vector
#'
#' This function returns one color or a vector of colors, similar to the
#' rainbow() function. The only difference is it uses the company color
#' palette
#'
#' @param color It can be the color name in which case the function will only
#'      return that color or a number in which case the function will return a
#'      vector of colors with the parameter's length. Defaults to "darkblue"
#' @export
#' @examples
#' B.color(10)
B.color <- function(color = "darkblue1") {
    # Create color palette as defined in
    # https://wiki.booking.com/display/~rtomasi/Booking.com+color+palette
    # on 2016-2-2
    B.colors <- c(darkblue = "#003580",
                  yellow = "#FEBA02",
                  green = "#55AF32",
                  red = "#E52923",
                  darkgray = "#7C90A6",
                  orange = "#EF6C0A",
                  lightblue = "#0896FF",
                  darkblue2 = "#02214C",
                  yellow2 = "#CF812D",
                  green2 = "#2C5520",
                  red2 = "#9D2124",
                  darkgray2 = "#3E4853",
                  orange2 = "#A44C20",
                  lightblue2 = "#155EAB",
                  darkblue3 = "#355E97",
                  yellow3 = "#FDCE59",
                  green3 = "#7BBD65",
                  red3 = "#E96B6B",
                  darkgray3 = "#A3B1BF",
                  orange3 = "#F09860",
                  lightblue3 = "#3CB3E7",
                  darkblue4 = "#819BBF",
                  yellow4 = "#FEE29E",
                  green4 = "#9BCD8A",
                  red4 = "#EE9494",
                  darkgray4 = "#BEC8D2",
                  orange4 = "#F5B68C",
                  lightblue4 = "#72C5F0",
                  darkblue5 = "#B3C2D8",
                  yellow5 = "#FFF0CE",
                  darkgray5 = "#E5E9ED",
                  green5 = "#CEE5C3",
                  red5 = "#F5BEBF",
                  orange5 = "#FAE2D0",
                  lightblue5 = "#B4E2F6",
                  darkblue6 = "#E2EDF9",
                  yellow6 = "#FFF8E6",
                  green6 = "#DEEDD8",
                  red6 = "#FCE9E9",
                  darkgray6 = "#F2F4F6",
                  orange6 = "#FDF0E8",
                  lightblue6 = "#ECF7FE"
                  )

    # If argument is a number then returns vector with colors with length equal
    # to the argument. Otherwise searches for the color name within the vector
    if(is.numeric(color)) {
        unname(B.colors[1:color[1]])
    }
    else {
        unname(B.colors[tolower(color)])
    }
}

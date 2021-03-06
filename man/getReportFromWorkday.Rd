% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/B.utils.R
\name{getReportFromWorkday}
\alias{getReportFromWorkday}
\title{Get Report from Workday}
\usage{
getReportFromWorkday(URL, destFile = NULL, authFile = NULL)
}
\arguments{
\item{URL}{This is the URL Workday provides for the specific format you're
looking to download}

\item{destFile}{This is the location and file name of the downloaded report}

\item{authFile}{This is the file containing your authentication settings in
Workday. Defaults to NULL where it gets the username and password from the
environment variables workday_user and workday_pwd respectively. Alternatively,
a filename can be specified and the credentials will be read from it. The file
should have the following format:

username:jdoe
password:Pass123}
}
\description{
This function allows you to get a web-service enabled report from Workday.
If the format is Simple XML, CSV or JSON then the destFile parameter is
optional and the function returns a dataframe with the report contents.
}
\details{
For other formats, the destFile parameter is mandatory and the only thing
this function will do is downloading the report into your hard drive.

In any format, it is recommended to set the destFile parameter as the data
might be more complex and the simple dataframe conversion in this function
might not suit your needs
}
\examples{
workdayURL <- "https://wd3-services1.myworkday.com/ccx/service/customreport2/booking/HSMIT/All_Active_and_Terminated_Workers_with_IDs?Effective_as_of_Date=2016-02-02-08\%3A00&format=simplexml"
destfile <- "./Data/report.xml"
authFile <- "settings.txt"
df <- getReportFromWorkday(workdayURL, destFile, authFile)
}


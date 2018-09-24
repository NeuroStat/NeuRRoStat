#' Plot trend of keywords in Google Scholar.
#'
#' This function does something.
#'
#' @param keyword the keyword you would like to search
#' @param start startdate (in year, e.g. 2000)
#' @param end enddate (in year, e.g. 2017)
#'
#' @export
plotKeyWord <- function(keyword,start,end){
warning('This function only works if you have Python installed either through anaconda2.
Or you have the following Python modules installed:
* bs4,
* urllib,
* urllib2,
* cookielib,
* re,
* time,
* sys')


# First locate a version of Python and throw error if no python is installed
WherePy <- system('which python', intern = TRUE)
if(!grepl(pattern = 'python', x = WherePy)){stop('Python needs to be installed')}
    
# Give warning that one needs Anaconda
print('Please make sure you installed Anaconda first')

# Create link to external script called extract_occurrences.py
extractPY <- system.file("extscrpt", "extract_occurrences.py", package = "NeuRRoStat")

# Create search command
searchArgs <- paste(extractPY, " '", keyword, "' ", start, " ", end, sep = "")

# First try to gather data using default python
warnMessDefPy <- 'Not all modules available in base Python, trying with Anaconda'
defPyStatus <- 0
defPy <- tryCatch({
    system2(command = WherePy, args = searchArgs, stdout = TRUE, stderr = TRUE)
  }, warning = function(w) {
    print(warnMessDefPy)
  }
)
if(defPy == warnMessDefPy){
  defPyStatus <- 1
}

# If needed, try using Anaconda
if(defPyStatus == 1){
  WherePy <- "/anaconda2/bin/python"
  AnfPy <- tryCatch({
    system2(command = WherePy, args = searchArgs, stdout = TRUE, stderr = TRUE)
  }, warning = function(w) {
    error('Unable to retrieve data, stopping here.')
  }
  )
}

# Assign working results to the same object
if(defPyStatus == 0){
  assign(x = 'dataKW', defPy)
} else{
  assign(x = 'dataKW', AnfPy)
}

# Now process the data, first position is not needed.
# Then split strings into two columns and rename into year and count.
dataKW_proc <- data.frame(dataKW[-1]) %>% apply(., 1, strsplit, split = ',') %>%
  unlist() %>% as.numeric() %>% matrix(., ncol = 2, byrow = TRUE) %>%
  as.data.frame() %>% rename(Year = V1, Count = V2)

Plot <- ggplot(dataKW_proc,aes(Year,Count)) + geom_point() + geom_line(colour="#990000") +
  ggtitle(paste("Publications mentioning", keyword, " in Google Scholar.", paste = "")) +
  scale_x_continuous(breaks = pretty(dataKW_proc$Year, n = 10)) +
  theme(plot.title = element_text(lineheight=.2, face="bold"),
        axis.title = element_text(size=16),
        axis.text = element_text(size=14),
        panel.grid.major = element_line(colour='gray', size=0.3),
        panel.background = element_blank())
print(Plot)
}

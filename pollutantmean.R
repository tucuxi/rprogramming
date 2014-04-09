pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
  
        s = 0
        n = 0L
        for (i in id) {
                mon <- read.csv(sprintf("%s/%03d.csv", directory, i))
                mon <- na.omit(subset(mon, select = pollutant))
                if (nrow(mon) > 0) {
                        s = s + sum(mon)
                        n = n + nrow(mon)
                }
        }
        s / n
}

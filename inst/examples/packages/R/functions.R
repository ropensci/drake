# This file contains all the functions of the workflow.
# If needed, you could split it up into multiple files.

# We need to know the date of the latest available log file
# in order to keep our logs up to date.

latest_log_date <- function(){
  read_html("http://cran-logs.rstudio.com/") %>%
    html_nodes("li:last-of-type") %>%
    html_nodes("a:last-of-type") %>%
    html_text() %>%
    max
}

# And we need to define functions to summarize
# and plot the data.

make_my_table <- function(downloads){
  group_by(downloads, package) %>%
    summarize(mean_downloads = mean(count))
}

make_my_plot <- function(downloads){
  ggplot(downloads) +
    geom_line(aes(x = date, y = count, group = package, color = package))
}

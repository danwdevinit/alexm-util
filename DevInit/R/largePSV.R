path <- "C:/Users/alexm/Downloads/CRS 2013 data/CRS 2013 data.txt"

dat <- read.table(
  path
  ,header = TRUE
  ,sep = "|"
  ,quote = "\""
  ,dec = "."
  ,fill = TRUE
  ,comment.char = ""
  ,fileEncoding="UTF-16LE"
  )

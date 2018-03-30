# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

grr_sampledata <- function() {
  fnm = file.path(base::system.file(package = "graphileonR"), "extdata/data.json")
  #Return json
  jsonlite::toJSON(jsonlite::fromJSON(fnm))
}

#curl http://coreos3.pranageo.com/ocpu/apps/pranageo/graphileonR/R/grr_meananalysis/json -X POST -F sampledata=@graphileonR/inst/extdata/data.json
#a=grr_meananalysis(sampledata = paste(readLines("inst/extdata/data.json"),collapse = " "))
grr_meananalysis <- function(sampledata) {
  sampledata <- jsonlite::fromJSON(sampledata)
  labels = c("work","sleep", "sport", "family", "shopping", "other")
  output <- sapply(labels, function(x){
    mean(sampledata[,x])
  })
  names(output) <- labels
  print(class(output))
  jsonlite::toJSON(as.list(output), auto_unbox = TRUE, pretty=TRUE)
}

#curl http://coreos3.pranageo.com/ocpu/apps/pranageo/graphileonR/R/grr_multiplereg/json -X POST -F sampledata=@graphileonR/inst/extdata/data.json
grr_multiplereg <- function(sampledata){
  sampledata <- jsonlite::fromJSON(sampledata)
  mymodel <- lm(sleep ~ work + family + shopping, data = sampledata)
  print(summary(mymodel))
  mymodel
}

pgocpu_temp_token <- function(urlpart){
  out <- strsplit(urlpart, "/")[[1]]
  out <- out[4]
  out
}
pgocpu_get_vars <- function(outputs, serv.url){
  outenv <- new.env()
  serv.url <- gsub("ocpu", "", serv.url)

  token <- pgocpu_temp_token(outputs[[1]][1])
  var_string <- paste0("/ocpu/tmp/", token, "/R/")
  idx <- grep(var_string, outputs)
  if (length(idx)>0){
    var_names <- sapply(idx, function(x) {
      strsplit(outputs[x], var_string)[[1]][2]

      sapply(idx, function(x){
        res22 <- httr::GET(paste0(serv.url, outputs[1], "/json"))
        #assign("", val, outenv)
      })
    })
  }
    a=1
  browser()

}
server = "http://coreos3.pranageo.com/ocpu"
res <- httr::GET(url=file.path(server, "info"))
res <- httr::POST(url=file.path(server, "library/MASS/scripts/ch01.R"))
out <-rawToChar(res$content)
out2 <- strsplit(out, "\n")
#pgocpu_get_vars(out2[[1]], server)

token <- pgocpu_temp_token(out2[[1]][1])
res11 <- httr::POST(url=paste0(server, "/tmp/", token, "/R/dd/sds"))

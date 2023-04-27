pathReport <- function(path, patt, dironly, level){
  if(level <= 0){
    if(dironly){
      path[dir.exists(path)]
    }else{
      path
    }
  }else{
    files <- list.files(path=path, pattern=patt, recursive = FALSE, full.names=TRUE)
    directories <- files[dir.exists(files)]
    if(length(directories)>0){
      if(dironly){
        c(path[dir.exists(path)], pathReport(files, patt, dironly=dironly, level=level-1))
      }else{
        c(path, pathReport(files, patt, dironly=dironly, level=level-1))
      }
    }else{
        path[dir.exists(path)]
      }
  }
}
  
sizeReport <- function(path, patt = '.*', dironly = FALSE, level = Inf){
    z <- pathReport(path, patt, dironly, level)
    size <- file.size(z)
    res <- data.frame(z,size)
    colnames(res) <-c('path','size')
    print(res)
}

# put into practice
sizeReport(path = "", level = 3, dironly = TRUE)




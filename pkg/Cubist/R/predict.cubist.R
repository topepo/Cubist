
predict.cubist <- function (object, newdata = NULL, neighbors = 0, ...) 
{
  
  if(is.null(newdata)) stop("newdata must be non-null")
  
  ## check order of data to make sure that it is the same
  newdata <- newdata[, object$vars$all,drop = FALSE]


  if(length(neighbors) > 1) stop("only a single value of neighbors is allowed")
  if(neighbors > 9) stop("'neighbors' must be less than 10")
  if(neighbors > 0)
    {
      object$model <- gsub("insts=\"0\"",
                           paste("insts=\"1\" nn=\"",
                                 neighbors,
                                 "\" maxd=\"",
                                 object$maxd,
                                 "\"",
                                 sep = ""),
                           object$model)
    }

  
  ## make cases file
  caseString <- makeDataFile(x = newdata, y = NULL)
  
  Z <- .C("predictions",
          as.character(caseString),
          as.character(object$names),
          as.character(object$data),
          as.character(object$model),
          pred = double(nrow(newdata)),    
          output = character(1),
          PACKAGE = "Cubist"
          )

  ## for testing
  ## cat(Z$output, '\n')

  Z$pred
}

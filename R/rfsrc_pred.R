predict_model.rfsrc <- function(x, newdata, type, ...) {
  res <- predict(x, newdata = newdata, ...)
  x=res$predicted
  switch(
    type,
    raw = data.frame(Response = apply(predicted, 1, which.max), stringsAsFactors = FALSE),
    prob = as.data.frame(x, check.names = FALSE)
  )
}
model_type.rfsrc<- function(x, ...) 'classification'
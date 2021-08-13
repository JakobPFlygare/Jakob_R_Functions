## Script containing all R functions

#Replace NA with median or mode
replaceNAwithMeanMode <- function(x) {
  if(is.factor(x)) replace(x, is.na(x), Mode(na.omit(x)))
  else if(is.numeric(x)) replace(x, is.na(x), mean(x, na.rm=TRUE))
  else x
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


## Convert to factors
colToFactor <- function(df,cols) {
  df[cols] <- lapply(df[cols], factor)
  df
  
}

colToNumeric <- function(df,cols) {
  df[cols] <- sapply(df[cols], as.numeric)
  df
  
}


#Converting rows into columns. Similar to a group by with a (case when...) then

myspread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}




options(warn=1)
library(dplyr,warn.conflicts=FALSE)
library(tibble)
S3_KEYS<-list(papelera='papelera.rds')
.s3_read<-function(k)NULL
.s3_write<-function(obj,k)invisible(NULL)
.normalize<-function(df,fn){s<-fn();for(col in names(s))if(!col %in% names(df))df[[col]]<-s[[col]][NA_integer_];df}
"%||%"<-function(a,b)if(!is.null(a))a else b
source('R/persistence.R',local=FALSE)
source('tests/test_papelera_filtering.R',local=FALSE)
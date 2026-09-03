if (file.exists(".Renviron")) readRenviron(".Renviron")
bucket <- Sys.getenv("S3_BUCKET")
raw <- tryCatch(aws.s3::s3readRDS(object = "networks/app_audit.rds", bucket = bucket),
                error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
if (!is.null(raw)) {
  cat("class:", paste(class(raw), collapse = ", "), "\n")
  cat("nrow:", nrow(raw), "  ncol:", ncol(raw), "\n")
  cat("names:", paste(names(raw), collapse = ", "), "\n")
  if (nrow(raw)) {
    print(utils::tail(raw[order(raw$ts), c("ts","user","module","action","description")], 15))
  }
}

if (file.exists(".Renviron")) readRenviron(".Renviron")
bucket <- Sys.getenv("S3_BUCKET")
for (prefix in c("networks/app_audit", "hd-admin/app_audit")) {
  cat("== prefix:", prefix, "==\n")
  objs <- tryCatch(aws.s3::get_bucket(bucket = bucket, prefix = prefix, max = 200L),
                    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); list() })
  if (!length(objs)) { cat("  (no keys found)\n\n"); next }
  for (o in objs) cat("  ", o[["Key"]], " size=", o[["Size"]], " modified=", o[["LastModified"]], "\n")
  cat("\n")
}

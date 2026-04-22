

### -------------------- DOWNLOAD ALL DEPOSITION FILES --------------------- ###

#' @export
DOWNLOAD_ALL_DEPOSITION_FILES <- function(id, path, token, instance, language = "en") {

  url = GET_BASE_URL(instance)

  res     = httr::GET(paste0(url, "/api/deposit/depositions/", id),
                query = list(access_token = token)
  )
  httr::stop_for_status(res)
  deposition   = httr::content(res, as = "parsed")
  filenames    = unlist(lapply(deposition$files,
                               function(x) x$filename))
  cat("\n", MSSG(language)$downloading_files_from, deposition$title, "\n")
  for (filename in filenames) {
    cat(paste0("      ", MSSG(language)$downloading," ", filename, "\n"))
    res = httr::GET(
      paste0(deposition$links$bucket, "/", filename),
      httr::add_headers(Authorization = paste("Bearer", token)),
      httr::write_disk(file.path(path, filename), overwrite = TRUE)
    )
    httr::stop_for_status(res)
  }
}







### ------------------ FUNCTION TO UPLOAD DATA TO ZENODO ------------------- ###




#' @export
UPLOAD_TO_DEPOSITION <- function(filepath,  name, id, token, instance, language = "en") {

  url = GET_BASE_URL(instance)

  deposition  = GET_DEPOSITION(id, token, instance)
  bucket_link = deposition$links$bucket
  cat("\n", MSSG(language)$uploading_file,
      paste0("'",name,"'"),
      MSSG(language)$to,
      deposition$title, "\n")
  res = httr::PUT(
    paste0(bucket_link, "/", basename(name)),
    httr::add_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/octet-stream"
    ),
    body = httr::upload_file(filepath)
  )

  httr::stop_for_status(res)
  deposition <- GET_DEPOSITION(id, token, instance)
  return(paste0(url, "/record/", id, "/files/", name))
}






### ----------------------------- DELETE FILE ------------------------------ ###




#' @export
DELETE_FILE <- function(filename, id, token, instance, language = "en"){
  deposition <- GET_DEPOSITION(id, token, instance)
  filenames = unlist(lapply(deposition$files, function(x) x$filename))
  link      = unlist(lapply(deposition$files, function(x) x$links$self))
  for(i in which(filenames == filename)){

    cat("\n", MSSG(language)$deleted_file,
        paste0("'",filename,"'"),
        MSSG(language)$from,
        deposition$title, "\n")
    res <- httr::DELETE(link[i],
                  httr::add_headers(Authorization = paste("Bearer", token)))
    httr::stop_for_status(res)
  }
}





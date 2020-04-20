#' Reads up to date information about Bovespa companies from a github file
#'
#' A csv file with information about available companies, file links and time periods is read from github.
#' This file is manually updated by the author. When run for the first time in a R session, a .RDATA file
#' containing the output of the function is saved for caching.
#'
#' @param type.data A string that sets the type of information to be returned ('companies' or 'companies_files').
#' If 'companies', it will return a dataframe with several information about companies, but without download links.
#' @inheritParams gdfpd.GetDFPData
#'
#' @return A dataframe with several information about Bovespa companies
#' @export
#'
#' @examples
#'
#' \dontrun{ # keep cran check fast
#' df.info <- gdfpd.get.info.companies()
#' str(df.info)
#' }
download_read_dfp_zip_file <- function(url_in, 
                                       companies_cvm_codes,
                                       type_format,
                                       cache_folder = 'gcvmd_cache', clean_data) {
  
  # create folder
  dir_zip <- file.path(cache_folder, 'DFP_zip_files')
  if (!dir.exists(dir_zip)) dir.create(dir_zip, recursive = TRUE)
  
  message('\tDowloading ', basename(url_in), appendLF = TRUE)
  dest_file <- file.path(dir_zip, basename(url_in))
  
  flag_dl <- my_download_file(dl_link = url_in, 
                              dest_file = dest_file, 
                              max_dl_tries = 10, be_quiet = TRUE)
  
  message('\t\tUnzipping', appendLF = TRUE)
  # unzip file in tempdir
  #message('\t\t\tunzipping file')
  unzip_dir <- file.path(tempdir(), tools::file_path_sans_ext(
    basename(url_in) ) ) 
  unzip(zipfile = dest_file,exdir = unzip_dir,
        junkpaths = TRUE)
  
  unzipped_files <- list.files(unzip_dir, full.names = TRUE)
  
  # filter by type and format
  type_files_format <- as.character(
    purrr::map(stringr::str_match_all(basename(unzipped_files), 'aberta_(.*)_\\d\\d\\d\\d'),
               2)
  )
  
  idx <- (type_files_format %in% type_format)
  unzipped_files <- unzipped_files[idx]
  
  #message('\t\t\t\tfound ', length(unzipped_files), ' files')
  #message('\t\t\treading files', appendLF = FALSE)
  
  df_out <- dplyr::bind_rows(purrr::map(unzipped_files, 
                                        read_dfp_csv, clean_data = clean_data))
  
  # filter by company
  if (!is.null(companies_cvm_codes)) { 
    idx <- df_out$CD_CVM %in% companies_cvm_codes
    df_out <- df_out[idx, ]
  } 
  
  message('\t\tGot ', nrow(df_out), ' rows | ', length(unique(df_out$CD_CVM)), ' companies')
  
  return(df_out)
  
}

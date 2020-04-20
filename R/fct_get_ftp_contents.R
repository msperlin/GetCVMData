get_contents_ftp <- function(ftp_url) {
  require(rvest)

  my_html <- xml2::read_html(ftp_url)
  
  all_links <- my_html %>%
    html_nodes('td') %>%
    html_text()
  
  idx <- str_detect(all_links, 'cia_aberta|/')
  all_links  <- all_links[idx]
  
  full_links <- paste0(ftp_url, all_links)
  
  year_files <- as.numeric(str_extract_all(full_links, '\\d\\d\\d\\d'))

  df_out <- dplyr::tibble(file_name = all_links,
                          full_links,
                          year_files)
  
  return(df_out)
  
}

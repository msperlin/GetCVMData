#' Downloads and reads DFP datasets
#'
#' The DFP (demonstrativos financeiros padronizados) is the annual reporting system of companies
#' traded at B3. This function will access the CVM ftp and parse all available files according to user 
#' choices
#' 
#' @param companies_cvm_codes Numeric CVM code  of companies. IF set to NULL (default), will return data for all available companies.
#' @param first_year First year of selected data
#' @param last_year Last year of selected data
#' @param type_docs Type of financial documents: BPA = Assets (ativos), BPP = Liabilities (passivo), 
#'                  DRE = income statement (demonstrativo de resultados),
#'                  DFC_MD = cash flow by direct method (fluxo de caixa pelo metodo direto), 
#'                  DFC_MI = cash flow by indirect method (fluxo de caixa pelo metodo indireto),
#'                  DMPL = equity mutations (mutacoes do patrimonio liquido),
#'                  DVA = value added report (desmonstrativo de valor agregado)
#' @param type_format Type of format of document (con = consolidated, ind = individual). Default = both (con|ind)
#' @param clean_data Clean data or return raw data? See read_dfp|itr_csv() for details
#' @param use_memoise Use memoise caching? If no (default), the function will read all .csv files. If yes, will use package 
#'                    memoise for caching results (code speed increases significantly)
#' @param cache_folder Path of cache folder to keep memoise and zip files
#'
#' @return A dataframe with selected data
#' @export
#'
#' @examples
#' \dontrun{
#' df_dfp <- get_dfp_data()
#' }
get_dfp_data <- function(companies_cvm_codes = NULL, 
                         first_year = 2010,
                         last_year = lubridate::year(Sys.Date()),
                         type_docs = 'BPA|BPP|DRE',
                         type_format = 'con|ind',
                         clean_data = TRUE,
                         use_memoise = FALSE,
                         cache_folder = 'gcvmd_cache') {
  
  # check args
  available_docs <- c('BPA', 
                      'BPP',
                      'DFC_MD',
                      'DFC_MI', 
                      'DMPL',
                      'DRE',
                      'DVA')
  
  if (type_docs == '*') {
    type_docs  <- available_docs
  } else {
    type_docs <- str_split(type_docs, '\\|')[[1]]
  }
  
  idx <- type_docs %in% available_docs
  if (any(!idx)) {
    stop(paste0('Cant find type type_docs: ', paste0(type_docs[!idx], collapse = ', ')),
         '\n\n', 
         'Available type_docs are: ', paste0(available_docs, collapse = ', '))
  }
  
  available_formats <- c("ind",
                         "con" )
  
  type_format <- str_split(type_format, '\\|')[[1]]
  
  
  idx <- type_format %in% available_formats
  if (any(!idx)) {
    stop(paste0('Cant find type type_format: ', paste0(type_format[!idx], collapse = ', ')),
         '\n\n', 
         'Available type_format are: ', paste0(available_formats, collapse = ', '))
  }
  
  if ((!is.null(companies_cvm_codes))&(!is.numeric(companies_cvm_codes))) {
    stop('Input companies_cvm_codes should be numeric (e.g. ')
  }
  
  df_cvm <- get_info_companies(cache_folder)
  
  # start download of files
  ## get docs
  
  l_args <- list(type_doc = type_docs,
                 companies_cvm_codes = rep(list(companies_cvm_codes), length(type_docs)),
                 type_format = rep(list(type_format), length(type_docs)),
                 first_year = first_year, 
                 last_year = last_year,
                 cache_folder = cache_folder,
                 clean_data = clean_data)

  df_dfp <- bind_rows(purrr::pmap(.l = l_args, 
                        .f = get_dfp_docs) )
  return(df_dfp)
  
}

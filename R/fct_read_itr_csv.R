read_itr_csv <- function(file_in, clean_data) {
  
  message('\t\tReading ', basename(file_in), appendLF = TRUE)
  df <- read_csv2(file = file_in, 
                  col_types = readr::cols(CD_CVM = readr::col_number(),
                                          CD_CONTA = readr:::col_character(),
                                          VL_CONTA = readr::col_number()),
                  locale = readr::locale(decimal_mark = ',', encoding = 'Latin1'), 
                  progress = FALSE)
  
  if (clean_data) {
    
    #message(' | cleaning data', appendLF = FALSE)
    
    # filter penultimo cases
    idx <- df$ORDEM_EXERC == 'PENÚLTIMO'
    df <- df[!idx, ]

  }
  
  # set col for cnpj number
  unique_cnpj <- unique(df$CNPJ_CIA)
  number_cnpj <- sapply(unique_cnpj, fix_cnpj)
  
  idx <- match(df$CNPJ_CIA, unique_cnpj)
  df$cnpj_number <- number_cnpj[idx]
  
  # set filename
  df$source_file <- basename(file_in)
  
  return(df)
}
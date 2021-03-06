% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_download_file.R
\name{my_download_file}
\alias{my_download_file}
\title{Downloads files from the internet}
\usage{
my_download_file(dl_link, dest_file, max_dl_tries = 10, be_quiet = TRUE)
}
\arguments{
\item{dl_link}{Link to file}

\item{dest_file}{Local file destination}

\item{max_dl_tries}{Maximum number of attempts}

\item{be_quiet}{Be quiet?}
}
\value{
Status (TRUE = OK)
}
\description{
This function will make k attempts of download and use different download options according to operating system.
}
\examples{

my_url <- paste0('http://www.rad.cvm.gov.br/enetconsulta/',
                  'frmDownloadDocumento.aspx?CodigoInstituicao=2',
                  '&NumeroSequencialDocumento=46133')

\dontrun{ # keep CHECK fast
dl_status <- my_download_file(my_url, 'tempfile.zip', 10)
}
}

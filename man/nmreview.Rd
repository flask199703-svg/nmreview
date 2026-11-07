\name{nmreview}
\alias{nmreview}
\alias{run_app}
\alias{nmreview_app}
\alias{import_project}
\alias{compare_models}
\alias{read_bootstrap}
\alias{read_vpc}
\alias{save_archive}
\alias{load_archive}
\alias{archive_summary}
\alias{demo_project}
\title{Local Review of NONMEM and PsN Results}
\description{Read completed estimation results, compare diagnostics, and preserve compact bootstrap and VPC results in a portable archive.}
\usage{
run_app(host = "127.0.0.1", port = NULL, launch.browser = interactive(), demo = FALSE)
nmreview_app(demo = FALSE)
}
\arguments{
\item{host}{Local address on which Shiny listens.}
\item{port}{Optional local TCP port.}
\item{launch.browser}{Open the application in a browser.}
\item{demo}{Initially load the reproducible synthetic example.}
}
\details{Use the application to browse a NONMEM output folder, select any reference and comparison model, inspect diagnostics, and import PsN results. See the repository documentation for supported formats and archive semantics. The included example is generated synthetic data.}
\value{\code{nmreview_app} returns a Shiny application object. \code{run_app} runs the application. Other exported functions read and return model or analysis lists or write portable archives as documented in the repository.}
\examples{
if (interactive()) run_app(demo = TRUE)
}

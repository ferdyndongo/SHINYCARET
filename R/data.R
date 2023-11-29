#' Grana Padano 
#'
#' DB completo aggiornato il 23/08/2022 per costruzione del modello di classificazione
#' @format ## `BDGrana2023`
#' A data frame with 2,219 rows and 20 columns:
#' \describe{
#'   \item{anno}{anno di registrazione del campione}
#'   \item{numero}{identificativo del campione}
#'   \item{matricola}{identificativo della provenienza del campione}
#'   \item{class}{classificazione del tipo di grana}
#'   \item{D13C}{}
#'   \item{D15N}{}
#'   \item{D2H}{}
#'   \item{Li}{}
#'   \item{Na}{}
#'   \item{Mn}{}
#'   \item{Fe}{}
#'   \item{Cu}{}
#'   \item{Se}{}
#'   \item{Rb}{}
#'   \item{Sr}{}
#'   \item{Mo}{}
#'   \item{Ba}{}
#'   \item{Re}{}
#'   \item{Bi}{}
#'   \item{U}{}
#' }
#' @source laboratorio dell'unità traciabilità:  <https://cri.fmach.it/Unita-di-ricerca/Tracciabilita>
"BDGrana2023"

#' #' Classification model for grana data 
#' #'
#' #' Random Forest Model for grana data updated at 23/08/2022
#' #' @format ## `model`
#' #' A data frame with 2,220 rows and 00 columns:
#' #' \describe{
#' #'   \item{method}{}
#' #'   \item{modelInfo}{}
#' #'   \item{modelType}{}
#' #'   \item{results}{}
#' #'   \item{pred}{}
#' #'   \item{bestTune}{}
#' #'   \item{call}{}
#' #'   \item{dots}{}
#' #'   \item{metric}{}
#' #'   \item{control}{}
#' #'   \item{finalModel}{}
#' #'   \item{preProcess}{}
#' #'   \item{trainingData}{}
#' #'   \item{ptype}{}
#' #'   \item{resample}{}
#' #'   \item{resampledCM}{}
#' #'   \item{perfNames}{}
#' #'   \item{maximize}{}
#' #'   \item{yLimits}{}
#' #'   \item{times}{}
#' #'   \item{levels}{}
#' #'   \item{terms}{}
#' #'   \item{coefnames}{}
#' #'   \item{xlevels}{}
#' #' }
#' #' @source laboratorio dell'unità traciabilità:  <https://cri.fmach.it/Unita-di-ricerca/Tracciabilita>
#' "model"

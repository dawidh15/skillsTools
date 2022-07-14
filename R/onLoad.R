skillsEnv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname)
{
  skillsEnv$dbname <- "iattc_pd.sqlite3"
  skillsEnv$LLTunaBilltbl <- "LLTunaBillfish"
}

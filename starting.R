library("devtools")
options(devtools.name = "Alain Danet",
  devtools.desc.author = "person('Alain', 'Danet',
  email='alain.danet@mnhn.fr', role = c('aut', 'cre'))",
  devtools.desc.license = "MIT + file LICENSE"
)

setup()
use_testthat()
use_vignette("intro")
use_travis()
use_package_doc()
use_cran_comments()
use_readme_rmd()
use_mit_license()

#To build metropolis talk
install.packages("binb")

upload_file(
  file = here::here("paper/manuscript.Rmd"),
  hide_code = TRUE
)

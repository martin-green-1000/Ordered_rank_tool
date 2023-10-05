

# Base image 

FROM rocker/shiny:4.0.1

LABEL org.opencontainers.image.licenses="GPL-2.0-or-later" \
      org.opencontainers.image.source="https://github.com/rocker-org/rocker-versioned2" \
      org.opencontainers.image.vendor="Rocker Project" \
      org.opencontainers.image.authors="Carl Boettiger <cboettig@ropensci.org>"



RUN /rocker_scripts/install_tidyverse.sh

# install  packages

 Install R packages
RUN install2.r --error \
    fontawesome \
    shiny \
  readr \
  readxl \
  shinyjs \
  bslib \
  shinydashboard \
  fmsb \
  reshape2 \
  stringr \
  shinydashboardPlus \
  sortable \
  shinyFiles \
  RColorBrewer \
  reshape2


COPY R ./app


# expose port
EXPOSE 5648

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 5648)"]

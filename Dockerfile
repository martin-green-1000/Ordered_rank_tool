

# Base image 

FROM rocker/shiny:4.0.1

LABEL org.opencontainers.image.licenses="GPL-2.0-or-later" \
      org.opencontainers.image.source="https://github.com/rocker-org/rocker-versioned2" \
      org.opencontainers.image.vendor="Rocker Project" \
      org.opencontainers.image.authors="Carl Boettiger <cboettig@ropensci.org>"



RUN /rocker_scripts/install_tidyverse.sh

# install  packages


RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sortable', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readxl', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('bslib', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyFiles', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('fontawesome', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('fmsb', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RColorBrewer', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reshape2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plot.matrix', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('htmlwidgets', repos='http://cran.rstudio.com/')"


COPY R ./app


# expose port
EXPOSE 5648

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 5648)"]

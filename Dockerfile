# NOTE: Build without caching to ensure latest version of git repo
#       docker build --no-cache -t cleanad .
# Would be better if synapser docker images were tagged
FROM rocker/rstudio:4.1.0

RUN R -e "install.packages('synapser', repos=c('http://ran.synapse.org', 'http://cran.fhcrc.org'))"

RUN install2.r --error \
    config \
    dplyr \
    glue \
    lubridate \
    purrr \
    readr \
    readxl \
    tidyr \
    log4r \
    mockery \
    optparse \
    testthat

RUN apt-get update --allow-releaseinfo-change && \
    apt-get install git-all -y

# Clone repo and install
RUN git clone https://github.com/Sage-Bionetworks/cleanAD.git && \
    chmod +x cleanAD/update_table.sh
RUN R CMD INSTALL ./cleanAD

CMD ["/bin/bash"]

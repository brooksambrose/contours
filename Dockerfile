FROM rocker/binder:3.6.3

# Copy your repository contents to the image
COPY --chown=${NB_USER}:${NB_USER} . ${HOME}

# set local r installation paths
ENV LOCR=/home/${NB_USER}/.local/R

RUN mkdir ${LOCR}

ENV R_LIBS_SITE ${LOCR}

# mybinder.org resource limits
ENV CPU_LIMIT ${CPU_LIMIT}
ENV MEM_LIMIT ${MEM_LIMIT}

# install system requirements first
USER root

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    htop \
    zlib1g-dev `#data.table` \
    libpoppler-cpp-dev `#pdftools` \
    libtesseract-dev libleptonica-dev tesseract-ocr-eng `#tesseract` \
    libxml2-dev `#igraph #xml2` \
  && apt-get purge \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN R CMD javareconf

#RUN R -e 'install.packages("qdap",Ncpus=parallel::detectCores()-1)'

USER ${NB_USER}

## Run an install.R script, if it exists.
RUN if [ -f install.R ]; then R --quiet -f install.R; fi

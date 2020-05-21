FROM rocker/binder:3.6.3

# Copy your repository contents to the image
COPY --chown=rstudio:rstudio . ${HOME}

# set local r installation paths
ENV LOCR=/home/rstudio/.local/R
RUN mkdir ${LOCR}
ENV R_LIBS_SITE ${LOCR}

# install system requirements first
USER root
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libpoppler-cpp-dev `#pdftools` \
  && apt-get purge \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*
USER ${NB_USER}

## Run an install.R script, if it exists.
RUN if [ -f install.R ]; then R --quiet -f install.R; fi

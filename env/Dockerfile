FROM mambaorg/micromamba

USER $MAMBA_USER

COPY --chown=$MAMBA_USER:$MAMBA_USER conda.yml .

RUN micromamba install -n base -f conda.yml -y \
	&& micromamba clean -a -y

ARG MAMBA_DOCKERFILE_ACTIVATE=1

USER root

RUN apt-get update && apt-get install -y procps && \
    apt-get clean

USER $MAMBA_USER

ENV PATH=/opt/conda/bin:/opt/conda/condabin:$PATH

RUN Rscript -e "install.packages('stationaRy', dependencies = TRUE, repos = 'http://cran.us.r-project.org')"

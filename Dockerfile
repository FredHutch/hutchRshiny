# build me as fredhutch/shinymicrobiome
FROM fredhutch/r-shiny-server-base:3.6.0
RUN apt-get update
RUN apt-get install -y pandoc libsodium-dev
RUN R -e "install.packages(c('BiocManager','remotes', 'detectseparation'),dependencies=TRUE, repos='http://cran.rstudio.com/')" && \
    R -e "BiocManager::install('phyloseq')" && \
    R -e "remotes::install_github('bryandmartin/corncob')" && \
    R -e "remotes::install_github('adw96/breakaway')"
RUN rm -rf /srv/shiny-server/
ADD ./app/ /srv/shiny-server/01_hello
EXPOSE 8888
WORKDIR /srv/shiny-server/01_hello
CMD /usr/bin/shiny-server.sh

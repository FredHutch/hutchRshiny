# build me as fredhutch/shinymicrobiome
FROM fredhutch/r-shiny-server-base:latest
RUN apt-get update
RUN apt-get install -y pandoc supervisor nginx
RUN R -e "install.packages(c('BiocManager','devtools'),dependencies=TRUE, repos='http://cran.rstudio.com/')" && \
    R -e "BiocManager::install('phyloseq')" && \
    R -e "devtools::install_github('bryandmartin/corncob')" && \
    R -e "devtools::install_github('adw96/breakaway')"
RUN rm -rf /srv/shiny-server/
ADD ./app/ /srv/shiny-server/01_hello
EXPOSE 8888
CMD /usr/bin/supervisord -c /srv/shiny-server/01_hello/system/sup.conf

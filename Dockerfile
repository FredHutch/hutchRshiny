FROM fredhutch/r-shiny-base:latest
RUN apt-get update
RUN apt-get install -y pandoc
RUN R -e "install.packages('BiocManager',dependencies=TRUE, repos='http://cran.rstudio.com/')" && \
    R -e "BiocManager::install('phyloseq')" && \
    R -e "devtools::install_github('bryandmartin/corncob')" && \
    R -e "devtools::install_github('adw96/breakaway')"
RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny
ADD app/. /home/shiny/
RUN chown -R shiny:shiny /home/shiny 
WORKDIR /home/shiny
USER shiny
EXPOSE 7777
CMD Rscript start.R 
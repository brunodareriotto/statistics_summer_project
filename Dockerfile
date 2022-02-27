FROM rocker/tidyverse:4.0.0

    RUN R -e "install.packages('basedosdados')"
    RUN R -e "install.packages('showtext')"
    RUN R -e "install.packages('geobr')"
    RUN R -e "install.packages('grid')"
    RUN R -e "install.packages('viridis')"
    RUN R -e "install.packages('readxl')"
    RUN R -e "install.packages('gridExtra')"
    RUN R -e "install.packages('ggrepel')"
    RUN R -e "install.packages('readr')"

    COPY /datascience_project.R /datascience_project.R
    COPY /inputs/RAIS_vinculos_layout.xls /inputs/RAIS_vinculos_layout.xls
    COPY /outputs /outputs

    CMD Rscript /datascience_project.R


    

FROM rocker/shiny:4.4.1

# ===============================
# 1. Dependencias del sistema para Linux
# ===============================
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    wget \
    perl \
    ghostscript \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    libfontconfig1 \
    libfreetype6 \
    ca-certificates \
 && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-latex-extra \
    texlive-fonts-recommended \
    texlive-xetex \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# ===============================
# 2. renv
# ===============================
RUN R -e "install.packages('renv', repos='https://cran.rstudio.com/')"

# ===============================
# 3. cachem (cache compartido entre sesiones)
# Se instala antes de renv::restore() para que esté disponible aunque renv.lock
# no lo incluya explícitamente; es una dependencia de shiny ≥ 1.6, así que
# habitualmente ya estará en el lockfile, pero esto garantiza su presencia.
# ===============================
RUN R -e "install.packages('cachem', repos='https://cran.rstudio.com/')"

# ===============================
# 4. Limpiar instalaciones por defecto innecesarias
# ===============================
RUN rm -rf /srv/shiny-server/*

# ===============================
# 5. Restaurar las dependencias exactas
# ===============================
WORKDIR /srv/shiny-server
COPY renv.lock .
RUN R -e "renv::restore()"

# ===============================
# 6. Copiar la aplicación y darle permisos
# ===============================
COPY app/ .
RUN chown -R shiny:shiny /srv/shiny-server \
 && chmod -R 750 /srv/shiny-server

# ===============================
# 7. Eliminar privilegios para incrementar seguridad
# ===============================
USER shiny

# ===============================
# 8. Ejecutar en el puerto por defecto de Shiny
# ===============================
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
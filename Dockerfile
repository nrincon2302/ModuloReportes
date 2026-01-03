FROM rocker/shiny:4.4.1

# ===============================
# 1. Dependencias del sistema para Linux
# ===============================
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# ===============================
# 2. renv
# ===============================
RUN R -e "install.packages('renv', repos='https://cran.rstudio.com/')"

# ===============================
# 3. Limpiar instalaciones por defecto innecesarias
# ===============================
RUN rm -rf /srv/shiny-server/*

# ===============================
# 4. Restaurar las dependencias exactas
# ===============================
WORKDIR /srv/shiny-server
COPY renv.lock .
RUN R -e "renv::restore()"

# ===============================
# 5. Copiar la aplicación y darle permisos
# ===============================
COPY app/ .
RUN chown -R shiny:shiny /srv/shiny-server \
 && chmod -R 750 /srv/shiny-server

# ===============================
# 6. Eliminar privilegios para incrementar seguridad
# ===============================
USER shiny

# ===============================
# 7. Ejecutar en el puerto por defecto de Shiny
# ===============================
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]

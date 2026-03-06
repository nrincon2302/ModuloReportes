FROM rocker/shiny:4.4.1

# ===============================
# 1. Dependencias del sistema
# ===============================
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl wget perl ghostscript \
    libcurl4-openssl-dev libssl-dev libxml2-dev libglpk-dev \
    libfontconfig1 libfreetype6 ca-certificates \
 && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
    texlive-latex-base texlive-latex-recommended texlive-latex-extra \
    texlive-fonts-recommended texlive-xetex \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

# ===============================
# 2. renv + cachem
# ===============================
RUN R -e "install.packages('renv',  repos='https://cran.rstudio.com/')"
RUN R -e "install.packages('cachem', repos='https://cran.rstudio.com/')"

# ===============================
# 3. Limpiar app de ejemplo
# ===============================
RUN rm -rf /srv/shiny-server/*

# ===============================
# 4. Restaurar dependencias
# ===============================
WORKDIR /srv/shiny-server
COPY renv.lock .
RUN R -e "renv::restore()"

# ===============================
# 5. Copiar aplicación
# ===============================
COPY app/ .

# ===============================
# 6. Configuración de Shiny Server
# Shiny Server (ya incluido en rocker/shiny) spawna un proceso R
# por sesión → cada tab/usuario corre aislado sin bloquear a otros.
# ===============================
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Crear directorio de logs con permisos correctos
RUN mkdir -p /var/log/shiny-server \
 && chown -R shiny:shiny /srv/shiny-server /var/log/shiny-server \
 && chmod -R 750 /srv/shiny-server

USER shiny

EXPOSE 3838

# Usar Shiny Server en lugar de shiny::runApp().
# shiny::runApp() = 1 proceso R para todas las sesiones (se bloquean entre sí).
# shiny-server    = 1 proceso R por sesión (aislamiento real).
CMD ["/usr/bin/shiny-server"]
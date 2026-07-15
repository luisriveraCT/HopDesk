# =============================================================================
# Dockerfile — HopDesk Shiny App
#
# STATUS: Blueprint only. Not yet active in deployment.
#         ShinyApps.io is the current deployment target.
#         Use this file when self-hosting on Networks infrastructure or a VPS.
#
# KEY DESIGN DECISIONS:
#   - Single-client image: CLIENT_ID is injected at runtime, not baked in.
#     The same image runs as "networks", "hd-admin", or any future client
#     by pointing to a different .env file at container start.
#   - Stateless container: all persistent state lives in S3. The container
#     can be killed and restarted without data loss.
#   - Build once, run many: the image is identical across all clients.
#     Only the environment variables differ between deployments.
#
# BUILD:
#   docker build -t hopdesk-app:latest .
#
# RUN (single client):
#   docker run --env-file .env.networks -p 3838:3838 hopdesk-app:latest
#
# FUTURE — renv migration:
#   When renv is configured (run `renv::snapshot()` from the project root),
#   replace the manual install.packages() block below with:
#     COPY renv.lock renv.lock
#     COPY renv/activate.R renv/activate.R
#     RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"
#     RUN R -e "renv::restore()"
#   This gives reproducible, lockfile-pinned package versions.
# =============================================================================

FROM rocker/shiny:4.5.2

# System dependencies required by R packages and LibreOffice (PDF export)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libreoffice-writer \
    libreoffice-core \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server/hopdesk

# Install R packages explicitly (no renv.lock yet — see FUTURE note above).
# Grouped by domain for readability. These mirror the packages in global.R
# plus namespace-qualified packages found across all R/ modules.
RUN R -e "install.packages(c( \
  'shiny', 'shinyjs', 'bslib', 'shinyWidgets', 'shinymanager', \
  'DT', 'htmltools', 'later', \
  'dplyr', 'tidyr', 'purrr', 'tibble', 'stringr', 'stringi', \
  'lubridate', 'scales', \
  'httr', 'jsonlite', 'aws.s3', 'digest', 'uuid', 'callr', \
  'readxl', 'openxlsx', 'officer', 'flextable', 'pagedown', 'base64enc', \
  'quantmod', 'zoo', 'visNetwork' \
), repos='https://cloud.r-project.org', Ncpus=parallel::detectCores())"

# Copy application code (separate layer — frequently changes, rebuilds fast)
COPY . .

# Shiny server config: serve the app at /hopdesk/ path
# The default rocker/shiny serves any subdir of /srv/shiny-server/ as /<name>/
# No custom server.conf needed unless you need custom log paths or auth.

# Internal port Shiny listens on. nginx (docker-compose) routes external traffic here.
EXPOSE 3838

# CLIENT_ID is the ONLY variable that changes per client at runtime.
# All other vars (S3_BUCKET, AWS_*, RESEND_*) are identical across deployments
# and are injected via --env-file at container start. NEVER bake them into the image.
ENV CLIENT_ID=""

# Verify the app responds within 90s of startup (Shiny + package load time)
HEALTHCHECK --interval=30s --timeout=10s --start-period=90s --retries=3 CMD curl -f http://localhost:3838/hopdesk/ || exit 1

CMD ["/usr/bin/shiny-server"]

# R frontend for Shiny dashboard (replacing Streamlit)

FROM rocker/r-ver:4.4.1

# App directory
WORKDIR /app

# System deps (adjust if your plots/libs need more)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libjpeg-dev \
    libfontconfig1-dev \
  && rm -rf /var/lib/apt/lists/*

# Install R packages used by the app
RUN R -e "install.packages( \
    c('shiny', 'plotly', 'DT', 'DBI', 'RPostgres', 'dplyr', 'httr', 'jsonlite'), \
    repos = 'https://cloud.r-project.org' \
  )"

# Copy app files (expects app.R or ui.R/server.R in this directory)
COPY . .

# Match the old Streamlit port mapping
EXPOSE 8501

# Run Shiny on 0.0.0.0:8501 so docker-compose `8501:8501` still works
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 8501)"]

# Use a slim R base image
FROM rocker/r-ver:4.4.1

# App directory
WORKDIR /usr/src/app

# System deps (minimal, enough for plumber & friends)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

# Install R packages (separate layer for caching)
RUN R -e "install.packages(c('plumber', 'jsonlite', 'dplyr'), repos = 'https://cloud.r-project.org')"

# Copy API source (adjust if your path differs)
COPY ./backend ./backend

# If model.rds lives outside backend/, copy it too
# COPY ./model.rds ./backend/model.rds

# Expose API port
EXPOSE 8000

# Run plumber API (mirrors uvicorn cmd style)
CMD ["R", "-e", "pr = plumber::plumb('backend/main.R'); pr$run(host = '0.0.0.0', port = 8000)"]

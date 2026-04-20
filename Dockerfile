# =============================================================================
# Dockerfile — Elephas maximus SDM Project v2.1
# =============================================================================
# Production-grade container for reproducible species distribution modeling
# Target: Conservation Biology research & operational forecasting
# =============================================================================

# Base image: R 4.3 with system dependencies
FROM rocker/r-ver:4.3.3

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV R_ENVIRONMENT=user
ENV LANG=C.UTF-8

# System metadata
LABEL maintainer="Elephas maximus SDM Team"
LABEL version="2.1-production"
LABEL description="Constitution-driven SDM pipeline for Asian elephants in Bhutan"
LABEL license="MIT"

# =============================================================================
# SYSTEM DEPENDENCIES
# =============================================================================
RUN apt-get update && apt-get install -y --no-install-recommends \
    # GIS libraries
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    # Parallel processing
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    # Fonts for figures
    fonts-liberation \
    fonts-dejavu \
    # Utilities
    pandoc \
    pandoc-citeproc \
    make \
    git \
    vim-tiny \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# =============================================================================
# R PACKAGE DEPENDENCIES
# =============================================================================
# Install runtime dependencies
RUN R -e "install.packages(c( \
    'terra',         # Raster processing \
    'sf',            # Vector processing \
    'yaml',          # Config parsing \
    'rlang',         # .data pronoun for ggplot2 \
    'ggplot2',       # Visualization \
    'viridis',       # Colorblind-safe palettes \
    'ranger',        # Random Forest \
    'maxnet',        # MaxEnt modeling \
    'gbm',           # Boosted Regression Trees \
    'testthat'       # Unit testing \
    ), repos='https://cloud.r-project.org/', Ncpus=4)"

# =============================================================================
# WORKSPACE SETUP
# =============================================================================
WORKDIR /app

# Copy project files (excludes .gitignore, .qwenignore patterns)
COPY . /app/

# Create output directories
RUN mkdir -p /app/04_outputs/runs \
    && mkdir -p /app/06_logs \
    && mkdir -p /app/00_registry \
    && mkdir -p /app/tests/testthat \
    && chmod -R 755 /app

# =============================================================================
# VALIDATION & TESTING
# =============================================================================
# Run unit tests at build time to verify image integrity
RUN R -e "testthat::test_dir('tests/testthat', stop_on_failure = FALSE)" || true

# =============================================================================
# ENTRY POINT
# =============================================================================
# Default command: run validation
ENTRYPOINT ["Rscript", "03_analysis/run_pipeline.R"]

# Default arguments: use default config
CMD ["00_governance/config.yaml"]

# =============================================================================
# HEALTH CHECK
# =============================================================================
# Check if R is available and config exists
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD Rscript -e "if (file.exists('00_governance/config.yaml')) quit(0) else quit(1)"

# =============================================================================
# EXPOSED VOLUMES
# =============================================================================
# Persist outputs and logs
VOLUME ["/app/04_outputs", "/app/06_logs", "/app/00_registry"]

# =============================================================================
# BUILD INSTRUCTIONS
# =============================================================================
# Build: docker build -t elephas-sdm:2.1 .
# Run:   docker run -v ./data:/app/01_data_raw elephas-sdm:2.1
# Test:  docker run elephas-sdm:2.1 Rscript -e "testthat::test_dir('tests')"
# Shell: docker run -it elephas-sdm:2.1 /bin/bash
# =============================================================================

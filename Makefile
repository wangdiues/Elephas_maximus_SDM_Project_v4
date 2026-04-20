# =============================================================================
# Makefile — Elephas maximus SDM Project v2.1
# =============================================================================
# Common operations for development, testing, and deployment
# Usage: make [target]
# =============================================================================

# Configuration
R := Rscript
DOCKER := docker
DOCKER_COMPOSE := docker-compose
GHCR := ghcr.io
IMAGE_NAME := elephas-sdm
VERSION := 2.1.0

# Directories
SRC_DIR := 03_analysis
TEST_DIR := tests/testthat
GOV_DIR := 00_governance
OUTPUT_DIR := 04_outputs
LOG_DIR := 06_logs

# Colors for output
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
NC := \033[0m  # No Color

# =============================================================================
# DEFAULT TARGET
# =============================================================================
.PHONY: all
all: lint test check-governance
	@echo "$(GREEN)✓ All checks passed$(NC)"

# =============================================================================
# CODE QUALITY
# =============================================================================
.PHONY: lint
lint:
	@echo "$(YELLOW)Running code linting...$(NC)"
	$(R) -e "library(lintr); lint_dir('$(SRC_DIR)')"
	@echo "$(GREEN)✓ Linting complete$(NC)"

.PHONY: check-governance
check-governance:
	@echo "$(YELLOW)Checking governance compliance...$(NC)"
	@for file in $(GOV_DIR)/governance.md $(GOV_DIR)/config.yaml $(GOV_DIR)/methods.md $(GOV_DIR)/targets.md $(GOV_DIR)/memory_log.md $(GOV_DIR)/progress_tracker.md; do \
		if [ -f "$$file" ]; then \
			echo "  ✓ $$file"; \
		else \
			echo "  ✗ $$file MISSING"; \
			exit 1; \
		fi; \
	done
	@echo "$(GREEN)✓ Governance check complete$(NC)"

# =============================================================================
# TESTING
# =============================================================================
.PHONY: test
test:
	@echo "$(YELLOW)Running unit tests...$(NC)"
	$(R) -e "testthat::test_dir('$(TEST_DIR)', reporter='summary', stop_on_failure=TRUE)"
	@echo "$(GREEN)✓ Tests complete$(NC)"

.PHONY: test-coverage
test-coverage:
	@echo "$(YELLOW)Running test coverage...$(NC)"
	$(R) -e "source_files <- list.files('03_analysis', pattern='^00_.*\\\\.R$$', full.names=TRUE); test_files <- list.files('tests/testthat', pattern='^test-.*\\\\.R$$', full.names=TRUE); cov <- covr::file_coverage(source_files, test_files); covr::to_cobertura(cov, 'coverage.xml')"
	@echo "$(GREEN)✓ Coverage report generated$(NC)"

.PHONY: test-validation
test-validation:
	@echo "$(YELLOW)Running validation gate...$(NC)"
	$(R) $(SRC_DIR)/00_data_validation.R
	@echo "$(GREEN)✓ Validation complete$(NC)"

# =============================================================================
# DOCKER OPERATIONS
# =============================================================================
.PHONY: docker-build
docker-build:
	@echo "$(YELLOW)Building Docker image...$(NC)"
	$(DOCKER) build -t $(IMAGE_NAME):$(VERSION) -t $(IMAGE_NAME):latest .
	@echo "$(GREEN)✓ Docker build complete$(NC)"

.PHONY: docker-push
docker-push:
	@echo "$(YELLOW)Pushing Docker image to registry...$(NC)"
	$(DOCKER) tag $(IMAGE_NAME):$(VERSION) $(GHCR)/$${USER:-elephas-sdm}/$(IMAGE_NAME):$(VERSION)
	$(DOCKER) push $(GHCR)/$${USER:-elephas-sdm}/$(IMAGE_NAME):$(VERSION)
	@echo "$(GREEN)✓ Docker push complete$(NC)"

.PHONY: docker-run
docker-run:
	@echo "$(YELLOW)Running pipeline in Docker...$(NC)"
	$(DOCKER) run --rm -v $(PWD):/app $(IMAGE_NAME):$(VERSION)
	@echo "$(GREEN)✓ Docker run complete$(NC)"

.PHONY: docker-clean
docker-clean:
	@echo "$(YELLOW)Cleaning Docker resources...$(NC)"
	$(DOCKER) system prune -f
	@echo "$(GREEN)✓ Docker clean complete$(NC)"

# =============================================================================
# DOCKER COMPOSE
# =============================================================================
.PHONY: compose-up
compose-up:
	@echo "$(YELLOW)Starting Docker Compose services...$(NC)"
	$(DOCKER_COMPOSE) --profile production up -d
	@echo "$(GREEN)✓ Services started$(NC)"

.PHONY: compose-down
compose-down:
	@echo "$(YELLOW)Stopping Docker Compose services...$(NC)"
	$(DOCKER_COMPOSE) down
	@echo "$(GREEN)✓ Services stopped$(NC)"

.PHONY: compose-logs
compose-logs:
	$(DOCKER_COMPOSE) logs -f

.PHONY: compose-validation
compose-validation:
	@echo "$(YELLOW)Running validation service...$(NC)"
	$(DOCKER_COMPOSE) --profile validation up sdm-validation
	@echo "$(GREEN)✓ Validation complete$(NC)"

.PHONY: compose-testing
compose-testing:
	@echo "$(YELLOW)Running testing service...$(NC)"
	$(DOCKER_COMPOSE) --profile testing up sdm-testing
	@echo "$(GREEN)✓ Testing complete$(NC)"

# =============================================================================
# PIPELINE EXECUTION
# =============================================================================
.PHONY: run
run:
	@echo "$(YELLOW)Running SDM pipeline...$(NC)"
	$(R) $(SRC_DIR)/run_pipeline.R
	@echo "$(GREEN)✓ Pipeline complete$(NC)"

.PHONY: run-strict
run-strict:
	@echo "$(YELLOW)Running SDM pipeline (strict mode)...$(NC)"
	$(R) $(SRC_DIR)/run_pipeline.R 00_governance/config.yaml
	@echo "$(GREEN)✓ Pipeline complete$(NC)"

.PHONY: run-validation
run-validation:
	@echo "$(YELLOW)Running validation only...$(NC)"
	$(R) $(SRC_DIR)/00_data_validation.R
	@echo "$(GREEN)✓ Validation complete$(NC)"

# =============================================================================
# DOCUMENTATION
# =============================================================================
.PHONY: docs
docs:
	@echo "$(YELLOW)Generating documentation...$(NC)"
	$(R) -e "rmarkdown::render_site('05_manuscripts')"
	@echo "$(GREEN)✓ Documentation complete$(NC)"

.PHONY: readme
readme:
	@echo "$(YELLOW)Previewing README...$(NC)"
	$(R) -e "rmarkdown::render('README.md', output_format='github_document')"

# =============================================================================
# CLEANUP
# =============================================================================
.PHONY: clean
clean:
	@echo "$(YELLOW)Cleaning temporary files...$(NC)"
	rm -rf $(OUTPUT_DIR)/runs/RUN_*
	rm -rf $(LOG_DIR)/*.log
	rm -rf test_results/
	rm -rf coverage.xml
	rm -rf .Rhistory
	find . -name "*.Rhistory" -delete
	find . -name "*.Rproj.user" -type d -exec rm -rf {} + 2>/dev/null || true
	@echo "$(GREEN)✓ Clean complete$(NC)"

.PHONY: clean-all
clean-all: clean
	@echo "$(YELLOW)Cleaning all generated files...$(NC)"
	rm -rf $(OUTPUT_DIR)/*
	rm -rf $(LOG_DIR)/*
	$(DOCKER) system prune -f
	@echo "$(GREEN)✓ Full clean complete$(NC)"

# =============================================================================
# INSTALLATION
# =============================================================================
.PHONY: install
install:
	@echo "$(YELLOW)Installing R dependencies...$(NC)"
	$(R) -e "install.packages(c('devtools', 'remotes'), repos='https://cloud.r-project.org')"
	$(R) -e "devtools::install_deps(dependencies=TRUE)"
	@echo "$(GREEN)✓ Installation complete$(NC)"

.PHONY: setup
setup: install
	@echo "$(YELLOW)Setting up project structure...$(NC)"
	mkdir -p $(OUTPUT_DIR)/runs
	mkdir -p $(LOG_DIR)
	mkdir -p $(TEST_DIR)
	@echo "$(GREEN)✓ Setup complete$(NC)"

# =============================================================================
# CI/CD HELPERS
# =============================================================================
.PHONY: ci-lint
ci-lint:
	$(R) -e "library(lintr); lint_results <- lint_dir('$(SRC_DIR)'); if (length(lint_results) > 0) print(lint_results); if (length(lint_results) > 0) quit(status=1)"

.PHONY: ci-test
ci-test:
	$(R) -e "testthat::test_dir('$(TEST_DIR)', reporter='junit', stop_on_failure=TRUE)"

.PHONY: ci-governance
ci-governance:
	@test -f $(GOV_DIR)/governance.md || (echo "governance.md missing" && exit 1)
	@test -f $(GOV_DIR)/config.yaml || (echo "config.yaml missing" && exit 1)
	@test -f $(GOV_DIR)/memory_log.md || (echo "memory_log.md missing" && exit 1)
	@test -f $(GOV_DIR)/progress_tracker.md || (echo "progress_tracker.md missing" && exit 1)

# =============================================================================
# HELP
# =============================================================================
.PHONY: help
help:
	@echo "$(GREEN)Elephas maximus SDM Project v$(VERSION)$(NC)"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Code Quality:"
	@echo "  lint              Run code linting"
	@echo "  check-governance  Check governance files exist"
	@echo ""
	@echo "Testing:"
	@echo "  test              Run unit tests"
	@echo "  test-coverage     Run test coverage"
	@echo "  test-validation   Run validation gate"
	@echo ""
	@echo "Docker:"
	@echo "  docker-build      Build Docker image"
	@echo "  docker-push       Push Docker image"
	@echo "  docker-run        Run pipeline in Docker"
	@echo "  docker-clean      Clean Docker resources"
	@echo ""
	@echo "Docker Compose:"
	@echo "  compose-up        Start all services"
	@echo "  compose-down      Stop all services"
	@echo "  compose-logs      View service logs"
	@echo "  compose-validation  Run validation service"
	@echo "  compose-testing   Run testing service"
	@echo ""
	@echo "Pipeline:"
	@echo "  run               Run SDM pipeline"
	@echo "  run-strict        Run in strict mode"
	@echo "  run-validation    Run validation only"
	@echo ""
	@echo "Documentation:"
	@echo "  docs              Generate documentation"
	@echo "  readme            Preview README"
	@echo ""
	@echo "Cleanup:"
	@echo "  clean             Remove temporary files"
	@echo "  clean-all         Remove all generated files"
	@echo ""
	@echo "Installation:"
	@echo "  install           Install R dependencies"
	@echo "  setup             Install and create directories"
	@echo ""
	@echo "CI/CD:"
	@echo "  ci-lint           CI linting"
	@echo "  ci-test           CI testing"
	@echo "  ci-governance     CI governance check"
	@echo ""
	@echo "Default:"
	@echo "  all               Run lint, test, and build"

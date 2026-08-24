# Industry templates ----------------------------------------------------------

#' Industry attribute templates
#'
#' Each template describes the composition of a workforce: the departments,
#' locations and seniority ladder, together with the sampling weights that
#' govern how employees are distributed across them. `manager_level_min` is the
#' lowest seniority index that can hold direct reports, which sets how steep the
#' generated reporting hierarchy is.
#'
#' @return A named list of template definitions.
#' @export
#' @examples
#' names(synthona_templates())
#' synthona_templates()$tech_product$departments
synthona_templates <- function() {
  list(
    tech_product = list(
      id = "tech_product",
      label = "Technology product organisation",
      departments = c("Engineering", "Product", "Sales", "Marketing", "Operations", "HR", "Finance"),
      dept_weights = c(0.30, 0.16, 0.14, 0.10, 0.12, 0.08, 0.10),
      locations = c("HQ", "Remote", "Regional"),
      location_weights = c(0.45, 0.35, 0.20),
      seniority_levels = c("IC-1", "IC-2", "IC-3", "Senior", "Director", "Exec"),
      seniority_weights = c(0.26, 0.24, 0.19, 0.17, 0.10, 0.04),
      manager_level_min = 4L,
      culture = "async_hybrid"
    ),
    professional_services = list(
      id = "professional_services",
      label = "Professional services firm",
      departments = c("Client Services", "Delivery", "Strategy", "Sales", "Operations", "People"),
      dept_weights = c(0.28, 0.22, 0.12, 0.14, 0.14, 0.10),
      locations = c("HQ", "Client Site", "Remote", "Regional"),
      location_weights = c(0.30, 0.25, 0.25, 0.20),
      seniority_levels = c("Analyst", "Consultant", "Manager", "Senior Manager", "Partner", "Exec"),
      seniority_weights = c(0.22, 0.24, 0.24, 0.16, 0.10, 0.04),
      manager_level_min = 3L,
      culture = "client_facing"
    ),
    manufacturing = list(
      id = "manufacturing",
      label = "Manufacturing organisation",
      departments = c("Plant Ops", "Engineering", "Supply Chain", "Quality", "Sales", "HR", "Finance"),
      dept_weights = c(0.24, 0.16, 0.18, 0.10, 0.10, 0.10, 0.12),
      locations = c("Plant", "HQ", "Regional", "Remote"),
      location_weights = c(0.42, 0.28, 0.20, 0.10),
      seniority_levels = c("Operator", "Specialist", "Supervisor", "Manager", "Director", "Exec"),
      seniority_weights = c(0.28, 0.24, 0.18, 0.16, 0.10, 0.04),
      manager_level_min = 3L,
      culture = "site_centric"
    ),
    healthcare_ops = list(
      id = "healthcare_ops",
      label = "Healthcare operations organisation",
      departments = c("Clinical Ops", "Admin", "IT", "People", "Finance", "Quality", "External Relations"),
      dept_weights = c(0.32, 0.14, 0.12, 0.10, 0.10, 0.12, 0.10),
      locations = c("Campus", "Regional", "Remote"),
      location_weights = c(0.55, 0.25, 0.20),
      seniority_levels = c("Staff", "Senior Staff", "Lead", "Manager", "Director", "Exec"),
      seniority_weights = c(0.30, 0.24, 0.18, 0.14, 0.10, 0.04),
      manager_level_min = 3L,
      culture = "high_reliability"
    )
  )
}

#' Retrieve one industry template
#'
#' @param template_id Template identifier, see [synthona_templates()].
#' @return A template definition list.
#' @export
get_template <- function(template_id) {
  templates <- synthona_templates()
  tpl <- templates[[template_id]]
  if (is.null(tpl)) {
    stop(
      "Unknown template: ", template_id,
      ". Available: ", paste(names(templates), collapse = ", "),
      call. = FALSE
    )
  }
  tpl
}

# Preferential-attachment multiplier by role bucket. Applied when generating
# scale-free structure so that senior roles accumulate ties faster.
ROLE_ATTACHMENT_BOOST <- c(
  individual = 1.00, manager = 1.20, senior_manager = 1.45,
  executive = 1.80, system = 2.00
)

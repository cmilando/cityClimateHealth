## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.

* Suggests or Enhances not in mainstream repositories: cmdstanr

  cmdstanr is available from https://stan-dev.r-universe.dev, which is
  declared in Additional_repositories. It is only used by condPois_sb(),
  which checks for it with requireNamespace() and stops with installation
  instructions if it (or CmdStan) is missing. All other functionality works
  without it.

* Examples with CPU or elapsed time > 5s: condPois_sb (~10 s)

  These examples use @examplesIf and only run when cmdstanr and a CmdStan
  installation are available, so they are skipped on CRAN machines.

## Test environments

* local macOS (aarch64), R 4.5.2: R CMD check --as-cran
* win-builder, R-devel: [fill in after devtools::check_win_devel()]

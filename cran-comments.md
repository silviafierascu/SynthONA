## R CMD check results

Local Windows 11, R 4.2.3, `R CMD check --as-cran`, 24 August 2026:

    Status: 2 NOTEs
    0 errors | 0 warnings | 2 notes

* **New submission.** This is the first release of the package.

* **"unable to verify current time"** — an artefact of the check machine being
  unable to reach a time server. Not reproducible on machines with network
  access to a time source.

The local run used `--no-manual`; that machine has no LaTeX installation. The
manual is built and checked on the Linux and macOS configurations below.

## Test environments

* local Windows 11, R 4.2.3
* GitHub Actions: macOS-latest (release), Windows-latest (release),
  Ubuntu-latest (devel, release, oldrel-1). All five configurations pass on
  the submitted source.

## Notes on package behaviour

* The package writes only to the directory supplied by the caller. The default
  for every export function is a session temporary directory
  (`default_output_dir()`); no function writes to the user's home directory,
  working directory, or package library.
* No function alters the calling session's random number generator state.
  Seeding runs through an internal helper that restores `.Random.seed` on exit,
  which is verified by a regression test.
* Examples and the vignette generate small networks (n <= 400) and skip the
  small-world reference computation, so all run well within the time limit.

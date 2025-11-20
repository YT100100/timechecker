## Resubmission

This is a re-submission. Thank you very much for carefully reviewing the package and providing valuable feedback. Based on your suggestions, in this version I have

-   Made the `Description` field in the DESCRIPTION file a single line.
-   Replaced `\dontrun` with `\donttest` in the examples of all functions.
-   Added a `verbose` argument to the `set_loop_timechecker` and `set_step_timechecker` functions. When `verbose = FALSE`, no output is printed to the console.

## Test environments

-   Local Windows 11, R 4.1.3
-   R-hub:
    -   linux
    -   m1-san
    -   macos
    -   macos-arm64
    -   windows
-   Win-builder:
    -   R-release

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies

None.

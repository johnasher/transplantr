## Submission of new version of CRAN package
This is a submission of the next version of transplantr package. In this version I have:

* Added 9 new calculation functions, and additional wrapper functions accepting US units (international units are used by default), to complement the existing set

* Expanded the text in the vignettes

* As I was advised by the CRAN volunteer when the previous version was accepted, I have provided expanded explanations of abbreviations in DESCRIPTION text, except those abbreviations exclusively used in abbreviated form in the community.

* The theoretical background to the equations in the documentation for each function and also to the vignettes. There are now 27 such functions in the package, and some of the papers describing the background to these have more than 10 authors, so listing all in the DESCRIPTION file would be very impractical. I have kept the comment to the description field of the DESCRIPTION file noting that references are in the vignettes and documentation of each individual function. The vignettes also contain DOI links to individual referenced publications and links to relevant organisation web pages (where the equations are published by transplant organisations rather than in the literature).

* re-run R CMD check with no ERRORs, WARNINGs or NOTEs.

## Test environments
* linux Mint 18 Cinnamon 64-bit , R 3.6.1
* win-builder (devel, release, oldrelease)
* R-hub builder

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

Win-builder status was OK for release, devel and oldrelease

R-hub builder produced a NOTE for possible wrong URLs. I think these were returning 
403 Forbidden HTTP codes due to robots blockers as the links are correct and 
work on manual testing using different devices on different networks.

## Downstream dependencies
There are currently no downstream dependencies of this package

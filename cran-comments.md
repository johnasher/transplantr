## Resubmission
This is a resubmission. In this version I have:

* Added more details about the package functionality to the DESCRIPTION text.

* Have added references for the theoretical background to the equations in the documentation for each function and also to the vignettes. There are 18 such functions in the package, and some of the papers describing the background to these have more than 10 authors, so listing all in the DESCRIPTION file would be very impractical. I have however added a comment to the description field of the DESCRIPTION file noting that references are in the vignettes and documentation of each individual function. The vignettes also contain DOI links to individual referenced publications and links to relevant organisation web pages (where the equations are published by transplant organisations rather than in the literature).

* re-run R CMD check with no ERRORs, WARNINGs or NOTEs.

These were the comments from the CRAN reviewer for the first submission:
  
> Thanks, please add more details about the package functionality in your
> Description text.

> If there are references describing the (theoretical background of)
> methods in your package, please add these in the Description field of
> your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year) <arXiv:...>
> authors (year, ISBN:...)
> with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.
>
> Please fix and resubmit.

## Test environments
* linux Mint 18 Cinnamon 64-bit , R 3.6.1
* win-builder (devel and release)
* R-hub builder

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

Win-builder produced a NOTE for possibly misspelled words.
These have been checked and are not actually misspelled.

R-hub builder produced a NOTE for possible wrong URLs. I think these were returning 
403 Forbidden HTTP codes due to robots blockers as the links are correct and 
work on manual testing using different devices on different networks.

## Downstream dependencies
There are currently no downstream dependencies of this package

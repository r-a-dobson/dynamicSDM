## R CMD check results

0 errors | 0 warnings | 3 notes

* This is a new release.

*> checking examples ... [25s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
               user system elapsed
  dynamic_proj 5.06   0.09    5.17

> checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

My comment on notes:

Both notes are not present on GitHub R-devel check, but appear on rhub R-devel check. 

Example is as quick as I can make it using /dontshow to reduce sample sizes. 

I do not find lastMiKTeXException in this directory when checked on my computer, nor does this fault appear on GitHub checks. Could be an erroneous note. 


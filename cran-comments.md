## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.


> checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

My comment on notes:

Note is not present on GitHub R-devel check, but appears on rhub R-devel check. 

I do not find lastMiKTeXException in this directory when checked on my computer, nor does this fault appear on GitHub checks. Could be an erroneous note. 



## CRAN Response

1) If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: <https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

*Please write TRUE and FALSE instead of T and F.
'T' and 'F' instead of TRUE and FALSE:
   man/extract_buffered_coords.Rd:
     extract_buffered_coords(
      ...
     )
   man/extract_buffered_raster.Rd:
     extract_buffered_raster(
       ...
     )
   man/extract_dynamic_coords.Rd:
     extract_dynamic_coords(
         ...)

2) \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user.
Does not seem necessary.
Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.

3) You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.
Instead of print()/cat() rather use message()/warning()  or
if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.
(except for print, summary, interactive functions)

4) Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().

5) Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.:
...
oldpar <- par(no.readonly = TRUE)    # code line i
on.exit(par(oldpar))            # code line i + 1 ...
par(mfrow=c(2,2))            # somewhere after ...
e.g.: R/spatiotemp_autocorr.R ; R/spatiotemp_bias.R If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.




# Changes made to address 

1) There is currently no single paper reference available for the entirety of the methods in the package for the description file. However, we have added in over 10 extra references to our function details and manual to support the methodological approaches for specific functions (e.g. see spatiotemporal_bias, spatiotemporal_pseudoabs and spatiotemporal_thin).

2) Thank you. For the examples that had \dontrun{} we have now changed to \donttest{}. We tried everything to get the examples to run under five seconds but this is not possible. 

3) We have changed this throughout. There is no usage of print or cat in the package, we have changed relevant comments to messages or warnings. 

4) This has been fixed. We realise that the Google Earth Engine based functions were writing to the user's home directory by default. This has been changed to the temporary directory. Further, we have removed any tests that write files to a user's Google Drive. 

5) Thank you for pointing this out. We have now incorportated on.exit() so that none of our functions alter the user's options permanently. We understand how important this is, so thank you for pointing this out.

Thank you for your feedback so far. Please let us know if there are any further changes that you would like us to make to the package.


# Cran response

You still have 2 \dontrun{} examples in convert_gbif.Rd and spatiotemp_check.Rd.
Please either change them to \donttest{} or let us know why you think \dontrun{} is the right choice so we can publish.


# Changes made to address

1) These were unfortunately missed on previous edits. Apologies - fixed now. 
# mlr ()

* GitHub: <https://github.com/topepo/Cubist>
* Email: <mailto:mxkuhn@gmail.com>

Run `revdepcheck::revdep_details(, "mlr")` for more info

## Error before installation

### Devel

```

  There is a binary version available but the source version is later:
     binary source needs_compilation
otel  0.2.0  0.3.0             FALSE

  These will not be installed


Package which is only available in source form, and may need
  compilation of C/C++/Fortran: ‘Rmpi’
installing the source package ‘otel’

Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) is not TRUE


```
### CRAN

```

  There is a binary version available but the source version is later:
     binary source needs_compilation
otel  0.2.0  0.3.0             FALSE

  These will not be installed


Package which is only available in source form, and may need
  compilation of C/C++/Fortran: ‘Rmpi’
installing the source package ‘otel’

Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) is not TRUE


```

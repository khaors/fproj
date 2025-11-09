---
project: fproj
summary: *fproj* is a modern Fortran interface to the _proj_ library. Proj transforms geospatial coordinates from one coordinate reference system (CRS) to another. This includes cartographic projections as well as geodetic transformations. PROJ is released under the X/MIT open source license
author: Oscar Garcia-Cabrejo
email: khaors@gmail.com
github: https://github.com/khaors/fproj
src_dir: src
output_dir: docs/
project_github: https://github.com/khaors/fproj
project_download:
graph: true
exclude_dir: archive
             FODDER
display: public
         protected
proc_internals: true
sort: permission-alpha
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
source: true
---
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Fortran](https://img.shields.io/badge/Fortran-734F96?logo=fortran&logoColor=fff)](https://fortran-lang.org/)
[![fpm](https://img.shields.io/badge/fpm-Fortran_package_manager-734f96)](https://fpm.fortran-lang.org)

Proj is a library for working with coordinate transformations and geographical projections in spatial problems.  This is a modern fortran interface that only includes the common procedures of the proj library.

## Motivation

## Get the code

Clone the repository

```
git clone https://github.com/khaors/fproj
cd fproj
```

## Compilation with fpm 

The Fortran Package Manager (fpm) can be used to build `fproj` using the provided `fpm.toml` file:


```
fpm build --profile release
```

## Testing

The test can be executed using:

```
fpm test --profile release
```

## Documentation

The documentation of the library can be accessed in the following links:

- [html files](https://khaors.github.io/fproj)

## Licensing, Authors, Acknowledgements
_fproj_ was written by Oscar Garcia-Cabrejo and is distributed under the [MIT license](https://github.com/khaors/fproj/blob/master/LICENSE). 

You can cite _fproj_ in research publications and reports as follows:
* Garcia-Cabrejo, O. (2025). ***fproj: A modern Fortran port of the proj library***. https://github.com/khaors/fproj. Accessed: *day month year*.

BibTeX entry:
```
@misc{Garcia-Cabrejo25,
 author = {Garcia-Cabrejo, O},
 title 	= {{fproj: A modern Fortran port of the proj library}},
 year 	= 2025,
 howpublished = {\url{https://github.com/khaors/fproj}},
 note 	= {Accessed: day month year}
}
```
---

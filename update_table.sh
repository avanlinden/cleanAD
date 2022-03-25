#!/bin/bash

chmod +x ./inst/scripts/generate-table-ad.R 

Rscript ./inst/scripts/generate-table-ad.R --config $1 --auth_token $2

#!/bin/bash

mkdir -p out/

dune exec bench/datastructures.exe -- +time +cycles \
     -quota 0.1 -ci-absolute -all-values -display blank -width 1000 \
    | sed -ne:n -e '2d;N;1,2bn' -eP\;D \
	  | sed -e "
      s/Name/Name BatchSize/g;
      s/:/ /g;
      s/Time R^2/Time-R^2/g;
      s/Cycls R^2/Cycls-R^2/g;
      s/95ci/2.5% 97.5%/g" \
          > out/data.txt


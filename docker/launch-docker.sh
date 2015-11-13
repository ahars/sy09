#!/bin/bash

if [ $# -ne 1 ]; then
        echo wrong number of parameters \(need one\)
        exit 0
fi

# Some variables
project=~/Github/sy09/docker
tp=$(pwd)
dir='/opt/tp'

# Building of the container
docker build --rm -t ahars/r-programming $project

# Running of the container
docker run -t -i --rm \
    --name r-programming \
    -v $tp:$dir \
    ahars/r-programming \
    Rscript $dir/$1


#!/bin/bash

# Some variables
project=~/Github/sy09/docker
tp=$(pwd)
dir='/opt/tp'
data='/opt/data'

if [ $# -gt 0 ]; then
    cmd='Rscript '$dir'/'$1
fi

# Building of the container
docker build --rm -t ahars/r-programming $project

# Running of the container
docker run -t -i --rm \
    --name r-programming \
    -v $tp:$dir \
    -v $tp/data:$data \
    ahars/r-programming \
    $cmd


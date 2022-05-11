#!/bin/bash

# log into the the Viz Docker Container
docker run --rm -it -v $(pwd):/home/viz -w=/home/viz viz /bin/bash
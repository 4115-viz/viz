#!/bin/bash

# log into the the Viz Docker Container
docker run -it -v $(pwd):/home/viz -w=/home/viz viz /bin/bash
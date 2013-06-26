##########################################################################
# Copyright, 2013, Arianna Morgan                                       ##
########################################################################## 
## The programmes in this repository are free software: you can         ##
## redistribute them and/or modify them under the terms of the GNU      ##
## General Public License as published by the Free Software Foundation, ## 
## either version 3 of the License, or (at your option) any later       ##
## version.                                                             ##
##                                                                      ##
## These program are distributed in the hope that they will be useful,  ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of       ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        ##
## GNU General Public License for more details.                         ##   
##########################################################################

This repository contains a series of R functions under development that
are intended to simulate power analyses.  I am developing these scripts
in order to provide easier ways to do power simulations in R for tests
that have analytic power solutions (e.g. t-tests, ANOVA, regression, etc.)
as well as more complex instruments such as mixed-effects models.

Goals of this project:

*Develop simulation scripts for:
	-t-tests
	-ANOVA
	-Multiple Regression
	-Mixed-Efffect models
	-Other tests

*Provide thoroughly commented R code so that it is easy to understand my
reasons for writing the simulations as I did.  I have noticed that much of
the power simulation code out there is hard-to-follow with respect to what
the code is doing and why it is being done that way.  

*Make power simulations in general easier to run and more accessible to users
outside the R community. In order to increase interest in R and reduce our
colleagues' dependence on non-Free software, code needs to be commented
clearly and thoroughly for newcomers to better understand it.  I have become 
frustrated with trying to use code to figure out what exactly I am trying to 
do in general with power analysis simulations.

*Create functions that are tools for teaching both statistics and R program-
ming. For many people R is a way to implement statistical instruments they
already know about.  For others, though, R can become an essential part of
(on-going) statistical education.  For those who tinker with code as a way to 
learn about stats, I hope these functions will be of use to you.

*Make the best functions possible for power simulations through experimenta-
tion and the incorporation of code suggestions and contributions from others.
If you want to make suggestions or repurpose the code in this repo, please do!
I would love to see any modifications that people make to these scripts.

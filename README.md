# Pareto Selection Tool - A Part of Data Audit Tool

Author: Wongyu Choi (wchoi@ahrinet.org)

This is a part of AHRI Analytical Data Audit Tool (a.k.a. selection tool) with a limited focus on Pareto Boundary Search. This is based on the 2017 version with the minor revisions in March, 2021.

## Installation

This is similar to any ordinary R-Shiny app. Use your R-Studio version of choice and download all required libraries. NOTE: it may be bit tedious to initially setup because vanilla R often does not have a production grade package manager.

## Where to use?

If you have a data having several tiered grouping variables and want to make selection based on your preferences this app comes handy.

## What is the Pareto Boundary Search?

It is a one of the simple multi-object optimization techniques. See https://web.stanford.edu/group/sisl/k12/optimization/MO-unit5-pdfs/5.8Pareto.pdf

## Anything special to note?

This app can give you to choose both categorical (dhttps://web.stanford.edu/group/sisl/k12/optimization/MO-unit5-pdfs/5.8Pareto.pdfeterministic) and numerical (pareto optimal) data types with straightforward data filter. Also, you can set **preferences** over several parameter of interest that will have more weight on your optimality selection.

## About full capability

AHRI Data Analytics team manages a full version that runs on a Kubernetes cluster or minimally Docker containers for multiple users. For inquiries, please contact me at wchoi@ahrinet.org.

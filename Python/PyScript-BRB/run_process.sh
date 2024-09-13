#!/bin/bash

# Change directory to the location of the CSV files
cd Documents/GitHub/SNA_isolation/Python/PyScript-BRB

# Loop through each CSV file in the directory
for csvfile in *.csv; do
    # Run the Python script with the current CSV file
    python process.py "$csvfile"
done

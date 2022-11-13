# BlotR
Rapidly extract band-intensities from Line immunoblots EuroImmun EuroLine Blot 

#Requirements
Requires installation of R on local computer
RStudio is recommended although App can be executed through command line (once R installed open a terminal or command line at app.R file path and type 'Rscript app.R' and visit the url listed)

<img width="296" alt="image" src="https://user-images.githubusercontent.com/98567746/201500764-02e4ff8c-6539-4cd8-b7c8-c1747bb9a018.png">

Initial run of application will require download of required dependencies

# Purpose
This Shiny App is designed to allow you to rapidly extract the band-intensities from EuroImmun Blot results
EuroLine LIA results must be generated from EuroImmun (R) scanner software and saved in single page view (one result per pdf), showing the scanned version of the band. 
Currently the app will extract details: 
 - Generic: Patient Name, DOB, Lab Number, Created On, Date of Incubation, Patient ID
 - Following blots 
  -- Myositis 16 Ag LIA
  -- Scleroderma (Nucleoli) profile
  -- ENA profile 3 plus DFS AND ENA Profile 5
  -- Liver Blot
  -- Neuronal Blot 12 Ag 

# Usage
Select all of the blots at once that you wish to parse using the browser - any re-selection will replace all existing files selected. 
Please note as the app must copy the files to its directory there will be some time delay when there are many files to register them all. 
If you wish to use the scanner detection functionality then you must tick the box before hitting run. Detecting the color of the strip in the PDF is time consuming and will add substantial time to the parsing function. This will determine whether the strip was read by the EuroBlotOne e.g. strip scan is black and white; or green e.g. flatbed scanner. 

Bug: Sometimes changing tabs (Summary of Tests/Summary Graph/Repeated) is required to initiate parsing. 

You can enter names of results you wish to filter out or highlight as qc values in lower case (comma seperated e.g. qc, eib, qap,...)

# Output

Once the blots have been parsed the app will have outputs of
- Table of the number of positives and their overall intensity 
- Graph of all results (output graph when large data takes a while to generate - this is in plotly format)
- Repeated results, intensities and changes where name and DOB match (incorrect entry of these results will make it miss repeated values).
- A spreadsheet with one blot result per row and the intensities will be available for download from the download button.

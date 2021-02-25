Zurich Walks

Zurich Walks analyses and visualizes the numbers of counted pedestrians and cyclists in the city of Zurich in 2019 and 2020.
Consequently, we focus on the impact of Covid19 and the two lockdowns in the different city districts and living zones throughout the year.

The data and all the files can be found in the open git repository:
https://github.com/PatriceRobin/zurich_walks/tree/master/Widmer_Robin

All the relevant data are in the Widmer_Robin.zip folder:
- The results and our interpretation of these results are in the file zurich_walks.html file.
- The original R Markdown which is the basis for the created html file is called zurich_walks.rmd
- The newly learned or used functions are saved in the Cheatsheet_bootcamp.xlsx file.

- All the used data is saved in the Data subfolder.
- The created animation zurich_walks_animation.avi is in the Video folder.
- The pngs that are used are in the png_output subfolder within the Video folder.
- The sources for the used R-packages are named packages.bib and can be found in the Sources folder

When running the zurich_walks.rmd file, some additional summaries and plots are created that are not included in the knitted html file.
Additionally, the png files for the video, the video itself, and if necessary, the video folder will be newly created, when running or knitting the zurich_walks.rmd file.

The video in the html file is not the one in the folder but the exact same video we uploaded to vimeo.
We choose this solution since a direct integration of a local videos in a html file caused problems with several browsers.

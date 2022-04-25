# 309Midterm2
Repo for the STA 309 Midterm 2 - Spring 2022

### Note:
As I was adding the final touches on this project and had updated my .Rmd file, I noticed that the project in R of this repo had disappeared and was no longer on my laptop. Luckily, all the files were saved. I tried pulling from GitHub and tried generating a new PAT, but neither of these worked - therefore, I manually updated the files that had been changed. All code and generated images should still be present in this repo. This is why we back up our code - thanks GitHub!!

## Assignment Guidelines:
Use the material you have learned this class to build a dashboard comparing the impact of the three COVID-19 surges (the so called 'alpha', 'delta' and 'omicron' surges) in the state of Ohio.  Impact can be measured in many ways, you have flexibility to specify the impact in your dashboard. 

## Repo Contents:
This repo includes code generating plots that look at Ohio Covid data broken down by the Alpha, Delta, and Omicron surges. The following files are included:

* **midterm2-fullcode.Rmd** - A markdown document containing well-cleaned, commented, and reproducible code. Knitting this markdown will display the three different graphs alongside the final dashboard, and will also save these images as .png files to your computer. **This is the best way to view the code for this project.**
* **midterm2-fullcode.html** - A knitted version of the above markdown. Will display the code as well as the generated plots and dashboard.
* **sta309midterm2gh.R** - An R scrap file that includes brainstorming, initial data handling, testing, and debugging. This file includes a lot of unused and commented code. For clean and readable code, look instead at the above markdown.
* **covp1-percByAge.png** - The first generated graph; a bar plot showing the percentage of cases belonging to each age range separated by surge.
* **covCaseCount.png** - The second generated graph; a line graph showing the daily count of Covid cases in Ohio, with annotations for special events such as holidays or the return to school, as well as the three main surges.
* **vaxTimelineCount.png** - The third generated graph; a plot with columns displaying the daily number of first-dose vaccines administered, as well as a smooth line that uses the loess method to display a local average in order to show broad vaccine trends. The plot includes annotations for new vaccine guidelines that relate to age ranges or job positions.
* **dashFinal.png** - The final generated dashboard image that contains the three generated images.
* **dashboardWireframe.jpg** - An initial wireframe for the intended dashboard. Many plots are different in the final product, but this wireframe was a form of brainstorming possible stories to tell.

The generated dashboard looks at the timeline of COVID-19 and its impact on various age groups, including the impact of age-related events such as vaccine timelines and the return to school, all through the lens of comparing the impacts of the three main surges of Covid.

![dashFinal](https://user-images.githubusercontent.com/78824872/165011408-ca7c5ce1-f556-4b7f-b3f6-038915679a1c.png)

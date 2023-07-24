# RShinyApps
Deploying RShiny App PRiO on Heroku
PRiO - Prioritization of Important and Impactful Work
PRiO Logo

# Overview
PRiO | Powering Productivity is an RShiny web application designed to assist individuals in prioritizing their tasks using interactive versions of the Eisenhower and Action Priority Matrices. With PRiO, you can gain clarity on which tasks require immediate attention and which can be deferred or delegated, helping you focus on what truly matters and boosting your productivity.

...add screenshot lol

# Features
Eisenhower Matrix: Classify your tasks into four quadrants: Urgent and Important, Important but not Urgent, Urgent but not Important, and Not Urgent or Important. This visual representation helps you understand the urgency and importance of each task.

Action Priority Matrix: Prioritize your tasks based on their impact and effort required. Identify quick wins and long-term projects that align with your goals and values.

Interactive Interface: PRiO provides an intuitive and interactive user interface, allowing you to add, rank, and refresh tasks seamlessly.

# Export Options
Export your prioritized matrix for offline reference or sharing with colleagues selecting the "Take Screenshot" button.

# Deployment
PRiO is deployed on Heroku, making it easily accessible from any web browser without the need for local installations.

# Getting Started
To run PRiO locally on your machine, follow these steps:

Clone this repository to your local machine using the following command:
git clone https://github.com/keepcreating-jelicia/PRiO.git

Make sure you have R and RShiny installed on your system.

Navigate to the project directory:
cd PRiO

Install the required R packages by running the following command in your R console:
install.packages(c("shiny", "tidyverse", "bslib", "magrittr", "gridExtra", "thematic", "ragg", "shinyscreenshot"))

Launch the application by running:
shiny::runApp()

# Contributing
Contributions to PRiO are welcome! If you have ideas for improvements, bug fixes, or new features, please submit a pull request.

# License
This project is licensed under the GNU General Public License v3.0, which allows you to freely use, modify, and distribute the code, subject to the terms and conditions of the license.

# Acknowledgments
Special thanks to the R and RShiny community and contributors for their excellent tools and resources that made the development of PRiO possible.

We hope PRiO enhances your prioritization process and brings focus to what truly matters. If you have any questions or feedback, please feel free to reach out. Happy prioritizing!

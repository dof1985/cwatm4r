# cwatm4r

[![latest](https://img.shields.io/github/last-commit/dof1985/cwatm4r)](https://github.com/dof1985/cwatm4r)

# Introduction
<p>
 The <a href = "https://github.com/iiasa/CWatM">Community Water Model (<i>CWatM</i>)</a> allows the assessment of 
 water supply and human and environmental water demands at both global and regional levels. 
 The model was developed by the Water Research Group in the <i>International Institute for Applied System Analysis (IIASA)</i>.
</p>
<p>
<i>cwatm4r</i> provides an interface to set up and run hydrological simulations using CWatM.
 It also has data preparation and post-processing capacities. 
</p>
<br/>

# Install and Use
<p>Install <i>cwatm4r</i> from GitHub using the <i>devtools</i> package</p>

  ```
 install.packages("devtools")
 devtools::install_github(repo = "dof1985/cwatm4r", build_vignettes = TRUE)
  ```
  
 <p>Load <i>cwatm4r</i> and follow the <i>Getting Started with cwatm4r</i> vignette</p>
  
  ```
 library(cwatm4r)
 browseVignettes(package = "cwatm4r")
  ```
<br/>

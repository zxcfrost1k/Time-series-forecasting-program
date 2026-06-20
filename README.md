# Time series forecasting program
<div>
  Development of <code>a time series forecasting program</code> based on linear autoregressive models using real estate values ​​as an example
</div>

<br>

<h2>
  Features
</h2>

<ul>
  <li>
    <b>Analysis:</b> <code>EDA</code>, <code>correlation analysis</code>, <code>outlier detection</code>, <code>trend identification</code>, <code>seasonality assessment</code>
  </li>
  <li>
    <b>Models:</b> <code>ARIMA</code>, <code>AR</code>, <code>ETS with automatic parameter selection</code>
  </li>
  <li>
    <b>Interactive App:</b> <code>Upload data</code>, <code>select models</code>, <code>adjust parameters</code>, <code>visualize forecasts</code>
  </li>
  <li>
    <b>Export:</b> <code>Download forecasts as CSV/Excel and plots as PNG</code>
  </li>
</ul>

<br>

<h2>
  Repository Structure
</h2>

<br>

<div>
  <code>├── mardown_forecasting.Rmd</code>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <em># Analysis notebook</em><br>
  <code>├── app.R</code>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  &nbsp;
  <em># Shiny web application</em><br>
  <code>├── dinamika-tsen.csv</code>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <em># Price dataset (2022-2026)</em><br>
  <code>├── forecasting.html</code>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  &nbsp;
  <em># Generated notebook</em><br>
  <code>└── README.md</code>
</div>

<br>

<h2>
  Data
</h2>

<div>
  Monthly real estate prices per square meter across Russian regions:
  <ul>
  <li>
    <code>Primary</code> and <code>Secondary</code> markets
  </li>
  <li>
    40+ regions including Moscow, Saint Petersburg
  </li>
  <li>
    Period: <code>January 2022</code> - <code>January 2026</code>
  </li>
</ul>
</div>

<br>

<h2>
  Quick Start
</h2>

<div>
  <b>Run the Shiny App</b><br>
  <code>install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "forecast", "tseries", "DT", "scales", "zoo"))</code>
  <code>shiny::runApp("app.R")</code>
</div>

<br>

<div>
  <b>Run Analysis</b><br>
  Open <code>mardown_forecasting.Rmd</code> in <code>RStudio</code> and knit to <code>HTML</code>.
</div>

<br>

<h2>
  Key Results
</h2>

<ul>
  <li>
    <b>Best Model:</b> <code>ARIMA(1,2,2)</code>
  </li>
  <li>
    <b>MAPE:</b> <code>2.38%</code>
  </li>
    <li>
    <b>RMSE:</b> <code>4,232 RUB/sq.m</code>
  </li>
  <li>
    <b>Forecast:</b> <code>~7% annual price growth expected</code>
  </li>
</ul>

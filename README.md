# Traffic-Prediction-System-Using-R-code
Traffic Forecaster – AI-Based Vehicle Density Prediction using R Shiny and Random Forest

Traffic Forecaster is an interactive machine learning web application built using R Shiny that predicts vehicle traffic density based on environmental and situational factors. The system uses a Random Forest regression model trained on a synthetic dataset to estimate the number of vehicles per hour under different real-world conditions such as weather, time, road incidents, and construction zones.

The project demonstrates the integration of data science, machine learning, and interactive visualization in a user-friendly dashboard interface. Users can adjust parameters including time of day, temperature, rainfall, visibility, weekend indicator, holidays, road incidents, and construction activity to simulate real-time traffic conditions. The application then predicts expected vehicle flow and classifies the traffic density into Low, Medium, or High categories.

The model is trained using the caret and randomForest libraries, ensuring robust prediction performance. Visualization of prediction results is provided through ggplot2, while the interface design uses bslib themes to create a modern, high-tech dashboard experience.

Key features include:

Synthetic dataset generation if dataset is not available
Random Forest model training for traffic prediction
Interactive Shiny dashboard UI
Real-time prediction based on user inputs
Traffic density classification system
Visual representation of predicted traffic volume
System log tracking for simulation runs

This project is suitable for learning applications of machine learning in smart transportation systems, urban planning, and predictive analytics. It also demonstrates practical implementation of R programming concepts, including data preprocessing, model training, reactive programming, and interactive visualization.

The application can be extended to integrate real-world datasets, IoT traffic sensors, or smart city infrastructure for improved accuracy and scalability.

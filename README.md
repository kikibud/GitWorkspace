# Project Title

This project aims to compare two different methods of monitoring substrate EC for developing new fertility management protocols in nursery greenhouses. Sensor-based and manually collected substrate EC values from a field experiment on lantana are stored in this repository.

## Description

### Introduction 

Substrate electrical conductivity (EC) measurement is a required Best Management Practice (BMP) for the application of supplemental fertilizers in nursery and greenhouse industries to protect and conserve water resources. The current method of measuring substrate EC is through the Pour-through (PT) procedure, a multi-step method in which representative plants are selected for EC measurement, and a predetermined volume of water is poured on the surface of each test plant. The resulting leachate is collected and EC is determined using an EC meter. This process can be extensive for large-scale nursery production zones, requiring a significant amount of time and manual labor. With the personnel shortages that exist in production nurseries, technologies are needed to improve and optimize EC measurement and recordkeeping so the BMP is effective. This project aims to develop a new, sensor-based method for measuring EC to reduce the time invested by producers compared to the current PT method and provide real-time information on the fertility status of container-grown plants.

### Experiment 

In this experiment, various conditions typical of a container nursery environment are simulated. Two locations, a nursery pad and a high tunnel, will be used to represent the two most common setups for plant cultivation in container nurseries, each with inherently different light exposure and precipitation. Two irrigation methods will be implemented to reflect common practices in each type of container nursery setup. Drip irrigation will be used in the high tunnel, providing a targeted delivery of water directly to the plant roots. On the nursery pad, sprinkler irrigation will be used to provide a more even distribution of water to the surface of the pots. Finally, two different fertilizer concentrations will be used in each location, including a “low” fertilizer concentration of 1.5 lb N/cu yd and a “high” concentration of 3 lb N/cu yd. 

Lantana was selected as the optimal plant for testing due to its hardiness, heat resistance, and fast growth rate. The two varieties selected were Lantana “Bandana Yellow” and Lantana “Bandana Red”. For the experiment, a total of 120 containers will be used, with each pot containing one of each variety of lantana. 60 of these pots will be placed in the high tunnel area, with the remaining 60 placed in the nursery pad space. In each location, half the pots will have a low fertilizer concentration, with the other half receiving a high fertilizer concentration. Three sensors will be installed in each location-fertilizer rate combination, for a total of six sensors per location, and 12 sensors overall.

Plants will be watered at an equal rate amongst all conditions. Sensor measurements will be taken every 20 seconds, with each measurement recorded as an average of three measurements to ensure no voltage spikes significantly skew the recorded measurement. Irrigation events will be scheduled once a day. The time, duration, and application rate of the irrigations are yet to be determined. Pour-though tests will be completed one hour after the irrigation is completed for comparison with the sensor-based readings.

Teros 12 sensors were installed in six containers each of the low and high fertilizer rates during transplanting. To install the sensors, a window was cut into the side of each container, approximately 1.25 inches from the base of the container, 3 inches long, and 0.75 inches tall. A small notch was cut in the bottom corner to fit the sensor cord when the window flap was closed. The sensor was passed through the opening and the flap was then sealed with duct tape. Media was added underneath the sensor until level with the bottom of the window. The sensor was then positioned horizontally in the center of the container, and the rest of the media was added. 

The experiment will run for a period of 10 weeks. Once completed, a destructive soil sampling technique will be employed to examine salt stratification in the containers, which could aid in determining optimal sensor placement in the pots. It is expected that the containers in the high tunnel and drip irrigation condition will have a significantly less even distribution of salts due to the application of water at a single location on the surface of the pot, while the sprinkler irrigation on the nursery pad will be more uniform due to the more even application of water on the surface. This experiment will help inform decisions on sensor protocol development depending on the setup and irrigation methods employed by a given container nursery.

### Data

Data from manual PT measurements are stored in a single CSV file. Values are recorded twice a week (Monday and Friday) and are composite samples from five randomly selected containers. Values for both substrate EC and pH are collected. EC is measured in uS/cm.

Data from Teros 12 sensors are collected using an ESP32, which queries the sensors and sends the response to a gateway node, which in turn uploads the sensor data to an AWS database. Sensor data is periodically written to a CSV, downloaded, and merged with previously collected data to serve as a backup of data on the server and allow for data manipulation in other platforms. Sensor data includes sensor number, raw VWC, temperature, and EC. Raw VWC values are unitless and must be converted into VWC (m^3/m^3) using the equations provided in the [Teros 12 manual](https://publications.metergroup.com/Manuals/20587_TEROS11-12_Manual_Web.pdf). Temperature values are recorded in degrees Celsius and EC is recorded in uS/cm.

## Future Files

Future files to be uploaded to this repository include the code for data analysis and previously collected data on sensor environmnetal sensitivity.

## Authors

Michelle Ezequelle
mezequelle@ufl.edu

Kaiwen Xiao
kaiwen.xiao@ufl.edu

## Version History

* 0.1
    * Initial Release

# Prostate Clinical Outlook (PCO)

Point of Care Decision Support System for Prostate Cancer Patients

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

What things you need to install the software and how to install them

```
1. Operating System: Mac OS X or Linux based OS 
2. Database: MySQL Database(initial userid/passwd(dbname) : shiny/shiny(CDSS) )
3. R installation(ref:https://cran.r-project.org/))

```

### Installing

A step by step series of examples that tell you have to get a development env running

```
0. Proceed with the prerequisites 
I. Install MySQL based on the instruction specified in the following page -
https://www.digitalocean.com/community/tutorials/how-to-install-mysql-on-ubuntu-18-04
Note that the user root has the password= tps0317!
Use command "mysql -u root -p'tps0317!'  to access mysql prompt
At the mysql prompt, 
	a. create user=shiny with password=shiny  (mysql> GRANT ALL PRIVILEGES ON *.* TO 'shiny'@'localhost' IDENTIFIED BY 'shiny';)
	b. create database call CDSS (mysql>CREATE DATABASE CDSS;)
	
II. R 3.6 installation
Follow the instructions provided at https://cran.r-project.org/.  Note that ubuntu-18-04 is the Bionic Beaver version, so follow the instructions specified for the "bionic" version of ubuntu.  
	a. Use vi to add the following line "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/" to the /etc/apt/sources.list file.  Also, uncomment corresponding lines containing the "deb-src" heading.  This will allow access to repository containing the source files.
	b. For secure APT, run the following commands
		sudo -i
		apt-get update
		apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
	b. Install R
		sudo -i
		apt-get update
		apt-get install r-base
		apt-get install r-base-dev


	c. Install the following libraries needed for R package installation:
		apt-get -y install libcurl4-openssl-dev libmysqlclient-dev libv8-dev
		apt install default-jre
		apt install default-jdk


1. Download PCO.zip source to local file system, i.e., /home/aic/PCO.zip
	mkdir -p /srv/shiny-server
	cd /srv/shiny-server
	unzip /home/aic/PCO.zip
    cd PCO

2. Install R libraries.  Make sure that no errors are shown on the screen output.
	Rscript ./install_libraries.R 

3. Install nginx proxy and create pco service
	sudo -i
	apt-get install nginx
	apt-get install apache2-utils
	service nginx stop
	#configure nginx for proxy service
	cp /srv/shiny-server/PCO/nginx/default /etc/nginx/sites-available/default
	#use simple authentication.  Create user1 username
	htpasswd -c /etc/nginx/.htpasswd user1
	service nginx start
	#create PCO service to run after reboot 
	cp /srv/shiny-server/PCO/etc/pco.service /etc/systemd/system/
	systemctl enable pco
	systemctl start pco

4. connect to the URL: http://localIPAddress

Troubleshooting Tips:


````````````````````

# IDMS China - Development Branch #

### Current Ongoing Release ###
* Release version: <Version>
* <Add confluence page>

### Branch Usage ###
* The develop/API branch is be used to work on the current release. Details of the current release to be updated in the release section.
* This branch should be used to create topic branches and be the target of the pull request created on the topic branch.
* Serve as origin branch for the Integration branch.
* Serve as the target for parallel feature branch merges, when those feature needs to be released

### Repository Structure ###
* Follows the Maven module structure
* Composed of fours modules
	- schneider-idms(Root POM project)
	- schneider-cache-central (Module 1)
	- schneider-idms-client (Module 2)
	- schneider-idms-service (Module 3)
	- schneider-idms-app (Module 4)

### How to build ###
* To build a development war file
	- $ cd ./schneider-idms/
	- $ mvn package
* To build a server deployable application jar
	- $ cd ./schneider-idms/
	- $ mvn package -P server

### Runtime properties ###
* The same WAR file can be ran in an Integration, Staging, Pre-prod or Production mode, using argument
	-Dspring.profiles.active=<DEV|INTG|STG|PPRD|PROD>
* The location of the app_root having the externalized configuration can be provided using the argument
	-Didmsc.app_root.location=<Absolute-path-to-app_root-folder>
* To ensure the correct Spring beans for the set environment is loaded, the below flag can be set to true
	-Dsys.print.beans=true

	

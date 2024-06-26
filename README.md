# First Time startup and Troubleshooting

Change file path so that the necessary .csv files can be found.
If gps location seems innacurate, open google maps on the same device, this will force an update to your current location and then it should work fine in the app.
All times are displayed in Central time (America/Chicago).


**If you have any suggestions or additions to this website contact me at joshkocik@gmail.com**


# Website Organization

## Current RVM:
This page calculates the closest Missouri River mile based on a linear distance from your device's location to the central point of that river mile tenth and as a result this runs into some issues with large switchbacks or in areas where the river has shifted from its previous path. To help combat this I have added a distance in meters from your location to the river mile this page outputs.

## Location:
### Calculate RVM:
This page works the same as Current RVM, however you can manually input a gps location.

### Boat Ramps:
This Page displays Missouri River boat launches as well as there associated miles. Each one has approximate GPS locations via a google maps link. It opens automatically in google maps and on devices without google maps it should open in the same browser the webpage is open in.

## River Conditions:
For all listed locations a map of recent gage height as well as flood stages will be displayed. By clicking on weather, the weather every three hours will be displayed including several metrics.

## Fish:

### Master Angler:
This displays the Nebraska master angler and state record criteria as well as the source of this information.
Online Resources: This is a list of useful links for working on the Missouri River.

# File Information

ClosestRVMTest.R is the most up to date version of this website and should contain all the code necessary to run the app.

RiverMilesTenths.csv contains the data I currently have for Missouri River miles (0-800). 

BoatRamp.csv contains all the information for the boat ramps I utilize along the Missouri River. Each one is link to an approximate google maps link.

MasterAngler.csv contains the 2023 (need to update) master angler and state record data.

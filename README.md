# wuenic4

Assuming that swipl.exe and R.exe are on the PATH,

For a single country
````
cd wuenic4
estimate.bat bgd
sha1sum.bat bgd
````
The output file is found in the folder out, bgd.txt.

For all countries
````
cd wuenic4
all.bat
sha1all.bat > v4.txt
````

Compare output to Version 3.9 (compares everything except Country, Comment and ChildrenInTarget):
````
diffcov.bat
````

The response should be `named integer(0)`.

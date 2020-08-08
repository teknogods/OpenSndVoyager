Get-Content .\OpenSndVoyager\src\OpenSndVoyager.rc | ForEach-Object { $_ -replace "1.0.0.0", $APPVEYOR_BUILD_VERSION } | Set-Content .\OpenSndVoyager\src\OpenSndVoyager2.rc
del .\OpenSndVoyager\src\OpenSndVoyager.rc
mv .\OpenSndVoyager\src\OpenSndVoyager2.rc .\OpenSndVoyager\src\OpenSndVoyager.rc
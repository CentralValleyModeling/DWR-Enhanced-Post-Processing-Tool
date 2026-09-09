# DWR Enhanced Post Processing Tool (EPPT)

The DWR Enhanced Post Processor Tool (EPPT) is a Windows desktop user interface application that will allow experienced CalSim/ CalLite users to review, compare and report study results. The major features of the application include:
1.	Quick Results, model run comparison using pre-defined metrics and selection options for time, parameter, and location. 
2.	DTS Report, similar to Quick Results but introduces derived time series (DTS) that allows for comparison between model variables.
3.	Schematic View (*will be developed in a subsequent version), comparison and drill-down of model runs presented using a schematic view of the system.
4.	QA/QC Report, model run comparison presented as a PDF report that includes summary information and detailed graphs and tables.

# Module Dependency Graph

![Dependency Graph](https://github.com/CalSimCalLite/DWR-Enhanced-Post-Processing-Tool/blob/master/DependencyGraph.png)

## Updating to a new Bundled Java Version
EPPT bundles an included OpenJDK release. Due to the use of JavaFX and Swing components, a JDK release must
be used, as the JRE does not include the necessary components for JavaFX + Swing interoperability.

The JDK that EPPT uses is stored on GitHub Packages to make it available as a Maven Dependency:
https://github.com/CentralValleyModeling/DWR-Enhanced-Post-Processing-Tool/packages/3226725

This is a direct re-upload of the Adoptium OpenJDK release with no modifications. To upload a new release, download the
appropriate Adoptium JDK release in zip format from https://adoptium.net/temurin/releases?version=21&os=windows&arch=x64 
and use the Maven deploy:deploy-file command to upload:
```cmd
mvn deploy:deploy-file -DrepositoryId=github -Durl=https://maven.pkg.github.com/CentralValleyModeling/DWR-Enhanced-Post-Processing-Tool/ -Dfile=OpenJDK21U-jdk_x64_windows_hotspot_[VERSIONHERE].zip -DgroupId=net.adoptium -DartifactId=jdk -Dversion=[VERSIONHERE]_win-x86_64 -Dpackaging=zip
```
Replace the file name and version as appropriate to match your updated JDK release. The updated JDK can be referenced in
the application/pom.xml.
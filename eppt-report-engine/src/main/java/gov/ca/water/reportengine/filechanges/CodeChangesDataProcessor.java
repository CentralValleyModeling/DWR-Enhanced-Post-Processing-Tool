/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.reportengine.filechanges;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.logging.Logger;
import java.util.stream.Stream;

import gov.ca.water.reportengine.EpptReportException;
import org.apache.commons.io.FileUtils;

import static java.util.stream.Collectors.toList;

public class CodeChangesDataProcessor
{
	private static final Logger LOGGER = Logger.getLogger(CodeChangesDataProcessor.class.getName());

	private static final int TYPE_COLUMN = 0;
	private static final int SUBTYPE_COLUMN = 1;
	private static final int WRESL_FILES_COLUMN = 2;

	//private Map<CodeChangesType, List<CodeChangesSubType>> _changesMap = new HashMap<>();
	private final List<CodeChangesType> _codeChangesTypes = new ArrayList<>();

	private List<String> _allFilesFromMaster = new ArrayList<>();

	public CodeChangesDataProcessor(Path csvPath) throws EpptReportException
	{
		loadCodeChangesCSV(csvPath);
	}

	public List<CodeChangesType> getCodeChangesTypes()
	{
		return _codeChangesTypes;
	}


	public CodeChangesStatistics processCodeChanges(Path baseOutputPath, Path altOutputPath) throws EpptReportException, IOException
	{
		Set<Path> modifiedFiles = new HashSet<>();
		Set<Path> addedInAlt = new HashSet<>();
		Set<Path> deletedFromBase = new HashSet<>();

		List<Path> basePaths = getAllPathsInDir(baseOutputPath);
		List<Path> altPaths = getAllPathsInDir(altOutputPath);

		for(Path basePath : basePaths)
		{
			if(altPaths.contains(basePath))//&& areFilesDifferent(basePath,altPath))
			{
				Path base = baseOutputPath.resolve(basePath);
				Path alt = altOutputPath.resolve(basePath);
				if(areFilesDifferent(base, alt))
				{
					modifiedFiles.add(basePath);
				}
			}
			else if(!altPaths.contains(basePath))
			{
				deletedFromBase.add(basePath);
			}
		}

		for(Path altPath : altPaths)
		{
			if(!basePaths.contains(altPath))
			{
				addedInAlt.add(altPath);
			}
		}


		//        Map<CodeChangesSubType, Set<String>> subtypeToFileChangesMap = new HashMap<>();
		//
		//
		//        for (CodeChangesType parent : _codeChangesTypes)
		//        {
		//            for (CodeChangesSubType subType : parent.getSubTypes())
		//            {
		//                Set<String> modifiedFilesForSubtype = new HashSet<>();
		//
		//                for (String wreslFile : subType.getWreslFiles())
		//                {
		//
		//                    _allFilesFromMaster.add(wreslFile);
		//                    boolean baseHasFile = doesFileExistInDirectory(baseOutputPath, wreslFile);
		//                    boolean altHasFile = doesFileExistInDirectory(altOutputPath, wreslFile);
		//                    if (baseHasFile && altHasFile)
		//                    {
		//                        //compare them
		//                        if (areFilesDifferent(baseOutputPath, altOutputPath))
		//                        {
		//                            modifiedFilesForSubtype.add(wreslFile);
		//                        }
		//                    }
		//                    else if (baseHasFile && !altHasFile)
		//                    {
		//                        //file was deleted in alt
		//                        deletedFromBase.add(wreslFile);
		//
		//                    }
		//                    else if (!baseHasFile && altHasFile)
		//                    {
		//                        //file was added to alt
		//                        addedInAlt.add(wreslFile);
		//                    }
		//                }
		//
		//                subtypeToFileChangesMap.put(subType, modifiedFilesForSubtype);
		//            }
		//        }
		return new CodeChangesStatistics(deletedFromBase, addedInAlt, modifiedFiles);

	}

	private List<Path> getAllPathsInDir(Path outputDirectory) throws EpptReportException
	{
		List<Path> allPaths;
		try(Stream<Path> paths = Files.walk(outputDirectory))
		{
			allPaths = paths.filter(p -> p.toFile().isFile())
							.map(outputDirectory::relativize)
							.collect(toList());

		}
		catch(IOException e)
		{
			throw new EpptReportException("Error getting all files", e);
		}
		return allPaths;
	}

	//    private List<Path> getAllFilesNotInMaster(Path outputDirectory) throws EpptReportException
	//    {
	//        List<Path> addedFiles = new ArrayList<>();
	//        List<Path> allPaths;
	//        try (Stream<Path> paths = Files.walk(outputDirectory))
	//        {
	//            allPaths = paths.filter(path1 -> Files.isRegularFile(path1)).collect(toList());
	//
	//
	//            List<Path> fullFilesFromMaster = _allFilesFromMaster.stream()
	//                    .map(outputDirectory::resolve)
	//                    .collect(toList());
	//
	//            for (Path basePath : allPaths)
	//            {
	//                if (!fullFilesFromMaster.contains(basePath))
	//                {
	//                    addedFiles.add(basePath);
	//                }
	//            }
	//
	//        }
	//        catch (IOException e)
	//        {
	//            throw new EpptReportException("Error getting all files", e);
	//        }
	//        return addedFiles;
	//    }


	//    private boolean doesFileExistInDirectory(Path outputPath, String fileLookingFor)
	//    {
	//        String trimmedFile = fileLookingFor.substring(1);
	//        String totalPath = outputPath.toString() + trimmedFile;
	//        File f = new File(totalPath);
	//        if (f.exists() && !f.isDirectory())
	//        {
	//            return true;
	//        }
	//        else
	//        {
	//            return false;
	//        }
	//    }

	private boolean areFilesDifferent(Path baseFile, Path altFile) throws IOException
	{
		return !FileUtils.contentEquals(baseFile.toFile(), altFile.toFile());
	}

	private void loadCodeChangesCSV(Path csvPath) throws EpptReportException
	{
		LOGGER.info("");

		String line = "";
		String csvSplitBy = ",";

		try(BufferedReader br = Files.newBufferedReader(csvPath))
		{

			//skip first line
			int z = 0;
			while((line = br.readLine()) != null)
			{
				if(z == 0)
				{
					z++;
					continue;
				}

				String[] row = line.split(csvSplitBy);

				String type = row[TYPE_COLUMN];
				String subtype = row[SUBTYPE_COLUMN];
				String wreslFile = null;
				if(row.length > 2)
				{
					wreslFile = row[WRESL_FILES_COLUMN];
				}

				boolean isNewType = true;
				for(int i = 0; i < _codeChangesTypes.size(); i++)
				{
					CodeChangesType parent = _codeChangesTypes.get(i);
					if(Objects.equals(parent.getName(), type))
					{
						isNewType = false;
						updateSubtypes(parent, subtype, wreslFile);
					}
				}

				if(isNewType)
				{
					//create new type
					CodeChangesType codeChangeType = new CodeChangesType(type);
					CodeChangesSubType codeChangesSubType = new CodeChangesSubType(subtype);
					if(wreslFile != null)
					{
						//add it and get rid of any trailing "." and or slashes by normalizing
						Path wreslPath = Paths.get(wreslFile);
						codeChangesSubType.addWreslFile(wreslPath.normalize().toString());
					}
					codeChangeType.addSubtype(codeChangesSubType);
					_codeChangesTypes.add(codeChangeType);
				}
			}
		}
		catch(IOException e)
		{
			throw new EpptReportException("Error loading values from the code changes xml file: " + csvPath, e);
		}
	}

	private void updateSubtypes(CodeChangesType type, String subtype, String file)
	{
		List<CodeChangesSubType> subTypes = type.getSubTypes();
		boolean foundSubtype = false;
		for(int i = 0; i < subTypes.size(); i++)
		{
			if(Objects.equals(subTypes.get(i).getName(), subtype))
			{
				if(file != null)
				{
					Path wreslPath = Paths.get(file);
					subTypes.get(i).addWreslFile(wreslPath.normalize().toString());
				}
				foundSubtype = true;
				break;
			}
		}
		if(foundSubtype == false)
		{
			CodeChangesSubType codeChangesSubType = new CodeChangesSubType(subtype);
			if(file != null)
			{
				Path wreslPath = Paths.get(file);
				codeChangesSubType.addWreslFile(wreslPath.normalize().toString());
			}
			type.addSubtype(codeChangesSubType);
		}
	}


}

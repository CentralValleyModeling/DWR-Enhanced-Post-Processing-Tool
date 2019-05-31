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
import java.util.logging.Level;
import java.util.stream.Stream;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.reportengine.EpptReportException;
import org.apache.commons.io.FileUtils;

import static java.util.stream.Collectors.toList;

public class CodeChangesDataProcessor
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();

	private static final int TYPE_COLUMN = 0;
	private static final int SUBTYPE_COLUMN = 1;
	private static final int WRESL_FILES_COLUMN = 2;
	private final List<CodeChangesType> _codeChangesTypes = new ArrayList<>();

	public CodeChangesDataProcessor(Path csvPath) throws EpptReportException
	{
		loadCodeChangesCSV(csvPath);
	}

	List<CodeChangesType> getCodeChangesTypes()
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
			if(altPaths.contains(basePath))
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
		return new CodeChangesStatistics(deletedFromBase, addedInAlt, modifiedFiles);

	}

	private List<Path> getAllPathsInDir(Path outputDirectory) throws EpptReportException
	{
		List<Path> allPaths;
		try(Stream<Path> paths = Files.walk(outputDirectory))
		{
			allPaths = paths.filter(p -> p.toFile().isFile())
							.filter(p->
							{
								String filename = p.getFileName().toString().toLowerCase();
								return filename.endsWith(".wresl") || filename.endsWith(".table");
							})
							.map(outputDirectory::relativize)
							.collect(toList());

		}
		catch(IOException e)
		{
			throw new EpptReportException("Error getting all files", e);
		}
		return allPaths;
	}

	private boolean areFilesDifferent(Path baseFile, Path altFile) throws IOException
	{
		return !FileUtils.contentEquals(baseFile.toFile(), altFile.toFile());
	}

	private void loadCodeChangesCSV(Path csvPath) throws EpptReportException
	{
		LOGGER.at(Level.INFO).log("Loading Code Changes configurationg from: %s", csvPath);

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
				for(CodeChangesType parent : _codeChangesTypes)
				{
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
		for(CodeChangesSubType subType : subTypes)
		{
			if(Objects.equals(subType.getName(), subtype))
			{
				if(file != null)
				{
					Path wreslPath = Paths.get(file);
					subType.addWreslFile(wreslPath.normalize().toString());
				}
				foundSubtype = true;
				break;
			}
		}
		if(!foundSubtype)
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

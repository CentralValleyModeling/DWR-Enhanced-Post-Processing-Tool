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

package gov.ca.water.calgui.project;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 10-10-2019
 */
public class EpptScenarioRunValidator
{
	private final EpptScenarioRun _epptScenarioRun;

	public EpptScenarioRunValidator(EpptScenarioRun epptScenarioRun)
	{
		_epptScenarioRun = epptScenarioRun;
	}

	public boolean isValid()
	{
		return getErrors().isEmpty();
	}

	public List<String> getErrors()
	{
		List<String> retval = new ArrayList<>();
		if(isNameInvalid())
		{
			retval.add("Invalid Scenario Name: " + _epptScenarioRun.getName());
		}
		if(isModelInvalid())
		{
			retval.add("Invalid Scenario Model: " + _epptScenarioRun.getModel());
		}
		if(isWaterYearTableInvalid())
		{
			retval.add("Invalid Scenario Water Year Table: " + _epptScenarioRun.getWaterYearTable());
		}
		if(isWreslMainInvalid())
		{
			retval.add("Invalid Scenario EPPT WRESL Main: " + _epptScenarioRun.getWreslMain());
		}
		if(isOutputPathInvalid())
		{
			retval.add("Invalid Scenario Model Directory: " + _epptScenarioRun.getOutputPath());
		}
		if(isColorInvalid())
		{
			retval.add("Invalid Scenario Color: " + _epptScenarioRun.getColor());
		}
		retval.addAll(getDssContainerErrors());
		return retval;
	}

	private boolean isColorInvalid()
	{
		return _epptScenarioRun.getColor() == null;
	}

	private boolean isOutputPathInvalid()
	{
		Path outputPath = _epptScenarioRun.getOutputPath();
		return outputPath == null || !outputPath.toFile().exists()
				|| !outputPath.toFile().isDirectory();
	}

	private boolean isWreslMainInvalid()
	{
		Path wreslMain = _epptScenarioRun.getWreslMain();
		return wreslMain == null || !wreslMain.toFile().exists()
				|| !wreslMain.toFile().isFile();
	}

	private boolean isWaterYearTableInvalid()
	{
		Path waterYearTable = _epptScenarioRun.getWaterYearTable();
		return waterYearTable == null || !waterYearTable.toFile().exists()
				|| !waterYearTable.toFile().isFile();
	}

	private boolean isModelInvalid()
	{
		return _epptScenarioRun.getModel() == null || _epptScenarioRun.getModel().toString().isEmpty();
	}

	private boolean isNameInvalid()
	{
		return _epptScenarioRun.getName() == null || _epptScenarioRun.getName().isEmpty();
	}

	private List<String> getDssContainerErrors()
	{
		List<String> retval = new ArrayList<>();
		EpptDssContainer dssContainer = _epptScenarioRun.getDssContainer();
		if(dssContainer == null)
		{
			retval.add("Scenario Run DSS Container is null");
		}
		else
		{
			NamedDssPath dvDssFile = dssContainer.getDvDssFile();
			if(isContainerInvalid(dvDssFile))
			{
				retval.add("DV DSS File is invalid. Ensure A and F parts are defined");
			}
			NamedDssPath dtsDssFile = dssContainer.getDtsDssFile();
			if(isContainerInvalid(dtsDssFile))
			{
				retval.add("QA_QC DSS File is invalid. Ensure A and F parts are defined");
			}
			NamedDssPath svDssFile = dssContainer.getSvDssFile();
			if(isContainerInvalid(svDssFile))
			{
				retval.add("SV DSS File is invalid. Ensure A and F parts are defined");
			}
		}
		return retval;
	}

	private boolean isContainerInvalid(NamedDssPath namedDssPath)
	{
		if(namedDssPath == null || namedDssPath.toString().isEmpty())
		{
			return true;
		}
		else
		{
			String aPart = namedDssPath.getAPart();
			String fPart = namedDssPath.getFPart();
			return aPart == null || aPart.isEmpty() || fPart == null || fPart.isEmpty();
		}
	}
}

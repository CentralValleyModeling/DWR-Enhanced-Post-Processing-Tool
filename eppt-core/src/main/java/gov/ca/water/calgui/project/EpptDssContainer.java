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
import java.util.Collections;
import java.util.List;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-01-2019
 */
public class EpptDssContainer
{
	private final Path _dvDssFile;
	private final Path _svDssFile;
	private final Path _ivDssFile;
	private final List<Path> _extraDssFiles;

	public EpptDssContainer(Path dvDssFile, Path svDssFile, Path ivDssFile, List<Path> extraDssFiles)
	{

		_dvDssFile = dvDssFile;
		_svDssFile = svDssFile;
		_ivDssFile = ivDssFile;
		_extraDssFiles = Collections.unmodifiableList(extraDssFiles);
	}

	/**
	 * @return path to the Derived Variable DSS file path
	 */
	public Path getDvDssFile()
	{
		return _dvDssFile;
	}

	/**
	 * @return path to the State Variable DSS file path
	 */
	public Path getSvDssFile()
	{
		return _svDssFile;
	}

	public Path getIvDssFile()
	{
		return _ivDssFile;
	}

	/**
	 * @return unmodifiable list of extra DSS file paths
	 */
	public List<Path> getExtraDssFiles()
	{
		return _extraDssFiles;
	}
}

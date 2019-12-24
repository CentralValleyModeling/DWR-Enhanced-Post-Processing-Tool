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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-01-2019
 */
public class EpptDssContainer
{
	private final NamedDssPath _dvDssFile;
	private final NamedDssPath _svDssFile;
	private final NamedDssPath _ivDssFile;
	private final NamedDssPath _dtsDssFile;
	private final List<NamedDssPath> _extraDssFiles;

	public EpptDssContainer(NamedDssPath dvDssFile, NamedDssPath svDssFile, NamedDssPath ivDssFile, NamedDssPath dtsDssFile,
							List<NamedDssPath> extraDssFiles)
	{
		_dvDssFile = dvDssFile;
		_svDssFile = svDssFile;
		_ivDssFile = ivDssFile;
		_dtsDssFile = dtsDssFile;
		_extraDssFiles = Collections.unmodifiableList(extraDssFiles);
	}

	public EpptDssContainer(EpptDssContainer epptDssContainer)
	{
		_dvDssFile = new NamedDssPath(epptDssContainer.getDvDssFile());
		_svDssFile = new NamedDssPath(epptDssContainer.getSvDssFile());
		_ivDssFile = new NamedDssPath(epptDssContainer.getIvDssFile());
		_dtsDssFile = new NamedDssPath(epptDssContainer.getDtsDssFile());
		_extraDssFiles = Collections.unmodifiableList(epptDssContainer.getExtraDssFiles().stream()
																	  .map(NamedDssPath::new)
																	  .collect(toList()));
	}

	/**
	 * @return path to the Derived Variable DSS file path
	 */
	public NamedDssPath getDvDssFile()
	{
		return _dvDssFile;
	}

	/**
	 * @return path to the State Variable DSS file path
	 */
	public NamedDssPath getSvDssFile()
	{
		return _svDssFile;
	}

	@Override
	public boolean equals(Object o)
	{
		if(this == o)
		{
			return true;
		}
		if(o == null || getClass() != o.getClass())
		{
			return false;
		}
		final EpptDssContainer that = (EpptDssContainer) o;
		return Objects.equals(getDvDssFile(), that.getDvDssFile()) &&
				Objects.equals(getSvDssFile(), that.getSvDssFile()) &&
				Objects.equals(getIvDssFile(), that.getIvDssFile()) &&
				Objects.equals(getDtsDssFile(), that.getDtsDssFile()) &&
				Objects.equals(getExtraDssFiles(), that.getExtraDssFiles());
	}

	@Override
	public int hashCode()
	{
		return Objects.hash(getDvDssFile(), getSvDssFile(), getIvDssFile(), getDtsDssFile(), getExtraDssFiles());
	}

	public NamedDssPath getIvDssFile()
	{
		return _ivDssFile;
	}

	public NamedDssPath getDtsDssFile()
	{
		return _dtsDssFile;
	}

	/**
	 * @return unmodifiable list of extra DSS file paths
	 */
	public List<NamedDssPath> getExtraDssFiles()
	{
		return _extraDssFiles;
	}

	public List<NamedDssPath> getAllDssFiles()
	{
		List<NamedDssPath> retval = new ArrayList<>();
		retval.add(getDtsDssFile());
		retval.add(getDvDssFile());
		retval.add(getSvDssFile());
		retval.add(getIvDssFile());
		retval.addAll(getExtraDssFiles());
		return retval;
	}

}

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
import java.util.Objects;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-01-2019
 */
public class NamedDssPath
{
	private final Path _dssPath;
	private final String _aliasName;
	private final String _aPart;
	private final String _ePart;
	private final String _fPart;

	public NamedDssPath(Path dssPath, String aliasName, String aPart, String ePart, String fPart)
	{
		_dssPath = dssPath;
		_aliasName = aliasName;
		_aPart = aPart;
		_ePart = ePart;
		_fPart = fPart;
	}

	public NamedDssPath(NamedDssPath ivDssFile)
	{
		_dssPath = ivDssFile.getDssPath();
		_aliasName = ivDssFile.getAliasName();
		_aPart = ivDssFile.getAPart();
		_ePart = ivDssFile.getEPart();
		_fPart = ivDssFile.getFPart();
	}

	public String getAPart()
	{
		return _aPart;
	}

	public String getEPart()
	{
		return _ePart;
	}

	public String getFPart()
	{
		return _fPart;
	}

	public Path getDssPath()
	{
		return _dssPath;
	}

	public String getAliasName()
	{
		return _aliasName;
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
		final NamedDssPath that = (NamedDssPath) o;
		return Objects.equals(getDssPath(), that.getDssPath()) &&
				Objects.equals(getAliasName(), that.getAliasName()) &&
				Objects.equals(_aPart, that._aPart) &&
				Objects.equals(_ePart, that._ePart) &&
				Objects.equals(_fPart, that._fPart);
	}

	@Override
	public int hashCode()
	{
		return Objects.hash(getDssPath(), getAliasName(), _aPart, _ePart, _fPart);
	}

	@Override
	public String toString()
	{
		if(_aliasName != null)
		{
			return _aliasName;
		}
		else
		{
			return _dssPath.toString();
		}
	}
}

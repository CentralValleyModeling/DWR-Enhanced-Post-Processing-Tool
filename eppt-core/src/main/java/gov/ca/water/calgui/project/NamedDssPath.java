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

	public NamedDssPath(Path dssPath, String aliasName)
	{
		_dssPath = dssPath;
		_aliasName = aliasName;
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
		return getDssPath().equals(that.getDssPath()) &&
				getAliasName().equals(that.getAliasName());
	}

	@Override
	public int hashCode()
	{
		return Objects.hash(getDssPath(), getAliasName());
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

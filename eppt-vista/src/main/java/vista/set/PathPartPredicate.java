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
package vista.set;

/**
 * Matches the given regular expression to a certain pathname part.
 */
@SuppressWarnings("serial")
public class PathPartPredicate extends RegExPredicate<DataReference>
{
	/**
	 * The path part id.
	 */
	private int _partId;

	/**
	 * initializes the regular expression compilers
	 */
	public PathPartPredicate(String regex, int partId)
	{
		// check (partId);
		super(regex);
		_partId = partId;
	}

	/**
	 * returns the path part id.
	 */
	public int getPartId()
	{
		return _partId;
	}

	@Override
	public boolean apply(DataReference first)
	{
		return (first != null)
				&& (first.getPathname().getPart(_partId).length() > 0)
				&& (_pattern.matcher(first.getPathname().getPart(_partId))
							.find());
	}
}

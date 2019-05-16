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
 * Function for filtering the entire pathname string using a regular expressioin
 */
@SuppressWarnings("serial")
public class PathnamePredicate extends RegExPredicate<DataReference>
{
	/**
	 * initializes the regular expression compilers
	 */
	public PathnamePredicate(String regex)
	{
		super(regex);
	}


	@Override
	public boolean apply(DataReference ref)
	{
		return (ref != null && _pattern.matcher(ref.getPathname()
												   .getFullPath()).find());
	}
}

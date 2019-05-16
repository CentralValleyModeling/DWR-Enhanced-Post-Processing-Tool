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
 * Matches name of Named implementers to name given in constructor.
 *
 * @see Named
 */
public class MatchNamePredicate implements Predicate<Named>
{
	/**
	 * name to be matched
	 */
	private String _name;

	/**
	 * constructor name to be matched
	 */
	public MatchNamePredicate(String name)
	{
		_name = name;
	}


	/**
	 * execute function returns true if object's name matches given name.
	 */
	@Override
	public boolean apply(Named type)
	{
		return type != null && type.getName().equals(_name);
	}
}

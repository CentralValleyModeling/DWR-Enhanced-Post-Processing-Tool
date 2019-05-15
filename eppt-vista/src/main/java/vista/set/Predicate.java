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
 * Primarily for defining a filtering criteria for a collection
 *
 * @param <T>
 * @author psandhu
 */
public interface Predicate<T>
{
	/**
	 * @param type
	 * @return true or false if the type is to be accepted or rejected in a collections.
	 */
	boolean apply(T type);
}

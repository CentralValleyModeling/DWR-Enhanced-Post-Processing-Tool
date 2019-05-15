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
package vista.gui;

/**
 * Informs an object that implements this interface that OK, Cancel or Apply
 * button is pressed. The object registers itself in the ButtonPanel either in
 * the constructor or by adding itself to the list of Changeables.
 *
 * @see ButtonPanel
 */
public interface Changeable
{
	/**
	 * Apply the changes (OK/Apply button pressed)
	 */
	void applyChanges();

	/**
	 * Done with making changes (OK/Cancel button pressed)
	 */
	void doneChanges();
}

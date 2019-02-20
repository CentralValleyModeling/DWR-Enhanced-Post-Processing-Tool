/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

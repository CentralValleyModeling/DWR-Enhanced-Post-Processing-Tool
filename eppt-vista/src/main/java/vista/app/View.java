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
package vista.app;

/**
 * An object which is the view onto some data
 *
 * @author Nicky Sandhu
 * @version $Id: View.java,v 1.1 2003/10/02 20:48:43 redwood Exp $
 */
public interface View
{
	/**
	 * gets the context of the view or the master object containing references
	 * to the context of this object
	 */
	SessionContext getContext();

	/**
	 * updates its view from its data
	 */
	void updateView();
}

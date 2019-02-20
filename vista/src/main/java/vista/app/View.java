/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

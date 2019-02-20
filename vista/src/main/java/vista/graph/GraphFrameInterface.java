/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import javax.swing.*;

/**
 * @author Nicky Sandhu
 * @version $Id: GraphFrameInterface.java,v 1.1 2003/10/02 20:49:00 redwood Exp
 * $
 */
public interface GraphFrameInterface
{
	/**
	 *
	 */
	void addToolBar(JToolBar tb);

	/**
	 *
	 */
	GECanvas getCanvas();

}

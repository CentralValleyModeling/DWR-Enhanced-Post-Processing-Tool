/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

/**
 * A element that needs to be an observer needs to implement this interface
 *
 * @author Nicky Sandhu
 * @version $Id: AnimationObserver.java,v 1.1 2003/10/02 20:48:47 redwood Exp $
 */
public interface AnimationObserver
{
	/**
	 * a method called when its time for the next update. Data and other
	 * messages are passed via the args object.
	 */
	void update(AnimationObservable observable, Object args);
}

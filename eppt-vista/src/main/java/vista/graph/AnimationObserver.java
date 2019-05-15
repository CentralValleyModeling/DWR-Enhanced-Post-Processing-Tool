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

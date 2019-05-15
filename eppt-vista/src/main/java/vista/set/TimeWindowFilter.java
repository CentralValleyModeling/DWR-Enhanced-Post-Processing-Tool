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

import vista.time.TimeWindow;

/**
 * Filters given data reference using its time window. A data reference is
 * acceptable if its time window contains the time window in this filter.
 * 
 * @author Nicky Sandhu
 * @version $Id: TimeWindowFilter.java,v 1.1 2003/10/02 20:49:34 redwood Exp $
 */
public class TimeWindowFilter implements Predicate<DataReference> {
	/**
	 * initializes the regular expression compilers
	 */
	public TimeWindowFilter(TimeWindow window) {
		_window = window;
	}

	/**
	 * returns the time window used for filtering...
	 */
	public TimeWindow getTimeWindow() {
		return _window;
	}

	/**
	 * returns string representation of filter...
	 */
	public String toString() {
		return "TimeWindow = " + _window;
	}

	/**
	 * The time window
	 */
	private TimeWindow _window;

	@Override
	public boolean apply(DataReference ref) {
		return (ref != null) 
		&& (_window.contains(ref.getTimeWindow()));
	}
}

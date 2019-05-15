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
package vista.time;

import java.text.DateFormat;

/**
 * Formats date according to HEC DSS criteria. namely midnight is interpreted as
 * 2400 of the previous day.
 * 
 * @author Nicky Sandhu
 * @version $Id: TimeFormat.java,v 1.1 2003/10/02 20:49:36 redwood Exp $
 */
public abstract class TimeFormat extends DateFormat {
	/**
	 * creates a time format for the given pattern
	 */
	public abstract TimeFormat create(String pattern);
}

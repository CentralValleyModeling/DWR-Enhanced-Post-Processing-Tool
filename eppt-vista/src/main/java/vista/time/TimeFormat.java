/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

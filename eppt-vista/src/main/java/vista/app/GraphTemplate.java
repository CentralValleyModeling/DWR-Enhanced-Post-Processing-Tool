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

import java.util.Vector;

import vista.set.DataReference;
import vista.set.PathnameFormat;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: GraphTemplate.java,v 1.2 1998/10/08 00:03:09 nsandhu Exp $
 */
public class GraphTemplate {
	/**
   *
   */
	public static String generateLegendText(DataReference ref) {
		String template = MainProperties.getProperty("graph.legendTemplate");
		return PathnameFormat.format(template, ref);
	}

	/**
   *
   */
	public static String generateTitleText(Vector refs) {
		if (refs.size() <= 0)
			return null;
		DataReference[] refArray = new DataReference[refs.size()];
		refs.copyInto(refArray);
		String template = MainProperties.getProperty("graph.titleTemplate");
		return PathnameFormat.format(template, refArray);
	}
}

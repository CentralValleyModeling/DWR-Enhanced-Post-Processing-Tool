/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import org.python.core.PyException;
import org.python.util.PythonInterpreter;

public class VScript {
	public static boolean DEBUG = false;

	public static void main(String[] args) throws PyException {
		if (args.length == 0)
			return;
		PythonInterpreter interp = new PythonInterpreter();
		// search for -i option
		int i = 0;
		for (i = 0; i < args.length; i++) {
			if (args[i].trim().equals("-i"))
				break;
		}
		String initFile = null;
		if (i < args.length) {
			if (args[i].trim().equals("-i")) {
				try {
					initFile = args[i + 1].trim();
				} catch (Exception e) {
					System.err
							.println("Invalid or missing initialization file");
				}
			}
		}
		if (initFile != null)
			interp.exec("execfile('" + initFile + "')");
		// search for -c option
		i = 0;
		while (i < args.length && !args[i].equals("-c"))
			i++;
		i++;
		if (i < args.length) {
			String pyfile = args[i].trim();
			// if arguments to script file being executed
			if (i + 1 < args.length) {
				int count = 0;
				interp.exec("import sys");
				String sysArray = null;
				for (int k = i; k < args.length; k++) {
					if (DEBUG)
						System.out.println("Adding environment variable "
								+ args[k]);
					if (sysArray == null) {
						sysArray = "'" + args[k] + "'";
					} else {
						sysArray += "," + "'" + args[k] + "'";
					}
				}
				if (DEBUG)
					System.out.println("sysArray : " + sysArray);
				interp.exec("sys.argv = [" + sysArray + "]");
			}
			//
			if (DEBUG)
				System.out.println("Executing file " + pyfile);
			interp.exec("execfile ('" + pyfile + "')");
			i++;
			// interp.exec("exit()");
		}
	}
}

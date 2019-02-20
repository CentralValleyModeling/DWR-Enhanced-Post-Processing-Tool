/*
    Copyright (C) 1996-2000 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0
	by Nicky Sandhu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    nsandhu@water.ca.gov

    Send bug reports to nsandhu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to nsandhu@water.ca.gov or call (916)-653-7552

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

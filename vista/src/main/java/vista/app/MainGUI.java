/*
    Copyright (C) 1996, 1997, 1998 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0beta
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

import vista.gui.VistaUtils;
import vista.set.Session;
import vista.set.TimeSeriesMath;

/**
 * The main GUI for the application
 * 
 * @author Nicky Sandhu
 * @version $Id: MainGUI.java,v 1.1 2003/10/02 20:48:33 redwood Exp $
 */
public class MainGUI extends DefaultFrame {
	/**
	 * The main method for the GUI. The command line for the gui is vista [-s
	 * session_file_name]
	 */
	public static void main(String[] args) {
		try {
			VistaUtils.showStartUpIcon();
			setDebugMode();
			new MainGUI(args);
		} catch (Exception e) {
			e.printStackTrace(System.err);
			System.err.println(e.getMessage());
		}
	}

	/**
	 * Constructor
	 */
	public MainGUI(String[] args) {
		if (args != null && args.length == 0)
			args = null;
		TimeSeriesMath.DUMB_PATCH = false;
		new SessionFrame(args);
	}

	/**
	 * prints message on usage if incorrect
	 */
	private void printUsageMsg() {
		System.out.println("Usage: program dssfile");
	}

	/**
	 * context for this gui
	 */
	private static SessionContext _sc = new SessionContext(new Session());

	/**
	 * gets the context ( contains data ) for this gui
	 */
	public static final SessionContext getContext() {
		return _sc;
	}

	/**
   *
   */
	public static void setDebugMode() {
		if (MainProperties.getProperty("debug").equals("true")) {
			Runtime.getRuntime().traceMethodCalls(true);
		}
	}

	/**
	 * returns the symbol table in use
	 */
	public static SymbolTable getSymbolTable() {
		return _st;
	}

	/**
	 * sets the symbol table to this one
	 */
	public static void setSymbolTable(SymbolTable st) {
		_st = st;
	}

	private static SymbolTable _st = new SymbolTable();
}

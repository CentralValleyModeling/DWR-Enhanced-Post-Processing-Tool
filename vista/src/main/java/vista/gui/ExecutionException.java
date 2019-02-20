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
package vista.gui;

/**
 * Exception super class for all exceptions thrown by all commands.
 * 
 * @author Nicky Sandhu
 * @version $Id: ExecutionException.java,v 1.1 2003/10/02 20:49:14 redwood Exp $
 */
public class ExecutionException extends Exception {
	/**
   *
   */
	public ExecutionException(String msg) {
		super(msg);
	}

	/**
	 * Displays error
	 */
	public ExecutionException(Command c) {
		this("Error executing command: " + c.getClass().getName());
	}

	/**
   *
   */
	public ExecutionException(Exception e, String msg) {
		this(msg);
		_ex = e;
	}

	/**
	 * writes out nested exception message and its own message
	 */
	public String getMessage() {
		String msg;
		if (_ex != null) {
			msg = "Nested Exception: " + _ex.toString();
			msg += super.getMessage();
		} else
			msg = super.getMessage();
		return msg;
	}

	/**
   *
   */
	public void printStackTrace(java.io.PrintWriter s) {
		if (_ex != null)
			_ex.printStackTrace(s);
		super.printStackTrace(s);
	}

	/**
   *
   */
	public void printStackTrace(java.io.PrintStream s) {
		if (_ex != null)
			_ex.printStackTrace(s);
		super.printStackTrace(s);
	}

	private Exception _ex;
}

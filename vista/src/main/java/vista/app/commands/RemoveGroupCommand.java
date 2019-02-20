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
package vista.app.commands;

import java.util.ArrayList;
import java.util.Iterator;

import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.Group;
import vista.set.Session;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: RemoveGroupCommand.java,v 1.1 2003/10/02 20:48:39 redwood Exp $
 */
public class RemoveGroupCommand implements Command {
	private Session _session;
	private int[] _gNumbers;
	private ArrayList<Object> _gRemoved;

	/**
	 * opens session and sets current session to
	 */
	public RemoveGroupCommand(Session s, int[] groupNumbers) {
		_session = s;
		_gNumbers = groupNumbers;
		_gRemoved = new ArrayList<Object>();
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_gNumbers == null || _gNumbers.length == 0)
			throw new IllegalArgumentException(
					"No references selected for deletion");
		// first get all the objects to be removed...
		for (int i = 0; i < _gNumbers.length; i++) {
			int ng = _gNumbers[i];
			_gRemoved.add(new Integer(ng));
			_gRemoved.add(_session.getGroup(ng));
		}
		// then remove them
		for (Iterator<Object> eg = _gRemoved.iterator(); eg.hasNext();) {
			eg.next();
			_session.removeGroup((Group) eg.next());
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		int n = _gRemoved.size();
		if (n == 0)
			return;
		for (Iterator<Object> eg = _gRemoved.iterator(); eg.hasNext();) {
			int gNumber = ((Integer) eg.next()).intValue();
			Group group = (Group) eg.next();
			_session.insertGroupAt(gNumber, group);
		}
	}

	/**
	 * checks if command is executable.
	 */
	public boolean isUnexecutable() {
		return true;
	}

	/**
	 * writes to script
	 */
	public void toScript(StringBuffer buf) {
	}
} // end of RemoveGroupCommand

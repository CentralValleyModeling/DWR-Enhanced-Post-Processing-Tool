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

import vista.db.dss.DSSUtil;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.Group;
import vista.set.PathPartPredicate;
import vista.set.Pathname;
import vista.set.TimeWindowFilter;
import vista.time.TimeWindow;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupFilterCommand.java,v 1.1 2003/10/02 20:48:31 redwood Exp $
 */
class GroupFilterCommand implements Command {
	private Group _group, _originalGroup;
	private String[] _regExp;
	private boolean _selecting;

	/**
	 * opens session and sets current session to
	 */
	public GroupFilterCommand(Group g, String[] regExp, boolean selecting) {
		_group = g;
		_regExp = regExp;
		_selecting = selecting;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_group == null)
			throw new ExecutionException("Group is null");
		else if (_regExp == null || _regExp.length != 6)
			throw new ExecutionException("No regular expression entered");
		else
			;
		//
		_originalGroup = Group.createGroup(_group);
		String modifierName = "";
		// filter by pathnames
		for (int i = 0; i < _regExp.length; i++) {
			if (_regExp[i].equals(""))
				continue;
			modifierName = "< " + Pathname.getPartName(i) + " = " + _regExp[i]
					+ ">";
			if (i == Pathname.D_PART) {
				TimeWindow window = DSSUtil.getTimeFactory().createTimeWindow(
						_regExp[i], "ddMMMyyyy");
				if (window != null)
					_group.filterBy(new TimeWindowFilter(window), _selecting);
			} else
				_group.filterBy(new PathPartPredicate(_regExp[i], i),
						_selecting);
		}
		// group name
		String groupName = _group.getName();
		_group.setName(groupName + "(modified:" + modifierName + ")");
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		if (_group == null || _originalGroup == null)
			return;
		int nrefs = _group.getNumberOfDataReferences();
		if (nrefs > 0)
			_group.removeDataReference(0, nrefs - 1);
		_originalGroup.copyInto(_group);
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
} // end of Open GroupCommand

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

import java.util.StringTokenizer;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import vista.db.dss.DSSUtil;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.DataReference;
import vista.set.Group;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeWindow;

/**
 * Encapsulates commands implementing group related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: CreateTWReferencesCommand.java,v 1.1 2003/10/02 20:48:25
 *          redwood Exp $
 */
public class CreateTWReferencesCommand implements Command {
	private Group _group;
	private int[] _rNumbers;
	private String _timeText;
	private DataReference[] _refs;

	/**
	 * opens group and sets current group to
	 */
	public CreateTWReferencesCommand(Group g, int[] referenceNumbers,
			String timeText) {
		_group = g;
		_rNumbers = referenceNumbers;
		_timeText = timeText;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_rNumbers == null || _rNumbers.length == 0)
			return;
		StringTokenizer tokenizer = new StringTokenizer(_timeText, "-");
		if (tokenizer.countTokens() != 2) {
			JOptionPane.showMessageDialog(null,
					"Incorrect time specification: " + _timeText);
			return;
		}
		TimeFactory tf = DSSUtil.getTimeFactory();
		TimeWindow tw = null;
		try {
			Time stTime = tf.getTimeInstance().create(
					tokenizer.nextToken().trim());
			Time endTime = tf.getTimeInstance().create(
					tokenizer.nextToken().trim());
			tw = tf.createTimeWindow(stTime, endTime);
		} catch (Exception e) {
			JOptionPane.showMessageDialog(new JFrame(), "Exception"
					+ e.getMessage() + " parsing time from: " + _timeText);
			return;
		}
		if (tw == null)
			return;
		_refs = new DataReference[_rNumbers.length];
		for (int i = 0; i < _rNumbers.length; i++) {
			DataReference ref = _group.getDataReference(_rNumbers[i]);
			tw = TimeFactory.createRoundedTimeWindow(tw, ref.getTimeInterval());
			ref = DataReference.create(ref, tw);
			_refs[i] = ref;
		}
		for (int i = _refs.length - 1; i >= 0; i--) {
			DataReference ref = _refs[i];
			if (ref != null)
				_group.insertDataReferenceAt(_rNumbers[0], ref);
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		if (_refs == null || _refs.length == 0)
			return;
		for (int i = 0; i < _refs.length; i++) {
			if (_refs[i] != null)
				_group.removeDataReference(_refs[i]);
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
} // end of CloneReferenceCommand

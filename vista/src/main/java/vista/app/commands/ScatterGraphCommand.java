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

import vista.app.DataGraphFrame;
import vista.app.DefaultGraphBuilder;
import vista.app.MultiScatterGraph;
import vista.graph.Graph;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.DataReference;
import vista.set.Group;
import vista.set.ProxyFactory;

/**
 * Encapsulates commands implementing group related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: ScatterGraphCommand.java,v 1.1 2003/10/02 20:48:40 redwood Exp
 *          $
 */
public class ScatterGraphCommand implements Command {
	private Group _group;
	private int[] _rNumbers;
	private String _filename;
	private DataReference _reference;

	/**
	 * opens group and sets current group to
	 */
	public ScatterGraphCommand(Group g, int[] referenceNumbers) {
		_group = g;
		_rNumbers = referenceNumbers;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_rNumbers == null || _rNumbers.length == 0)
			return;
		if (_rNumbers.length == 1)
			return;
		if (_rNumbers.length == 2) {
			DefaultGraphBuilder gb = new DefaultGraphBuilder();
			for (int i = 0; i < _rNumbers.length; i++) {
				for (int j = i + 1; j < _rNumbers.length; j++) {
					DataReference ref1 = _group.getDataReference(_rNumbers[i]);
					DataReference ref2 = _group.getDataReference(_rNumbers[j]);
					DataReference ref = ProxyFactory
							.createPairedTimeSeriesProxy(ref1, ref2);
					gb.addData(ref);
				}
			}
			Graph[] graphs = gb.createGraphs();
			if (graphs != null || graphs.length > 0) {
				for (int i = 0; i < graphs.length; i++) {
					new DataGraphFrame(graphs[i], "Graph").setVisible(true);
				}
			}
		} else {
			// check to get only regular time series
			DataReference[] refs = new DataReference[_rNumbers.length];
			for (int i = 0; i < refs.length; i++) {
				refs[i] = _group.getDataReference(_rNumbers[i]);
			}
			new MultiScatterGraph(refs);
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		throw new ExecutionException("Cannot undo graphing of data");
	}

	/**
	 * checks if command is executable.
	 */
	public boolean isUnexecutable() {
		return false;
	}

	/**
	 * writes to script
	 */
	public void toScript(StringBuffer buf) {
	}
} // end of GraphDataCommand

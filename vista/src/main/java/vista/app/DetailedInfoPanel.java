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

import java.awt.GridLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;

import net.miginfocom.swing.MigLayout;

import vista.db.dss.DSSData;
import vista.db.dss.DSSDataReader;
import vista.db.dss.DSSUtil;
import vista.set.DataReference;
import vista.set.DataSet;
import vista.set.SetUtils;
import vista.time.TimeWindow;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: StatsDisplayPanel.java,v 1.1 2003/10/02 20:48:42 redwood Exp $
 */
@SuppressWarnings("serial")
public class DetailedInfoPanel extends JPanel {
	public DetailedInfoPanel(DataReference ref) {
		if (ref == null)
			throw new IllegalArgumentException(
					"Null data set cannot have stats");
		setLayout(new GridLayout(2, 1));
		String filename = ref.getFilename();
		String pathname = ref.getPathname().getFullPath();
		TimeWindow tw = DSSUtil.createTimeWindow(ref.getPathname());
		int st = (int) tw.getStartTime().getTimeInMinutes();
		int et = (int) tw.getEndTime().getTimeInMinutes();
		DSSDataReader reader = new DSSDataReader();
		int recType = reader.recordType(filename, pathname);
		DSSData data =  reader.getData(filename, pathname, st, et,
				false);
		setLayout(new MigLayout("wrap 2"));
		add(new JLabel("Record Type")); add(new JLabel(""+recType));
		add(new JLabel("Data Offset")); add(new JLabel(""+data._offset));
		add(new JLabel("Number Read")); add(new JLabel(""+data._numberRead));
		add(new JLabel("X Type")); add(new JLabel(""+data._xType));
		add(new JLabel("Y Type")); add(new JLabel(""+data._yType));
		add(new JLabel("X Units")); add(new JLabel(""+data._xUnits));
		add(new JLabel("Y Units")); add(new JLabel(""+data._yUnits));
	}
}

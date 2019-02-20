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
package vista.graph;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: PieChart.java,v 1.1 2003/10/02 20:49:05 redwood Exp $
 */
public class PieChart extends GEContainer {
	private GEContainer _pc;
	private PieChartModel _pcm;

	public PieChart(PieChartModel pcm) {
		this(new GEAttr(), pcm);
	}

	public PieChart(GEAttr attr, PieChartModel pcm) {
		super(attr);
		_pcm = pcm;
		setLayout(new GEBorderLayout());
		add(GEBorderLayout.NORTH, new TextLine(new TextLineAttr(), pcm
				.getTitle()));
		add(GEBorderLayout.CENTER, _pc = new GEContainer(new GEAttr()));
	}

	/**
   *
   */
	public void Draw() {
		Graphics g = getGraphics();
		Rectangle r = _pc.getBounds();
		_pcm.reset();
		double max = _pcm.getSumOfValues();
		int startAngle = 0;
		double sum = 0;
		int index = 0;
		int cl = GraphUtils._colorTable.length;
		while (_pcm.hasMorePies()) {
			PieModel pm = _pcm.nextPie();
			String label = pm.getLabel();
			double val = pm.getValue();
			int w = Math.min(r.width, r.height);
			int arcAngle = (int) Math.round(val / max * 360.0);
			g.setColor(GraphUtils._colorTable[index % cl]);
			g.fillArc(r.x, r.y, w, w, startAngle, arcAngle);
			startAngle += arcAngle;
			sum += val;
			index++;
		}
	}

	/**
	 * draws with update pie chart model if any
	 */
	public void animateNext() {
		Draw();
	}

	/**
	 * calculates the preferred size of this element
	 * 
	 * @return the preferred size
	 */
	public Dimension getPreferredSize() {
		return new Dimension(125, 125);
	}

	/**
	 * calculates the minimum size of this element
	 * 
	 * @return the minimum size
	 */
	public Dimension getMinimumSize() {
		return new Dimension(125, 125);
	}
}

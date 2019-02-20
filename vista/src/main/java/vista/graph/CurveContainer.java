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
import java.awt.Rectangle;

/**
 * A container for curves independent of plot
 * 
 * @author Nicky Sandhu
 * @version $Id: CurveContainer.java,v 1.3 1998/10/08 00:03:29 nsandhu Exp $
 */
public class CurveContainer extends GEContainer implements ScaledElement {
	private static int _curveNumber = 0;
	private Axis topAxis, leftAxis, bottomAxis, rightAxis;

	/**
   *
   */
	public CurveContainer() {
		this(new GEAttr());
	}

	/**
   *
   */
	public CurveContainer(GEAttr attr) {
		super(attr);
		Scale xs = new Scale(0.0, 1.0, 0, 10);
		Scale ys = new Scale(0.0, 1.0, 0, 10);
		setLayout(new GEScaledLayout(xs, ys));
	}

	/**
   *
   */
	public void addCurve(Curve c) {
		CurveDataModel cdm = c.getModel();
		int xapos = cdm.getXAxisPosition();
		int yapos = cdm.getYAxisPosition();
		Axis xAxis = createAxis(xapos);
		Axis yAxis = createAxis(yapos);
		if (xAxis.getOrientation() == yAxis.getOrientation()) {
			throw new IllegalArgumentException(
					"x and y axis orientation is the same");
		}
		// set axis
		c.setXAxis(xAxis);
		c.setYAxis(yAxis);
		// set name
		c.setLocalName("Curve#" + _curveNumber++);
		add(new DoubleRect(0.0, 1.0, 1.0, 1.0), c);
	}

	/**
   *
   */
	public void removeCurve(Curve c) {
		remove(c);
		Axis xa = c.getXAxis();
		xa.detachCurve(c);
		Axis ya = c.getYAxis();
		ya.detachCurve(c);
		if (xa.getNumberAttached() == 0)
			removeAxis(xa);
		if (ya.getNumberAttached() == 0)
			removeAxis(ya);
	}

	/**
   *
   */
	void removeAxis(Axis a) {
		remove(a);
		getLayout().removeLayoutElement(a);
	}

	/**
	 * returns the Axis object for given position. If Axis object is null it
	 * returns null.
	 */
	public Axis getAxis(int axisPosition) {
		switch (axisPosition) {
		case AxisAttr.TOP:
			return topAxis;
		case AxisAttr.BOTTOM:
			return bottomAxis;
		case AxisAttr.LEFT:
			return leftAxis;
		case AxisAttr.RIGHT:
			return rightAxis;
		}
		return null;
	}

	/**
   *
   */
	public Axis createAxis(int pos) {
		Rectangle r = getBounds();
		Axis a = null;
		switch (pos) {
		case AxisAttr.TOP:
			if (topAxis == null) {
				topAxis = new Axis(new AxisAttr(), AxisAttr.TOP);
				Dimension d = topAxis.getPreferredSize();
				topAxis.setBounds(new Rectangle(r.x, r.y - d.height, r.width,
						d.height));
				add(new DoubleRect(0.0, 1.0, 1.0, -0.15), topAxis);
			}
			a = topAxis;
		case AxisAttr.BOTTOM:
			if (bottomAxis == null) {
				bottomAxis = new Axis(new AxisAttr(), AxisAttr.BOTTOM);
				Dimension d = bottomAxis.getPreferredSize();
				bottomAxis.setBounds(new Rectangle(r.x, r.y + r.height,
						r.width, d.height));
				add(new DoubleRect(0.0, 0.0, 1.0, 0.15), bottomAxis);
			}
			a = bottomAxis;
		case AxisAttr.LEFT:
			if (leftAxis == null) {
				leftAxis = new Axis(new AxisAttr(), AxisAttr.LEFT);
				Dimension d = leftAxis.getPreferredSize();
				leftAxis.setBounds(new Rectangle(r.x - d.width, r.y, d.width,
						r.height));
				add(new DoubleRect(0.0, 1.0, -0.15, 1.0), leftAxis);
			}
			a = leftAxis;
		case AxisAttr.RIGHT:
			if (rightAxis == null) {
				rightAxis = new Axis(new AxisAttr(), AxisAttr.RIGHT);
				Dimension d = rightAxis.getPreferredSize();
				rightAxis.setBounds(new Rectangle(r.x + r.width, r.y, d.width,
						r.height));
				add(new DoubleRect(1.0, 1.0, 0.15, 1.0), rightAxis);
			}
			a = rightAxis;
		}
		if (a != null)
			a.setTickGenerator(new SimpleTickGenerator());
		return a;
	}
}

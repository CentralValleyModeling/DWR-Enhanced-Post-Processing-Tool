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
import java.util.Properties;

/**
 * A superclass of all curves. A curve is a representation of a collection of
 * (x,y) points. The first co-ordinate is plotted along the x axis and the
 * second along the y axis.
 * 
 * A curve data model contains all the information for displaying this view.
 * <p>
 * To each curve is attached an x and a y axis.
 * 
 * @see CurveDataModel
 * @author Nicky Sandhu
 * @version $Id: Curve.java,v 1.1 2003/10/02 20:48:51 redwood Exp $
 */
public abstract class Curve extends GraphicElement {
	private CurveDataModel _cdm;

	/**
	 * creates a curve with default attributes and the given curve data model
	 */
	public Curve(CurveDataModel cdm) {
		this(new CurveAttr(), cdm);
	}

	/**
	 * creates a curve with given attributes and data model
	 */
	public Curve(CurveAttr attributes, CurveDataModel cdm) {
		super(attributes);
		getAttributes()._clipWithinBounds = true;
		_cdm = cdm;
		setLocalName("");
	}

	/**
	 * sets local name to be used for attribute property referenencing
	 */
	void setLocalName(String str) {
		_localName = str;
	}

	/**
	 * sets local name to be used for attribute property referenencing
	 */
	public String getLocalName() {
		return _localName;
	}

	/**
	 * gets the curve data model
	 */
	public CurveDataModel getModel() {
		return _cdm;
	}

	/**
	 * set the new model and informs axes of change in data model so that they
	 * may resize and relabel themseleves
	 */
	public void setModel(CurveDataModel cdm) {
		_xAxis.getMediator().detach(_cdm);
		_xAxis.getMediator().attach(cdm);
		_yAxis.getMediator().detach(_cdm);
		_yAxis.getMediator().attach(cdm);
		_cdm = cdm;
		_xAxis.regenerateTicks();
		_yAxis.regenerateTicks();
	}

	/**
	 * set x axis used for scaling the x data value
	 */
	public void setXAxis(Axis xAxis) {
		if (_xAxis != null) { // remove curve from old axis
			_xAxis.detachCurve(this);
		}
		_xAxis = xAxis;
		_xAxis.attachCurve(this);
	}

	/**
	 * set y axis for scaling the y data value.
	 */
	public void setYAxis(Axis yAxis) {
		if (_yAxis != null) { // remove curve from old axis
			_yAxis.detachCurve(this);
		}
		_yAxis = yAxis;
		_yAxis.attachCurve(this);
	}

	/**
	 * get x axis used for scaling the x data value
	 */
	public Axis getXAxis() {
		return _xAxis;
	}

	/**
	 * get y axis for scaling the y data value.
	 */
	public Axis getYAxis() {
		return _yAxis;
	}

	/**
	 * sets Curve's background color to transparent
	 */
	public void preDraw() {
		getAttributes()._backgroundColor = null;
		super.preDraw();
	}

	/**
	 * implements the draw of GraphicElement
	 * 
	 * @see GraphicElement#Draw()
	 */
	public void Draw() {
		drawCurve();
	}

	/**
	 * Draws the data by scaling it and joining consecutive data points and/or
	 * plotting symbols for data points.
	 */
	protected abstract void drawCurve();

	/**
	 * gets preferred size
	 */
	public Dimension getPreferredSize() {
		return new Dimension(100, 100);
	}

	/**
	 * gets minimum size
	 */
	public Dimension getMinimumSize() {
		return new Dimension(10, 10);
	}

	/**
   *
   */
	public void setSymbol(Symbol s) {
		((CurveAttr) getAttributes())._symbol = s;
	}

	/**
   *
   */
	public Symbol getSymbol() {
		return ((CurveAttr) getAttributes())._symbol;
	}

	/**
   *
   */
	public String getPrefixTag(String prefixTag) {
		return prefixTag + _localName + ".";
	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 * 
	 * @param prefixTag
	 *            A tag to assign the context for these properties e.g. if these
	 *            properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag) {

		if (p == null)
			return;
		CurveAttr attr = (CurveAttr) getAttributes();
		prefixTag = getPrefixTag(prefixTag);
		p.put(prefixTag + "drawSymbol", new Boolean(attr._drawSymbol)
				.toString());
		p.put(prefixTag + "drawLines", new Boolean(attr._drawLines).toString());
		p.put(prefixTag + "thickness", new Float(attr._thickness).toString());
		p.put(prefixTag + "dataPerSymbol", new Integer(attr._dataPerSymbol)
				.toString());
		getSymbol().toProperties(p, prefixTag);
		super.toProperties(p, prefixTag);
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag) {
		String localTag = getPrefixTag(prefixTag);
		CurveAttr attr = (CurveAttr) getAttributes();
		String property = p.getProperty(localTag + "drawSymbol");
		if (property != null)
			attr._drawSymbol = new Boolean(property).booleanValue();
		property = p.getProperty(localTag + "drawLines");
		if (property != null)
			attr._drawLines = new Boolean(property).booleanValue();
		property = p.getProperty(localTag + "thickness");
		if (property != null)
			attr._thickness = new Float(property).floatValue();
		property = p.getProperty(localTag + "dataPerSymbol");
		if (property != null)
			attr._dataPerSymbol = new Integer(property).intValue();
		getSymbol().fromProperties(p, localTag);
		super.fromProperties(p, localTag);
	}

	/**
	 * sets DrawSymbol
	 */
	public void setDrawSymbol(boolean drawSymbol) {
		((CurveAttr) getAttributes())._drawSymbol = drawSymbol;
	}

	/**
	 * gets DrawSymbol
	 */
	public boolean getDrawSymbol() {
		return ((CurveAttr) getAttributes())._drawSymbol;
	}

	/**
	 * sets DrawLines
	 */
	public void setDrawLines(boolean drawLines) {
		((CurveAttr) getAttributes())._drawLines = drawLines;
	}

	/**
	 * gets DrawLines
	 */
	public boolean getDrawLines() {
		return ((CurveAttr) getAttributes())._drawLines;
	}

	/**
	 * create dialog panel for this element.
	 */
	public GEDialogPanel createDialogPanel() {
		return new CurveDialogPanel(this);
	}

	/**
   *
   */
	public int getDataPerSymbol() {
		return ((CurveAttr) getAttributes())._dataPerSymbol;
	}

	/**
   *
   */
	public void setDataPerSymbol(int d) {
		if (d < 1)
			return;
		((CurveAttr) getAttributes())._dataPerSymbol = d;
	}

	/**
   *
   */
	public float getLineThickness() {
		return ((CurveAttr) getAttributes())._thickness;
	}

	/**
   *
   */
	public void setLineThickness(float d) {
		if (d < 0.1f)
			return;
		((CurveAttr) getAttributes())._thickness = d;
	}

	/**
   *
   */
	public float[] getDashArray() {
		return ((CurveAttr) getAttributes())._dashArray;
	}

	/**
   *
   */
	public void setDashArray(float[] dashArray) {
		if (dashArray == null)
			return;
		((CurveAttr) getAttributes())._dashArray = dashArray;
	}

	/**
	 * returns the curve legend line for this curve
	 */
	public CurveLegendLine getLegendLine() {
		return new CurveLegendLine(this);
	}

	/**
	 * Axis for scaling x values
	 */
	private Axis _xAxis;
	/**
	 * Axis for scaling y values
	 */
	private Axis _yAxis;
	/**
	 * symbol to be plotted
	 */
	private Symbol _symbol;
	/**
	 * name to be used while serializing attributes
	 */
	private String _localName;
}

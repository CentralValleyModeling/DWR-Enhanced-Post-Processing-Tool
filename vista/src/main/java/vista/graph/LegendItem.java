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

import java.awt.Color;
import java.awt.Insets;
import java.util.Properties;

/**
 * A legend item for each Curve on the Plot consisting of a LineElement and
 * associated TextLine. The GELineLayout is used for managing layout
 * 
 * @author Nicky Sandhu
 * @version $Id: LegendItem.java,v 1.1 2003/10/02 20:49:04 redwood Exp $
 */
public class LegendItem extends GEContainer implements ScaledElement {
	/**
	 * constructor
	 */
	public LegendItem(Curve curve) {
		this(new LegendItemAttr(), curve);
	}

	/**
	 * constructor
	 */
	public LegendItem(LegendItemAttr attributes, Curve curve) {

		super(attributes);
		GEBorderLayout layout = new GEBorderLayout();
		layout.setScaleComponents(true);
		setLayout(layout);
		//
		setCurve(curve);
		//
		if (curve != null)
			setLegendText(curve.getModel().getLegendText());
		else
			setLegendText("");
	}

	/**
   *
   */
	public void setLegendText(String txt) {
		if (txt == null)
			return;
		if (_textLine == null) {
			TextLineAttr attr = (TextLineAttr) getAttributes();
			attr._justification = TextLineAttr.LEFT;
			attr._foregroundColor = Color.black;
			_textLine = new TextLine(attr, txt);
			_textLine.setInsets(new Insets(2, 10, 2, 10));
			add(GEBorderLayout.CENTER, _textLine);
		} else {
			_textLine.setText(txt);
		}
	}

	/**
   *
   */
	public String getLegendText() {
		return _textLine.getText();
	}

	/**
   *
   */
	public Curve getCurve() {
		return _curve;
	}

	/**
   *
   */
	public void setCurve(Curve curve) {
		if (curve == null)
			return;
		if (_cll != null)
			remove(_cll);
		_curve = curve;
		add(GEBorderLayout.WEST, _cll = _curve.getLegendLine());
	}

	/**
   *
   */
	public void setLegendName(String name) {
		_textLine.setText(name);
	}

	/**
   *
   */
	public String getLegendName() {
		return _textLine.getText();
	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 * 
	 * @param prefixTag
	 *            A tag to assign the context for these properties e.g. if these
	 *            properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag) {
		super.toProperties(p, getPrefixTag(prefixTag));
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag) {
		super.fromProperties(p, getPrefixTag(prefixTag));
	}

	/**
   *
   */
	public String getPrefixTag(String prefixTag) {
		return prefixTag + getCurve().getLocalName() + "LegendItem.";
	}

	/**
	 * The text associated with the line element
	 */
	private TextLine _textLine;
	/**
   *
   */
	private Curve _curve;
	/**
   *
   */
	private CurveLegendLine _cll;
}

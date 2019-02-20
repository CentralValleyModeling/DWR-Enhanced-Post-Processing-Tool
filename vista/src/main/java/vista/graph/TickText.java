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

import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.Enumeration;
import java.util.Properties;

/**
 * A class handling TextLine objects which share common data values. The
 * GELineLayoutManager is used for managing layout. Also these labels are given
 * equal space along the linear length such that they match up with the TickText
 * major tick marks.
 * <p>
 * 
 * Attributes for all TextLine objects such as font, background color,
 * justifcation are in the attribute object. This allows the TextLine objects to
 * share such data in an external object. (Flyweight sharing pattern)
 * 
 * @see TextLineAttr
 * @author Nicky Sandhu
 * @version $Id: TickText.java,v 1.1 2003/10/02 20:49:11 redwood Exp $
 */
public class TickText extends GEContainer implements FontResizable {
	/**
	 * creates a container for text sharing common attributes with a row layout
	 * manager.
	 * 
	 * @see GERowLayoutManager
	 */
	public TickText(String[] labels, TickData tickData) {
		this(new TickTextAttr(), labels, tickData);
	}

	/**
	 * creates a container for text sharing common attributes with a row layout
	 * manager.
	 * 
	 * @see GERowLayoutManager
	 */
	public TickText(TickTextAttr attributes, String[] labels, TickData tickData) {

		super(attributes);
		setLayout(new GETickLayout(GETickLayout.HORIZONTAL,
				GETickLayout.CENTERED_ON_BOUNDS, tickData));

		if (labels != null)
			setLabels(labels);
	}

	/**
	 * sets the tick labels
	 */
	public void setLabels(String[] labels) {
		removeAll();

		for (int i = 0; i < labels.length; i++) {
			TextLine tl = new TextLine((TextLineAttr) getAttributes(),
					labels[i]);
			this.add(tl);
		}

	}

	/**
	 * calls on it super class to draw all its components. Also has the ability
	 * of marking the major tick marks between labels.
	 */
	public void Draw() {
		super.Draw();
		TickTextAttr attr = (TickTextAttr) getAttributes();
		Graphics gc = getGraphics();
		if (attr._boundariesMarked) {
			GELayoutManager layout = getLayout();
			if (layout instanceof GETickLayout) {
				for (Enumeration iterator = getDrawIterator(); iterator
						.hasMoreElements();) {
					GraphicElement ge = (GraphicElement) iterator.nextElement();
					Rectangle r = ge.getInsetedBounds();
					if (((GETickLayout) layout).getOrientation() == GETickLayout.HORIZONTAL)
						gc.drawLine(r.x, r.y, r.x, r.y + r.height);
					else {
						gc.drawLine(r.x, r.y, r.x + r.width, r.y);
					}
				} // iterator loop
			} // if tick layout
		}
	}

	/**
    *
    */
	public void setFontSize(int size) {
		TickTextAttr attr = (TickTextAttr) getAttributes();
		Font font = attr._font;
		attr._font = new Font(font.getName(), font.getStyle(), size);
		attr._resizeOnTheFly = false;
	}

	/**
	 * sets font by ratio.
	 */
	public void setFontByRatio(double fontResizeRatio) {
		TickTextAttr attr = (TickTextAttr) getAttributes();
		if (attr._resizeOnTheFly == false)
			return;
		int newFontSize = (int) (attr._originalFontSize * fontResizeRatio);
		if (newFontSize < 0)
			newFontSize = 0;
		if (newFontSize > TextLineAttr._fontTable.length - 1)
			newFontSize = TextLineAttr._fontTable.length - 1;
		attr._font = TextLineAttr._fontTable[newFontSize];

	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 * 
	 * @param prefixTag
	 *            A tag to assign the context for these properties e.g. if these
	 *            properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag) {
		String localTag = getPrefixTag(prefixTag);
		if (p == null)
			return;
		TickTextAttr attr = (TickTextAttr) getAttributes();

		p.put(localTag + "centeredOnBounds",
				new Boolean(attr._centeredOnBounds).toString());
		p.put(localTag + "boundariesMarked",
				new Boolean(attr._boundariesMarked).toString());

		super.toProperties(p, localTag);
	}

	/**
   *
   */
	public String getPrefixTag(String prefixTag) {
		return prefixTag + getName() + "TickText.";
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag) {
		super.fromProperties(p, getPrefixTag(prefixTag));
	}

}

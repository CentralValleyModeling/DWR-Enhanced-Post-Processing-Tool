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
package vista.gui;

import javax.swing.UIDefaults;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;

/**
 * This class describes the default Metal Theme.
 * 
 * @version 1.6 02/04/98
 * @author Steve Wilson
 */

public class VistaMetalTheme extends DefaultMetalTheme {

	public String getName() {
		return "Vista";
	}

	private final ColorUIResource primary1 = new ColorUIResource(0, 0, 0);
	private final ColorUIResource primary2 = new ColorUIResource(188, 188, 204);
	private final ColorUIResource primary3 = new ColorUIResource(220, 220, 255);
	private final ColorUIResource primaryHighlight = new ColorUIResource(102,
			102, 150);

	private final ColorUIResource secondary2 = new ColorUIResource(200, 200,
			230);
	private final ColorUIResource secondary3 = new ColorUIResource(200, 210,
			255);
	private final ColorUIResource controlHighlight = new ColorUIResource(102,
			102, 150);

	protected ColorUIResource getPrimary1() {
		return primary1;
	}

	protected ColorUIResource getPrimary2() {
		return primary2;
	}

	protected ColorUIResource getPrimary3() {
		return primary3;
	}

	public ColorUIResource getPrimaryControlHighlight() {
		return primaryHighlight;
	}

	protected ColorUIResource getSecondary2() {
		return secondary2;
	}

	protected ColorUIResource getSecondary3() {
		return secondary3;
	}

	public ColorUIResource getControlHighlight() {
		return super.getSecondary3();
	}

	public ColorUIResource getFocusColor() {
		return getBlack();
	}

	private ColorUIResource lightBlue = new ColorUIResource(230, 230, 250);

	public ColorUIResource getTextHighlightColor() {
		return getBlack();
	}

	public ColorUIResource getHighlightedTextColor() {
		return lightBlue;
	}

	public ColorUIResource getMenuSelectedBackground() {
		return getBlack();
	}

	public ColorUIResource getMenuSelectedForeground() {
		return lightBlue;
	}

	public ColorUIResource getAcceleratorForeground() {
		return getBlack();
	}

	public ColorUIResource getAcceleratorSelectedForeground() {
		return getWhite();
	}

	public ColorUIResource getWindowBackground() {
		return lightBlue;
	}

	public ColorUIResource getWindowTitleBackground() {
		return getPrimary3();
	}

	public ColorUIResource getWindowTitleForeground() {
		return getBlack();
	}

	public ColorUIResource getWindowTitleInactiveBackground() {
		return getSecondary3();
	}

	public ColorUIResource getWindowTitleInactiveForeground() {
		return getBlack();
	}

	public void addCustomEntriesToTable(UIDefaults table) {

		Border blackLineBorder = new BorderUIResource(
				new LineBorder(getBlack()));
		Border whiteLineBorder = new BorderUIResource(
				new LineBorder(getWhite()));

		table.put("ToolTip.border", blackLineBorder);
		table.put("TitledBorder.border", blackLineBorder);
		table.put("Table.focusCellHighlightBorder", whiteLineBorder);
		table.put("Table.focusCellForeground", getWhite());

	}
}

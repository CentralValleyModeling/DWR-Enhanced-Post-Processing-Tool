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
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Toolkit;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPanel;

/**
 * Displays fonts available in the system and enables selection of font family,
 * style and size.
 * 
 * @author Nicky Sandhu
 * @version $Id: FontChoice.java,v 1.1 2003/10/02 20:48:55 redwood Exp $
 */
public class FontChoice extends JPanel {
	/**
   *
   */
	public FontChoice(Font font) {
		String[] fontList = Toolkit.getDefaultToolkit().getFontList();
		fontBox = new JComboBox();
		String fontFamily = font.getFamily();
		for (int i = 0; i < fontList.length; i++) {
			fontBox.addItem(fontList[i]);
			if (fontFamily.equals(fontList[i]))
				fontBox.setSelectedIndex(i);
		}
		// ! java bug fix
		if (fontFamily.equals("Times Roman"))
			fontBox.setSelectedItem("TimesRoman");
		//
		plainCheck = new JCheckBox("Plain?", font.isPlain());
		boldCheck = new JCheckBox("Bold?", font.isBold());
		italicCheck = new JCheckBox("Italic?", font.isItalic());
		//
		sizeBox = new JComboBox();
		int fontSize = font.getSize();
		int minFontSize = 3, maxFontSize = 60;
		for (int i = minFontSize; i < maxFontSize; i++) {
			sizeBox.addItem(new Integer(i).toString());
		}
		sizeBox.setSelectedIndex(font.getSize() - minFontSize);
		Dimension pSize = sizeBox.getPreferredSize();
		pSize.width = pSize.width * 2;
		sizeBox.setPreferredSize(pSize);
		// 
		JPanel stylePanel = new JPanel();
		stylePanel.setLayout(new GridLayout(3, 1));
		stylePanel.add(plainCheck);
		stylePanel.add(boldCheck);
		stylePanel.add(italicCheck);
		stylePanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Styles"));
		//
		JPanel mPanel = this;
		mPanel.setLayout(new FlowLayout());
		mPanel.add(fontBox);
		mPanel.add(stylePanel);
		mPanel.add(sizeBox);
		mPanel.setBorder(BorderFactory.createEtchedBorder());
	}

	/**
	 * returns a font for the characteristics.
	 */
	public Font getSelectedFont() {
		int plain = plainCheck.isSelected() ? Font.PLAIN : 0;
		int bold = boldCheck.isSelected() ? Font.BOLD : 0;
		int italic = italicCheck.isSelected() ? Font.ITALIC : 0;
		int style = plain | bold | italic;
		int size = new Integer((String) sizeBox.getSelectedItem()).intValue();
		return new Font((String) fontBox.getSelectedItem(), style, size);
	}

	/**
   *
   */
	private JCheckBox plainCheck, boldCheck, italicCheck;
	private JComboBox sizeBox, fontBox;
}

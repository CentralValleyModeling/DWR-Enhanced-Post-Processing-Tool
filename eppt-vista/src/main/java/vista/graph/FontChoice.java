/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */
package vista.graph;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Toolkit;
import javax.swing.*;

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

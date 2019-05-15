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

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import javax.swing.*;

/**
 * An editor for the attributes and state of the TextLine object
 *
 * @author Nicky Sandhu
 * @version $Id: TextDialogPanel.java,v 1.1 2003/10/02 20:49:09 redwood Exp $
 * @see TextLine
 */
public class TextDialogPanel extends GEDialogPanel
{
	/**
	 *
	 */
	protected final String BASIC = "Basic";
	/**
	 *
	 */
	protected final String TEXT = "Text";
	/**
	 *
	 */
	protected final String CENTER = "Center";
	/**
	 *
	 */
	protected final String LEFT = "Left";
	/**
	 *
	 */
	protected final String RIGHT = "Right";
	/**
	 *
	 */
	protected final String SIDE_BY_SIDE = "Side by Side";
	/**
	 *
	 */
	protected final String TOP_ON_TOP = "Top on Top";
	/**
	 *
	 */
	private JComboBox justificationChoice, arrangementChoice;
	private FontChoice fontPanel;
	private JTextField textField;
	/**
	 * constructor
	 */
	public TextDialogPanel(TextLine textLine)
	{
		super(textLine);
	}

	/**
	 * creates panels
	 */
	protected JPanel createPanel()
	{
		JPanel basicPanel = super.createPanel();
		// text editing
		TextLine tL = (TextLine) getGraphicElement();
		textField = new JTextField(tL.getText(), 15);
		textField.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), Labels.TEXT_STRING));
		// FontChooser required... wait for swing set!
		fontPanel = new FontChoice(tL.getFont());
		fontPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), Labels.FONT));
		TextLineAttr attr = (TextLineAttr) tL.getAttributes();
		//
		justificationChoice = new JComboBox();
		justificationChoice.addItem(CENTER);
		justificationChoice.addItem(LEFT);
		justificationChoice.addItem(RIGHT);
		JPanel justPanel = new JPanel();
		justPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
		justPanel.add(new JLabel(Labels.JUSTIFICATION));
		justPanel.add(justificationChoice);
		switch(attr._justification)
		{
			case TextLineAttr.CENTER:
				justificationChoice.setSelectedItem(CENTER);
				break;
			case TextLineAttr.LEFT:
				justificationChoice.setSelectedItem(LEFT);
				break;
			case TextLineAttr.RIGHT:
				justificationChoice.setSelectedItem(RIGHT);
				break;
			default:
				justificationChoice.setSelectedItem(LEFT);
		}
		//
		arrangementChoice = new JComboBox();
		arrangementChoice.addItem(SIDE_BY_SIDE);
		arrangementChoice.addItem(TOP_ON_TOP);
		JPanel arrangementPanel = new JPanel();
		arrangementPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
		arrangementPanel.add(new JLabel(Labels.ARRANGEMENT));
		arrangementPanel.add(arrangementChoice);
		switch(attr._textArrangement)
		{
			case TextLineAttr.SIDE_BY_SIDE:
				arrangementChoice.setSelectedItem(SIDE_BY_SIDE);
				break;
			case TextLineAttr.TOP_ON_TOP:
				arrangementChoice.setSelectedItem(TOP_ON_TOP);
				break;
			default:
				arrangementChoice.setSelectedItem(SIDE_BY_SIDE);
		}
		//
		JPanel taPanel = new JPanel();
		taPanel.setLayout(new GridLayout(2, 1));
		taPanel.add(justPanel);
		taPanel.add(arrangementPanel);
		taPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory
				.createEtchedBorder(), "Text Attributes"));
		//
		JPanel textPanel = new JPanel();
		textPanel.setLayout(new GridLayout(5, 1));
		textPanel.add(textField);
		textPanel.add(fontPanel);
		textPanel.add(taPanel);
		textPanel.setLayout(new GridLayout(textPanel.getComponentCount(), 1));
		//
		JTabbedPane interiorPane = new JTabbedPane();
		interiorPane.addTab(BASIC, null, basicPanel);
		interiorPane.addTab(TEXT, null, textPanel);
		interiorPane.setSelectedIndex(0);

		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		mainPanel.add("Center", interiorPane);

		return mainPanel;
	}

	/**
	 * apply changes for both the basic graphic element and its specialization
	 */
	public void applyChanges()
	{
		super.applyChanges();
		TextLine tL = (TextLine) getGraphicElement();
		TextLineAttr attr = (TextLineAttr) tL.getAttributes();
		tL.setText(textField.getText());
		attr._font = fontPanel.getSelectedFont();
		attr._originalFontSize = attr._font.getSize();
		if(justificationChoice.getSelectedItem().equals(CENTER))
		{
			attr._justification = TextLineAttr.CENTER;
		}
		else if(justificationChoice.getSelectedItem().equals(LEFT))
		{
			attr._justification = TextLineAttr.LEFT;
		}
		else
		{
			attr._justification = TextLineAttr.RIGHT;
		}
		if(arrangementChoice.getSelectedItem().equals(SIDE_BY_SIDE))
		{
			attr._textArrangement = TextLineAttr.SIDE_BY_SIDE;
		}
		else
		{
			attr._textArrangement = TextLineAttr.TOP_ON_TOP;
		}
	}

}

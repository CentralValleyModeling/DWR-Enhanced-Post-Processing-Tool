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

package gov.ca.water.quickresults.ui.projectconfig;

import java.awt.Color;
import java.awt.Component;
import javax.swing.*;
import javax.swing.tree.DefaultTreeCellRenderer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-10-2019
 */
public class ScenarioRunCellRenderer extends DefaultTreeCellRenderer
{
	@Override
	public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded,
												  boolean leaf, int row, boolean hasFocus)
	{
		Component treeCellRendererComponent = super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf,
				row, hasFocus);
		if(value instanceof ScenarioRunNode)
		{
			JRadioButton radioButton = buildRadioButton((ScenarioRunNode) value, selected);
			treeCellRendererComponent = radioButton;
		}
		else if(value instanceof DssPathNode)
		{
			setToolTipText(((DssPathNode) value).getDssPath().toString());
		}
		return treeCellRendererComponent;
	}

	private JRadioButton buildRadioButton(ScenarioRunNode value, boolean selected)
	{
		JRadioButton radioButton = new JRadioButton(getText());
		radioButton.addActionListener(e -> value.setBase(radioButton.isSelected()));
		radioButton.setSelected(value.isBase());
		Color bColor;
		if(selected)
		{
			bColor = getBackgroundSelectionColor();
		}
		else
		{
			bColor = getBackgroundNonSelectionColor();
			if(bColor == null)
			{
				bColor = getBackground();
			}
		}
		radioButton.setBackground(bColor);
		radioButton.setForeground(getForeground());
		radioButton.setToolTipText(buildRadioTooltip(value));
		return radioButton;
	}

	private String buildRadioTooltip(ScenarioRunNode value)
	{
		String retval = "<html>Name: " + value.getScenarioRun().getName() + "<br/>";
		retval += "Description : " + value.getScenarioRun().getDescription() + "<br/>";
		retval += "Model: " + value.getScenarioRun().getModel() + "<br/>";
		retval += "Output Directory: " + value.getScenarioRun().getOutputPath() + "<br/>";
		retval += "WRESL Main: " + value.getScenarioRun().getWreslMain() + "</html";
		return retval;
	}

	@Override
	public String getToolTipText()
	{
		return super.getToolTipText();
	}
}

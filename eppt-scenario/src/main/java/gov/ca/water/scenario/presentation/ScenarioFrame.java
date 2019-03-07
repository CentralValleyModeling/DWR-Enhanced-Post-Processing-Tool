/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.scenario.presentation;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;

/**
 * This frame is used for the "View Scenario Settings" button in the
 * "Run Settings" tab.
 *
 * @author Mohan
 */
public class ScenarioFrame extends JFrame implements ItemListener
{
	private static final Logger LOG = Logger.getLogger(ScenarioFrame.class.getName());
	private JPanel basePanel = new JPanel();
	private JPanel comparisonPanel = new JPanel();
	private JPanel differencePanel = new JPanel();
	private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();

	public ScenarioFrame(List<DataTableModel> dtmList)
	{
		try
		{
			JRadioButton baseBut = new JRadioButton("Base");
			JRadioButton comparisonBut = new JRadioButton("Comparison");
			JRadioButton differenceBut = new JRadioButton("Difference");

			ButtonGroup group = new ButtonGroup();
			group.add(baseBut);
			group.add(comparisonBut);
			group.add(differenceBut);

			Box box = new Box(BoxLayout.LINE_AXIS);
			box.add(baseBut);
			box.add(comparisonBut);
			box.add(differenceBut);

			Box vbox = new Box(BoxLayout.PAGE_AXIS);
			vbox.add(box);

			baseBut.addItemListener(this);
			comparisonBut.addItemListener(this);
			differenceBut.addItemListener(this);

			baseBut.setEnabled(true);
			comparisonBut.setEnabled(false);
			differenceBut.setEnabled(false);
			baseBut.setSelected(true);

			basePanel = buildJpanel(dtmList.get(0)); // Base table.
			if(dtmList.size() > 2)
			{
				comparisonPanel = buildJpanel(dtmList.get(1)); // Comparison table.
				differencePanel = buildJpanel(dtmList.get(2)); // Difference table.
				comparisonBut.setEnabled(true);
				differenceBut.setEnabled(true);
				comparisonBut.setSelected(true);
			}

			setLayout(new GridBagLayout());
			GridBagConstraints c = new GridBagConstraints();
			c.gridx = 0;
			c.gridy = 0;

			c.insets = new Insets(10, 10, 10, 10);
			c.fill = GridBagConstraints.BOTH;

			add(vbox, c);
			c.gridx = 0;
			c.gridy = 1;
			c.weightx = 1;
			c.weighty = 1;
			add(basePanel, c);
			add(comparisonPanel, c);
			add(differencePanel, c);
			setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
			ImageIcon icon = new ImageIcon(getClass().getResource("/images/CalLiteIcon.png"));
			setIconImage(icon.getImage());
			pack();
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display scenario frame.";
			errorHandlingSvc.businessErrorHandler(messageText, e);
		}
	}

	/**
	 * This method will build the {@link JPanel} with the {@link DataTableModel}
	 * passed in.
	 *
	 * @param dataTableModel The data to display for "View Scenario Settings" button.
	 * @return Will return the {@link JPanel} for "View Scenario Settings".
	 */
	private JPanel buildJpanel(DataTableModel dataTableModel)
	{
		JPanel jPanel = new JPanel();
		jPanel.setPreferredSize(new Dimension(600, 700));
		jPanel.setMinimumSize(new Dimension(600, 700));
		jPanel.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(10, 10, 10, 10);
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1;
		c.weighty = 1;
		JTable scentable = new JTable();
		scentable.setModel(dataTableModel);
		JScrollPane scrollingtable = new JScrollPane(scentable);
		scrollingtable.setPreferredSize(new Dimension(480, 600));
		jPanel.add(scrollingtable, c);
		return jPanel;
	}

	@Override
	public void itemStateChanged(ItemEvent ie)
	{
		if(ie.getItem() instanceof JRadioButton)
		{
			JRadioButton rb = (JRadioButton) ie.getItem();
			if(rb.isSelected())
			{
				if(rb.getText().equals("Base"))
				{
					basePanel.setVisible(true);
					comparisonPanel.setVisible(false);
					differencePanel.setVisible(false);

				}
				else if(rb.getText().equals("Comparison"))
				{
					basePanel.setVisible(false);
					comparisonPanel.setVisible(true);
					differencePanel.setVisible(false);

				}
				else if(rb.getText().equals("Difference"))
				{
					basePanel.setVisible(false);
					comparisonPanel.setVisible(false);
					differencePanel.setVisible(true);
				}
			}
		}
	}
}
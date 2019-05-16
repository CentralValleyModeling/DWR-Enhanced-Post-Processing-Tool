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

package gov.ca.water.eppt.nbui.actions;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

import gov.ca.water.quickresults.ui.report.QAQCReportPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle;
import org.openide.windows.WindowManager;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-18-2019
 */
@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.QAQCReportAction"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/button_blue_play.png",
		displayName = "Generate QA/QC Report"
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/Tools", position = 50)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 777)
		})
@NbBundle.Messages("CTL_QAQCReportAction=Generate QA/QC Report")
public class QAQCReportAction implements ActionListener
{

	private final QAQCReportPanel _reportPanel = new QAQCReportPanel();
	private JDialog _qaQcReportDialog;

	@Override
	public void actionPerformed(ActionEvent e)
	{
		if(_qaQcReportDialog == null)
		{
			_qaQcReportDialog = new JDialog(WindowManager.getDefault().getMainWindow(), "Generate QA/QC Report");
			_qaQcReportDialog.setLayout(new BorderLayout());
			_qaQcReportDialog.add(_reportPanel, BorderLayout.CENTER);
			_qaQcReportDialog.pack();
			_qaQcReportDialog.setLocationRelativeTo(WindowManager.getDefault().getMainWindow());
		}
		if(!_qaQcReportDialog.isVisible())
		{
			_reportPanel.fillComboScenarioRuns();
		}
		_qaQcReportDialog.revalidate();
		_qaQcReportDialog.setVisible(true);
		_qaQcReportDialog.toFront();
	}

}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.presentation;

import java.awt.Component;
import java.awt.HeadlessException;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.bus_delegate.IApplyDynamicConDele;
import gov.ca.water.calgui.bus_delegate.impl.ApplyDynamicConDeleImp;
import gov.ca.water.calgui.bus_service.impl.XMLParsingSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IDialogSvc;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.DialogSvcImpl;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

/**
 * This class is for Listening all the mouse events which are generated by the
 * application.
 *
 * @author Mohan
 */
public class GlobalMouseListener implements MouseListener
{

	private static final Logger LOG = Logger.getLogger(GlobalMouseListener.class.getName());
	private IApplyDynamicConDele applyDynamicConDele = new ApplyDynamicConDeleImp();
	private SwingEngine _swingEngine = XMLParsingSvcImpl.getXMLParsingSvcImplInstance().getSwingEngine();
	private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
	private IDialogSvc dialogSvc = DialogSvcImpl.getDialogSvcInstance();

	@Override
	public void mouseClicked(MouseEvent me)
	{
		try
		{
			LOG.debug("mouseClicked");
			JComponent component = (JComponent) me.getComponent();
			String cName = component.getName();

			if(SwingUtilities.isRightMouseButton(me))
			{

				// Right-click on a regulations checkbox updates right-hand
				// panel

				// if (((JCheckBox) component).isSelected()) {
				applyDynamicConDele.applyDynamicControl(cName, ((JCheckBox) component).isSelected(),
						component.isEnabled(), false);
				// the false value don't mean any thing because we implement
				// right
				// click on only check box.
				if("ckbReg_TRNTY".equals(cName) || "ckbReg_PUMP".equals(cName))
				{

					// Special handling for Trinity and Pumping regulations

					// These two regulations cannot be turned off, so their
					// checkboxes are not enabled under user-defined. This code
					// forces the display of the D1485/D-1641 selector in
					// reg_panTab

					_swingEngine.find("btnRegCopy").setEnabled(false);
					_swingEngine.find("btnRegPaste").setEnabled(false);
					_swingEngine.find("btnRegD1641").setEnabled(true);
					_swingEngine.find("btnRegD1485").setEnabled(true);

					this._swingEngine.find("reg_panTab").setVisible(true);
					this._swingEngine.find("reg_panTabPlaceholder").setVisible(false);
				}
				LOG.debug(cName);
			}
			else
			{
				// Otherwise, we're looking for a double-click on a "ckbp"
				// checkbox from quick results.
				int button = me.getButton();
				Integer iClickCount = me.getClickCount();
				if(button != MouseEvent.NOBUTTON && button != MouseEvent.BUTTON1)
				{
					// Nothing for right mousepress
				}
				else
				{
					// Double Click
					if(iClickCount == 2 && cName.startsWith("ckbp"))
					{
						JCheckBox chk = (JCheckBox) component;
						JList lstScenarios = (JList) _swingEngine.find("SelectedList");
						if(lstScenarios.getModel().getSize() == 0)
						{
							dialogSvc.getOK("Error - No scenarios loaded", JOptionPane.ERROR_MESSAGE);
						}
						else
						{
							List<RBListItemBO> scenarios = getScenarios();
							DisplayFrame.showDisplayFrames(_swingEngine,
									DisplayFrame.quickState(_swingEngine) + ";Locs-" + chk.getText()
									+ ";Index-" + chk.getName(), scenarios);
						}
					}
					// Placeholder for future handling of double-clicks
				}
			}
		}
		catch(HeadlessException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to initialize mouse listeners.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) _swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
	}

	private List<RBListItemBO> getScenarios()
	{
		List<RBListItemBO> retval = new ArrayList<>();
		Component component = _swingEngine.find("SelectedList");
		if(component instanceof JList)
		{
			JList<RBListItemBO> lstScenarios = (JList<RBListItemBO>) component;
			ListModel<RBListItemBO> model = lstScenarios.getModel();
			for(int i = 0; i < model.getSize(); i++)
			{
				retval.add(model.getElementAt(i));
			}
		}
		return retval;
	}

	@Override
	public void mouseReleased(MouseEvent arg0)
	{
	}

	@Override
	public void mouseEntered(MouseEvent arg0)
	{
	}

	@Override
	public void mouseExited(MouseEvent arg0)
	{
	}

	@Override
	public void mousePressed(MouseEvent arg0)
	{
	}
}

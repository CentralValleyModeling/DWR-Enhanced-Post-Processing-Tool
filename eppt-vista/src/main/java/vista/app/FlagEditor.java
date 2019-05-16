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
package vista.app;

import java.awt.Frame;
import javax.swing.*;

import vista.db.dss.DSSUtil;
import vista.graph.Curve;
import vista.graph.GECanvas;
import vista.graph.Graph;
import vista.graph.RangeActor;
import vista.graph.RangeSelected;
import vista.graph.RangeSelector;
import vista.set.Constants;
import vista.set.DataReference;
import vista.set.DataRetrievalException;
import vista.set.DataSet;
import vista.set.DataSetElement;
import vista.set.DataSetIterator;
import vista.set.FlagUtils;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;

/**
 * an instance of this class starts editing, pops up query for range selection,
 * pops up query for flag id to set range to and then sets the range to that
 * flag. If range is already flagged it requests for override vs preserve
 * option.
 *
 * @author Nicky Sandhu
 * @version $Id: FlagEditor.java,v 1.1 2003/10/02 20:48:30 redwood Exp $
 */
public class FlagEditor implements RangeActor
{
	private Curve curve;
	private GECanvas gC;
	private RangeSelected rs;
	private FlagChoiceFrame fcf;

	/**
	 * Edits the flags for the given curve
	 */
	public FlagEditor(GECanvas gC, Curve curve, boolean boxSelection)
	{
		this.gC = gC;
		this.curve = curve;
		if(boxSelection)
		{
			rs = new RangeSelector(gC, curve, this);
		}
		else
		{
			rs = new XRangeSelector(this, gC, curve);
		}
	}

	@Override
	public void selectedRange(int xmin, int xmax, int ymin, int ymax)
	{
		fcf = new FlagChoiceFrame(JOptionPane.getFrameForComponent(gC), this);
	}

	/**
	 *
	 */
	public void emailRangeTo()
	{
		String rcp = MainProperties.getProperty("email.maintainers");
		String subject = "Data Quality Flags [VISTA]";
		DataReference ref = (DataReference) curve.getModel()
												 .getReferenceObject();
		TimeFactory tf = DSSUtil.getTimeFactory();
		Time time = tf.getTimeInstance();
		TimeInterval ti = DSSUtil.createTimeInterval(ref.getPathname());
		String msg = "Data Reference:\n" + "Server: " + ref.getServername()
				+ "\n" + "Filename: " + ref.getFilename() + "\n" + "Pathname: "
				+ ref.getPathname() + "\n" + "Data Start Time: "
				+ time.create(Math.round(rs.getXRangeMin())).floor(ti) + "\n"
				+ "Data End Time: "
				+ time.create(Math.round(rs.getXRangeMax())).ceiling(ti) + "\n";
		Frame f = JOptionPane.getFrameForComponent(gC);
	}

	/**
	 * This method is called by the flag choice dialog once the user is done
	 * selecting the flag to which to set the data. Then given the minimum and
	 * maximum range, the data set and the flag Id, the data set is changed.
	 */
	public void flagRangeTo(int flagId)
	{
		gC.paint(gC.getGraphics());
		DataSet ds = null;
		try
		{
			ds = ((DataReference) curve.getModel().getReferenceObject())
					.getData();
		}
		catch(DataRetrievalException dre)
		{
			throw new IllegalArgumentException(dre.getMessage());
		}
		DataSetIterator iterator = ds.getIterator();
		boolean overrideDecided = false;
		boolean override = true;
		double minx = rs.getXRangeMin();
		double maxx = rs.getXRangeMax();
		double miny = rs.getYRangeMin();
		double maxy = rs.getYRangeMax();
		int userId = DSSUtil.getUserId();
		for(iterator.resetIterator(); !iterator.atEnd(); iterator.advance())
		{
			DataSetElement dse = iterator.getElement();
			double x = dse.getX();
			double y = dse.getY();
			if((x >= minx && x <= maxx) && (y >= miny && y <= maxy))
			{

				if(!overrideDecided && FlagUtils.isScreened(dse))
				{
					overrideDecided = true;
					// check one of override vs preserve with modal dialog
					Object[] choices = {"Override flags", "Preserve flags"};
					Object choice = JOptionPane.showInputDialog(gC,
							"Override vs Preserve Flags",
							"Flag preservation choice",
							JOptionPane.QUESTION_MESSAGE, null, choices,
							choices[1]);
					override = choice.equals(choices[0]);
				}
				//
				if(FlagUtils.isScreened(dse) && !override)
				{
					// don't do anything...
				}
				else
				{
					if(y == Constants.MISSING || y == Constants.MISSING_VALUE
							|| y == Constants.MISSING_RECORD)
					{
						FlagUtils.setQualityFlag(dse, FlagUtils.MISSING_FLAG,
								userId);
					}
					else
					{
						FlagUtils.setQualityFlag(dse, flagId, userId);
					}
					iterator.putElement(dse);
				}
			}
		}
		Graph graph = (Graph) gC.getGraphicElement();
		AppUtils.setCurveFilter(graph, AppUtils.getCurrentCurveFilter());
		gC.redoNextPaint();
		gC.paint(gC.getGraphics());
		// curve.dataSetChanged();
		// table.dataSetChanged();
	}

	/**
	 *
	 */
	public void doneChanges()
	{
		Graph graph = (Graph) gC.getGraphicElement();
		AppUtils.setCurveFilter(graph, AppUtils.getCurrentCurveFilter());
		gC.redoNextPaint();
		gC.paint(gC.getGraphics());
	}

}

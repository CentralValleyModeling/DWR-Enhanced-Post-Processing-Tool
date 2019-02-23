/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import java.lang.reflect.Constructor;

import vista.graph.Curve;
import vista.graph.CurveAttr;
import vista.graph.CurveDataModel;
import vista.graph.FlaggedCurve;
import vista.graph.GraphUtils;
import vista.graph.ReferenceCurve;
import vista.set.DataReference;
import vista.set.DataRetrievalException;
import vista.set.DataSet;
import vista.set.DataSetAttr;
import vista.set.DefaultDataSet;
import vista.set.IrregularTimeSeries;
import vista.set.RegularTimeSeries;

/**
 * @author Nicky Sandhu
 * @version $Id: CurveFactory.java,v 1.1 2003/10/02 20:48:25 redwood Exp $
 */
public class CurveFactory
{
	public static int PLAIN = 1;
	public static int DOTTED = 1;
	public static int SHORT_DASHED = 1;
	public static int LONG_DASHED = 1;
	public static int SHORT_LONG_DASHED = 1;
	/**
	 *
	 */
	private static boolean _enhancedGraphics = false;

	/**
	 *
	 */
	public static void setDashedAttribute(Curve crv, int type)
	{
		CurveAttr attr = (CurveAttr) crv.getAttributes();
		if(type == PLAIN)
		{
			attr._dashArray = new float[]{1};
		}
		else if(type == DOTTED)
		{
			attr._dashArray = new float[]{2, 2};
		}
		else if(type == SHORT_DASHED)
		{
			attr._dashArray = new float[]{4, 4};
		}
		else if(type == LONG_DASHED)
		{
			attr._dashArray = new float[]{8, 8};
		}
		else if(type == SHORT_LONG_DASHED)
		{
			attr._dashArray = new float[]{4, 4, 8, 4};
		}
		else
		{
			attr._dashArray = new float[]{1};
		}

	}

	/**
	 *
	 */
	public static Curve createFlaggedCurve(DataReference ref, int xAxisPos,
										   int yAxisPos, String legend)
	{
		try
		{
			Curve curve = createFlaggedCurve(ref.getData(), xAxisPos, yAxisPos,
					legend);
			curve.getModel().setReferenceObject(ref);
			return curve;
		}
		catch(DataRetrievalException dre)
		{
			throw new IllegalArgumentException(dre.getMessage());
		}
	}

	/**
	 *
	 */
	public static Curve createCurve(DataReference ref, int xAxisPos,
									int yAxisPos, String legend)
	{
		_enhancedGraphics = GraphUtils.isJDK2();
		try
		{
			Curve curve = createFlaggedCurve(ref.getData(), xAxisPos, yAxisPos,
					legend);
			curve.getModel().setReferenceObject(ref);
			return curve;
		}
		catch(DataRetrievalException dre)
		{
			throw new IllegalArgumentException(dre.getMessage());
		}
	}

	/**
	 *
	 */
	public static Curve createFlaggedCurve(DataSet ds, int xAxisPos,
										   int yAxisPos, String legend)
	{
		CurveDataModel cdm = null;
		if(ds instanceof RegularTimeSeries)
		{
			DataSetAttr attr = ds.getAttributes();
			if(attr == null || attr.getYType().indexOf("PER") >= 0)
			{
				cdm = new PerValFlaggedCurveModel((RegularTimeSeries) ds,
						AppUtils.getCurrentCurveFilter(), xAxisPos, yAxisPos,
						legend);
			}
			else
			{
				cdm = new InstValFlaggedCurveModel(ds, AppUtils
						.getCurrentCurveFilter(), xAxisPos, yAxisPos, legend);
			}
		}
		else
		{
			cdm = new InstValFlaggedCurveModel(ds, AppUtils
					.getCurrentCurveFilter(), xAxisPos, yAxisPos, legend);
		}
		if(_enhancedGraphics)
		{
			try
			{
				Class cl = Class.forName("vista.graph.FlaggedCurve2D");
				Class[] params = {CurveAttr.class, CurveDataModel.class};
				Constructor cst = cl.getDeclaredConstructor(params);
				return (Curve) cst.newInstance(new Object[]{
						AppUtils.getNextCurveAttr(ds), cdm});
			}
			catch(Exception exc)
			{
				exc.printStackTrace(System.err);
				throw new RuntimeException(exc.getMessage());
			}
			// return new FlaggedCurve2D(AppUtils.getNextCurveAttr(ds),cdm);
			// return new FlaggedCurve(AppUtils.getNextCurveAttr(ds),cdm);
		}
		else
		{
			return new FlaggedCurve(AppUtils.getNextCurveAttr(ds), cdm);
		}
	}

	/**
	 *
	 */
	public static Curve createCurve(DataSet ds, int xAxisPos, int yAxisPos,
									String legend)
	{
		if(ds instanceof RegularTimeSeries)
		{
			return createCurve((RegularTimeSeries) ds, xAxisPos, yAxisPos,
					legend);
		}
		else if(ds instanceof IrregularTimeSeries)
		{
			return createCurve((IrregularTimeSeries) ds, xAxisPos, yAxisPos,
					legend);
		}
		else if(ds instanceof DefaultDataSet)
		{
			return createCurve((DefaultDataSet) ds, xAxisPos, yAxisPos, legend);
		}
		else
		{
			throw new IllegalArgumentException(
					"Class of data set is not recognized " + ds.getClass());
		}
	}

	/**
	 *
	 */

	public static Curve createCurve(RegularTimeSeries rts, int xAxisPos,
									int yAxisPos, String legend)
	{
		DataSetAttr attr = rts.getAttributes();
		CurveDataModel cdm = null;
		if(attr != null)
		{
			if(attr.getYType().indexOf("PER") >= 0)
			{
				cdm = new PerValCurveModel(rts, null, xAxisPos, yAxisPos, legend);
			}
		}
		if(cdm == null)
		{
			cdm = new InstValCurveModel(rts, null, xAxisPos, yAxisPos, legend);
		}
		CurveDataModel cdmf = createFlaggedCurveDataModel(rts, xAxisPos,
				yAxisPos, legend);
		return new ReferenceCurve(AppUtils.getNextCurveAttr(rts), cdm, cdmf);
	}

	/**
	 *
	 */
	public static Curve createCurve(IrregularTimeSeries rts, int xAxisPos,
									int yAxisPos, String legend)
	{
		DataSetAttr attr = rts.getAttributes();
		CurveDataModel cdm = new InstValCurveModel(rts, AppUtils
				.getCurrentCurveFilter(), xAxisPos, yAxisPos, legend);
		CurveDataModel cdmf = createFlaggedCurveDataModel(rts, xAxisPos,
				yAxisPos, legend);
		return new ReferenceCurve(AppUtils.getNextCurveAttr(rts), cdm, cdmf);
	}

	/**
	 *
	 */
	public static Curve createCurve(DefaultDataSet rts, int xAxisPos,
									int yAxisPos, String legend)
	{
		DataSetAttr attr = rts.getAttributes();
		CurveDataModel cdm = new InstValCurveModel(rts, AppUtils
				.getCurrentCurveFilter(), xAxisPos, yAxisPos, legend);
		CurveDataModel cdmf = createFlaggedCurveDataModel(rts, xAxisPos,
				yAxisPos, legend);
		return new ReferenceCurve(AppUtils.getNextCurveAttr(rts), cdm, cdmf);
	}

	/**
	 *
	 */
	public static CurveDataModel createCurveDataModel(RegularTimeSeries rts,
													  int xAxisPos, int yAxisPos, String legend)
	{
		DataSetAttr attr = rts.getAttributes();
		CurveDataModel cdm = new InstValCurveModel(rts, AppUtils
				.getCurrentCurveFilter(), xAxisPos, yAxisPos, legend);
		if(attr != null)
		{
			if(attr.getYType().indexOf("PER") >= 0)
			{
				cdm = new PerValCurveModel(rts, AppUtils
						.getCurrentCurveFilter(), xAxisPos, yAxisPos, legend);
			}
		}
		return cdm;
	}

	/**
	 *
	 */
	public static CurveDataModel createCurveDataModel(IrregularTimeSeries rts,
													  int xAxisPos, int yAxisPos, String legend)
	{
		DataSetAttr attr = rts.getAttributes();
		CurveDataModel cdm = new InstValCurveModel(rts, AppUtils
				.getCurrentCurveFilter(), xAxisPos, yAxisPos, legend);
		return cdm;
	}

	/**
	 *
	 */
	public static CurveDataModel createCurveDataModel(DefaultDataSet rts,
													  int xAxisPos, int yAxisPos, String legend)
	{
		DataSetAttr attr = rts.getAttributes();
		CurveDataModel cdm = new InstValCurveModel(rts, AppUtils
				.getCurrentCurveFilter(), xAxisPos, yAxisPos, legend);
		return cdm;
	}

	/**
	 *
	 */
	public static CurveDataModel createFlaggedCurveDataModel(DataReference ref,
															 int xAxisPos, int yAxisPos, String legend)
	{
		DataSet ds = null;
		try
		{
			ds = ref.getData();
		}
		catch(Exception e)
		{
			return null;
		}
		return createFlaggedCurveDataModel(ds, xAxisPos, yAxisPos, legend);

	}

	/**
	 *
	 */
	public static CurveDataModel createFlaggedCurveDataModel(DataSet ds,
															 int xAxisPos, int yAxisPos, String legend)
	{
		CurveDataModel cdm = null;
		if(ds instanceof RegularTimeSeries)
		{
			DataSetAttr attr = ds.getAttributes();
			if(attr == null || attr.getYType().indexOf("PER") >= 0)
			{
				cdm = new PerValFlaggedCurveModel((RegularTimeSeries) ds, AppUtils.getCurrentCurveFilter(),
						xAxisPos, yAxisPos, legend);
			}
			else
			{
				cdm = new InstValFlaggedCurveModel(ds, AppUtils.getCurrentCurveFilter(), xAxisPos,
						yAxisPos, legend);
			}
		}
		else
		{
			cdm = new InstValFlaggedCurveModel(ds, AppUtils.getCurrentCurveFilter(), xAxisPos, yAxisPos,
					legend);
		}
		return cdm;
	}
}

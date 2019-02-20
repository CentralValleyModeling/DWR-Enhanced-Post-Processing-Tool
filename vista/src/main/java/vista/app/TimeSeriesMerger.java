package vista.app;

import java.awt.Color;
import java.util.ArrayList;

import javax.swing.JOptionPane;

import vista.graph.Curve;
import vista.graph.GEAttr;
import vista.graph.GECanvas;
import vista.graph.GEContainer;
import vista.graph.Graph;
import vista.graph.GraphicElement;
import vista.graph.LegendItem;
import vista.graph.Plot;
import vista.graph.RangeActor;
import vista.set.DataReference;
import vista.set.DefaultReference;
import vista.set.RegularTimeSeries;
import vista.set.TimeSeries;
import vista.set.TimeSeriesMergeUtils;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

public class TimeSeriesMerger implements RangeActor{
	private TimeSeries merged;
	private Curve mergedCurve;
	private GECanvas canvas;
	private LegendItem mergedLegendItem;
	private XRangeSelector rangeSelector;
	private Curve[] curves;
	private Plot plot;
	private Graph graph;

	public TimeSeriesMerger(GECanvas canvas){
		this.canvas = canvas;
		graph = (Graph) canvas.getGraphicElement();
		plot = graph.getPlot();
		GEContainer curveContainer = plot.getCurveContainer();
		GraphicElement[] curves = curveContainer.getElements(Curve.class);
		this.curves = new Curve[curves.length];
		System.arraycopy(curves, 0, this.curves, 0, curves.length);
		TimeSeries[] timeSeries = extractTimeSeriesFromCurves(curves);
		if (timeSeries == null){
			return;
		}
		// do merge using order in plot and union of time windows
		TimeWindow tw = TimeSeriesMergeUtils.getTimeWindow(timeSeries);
		merged = TimeSeriesMergeUtils.merge(timeSeries, tw);
		DataReference mergedReference = new DefaultReference(merged);
		mergedCurve = CurveFactory.createCurve(mergedReference, ((Curve)curves[0]).getXAxis().getPosition(), ((Curve)curves[0]).getYAxis().getPosition(), "Merged");
		mergedCurve.getAttributes()._foregroundColor = Color.gray;
		plot.addCurve(mergedCurve);
		graph.getLegend().add(mergedLegendItem = new LegendItem(mergedCurve));
		canvas.redoNextPaint();
		canvas.repaint();
	}

	private TimeSeries[] extractTimeSeriesFromCurves(GraphicElement[] curves) {
		ArrayList<TimeSeries> tsList = new ArrayList<TimeSeries>();
		for(int i=0; i < curves.length; i++){
			Curve c = (Curve) curves[i];
			DataReference ref = (DataReference) c.getModel().getReferenceObject();
			if (ref.getData() instanceof TimeSeries){
				tsList.add((TimeSeries)ref.getData());
			} else {
				throw new RuntimeException("Not all data in plot are time series");
			}
		}
		TimeSeries[] timeSeries = new TimeSeries[tsList.size()];
		if (tsList.size()==0){
			return null;
		}else{
			timeSeries = tsList.toArray(timeSeries);
		}
		//
		if (!checkTimeSeriesAreMergable(timeSeries)){
			throw new RuntimeException("Select only time series that are compatible for a merge");
		}
		return timeSeries;
	}

	public TimeSeries getMergedData() {
		return merged;
	}
	
	/**
	 * For the given time window, use the curves in the order provided
	 * and replace into the merged curve
	 * @param curves
	 * @param timeWindow
	 * @return
	 */
	public void doMerge(Curve[] curves, TimeWindow timeWindow){
		TimeSeries[] timeSeries = extractTimeSeriesFromCurves(curves);
		TimeSeries replacer = TimeSeriesMergeUtils.merge(timeSeries, timeWindow);
		TimeSeriesMergeUtils.replaceInPlace(merged, replacer);
		canvas.redoNextPaint();
		canvas.repaint();
	}

	public void removeDataFromGraph() {
		Graph graph = (Graph) this.canvas.getGraphicElement();
		graph.getPlot().removeCurve(mergedCurve);
		graph.getLegend().remove(mergedLegendItem);
		this.canvas.redoNextPaint();
		this.canvas.repaint();
	}
	
	/**
	 * returns the messages 
	 * @param tsList
	 * @return
	 */
	public boolean checkTimeSeriesAreMergable(TimeSeries[] tsList) {
		if (tsList==null || tsList.length==0){
			throw new RuntimeException("No time series or selection is empty?");
		}
		if (tsList.length == 1){
			int showConfirmDialog = JOptionPane.showConfirmDialog(null, "Only one time series to merge ?", "Time series merge", JOptionPane.OK_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
			if (showConfirmDialog == JOptionPane.CANCEL_OPTION){
				return false;
			}
		}
		if (TimeSeriesMergeUtils.isAllRegular(tsList)){
			TimeInterval ti = null;
			for(int i=0; i < tsList.length; i++){
				TimeSeries ts = tsList[i];
				RegularTimeSeries rts = (RegularTimeSeries) ts;
				if (ti==null){
					ti = rts.getTimeInterval();
				} else {
					int compare = rts.getTimeInterval().compare(ti);
					if (compare != 0){
						String msg = "Time Series: " +rts.getName() + "with time interval: "+rts.getTimeInterval()+" does not have expected time interval "+ti+" as others in merge list";
						throw new RuntimeException(msg);
					}
				}
			}
		}
		// check units
		String units = null;
		for(int i=0; i < tsList.length; i++){
			TimeSeries ts = tsList[i];
			String yUnits = ts.getAttributes().getYUnits();
			if (units==null){
				units=yUnits;
			} else if (!units.equalsIgnoreCase(yUnits)){
				throw new RuntimeException("Units are incompatible: "+units+" vs "+yUnits+" on "+ts.getName());
			}
		}		
		return true;
	}
	
	public void selectRange(){
		rangeSelector = new XRangeSelector(this, canvas, mergedCurve);
	}

	@Override
	public void selectedRange(int xmin, int xmax, int ymin, int ymax) {
		TimeFactory tf = TimeFactory.getInstance();
		Time st = tf.createTime(Math.round(mergedCurve.getXAxis().getScale().scaleToDC(xmin)));
		Time et = tf.createTime(Math.round(mergedCurve.getXAxis().getScale().scaleToDC(xmax)));
		if (merged instanceof RegularTimeSeries){
			TimeInterval ti = ((RegularTimeSeries) merged).getTimeInterval();
			if (ti != null){
				st = st.ceiling(ti);
				et = et.floor(ti);
			}
		}
		TimeWindow timeWindow = TimeFactory.getInstance().createTimeWindow(st, et);
		//
		ReorderMergingCurvesDialog reorderDialog = new ReorderMergingCurvesDialog(canvas,this.curves);
		//
		Curve[] newOrder = reorderDialog.getCurves();
		if (newOrder != null){
			this.doMerge(reorderDialog.getCurves(), timeWindow);
		}
	}

}

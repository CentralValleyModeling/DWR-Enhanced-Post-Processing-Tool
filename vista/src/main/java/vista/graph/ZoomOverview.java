package vista.graph;

import java.awt.Graphics;

/**
 * Shows the zoom overview into the plot.
 * @author psandhu
 *
 */
@SuppressWarnings("serial")
public class ZoomOverview extends GECanvas{
	
	private Zoom zoom;

	public ZoomOverview(GraphicElement ge, Zoom zoom) {
		super(ge);
		this.zoom = zoom;
	}

	@Override
	public void paint(Graphics g) {
		zoom.getZoomRectangle();
		super.paint(g);
	}

	
}

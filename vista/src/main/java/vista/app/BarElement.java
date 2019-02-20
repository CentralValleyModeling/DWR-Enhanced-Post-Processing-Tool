/*
    Copyright (C) 1996, 1997, 1998 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0beta
	by Nicky Sandhu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    nsandhu@water.ca.gov

    Send bug reports to nsandhu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to nsandhu@water.ca.gov or call (916)-653-7552

 */
package vista.app;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.text.NumberFormat;

import vista.graph.AnimateElement;
import vista.graph.AnimationObservable;
import vista.graph.AnimationObserver;
import vista.graph.GEAttr;
import vista.graph.Scale;

/**
   * 
   */
public class BarElement extends AnimateElement implements AnimationObserver {
	/**
   * 
   */
	public BarElement(GEAttr attr) {
		super(attr);
		_s = new Scale(0, 100, 0, 10);
	}

	/**
   * 
   */
	public void setRange(float min, float max) {
		_s.setDCRange(min, max);
	}

	/**
   * 
   */
	public void setFluxInput(FluxInput input) {
		fluxInput = input;
	}

	/**
   * 
   */
	public void update(AnimationObservable o, Object arg) {
		_value = fluxInput.getNextValue();
	}

	/**
   *
   */
	public void Draw() {
	}

	/**
   * 
   */
	public void animateNext() {
		// System.out.println(this.getClass().getName() + ".animateNext()");
		Graphics gc = getGraphics();
		Color previousColor = gc.getColor();
		Font previousFont = gc.getFont();

		Rectangle r = getInsetedBounds();
		Rectangle barRectangle = r;

		_s.setUCRange(r.y + r.height, r.y);

		gc.setColor(_barColor);

		gc.fillRect(r.x, _s.scaleToUC(_value), r.width, _s.scaleToUC(0)
				- _s.scaleToUC(_value));
		gc.setColor(_lineColor);
		gc.setFont(_valueFont);

		float max = (float) _s.getDataMaximum();
		float min = (float) _s.getDataMinimum();
		drawLineWithValue(min, r);
		drawLineWithValue((max + min) / 2, r);
		drawLineWithValue(max, r);
		gc.drawString(_nf.format(_value) + "%", r.x + 2 * r.width, r.y);
		gc.setColor(previousColor);
		gc.setFont(previousFont);
	}

	/**
   * 
   */
	private void drawLineWithValue(float value, Rectangle r) {
		Graphics gc = getGraphics();
		gc.drawLine(r.x, _s.scaleToUC(value), r.x + r.width, _s
				.scaleToUC(value));
		gc.drawString(new Integer((int) value).toString().trim(), r.x + 2, _s
				.scaleToUC(value));
	}

	/**
   * 
   */
	public Dimension getPreferredSize() {
		return new Dimension(50, 100);
	}

	/**
   * 
   */
	public Dimension getMinimumSize() {
		return getPreferredSize();
	}

	/**
	 * Value in units of percentage
	 */
	protected float _value;
	/**
   * 
   */
	protected FluxInput fluxInput;
	/**
   * 
   */
	protected Scale _s;
	/**
   * 
   */
	protected Color _lineColor = Color.yellow;
	/**
   * 
   */
	protected Color _barColor = Color.red;
	/**
   * 
   */
	protected Font _valueFont = new Font("Times Roman", Font.PLAIN, 10);
	static NumberFormat _nf = NumberFormat.getInstance();
	static {
		_nf.setMaximumFractionDigits(1);
	}
}

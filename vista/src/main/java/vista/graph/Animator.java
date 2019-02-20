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
package vista.graph;

/**
 * This class handles the animation. Animation is controlled in the run method
 * of this class and a thread object is used to run that method. Objects that
 * need to be animated every frame register themselves with the Animator which
 * then uses the observer pattern to notify them at the appropriate time.
 */
public class Animator implements Runnable {
	/**
	 * Initializes the observer
	 */
	public Animator() {
		obs = new AnimationObservable();
		displayNotifier = new AnimationObservable();
	}

	/**
	 * creates a thread and starts animation.
	 */
	public void startAnimation() {
		if (animatorThread == null)
			animatorThread = new Thread(this);
		animatorThread.start();
	}

	/**
	 * stops animation by setting thread to null
	 */
	public void stopAnimation() {
		animatorThread = null;
	}

	/**
	 * checks the state of animation
	 */
	public boolean isAnimationRunning() {
		return (animatorThread != null);
	}

	/**
	 * The main loop of the animation thread. It keeps the animation going at an
	 * even pace.
	 */
	public void run() {
		long startTime = System.currentTimeMillis();
		while (Thread.currentThread() == animatorThread) {
			animate();
			notifyDisplays();
			try {
				startTime += interval;
				Thread.sleep(Math
						.max(0, startTime - System.currentTimeMillis()));
			} catch (InterruptedException e) {
				break;
			}
		}
	}

	/**
	 * notifies all observers
	 */
	protected void notifyDisplays() {
		displayNotifier.notifyAll(this);
	}

	/**
	 * animates registered objects by notifying them.
	 */
	protected void animate() {
		if (DEBUG)
			System.out.println("Notifying Observers");
		obs.notifyAll(this);
	}

	/**
	 * Adds the element to the list of observers for animation
	 */
	public void addAnimateElement(AnimationObserver element) {
		obs.addObserver(element);
	}

	/**
	 * adds display
	 */
	public void addAnimateDisplay(AnimationObserver display) {
		displayNotifier.addObserver(display);
	}

	/**
	 * sets the interval or interval between animation frames
	 */
	public void setInterval(long d) {
		interval = d;
	}

	/**
	 * gets the interval between animation frames
	 */
	public long getInterval() {
		return interval;
	}

	/**
	 * The observable for animation
	 */
	protected AnimationObservable obs = null;
	/**
	 * The observable for displays
	 */
	protected AnimationObservable displayNotifier = null;
	/**
	 * length in millisecs to make animator thread sleep to maintain even
	 * animation speed.
	 */
	protected long sleepInterval = 0;
	/**
	 * Interval between two animations in millisecs
	 */
	protected long interval = 10;
	/**
	 * The thread to run this animation
	 */
	protected Thread animatorThread = null;
	/**
	 * debuggin' purposes
	 */
	private static final boolean DEBUG = false;
}

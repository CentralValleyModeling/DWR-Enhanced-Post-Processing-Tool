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
package vista.set;

/**
 * A proxy for this session which loads the groups only when operations other
 * than setName() and getName() are requested.
 * 
 * @author Nicky Sandhu
 * @version $Id: SessionProxy.java,v 1.1 2003/10/02 20:49:31 redwood Exp $
 */
public abstract class SessionProxy extends Session {
	/**
	 * return the initialized session object. This is the session that contains
	 * the state which is then copied into the proxy objects state to initialize
	 * it.
	 */
	protected abstract Session getInitializedSession();

	/**
	 * copies the groups from this session into specified one.
	 */
	public void copyInto(Session s) {
		if (!_initialized) {
			initializeSession();
		}
		super.copyInto(s);
	}

	/**
	 * a shallow copy of the name and list of groups
	 */
	public Object clone() {
		if (!_initialized) {
			initializeSession();
		}
		return super.clone();
	}

	/**
	 * Creates a union of this session with given session. This is basically
	 * combining the group(s) in both sessions in one session and returning it.
	 */
	public Session createUnion(Session s) {
		if (!_initialized) {
			initializeSession();
		}
		return super.createUnion(s);
	}

	/**
	 * gets the number of groups in the list.
	 */
	public int getNumberOfGroups() {
		if (!_initialized) {
			initializeSession();
		}
		return super.getNumberOfGroups();
	}

	/**
	 * adds group if not already present
	 */
	public void addGroup(Group g) {
		if (!_initialized) {
			initializeSession();
		}
		super.addGroup(g);
	}

	/**
	 * removes group from list.
	 */
	public void removeGroup(Group g) {
		if (!_initialized) {
			initializeSession();
		}
		super.removeGroup(g);
	}

	/**
	 * gets the group by index
	 */
	public Group getGroup(int index) {
		if (!_initialized) {
			initializeSession();
		}
		return super.getGroup(index);
	}

	/**
	 * gets group by name
	 */
	public Group getGroup(String groupName) {
		if (!_initialized) {
			initializeSession();
		}
		return super.getGroup(groupName);
	}

	/**
	 * gets all the groups
	 */
	public Group[] getAllGroups() {
		if (!_initialized) {
			initializeSession();
		}
		return super.getAllGroups();
	}

	/**
	 * adds state listener
	 */
	void addStateListener(SessionProxyState s) {
		_stateListener = s;
	}

	/**
	 * informs state listener that session has been initialized
	 */
	void informStateListener() {
		if (_stateListener != null) {
			_stateListener.sessionIsInitialized();
		}
	}

	/**
	 * initialize session and set initialize to true.
	 */
	private void initializeSession() {
		Session s = getInitializedSession();
		s.copyInto(this);
		_initialized = true;
		informStateListener();
	}

	/**
	 * true if initialized
	 */
	private boolean _initialized = false;
	/**
	 * state object listening for initialization on this proxy.
	 */
	private SessionProxyState _stateListener;
}

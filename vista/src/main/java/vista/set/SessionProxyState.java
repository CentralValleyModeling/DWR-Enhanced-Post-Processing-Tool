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
 * The state pattern for use on proxy so that once initialized there is no
 * overhead for checking for initialization. It does this by delegating to proxy
 * object if uninitialized and to the actual object once initialized...
 * 
 * @author Nicky Sandhu
 * @version $Id: SessionProxyState.java,v 1.1 2003/10/02 20:49:32 redwood Exp $
 */
public abstract class SessionProxyState extends Session {
	/**
	 * initializes state object with given proxy. The proxy informs this state
	 * object on being initialized.
	 */
	public SessionProxyState(SessionProxy proxy) {
		proxy.addStateListener(this);
		_delegate = proxy;
	}

	/**
	 * copies the groups from this session into specified one.
	 */
	public void copyInto(Session s) {
		_delegate.copyInto(s);
	}

	/**
	 * a shallow copy of the name and list of groups
	 */
	public Object clone() {
		return _delegate.clone();
	}

	/**
	 * Creates a union of this session with given session. This is basically
	 * combining the group(s) in both sessions in one session and returning it.
	 */
	public Session createUnion(Session s) {
		return _delegate.createUnion(s);
	}

	/**
	 * gets the number of groups in the list.
	 */
	public int getNumberOfGroups() {
		return _delegate.getNumberOfGroups();
	}

	/**
	 * adds group if not already present
	 */
	public void addGroup(Group g) {
		_delegate.addGroup(g);
	}

	/**
	 * removes group from list.
	 */
	public void removeGroup(Group g) {
		_delegate.removeGroup(g);
	}

	/**
	 * gets the group by index
	 */
	public Group getGroup(int index) {
		return _delegate.getGroup(index);
	}

	/**
	 * gets group by name
	 */
	public Group getGroup(String groupName) {
		return _delegate.getGroup(groupName);
	}

	/**
	 * gets all the groups
	 */
	public Group[] getAllGroups() {
		return _delegate.getAllGroups();
	}

	/**
	 * initialize session and set initialize to true.
	 */
	void sessionIsInitialized() {
		Session s = new Session();
		_delegate.copyInto(s);
		_delegate = s;
	}

	/**
	 * the session which delegates to the proxy or actual session depending upon
	 * the state of the proxy.
	 */
	private Session _delegate;
}

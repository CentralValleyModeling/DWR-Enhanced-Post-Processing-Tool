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

import javax.swing.AbstractListModel;
import javax.swing.JList;

import vista.set.Group;

/**
 * Displays group as a list with pathname as a string "/A/B/C/D/E/F".
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupList.java,v 1.2 1999/01/07 21:05:32 nsandhu Exp $
 */
public class GroupList extends AbstractListModel {
	/**
	 * Generates list for group
	 */
	public GroupList(Group g) {
		_group = g;
		_list = new JList(this);
		_list.setVisible(true);
	}

	/**
	 * gets element at particular index
	 */
	public Object getElementAt(int index) {
		return _group.getDataReference(index);
		// .getPathname();
	}

	/**
	 * gets the size of the list
	 */
	public int getSize() {
		return _group.getNumberOfDataReferences();
	}

	/**
	 * gets the completed list
	 */
	public JList getListBox() {
		return _list;
	}

	/**
   *
   */
	Group getGroup() {
		return _group;
	}

	/**
   *
   */
	void setGroup(Group g) {
		_group = g;
	}

	/**
	 * the list
	 */
	private JList _list;
	/**
	 * the group
	 */
	private Group _group;
}

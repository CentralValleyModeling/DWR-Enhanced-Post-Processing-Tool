/*
    Copyright (C) 1996-2000 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0
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

import java.util.regex.Pattern;

/**
 * A path parts filter which filters on a data reference and returns true to
 * keep it and false to reject it.
 * 
 * @author Nicky Sandhu
 * @version $Id: PathPartsFilter.java,v 1.1 2003/10/02 20:49:28 redwood Exp $
 */
public class PathPartsFilter implements Predicate<DataReference> {
	Pattern[] _patterns = new Pattern[6];

	/**
	 * sets up a path parts filter with the given filters
	 */
	public PathPartsFilter(String[] pathParts) {
		setPathParts(pathParts);
	}

	/**
    *
    */
	public void setPathParts(String[] pathParts) {
		for (int i = 0; i < Math.min(_patterns.length, pathParts.length); i++) {
			String part = pathParts[i] == null ? "" : pathParts[i].trim()
					.toUpperCase();
			try {
				if (part.equals(""))
					_patterns[i] = null;
				else
					_patterns[i] = Pattern.compile(part);
			} catch (Exception mpe) {
				throw new RuntimeException("Incorrect Regular Expression "
						+ part);
			}
		}
	}

	@Override
	public boolean apply(DataReference ref) {
		if (ref == null)
			return false;
		Pathname path = ref.getPathname();
		boolean keepThis = true;
		for (int i = 0; i < _patterns.length; i++) {
			if (_patterns[i] == null)
				continue;
			keepThis = keepThis && _patterns[i].matcher(path.getPart(i)).find();
		}
		return keepThis;
	}
}

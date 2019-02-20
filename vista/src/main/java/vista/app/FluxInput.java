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

import java.io.IOException;
import java.util.StringTokenizer;

/**
   * 
   */
public class FluxInput {
	/**
   * 
   */
	public FluxInput(String filename, String pathname) {
		_filename = filename;
		_pathname = pathname;
		try {
			input = new AsciiFileInput();
			input.initializeInput(filename);
			while (input.readString().compareTo(pathname) != 0)
				;
			input.readString();
			input.readString();
		} catch (IOException ioe) {
			System.out.println("Error initiatializing flux input " + filename
					+ "::" + pathname);
			System.out.println(ioe);
		} catch (Exception e) {
			System.out.println("Error initiatializing flux input " + filename
					+ "::" + pathname);
			System.out.println(e);
		}
	}

	/**
   * 
   */
	public float getNextValue() {
		String line = null;
		try {
			line = input.readString();
		} catch (IOException ioe) {
			System.out.println("Error reading flux " + ioe);
		}
		try {
			StringTokenizer st = new StringTokenizer(line);
			st.nextToken();
			st.nextToken();
			value = new Float(st.nextToken()).floatValue();
		} catch (Exception nse) {
			System.out.println("nse caught");
		}
		return value;
	}

	/**
   * 
   */
	public void resetInput() {
		try {
			input.closeStream();
		} catch (IOException ioe) {
			System.out.println("Error closing " + _filename);
		}
		try {
			input = new AsciiFileInput();
			input.initializeInput(_filename);
			while (input.readString().compareTo(_pathname) != 0)
				;
			input.readString();
			input.readString();
		} catch (IOException ioe) {
			System.out.println("Error resetting flux input " + _filename + "::"
					+ _pathname);
			System.out.println(ioe);
		} catch (Exception e) {
			System.out.println("Error resetting flux input " + _filename + "::"
					+ _pathname);
			System.out.println(e);
		}
	}

	GenericFileInput input;
	float value;
	String _pathname, _filename;
}

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
package vista.time;

import java.text.FieldPosition;
import java.text.ParsePosition;
import java.util.Date;
import java.util.Locale;

/**
 * Formats date according to HEC DSS criteria. namely midnight is interpreted as
 * 2400 of the previous day. It accepts patterns based on substrings of
 * ddMMMyyyy HHmm only. It then parses the correct string after using
 * DefaultTimeFormat to first format the time into a string.
 * 
 * @author Nicky Sandhu
 * @version $Id: SubTimeFormat.java,v 1.1 2003/10/02 20:49:36 redwood Exp $
 */
public class SubTimeFormat extends TimeFormat {
	/**
	 * creates a time format for the given pattern
	 */
	public TimeFormat create(String pattern) {
		return new SubTimeFormat(pattern);
	}

	/**
	 * Use pattern for US locale
	 */
	public SubTimeFormat() {
		this("ddMMMyyyy HHmm", Locale.US);
	}

	/**
	 * Use pattern for US locale
	 */
	public SubTimeFormat(String pattern) {
		this(pattern, Locale.US);
	}

	/**
	 * sets up formatting by given string and given locale. Time zone is assumed
	 * to be GMT. This does not have the daylight savings time adjustments.
	 */
	public SubTimeFormat(String pattern, Locale locale) {
		_fullPattern = "ddMMMyyyy HHmm";
		_formatter = new DefaultTimeFormat(_fullPattern, locale);
		_pattern = pattern;
	}

	/**
	 * Formats a DefaultTime into a date/time string. The only difference with
	 * java format is the conversion of 0000 hour string to 2400 and pushing
	 * back the data by one day. Thus 01JAN1900 0000 becomes 31DEC1899 2400.
	 * Rest of the times are as returned by the format of java.text.DateFormat
	 * 
	 * @return the formatted date/time string.
	 */
	public StringBuffer format(Date date, StringBuffer toAppendTo,
			FieldPosition fieldPosition) {
		String str = _formatter.format(date, toAppendTo, fieldPosition)
				.toString();
		StringBuffer fsb = new StringBuffer(_pattern);
		// for(int i=0; i < fsb.length(); i++) fsb.setCharAt(i,' ');
		String[] subPatternArray = { "dd", "MMM", "yyyy", "HH", "mm" };
		for (int k = 0; k < subPatternArray.length; k++) {
			String pstr = subPatternArray[k];
			int i = _pattern.indexOf(pstr);
			int j = _fullPattern.indexOf(pstr);
			if (i < 0 || j < 0)
				continue;
			int lp = pstr.length();
			for (int m = j; m < j + lp; m++) {
				fsb.setCharAt(i + m - j, str.charAt(m));
			}
		}
		return fsb;
		// return new StringBuffer(fsb.toString().trim());
	}

	/**
	 * Parse a date/time string according to the given parse position. For
	 * example, a time text "07/10/96 4:5 PM, PDT" will be parsed into a Date
	 * that is equivalent to Date(837039928046).
	 * 
	 */
	public Date parse(String text, ParsePosition pos) {
		Date date = _formatter.parse(text, pos);
		return date;
	}

	/**
    *
    */
	public String toString() {
		return "SubTimeFormat: " + _pattern;
	}

	/**
	 * The format object
	 */
	private DefaultTimeFormat _formatter;
	/**
	 * the format pattern
	 */
	private String _pattern, _fullPattern;
}

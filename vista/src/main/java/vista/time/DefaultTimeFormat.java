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

import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Formats date according to HEC DSS criteria. namely midnight is interpreted as
 * 2400 of the previous day.
 * 
 * @author Nicky Sandhu
 * @version $Id: DefaultTimeFormat.java,v 1.1 2003/10/02 20:49:35 redwood Exp $
 */
public class DefaultTimeFormat extends TimeFormat {
	/**
	 * creates a time format for the given pattern
	 */
	public TimeFormat create(String pattern) {
		return new DefaultTimeFormat(pattern);
	}

	/**
	 * Use pattern for US locale
	 */
	public DefaultTimeFormat() {
		this("ddMMMyyyy HHmm", Locale.US);
	}

	/**
	 * Use pattern for US locale
	 */
	public DefaultTimeFormat(String pattern) {
		this(pattern, Locale.US);
	}

	/**
	 * sets up formatting by given string and given locale. Time zone is assumed
	 * to be GMT. This does not have the daylight savings time adjustments.
	 */
	public DefaultTimeFormat(String pattern, Locale locale) {
		_pattern = pattern;
		_formatter = new SimpleDateFormat(pattern, locale);
		_formatter.setTimeZone(TimeZone.getTimeZone("GMT"));
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
		if (_pattern.indexOf("HH") > 0) {
			if (str.indexOf("0000") > 0) {
				Date d = new Date(date.getTime() - 60000);
				String str2 = _formatter.format(d);
				int pos = str2.indexOf("2359");
				if (pos != -1) {
					str = str2.substring(0, pos);
					str = str.concat("2400");
				}
			}
		}

		return new StringBuffer(str);
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
		return "Default Time Format: " + _pattern;
	}

	/**
	 * The format object
	 */
	private DateFormat _formatter;
	/**
	 * the format pattern
	 */
	private String _pattern;
}

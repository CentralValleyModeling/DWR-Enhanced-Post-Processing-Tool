/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package vista.db.jdbc.bdat;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import vista.set.Group;
import vista.set.GroupProxy;

@SuppressWarnings("serial")
public class BDATGroup extends GroupProxy
{
	public static final int MAX_LIMIT = 10000;
	static Logger logger = Logger.getLogger("vista.bdat");
	private BDATConnection manager;
	private boolean limited = false;

	public BDATGroup(BDATConnection connection)
	{
		manager = connection;
		setName("BDAT::" + manager.getDatabaseName() + "@"
				+ manager.getServerName() + ":" + manager.getPortNumber()
				+ " as " + manager.getUser());
	}

	@Override
	protected Group getInitializedGroup()
	{
		Group g = new Group();
		Connection connection = null;
		try
		{
			// FIXME: get password should be on manager ui
			connection = manager.getConnection();
			Statement statement = connection.createStatement();
			//FIXME: old statement and then new statement (duplicate rows in that view)
			String sql = "select * from emp_cms.result_detail_view where result_id in (select distinct(result_id) from emp_cms.result_detail_view)";
			/*
			sql = "select"
					+ " result_id, station_id, abbreviation, constituent_id, constituent_name, aggregate_id, aggregate_name, interval_id, interval_name, reading_type_id, reading_type_name, rank_id, rank_name, probe_depth, start_date, end_date from emp_cms.result_detail_view "
					+ " group by"
					+ " result_id, station_id, abbreviation, constituent_id, constituent_name, aggregate_id, aggregate_name, interval_id, interval_name, reading_type_id, reading_type_name, rank_id, rank_name, probe_depth, start_date, end_date";
					*/
			ResultSet resultSet = statement.executeQuery(sql);
			int count = 0;
			while(resultSet.next())
			{
				int resultId = resultSet.getInt("result_id");
				String abbreviation = resultSet.getString("abbreviation");
				String constituentName = resultSet
						.getString("constituent_name");
				String aggregateName = resultSet.getString("aggregate_name");
				String intervalName = resultSet.getString("interval_name");
				String readingTypeName = resultSet
						.getString("reading_type_name");
				String rankName = resultSet.getString("rank_name");
				String probeDepth = resultSet.getString("probe_depth");
				Date startDate = resultSet.getDate("start_date");
				Date endDate = resultSet.getDate("end_date");
				if(logger.isLoggable(Level.FINE))
				{
					logger.fine("Creating ref : " + resultId + ","
							+ abbreviation + "," + constituentName + ","
							+ startDate + "-" + endDate);
				}
				g.addDataReference(new BDATDataReference(manager, resultId,
						abbreviation, constituentName, aggregateName,
						intervalName, readingTypeName, rankName, probeDepth,
						startDate, endDate));
				count++;
				if(count >= MAX_LIMIT)
				{
					limited = true;
					break;
				}
			}
		}
		catch(SQLException e)
		{
			logger.log(Level.WARNING, "Error initializing Group: " + this, e);
		}
		finally
		{
			if(connection != null)
			{
				try
				{
					connection.close();
				}
				catch(SQLException e)
				{
					e.printStackTrace();
				}
			}
		}
		return g;
	}
}

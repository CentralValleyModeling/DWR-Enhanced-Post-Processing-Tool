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
package vista.set;

import vista.db.dss.DSSUtil;

/**
 * A utility class for setting and clearing flags. This involves bit
 * manipulation which is done using bitwise shift, and and or operations.
 *
 * @author Nicky Sandhu
 * @version $Id: FlagUtils.java,v 1.1 2003/10/02 20:49:23 redwood Exp $
 */
public class FlagUtils
{
	/**
	 * id for unscreened flag
	 */
	public static final int UNSCREENED_FLAG = 0;
	/**
	 * id for identifying ok flag
	 */
	public static final int OK_FLAG = 2;
	/**
	 * id for missing flag
	 */
	public static final int MISSING_FLAG = 4;
	/**
	 * id for questionable flag
	 */
	public static final int QUESTIONABLE_FLAG = 8;
	/**
	 * id for reject flag
	 */
	public static final int REJECT_FLAG = 16;
	/**
	 * ok flag filter
	 */
	public static final ElementFilter UNSCREENED_FILTER = new FlagFilter(
			FlagUtils.UNSCREENED_FLAG);
	/**
	 * ok flag filter
	 */
	public static final ElementFilter OK_FILTER = new FlagFilter(
			FlagUtils.OK_FLAG);
	/**
	 * missing flag filter
	 */
	public static final ElementFilter MISSING_FILTER = new FlagFilter(
			FlagUtils.MISSING_FLAG);
	/**
	 * reject flag filter
	 */
	public static final ElementFilter REJECT_FILTER = new FlagFilter(
			FlagUtils.REJECT_FLAG);
	/**
	 * questionable flag filter
	 */
	public static final ElementFilter QUESTIONABLE_FILTER = new FlagFilter(
			FlagUtils.QUESTIONABLE_FLAG);
	/**
	 * the screened bit #
	 */
	public static final int SCREENED_BIT = 1;
	/**
	 * the ok bit #
	 */
	public static final int OK_BIT = 2;
	/**
	 * the missing bit #
	 */
	public static final int MISSING_BIT = 3;
	/**
	 * the questionable bit #
	 */
	public static final int QUESTIONABLE_BIT = 4;
	/**
	 * the reject bit#
	 */
	public static final int REJECT_BIT = 5;
	/**
	 * the minimum bit used for user id, All bits between min and max user id
	 * bits are used for identifying users who may have set the flags.
	 */
	public static final int MIN_USER_ID_BIT = 25;
	/**
	 * the maximum bit used for user id
	 */
	public static final int MAX_USER_ID_BIT = 31;
	/**
	 * the range of current data bit
	 */
	private static final int RANGE_OF_CURRENT_DATA_BIT1 = 6;
	/**
	 *
	 */
	private static final int RANGE_OF_CURRENT_DATA_BIT2 = 7;
	/**
	 *
	 */
	private static final int CURRENT_VALUE_DIFFERENT_BIT = 8;

	/**
	 *
	 */
	public static final String getQualityFlagName(int flagId)
	{
		switch(flagId)
		{
			case FlagUtils.UNSCREENED_FLAG:
				return "UNSCREENED";
			case FlagUtils.QUESTIONABLE_FLAG:
				return "QUESTIONABLE";
			case FlagUtils.MISSING_FLAG:
				return "MISSING";
			case FlagUtils.REJECT_FLAG:
				return "REJECT";
			case FlagUtils.OK_FLAG:
				return "OK";
			default:
				throw new IllegalArgumentException("Incorrect flag Id: " + flagId);
		}
	}

	/**
	 *
	 */
	public static final int getQualityFlagId(String flagName)
	{
		if(flagName.equals("UNSCREENED"))
		{
			return FlagUtils.UNSCREENED_FLAG;
		}
		else if(flagName.equals("QUESTIONABLE"))
		{
			return FlagUtils.QUESTIONABLE_FLAG;
		}
		else if(flagName.equals("MISSING"))
		{
			return FlagUtils.MISSING_FLAG;
		}
		else if(flagName.equals("REJECT"))
		{
			return FlagUtils.REJECT_FLAG;
		}
		else if(flagName.equals("OK"))
		{
			return FlagUtils.OK_FLAG;
		}
		else
		{
			throw new IllegalArgumentException("Incorrect flag name: "
					+ flagName);
		}
	}

	/**
	 * checks if flag is screened
	 */
	public static final boolean isScreened(DataSetElement dse)
	{
		checkInput(dse);
		int flag = dse.getFlag();
		return (isBitSet(flag, SCREENED_BIT));
	}

	/**
	 * returns the id of the user if the flag has been marked else return null.
	 * The user id is stored in bits 31-25 and is the interpretation of bits
	 * 31-25 as 8-1 bit integer in the same order, i.e. bit 31 is the highest
	 * order bit and bit 25 is the lowest order bit. Bit 25 is mapped onto bit 1
	 * ( lowest order ) in the interpretation.
	 */
	public static final String getLastCheckedBy(DataSetElement dse)
	{
		checkInput(dse);
		int flag = 0;
		if(dse instanceof FlaggedDataSetElement)
		{
			flag = dse.getFlag();
		}
		else if(dse instanceof FlaggedTimeElement)
		{
			flag = dse.getFlag();
		}
		else
		{
			return null;
		}
		if(!isBitSet(flag, SCREENED_BIT))
		{
			return null;
		}
		int uId = getUserId(dse.getFlag());
		return DSSUtil.getUserName(uId);
	}

	/**
	 * returns the id of the user if the flag has been marked else return null.
	 * The user id is stored in bits 31-25 and is the interpretation of bits
	 * 31-25 as 8-1 bit integer in the same order, i.e. bit 31 is the highest
	 * order bit and bit 25 is the lowest order bit. Bit 25 is mapped onto bit 1
	 * ( lowest order ) in the interpretation.
	 */
	public static final String getLastCheckedBy(int flag)
	{
		if(!isBitSet(flag, SCREENED_BIT))
		{
			return null;
		}
		int uId = getUserId(flag);
		return DSSUtil.getUserName(uId);
	}

	/**
	 * gets the quality flag in the data set element. The
	 *
	 * @return one of OK_FLAG, MISSING_FLAG, QUESTIONABLE_FLAG, REJECT_FLAG
	 */
	public static final int getQualityFlag(DataSetElement dse)
	{
		// doesn't require input flag checking due to flag = 0 being
		// unscreened...
		int flag = dse.getFlag();
		//
		if(!isBitSet(flag, SCREENED_BIT))
		{
			return UNSCREENED_FLAG;
		}
		//
		if(isBitSet(flag, OK_BIT))
		{
			return OK_FLAG;
		}
		else if(isBitSet(flag, MISSING_BIT))
		{
			return MISSING_FLAG;
		}
		else if(isBitSet(flag, QUESTIONABLE_BIT))
		{
			return QUESTIONABLE_FLAG;
		}
		else if(isBitSet(flag, REJECT_BIT))
		{
			return REJECT_FLAG;
		}
		else
		{
			return UNSCREENED_FLAG; // can't recognize flag, send unscreened
		}
	}

	/**
	 * gets the quality flag from the flag integer value.
	 *
	 * @return one of OK_FLAG, MISSING_FLAG, QUESTIONABLE_FLAG, REJECT_FLAG
	 */
	public static final int getQualityFlag(int flag)
	{
		//
		if(!isBitSet(flag, SCREENED_BIT))
		{
			return UNSCREENED_FLAG;
		}
		//
		if(isBitSet(flag, OK_BIT))
		{
			return OK_FLAG;
		}
		else if(isBitSet(flag, MISSING_BIT))
		{
			return MISSING_FLAG;
		}
		else if(isBitSet(flag, QUESTIONABLE_BIT))
		{
			return QUESTIONABLE_FLAG;
		}
		else if(isBitSet(flag, REJECT_BIT))
		{
			return REJECT_FLAG;
		}
		else
		{
			return UNSCREENED_FLAG; // can't recognize flag, send unscreened
		}
	}

	/**
	 * @param dse      The data set element whose flag is to be set
	 * @param flagType the type of flag
	 * @see OK_FLAG, MISSING_FLAG, QUESTIONABLE_FLAG, REJECT_FLAG
	 */
	public static final void setQualityFlag(DataSetElement dse, int flagType,
											int uId)
	{
		checkInput(dse);
		checkFlagType(flagType);
		checkUserId(uId);
		int flag = dse.getFlag();
		flag = setFlagTypeAndUserId(flag, flagType, uId);
		dse.setFlag(flag);
	}

	public static int setFlagTypeAndUserId(int flag, int flagType, int uId)
	{
		// check to clear other flags
		for(int i = 2; i <= 5; i++)
		{
			flag = clearBit(flag, i);
		}
		//
		flag = setBit(flag, SCREENED_BIT);
		//
		switch(flagType)
		{
			case OK_FLAG:
				flag = setBit(flag, OK_BIT);
				break;
			case MISSING_FLAG:
				flag = setBit(flag, MISSING_BIT);
				break;
			case QUESTIONABLE_FLAG:
				flag = setBit(flag, QUESTIONABLE_BIT);
				break;
			case REJECT_FLAG:
				flag = setBit(flag, REJECT_BIT);
				break;
			default:
				throw new IllegalArgumentException("This should not happen...");
		}
		//
		flag = setUserId(flag, uId);
		return flag;
	}

	/**
	 * clears all flags
	 *
	 * @param dse The data set element whose flag is to be set
	 */
	public static final void clearAllFlags(DataSetElement dse, int uId)
	{
		checkInput(dse);
		checkUserId(uId);
		int flag = dse.getFlag();
		// check to clear other flags
		for(int i = 1; i <= 5; i++)
		{
			flag = clearBit(flag, i);
		}
		// clear out user names
		for(int i = MIN_USER_ID_BIT; i <= MAX_USER_ID_BIT; i++)
		{
			flag = clearBit(flag, i);
		}
		dse.setFlag(flag);
	}

	/**
	 * Checks if bit position is set in the integer
	 *
	 * @param n           The integer
	 * @param bitPosition position of bit in binary representation 1 thru 32 ( 1 is the
	 *                    least significant and 32 is the most
	 * @return true if bit is set
	 */
	public static final boolean isBitSet(int n, int bitPosition)
	{
		return (((n >> (bitPosition - 1)) & 0x0001) == 0x0001);
	}

	/**
	 * Sets bit position in a copy of the integer and returns it
	 *
	 * @param n           The integer
	 * @param bitPosition position of bit in binary representation 1 thru 32 ( 1 is the
	 *                    least significant and 32 is the most
	 * @return a copy of the integer with the bit set in that position
	 */
	public static final int setBit(int n, int bitPosition)
	{
		int number = n;
		int bitNumber = 1 << (bitPosition - 1);
		return (number | bitNumber);
	}

	/**
	 * Clears bit position in a copy of the integer and returns it
	 *
	 * @param n           The integer
	 * @param bitPosition position of bit in binary representation 1 thru 32 ( 1 is the
	 *                    least significant and 32 is the most
	 * @return a copy of the integer with the bit cleared in that position
	 */
	public static final int clearBit(int n, int bitPosition)
	{
		int number = n;
		int bitNumber = ~(1 << (bitPosition - 1));
		return (number & bitNumber);
	}

	/**
	 *
	 */
	private static final void checkInput(DataSetElement dse)
	{
		if(dse instanceof FlaggedDataSetElement
				|| dse instanceof FlaggedTimeElement)
		{
		}
		else
		{
			throw new IllegalArgumentException(
					"Flags not available in data set element");
		}
	}

	/**
	 * Interpret bits (25-31) as an integer id between 0 and 127.
	 */
	public static final int getUserId(int flag)
	{
		int uId = 0;
		int e = 1;
		for(int i = MIN_USER_ID_BIT; i <= MAX_USER_ID_BIT; i++)
		{
			if(isBitSet(flag, i))
			{
				uId += e;
			}
			e *= 2;
		}
		return uId;
	}

	/**
	 * Interpret bits (25-31) as an integer id between 0 and 127.
	 */
	public static final int setUserId(int flag, int uId)
	{
		int uIndex = MIN_USER_ID_BIT - 1;
		for(int i = MIN_USER_ID_BIT; i <= MAX_USER_ID_BIT; i++)
		{
			if(isBitSet(uId, i - uIndex))
			{
				flag = setBit(flag, i);
			}
			else
			{
				flag = clearBit(flag, i);
			}
		}
		return flag;
	}

	/**
	 *
	 */
	public static final void checkFlagType(int flagType)
	{
		if(flagType != OK_FLAG && flagType != MISSING_FLAG
				&& flagType != QUESTIONABLE_FLAG && flagType != REJECT_FLAG)
		{
			throw new IllegalArgumentException("Unrecognized flag type, "
					+ flagType);
		}
	}

	/**
	 * checks if user exists for given id number
	 */
	public static final void checkUserId(int uId)
	{
		try
		{
			DSSUtil.getUserName(uId);
		}
		catch(ArrayIndexOutOfBoundsException e)
		{
			throw new IllegalArgumentException("user Id=" + uId
					+ " is out of bounds");
		}
	}

	/**
	 * Converts from a string representation of a flag value to its
	 * integer stored value. A string representation of a flag is
	 * (UNSCREENED|MISSING|QUESTIONABLE|REJECT)"|"<username>
	 *
	 * @param flagString
	 * @return
	 */
	public static final int makeFlagValue(String flagString)
	{
		int flag = 0;
		String[] fields = flagString.split("\\|");
		if(fields == null || fields.length != 2)
		{
			throw new RuntimeException("Invalid flag value: " + flagString);
		}
		int qualityFlagId = FlagUtils.getQualityFlagId(fields[0]);
		int userId = DSSUtil.getUserId(fields[1].toLowerCase());
		FlaggedDataSetElement dse = new FlaggedDataSetElement();
		dse.setFlag(0);
		if(qualityFlagId == 0)
		{
			FlagUtils.clearAllFlags(dse, userId);
		}
		else
		{
			FlagUtils.setQualityFlag(dse, qualityFlagId, userId);
		}
		return dse.getFlag();
	}
}

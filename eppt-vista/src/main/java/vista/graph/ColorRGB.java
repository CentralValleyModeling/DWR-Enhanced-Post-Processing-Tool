/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Color;
import java.util.Properties;
import java.util.StringTokenizer;

/**
 * Defines the rgb representation of colors by their names. E.g. the color
 * "linen" is r=250,g=240,b=230.
 * 
 * @author Nicky Sandhu
 * @version $Id: ColorRGB.java,v 1.1 2003/10/02 20:48:50 redwood Exp $
 */
public class ColorRGB {
	/**
	 * loads name to rgb mapping from file.
	 */
	public ColorRGB() {
		try {
			java.io.InputStream is = Object.class
					.getResourceAsStream("/vista/graph/rgb.properties");
			_colorDefinitions.load(is);
		} catch (java.io.IOException ioe) {
			System.out.println("Color Definitions file " + "rgb.properties"
					+ " not found");
			createDefaultProperties();
		}
	}

	/**
	 * loads name to rgb mapping from file.
	 */
	public ColorRGB(String file) {
		try {
			java.io.InputStream is = new java.io.FileInputStream(file);
			_colorDefinitions.load(is);
		} catch (java.io.IOException ioe) {
			System.out.println("Color Definitions file " + file
					+ " not found: Using defaults");
			createDefaultProperties();
		}
	}

	/**
	 * returns an array of available colors
	 */
	public String[] getAvailableColors() {
		String[] keyNames = new String[_colorDefinitions.size()];
		int count = 0;
		for (java.util.Enumeration keys = _colorDefinitions.keys(); keys
				.hasMoreElements(); count++) {
			keyNames[count] = (String) keys.nextElement();
		}
		return keyNames;
	}

	/**
	 * returns the color as a rgb string.
	 */
	public final String getRGBString(String colorName) {
		return _colorDefinitions.getProperty(colorName);
	}

	/**
	 * creates color
	 */
	public final Color createColor(String colorName) {
		if (colorName != null)
			return parseColorProperty(colorName);
		else
			return Color.black;
	}

	/**
	 * parses string to get color. The string either contains the Color default
	 * toString representation or the name of the color as defined in the
	 * rgb.properties file.
	 */
	public final Color parseColorProperty(String colorProperty) {
		StringTokenizer chopper;
		String rgbString = "";
		if (colorProperty.indexOf("[") == -1) {
			rgbString = _colorDefinitions.getProperty(colorProperty);
			if (DEBUG)
				_colorDefinitions.list(System.out);
		} else {
			chopper = new StringTokenizer(colorProperty, "[");
			chopper.nextToken();
			rgbString = chopper.nextToken();
			rgbString = rgbString.substring(0, rgbString.indexOf("]"));
		}
		if (rgbString == null)
			return new Color(0, 0, 0);
		if (DEBUG)
			System.out.println("RGB : " + rgbString);

		chopper = new StringTokenizer(rgbString, ",");
		String colorComponent = chopper.nextToken().trim();
		if (DEBUG)
			System.out.println(colorComponent);
		int red = new Integer(colorComponent.trim().substring(
				colorComponent.indexOf("=") + 1)).intValue();
		colorComponent = chopper.nextToken().trim();
		if (DEBUG)
			System.out.println(colorComponent);
		int green = new Integer(colorComponent.substring(colorComponent
				.indexOf("=") + 1)).intValue();
		colorComponent = chopper.nextToken().trim();
		if (DEBUG)
			System.out.println(colorComponent);
		int blue = new Integer(colorComponent.substring(colorComponent
				.indexOf("=") + 1)).intValue();
		if (DEBUG)
			System.out.println("r = " + red + " g = " + green + " b = " + blue);

		return new Color(red, green, blue);
	}

	/**
	 * creates the default name to rgb mapping.
	 */
	private void createDefaultProperties() {
		_colorDefinitions.put("snow", "r=255,g=250,b=250");
		_colorDefinitions.put("ghost_white", "r=248,g=248,b=255");
		_colorDefinitions.put("GhostWhite", "r=248,g=248,b=255");
		_colorDefinitions.put("white_smoke", "r=245,g=245,b=245");
		_colorDefinitions.put("WhiteSmoke", "r=245,g=245,b=245");
		_colorDefinitions.put("gainsboro", "r=220,g=220,b=220");
		_colorDefinitions.put("floral_white", "r=255,g=250,b=240");
		_colorDefinitions.put("FloralWhite", "r=255,g=250,b=240");
		_colorDefinitions.put("old_lace", "r=253,g=245,b=230");
		_colorDefinitions.put("OldLace", "r=253,g=245,b=230");
		_colorDefinitions.put("linen", "r=250,g=240,b=230");
		_colorDefinitions.put("antique_white", "r=250,g=235,b=215");
		_colorDefinitions.put("AntiqueWhite", "r=250,g=235,b=215");
		_colorDefinitions.put("papaya_whip", "r=255,g=239,b=213");
		_colorDefinitions.put("PapayaWhip", "r=255,g=239,b=213");
		_colorDefinitions.put("blanched_almond", "r=255,g=235,b=205");
		_colorDefinitions.put("BlanchedAlmond", "r=255,g=235,b=205");
		_colorDefinitions.put("bisque", "r=255,g=228,b=196");
		_colorDefinitions.put("peach_puff", "r=255,g=218,b=185");
		_colorDefinitions.put("PeachPuff", "r=255,g=218,b=185");
		_colorDefinitions.put("navajo_white", "r=255,g=222,b=173");
		_colorDefinitions.put("NavajoWhite", "r=255,g=222,b=173");
		_colorDefinitions.put("moccasin", "r=255,g=228,b=181");
		_colorDefinitions.put("cornsilk", "r=255,g=248,b=220");
		_colorDefinitions.put("ivory", "r=255,g=255,b=240");
		_colorDefinitions.put("lemon_chiffon", "r=255,g=250,b=205");
		_colorDefinitions.put("LemonChiffon", "r=255,g=250,b=205");
		_colorDefinitions.put("seashell", "r=255,g=245,b=238");
		_colorDefinitions.put("honeydew", "r=240,g=255,b=240");
		_colorDefinitions.put("mint_cream", "r=245,g=255,b=250");
		_colorDefinitions.put("MintCream", "r=245,g=255,b=250");
		_colorDefinitions.put("azure", "r=240,g=255,b=255");
		_colorDefinitions.put("alice_blue", "r=240,g=248,b=255");
		_colorDefinitions.put("AliceBlue", "r=240,g=248,b=255");
		_colorDefinitions.put("lavender", "r=230,g=230,b=250");
		_colorDefinitions.put("lavender_blush", "r=255,g=240,b=245");
		_colorDefinitions.put("LavenderBlush", "r=255,g=240,b=245");
		_colorDefinitions.put("misty_rose", "r=255,g=228,b=225");
		_colorDefinitions.put("MistyRose", "r=255,g=228,b=225");
		_colorDefinitions.put("white", "r=255,g=255,b=255");
		_colorDefinitions.put("black", "r=0,g=0,b=0  ");
		_colorDefinitions.put("dark_slate_gray", "r=47,g=79,b=79 ");
		_colorDefinitions.put("DarkSlateGray", "r=47,g=79,b=79 ");
		_colorDefinitions.put("dark_slate_grey", "r=47,g=79,b=79 ");
		_colorDefinitions.put("DarkSlateGrey", "r=47,g=79,b=79 ");
		_colorDefinitions.put("dim_gray", "r=105,g=105,b=105");
		_colorDefinitions.put("DimGray", "r=105,g=105,b=105");
		_colorDefinitions.put("dim_grey", "r=105,g=105,b=105");
		_colorDefinitions.put("DimGrey", "r=105,g=105,b=105");
		_colorDefinitions.put("slate_gray", "r=112,g=128,b=144");
		_colorDefinitions.put("SlateGray", "r=112,g=128,b=144");
		_colorDefinitions.put("slate_grey", "r=112,g=128,b=144");
		_colorDefinitions.put("SlateGrey", "r=112,g=128,b=144");
		_colorDefinitions.put("light_slate_gray", "r=119,g=136,b=153");
		_colorDefinitions.put("LightSlateGray", "r=119,g=136,b=153");
		_colorDefinitions.put("light_slate_grey", "r=119,g=136,b=153");
		_colorDefinitions.put("LightSlateGrey", "r=119,g=136,b=153");
		_colorDefinitions.put("gray", "r=190,g=190,b=190");
		_colorDefinitions.put("grey", "r=190,g=190,b=190");
		_colorDefinitions.put("light_grey", "r=211,g=211,b=211");
		_colorDefinitions.put("LightGrey", "r=211,g=211,b=211");
		_colorDefinitions.put("light_gray", "r=211,g=211,b=211");
		_colorDefinitions.put("LightGray", "r=211,g=211,b=211");
		_colorDefinitions.put("midnight_blue", "r=25,g=25,b=112");
		_colorDefinitions.put("MidnightBlue", "r=25,g=25,b=112");
		_colorDefinitions.put("navy", "r=0,g=0,b=128");
		_colorDefinitions.put("navy_blue", "r=0,g=0,b=128");
		_colorDefinitions.put("NavyBlue", "r=0,g=0,b=128");
		_colorDefinitions.put("cornflower_blue", "r=100,g=149,b=237");
		_colorDefinitions.put("CornflowerBlue", "r=100,g=149,b=237");
		_colorDefinitions.put("dark_slate_blue", "r=72,g=61,b=139");
		_colorDefinitions.put("DarkSlateBlue", "r=72,g=61,b=139");
		_colorDefinitions.put("slate_blue", "r=106,g=90,b=205");
		_colorDefinitions.put("SlateBlue", "r=106,g=90,b=205");
		_colorDefinitions.put("medium_slate_blue", "r=123,g=104,b=238");
		_colorDefinitions.put("MediumSlateBlue", "r=123,g=104,b=238");
		_colorDefinitions.put("light_slate_blue", "r=132,g=112,b=255");
		_colorDefinitions.put("LightSlateBlue", "r=132,g=112,b=255");
		_colorDefinitions.put("medium_blue", "r=0,g=0,b=205");
		_colorDefinitions.put("MediumBlue", "r=0,g=0,b=205");
		_colorDefinitions.put("royal_blue", "r=65,g=105,b=225");
		_colorDefinitions.put("RoyalBlue", "r=65,g=105,b=225");
		_colorDefinitions.put("blue", "r=0,g=0,b=255");
		_colorDefinitions.put("dodger_blue", "r=30,g=144,b=255");
		_colorDefinitions.put("DodgerBlue", "r=30,g=144,b=255");
		_colorDefinitions.put("deep_sky_blue", "r=0,g=191,b=255");
		_colorDefinitions.put("DeepSkyBlue", "r=0,g=191,b=255");
		_colorDefinitions.put("sky_blue", "r=135,g=206,b=235");
		_colorDefinitions.put("SkyBlue", "r=135,g=206,b=235");
		_colorDefinitions.put("light_sky_blue", "r=135,g=206,b=250");
		_colorDefinitions.put("LightSkyBlue", "r=135,g=206,b=250");
		_colorDefinitions.put("steel_blue", "r=70,g=130,b=180");
		_colorDefinitions.put("SteelBlue", "r=70,g=130,b=180");
		_colorDefinitions.put("light_steel_blue", "r=176,g=196,b=222");
		_colorDefinitions.put("LightSteelBlue", "r=176,g=196,b=222");
		_colorDefinitions.put("light_blue", "r=173,g=216,b=230");
		_colorDefinitions.put("LightBlue", "r=173,g=216,b=230");
		_colorDefinitions.put("powder_blue", "r=176,g=224,b=230");
		_colorDefinitions.put("PowderBlue", "r=176,g=224,b=230");
		_colorDefinitions.put("pale_turquoise", "r=175,g=238,b=238");
		_colorDefinitions.put("PaleTurquoise", "r=175,g=238,b=238");
		_colorDefinitions.put("dark_turquoise", "r=0,g=206,b=209");
		_colorDefinitions.put("DarkTurquoise", "r=0,g=206,b=209");
		_colorDefinitions.put("medium_turquoise", "r=72,g=209,b=204");
		_colorDefinitions.put("MediumTurquoise", "r=72,g=209,b=204");
		_colorDefinitions.put("turquoise", "r=64,g=224,b=208");
		_colorDefinitions.put("cyan", "r=0,g=255,b=255");
		_colorDefinitions.put("light_cyan", "r=224,g=255,b=255");
		_colorDefinitions.put("LightCyan", "r=224,g=255,b=255");
		_colorDefinitions.put("cadet_blue", "r=95,g=158,b=160");
		_colorDefinitions.put("CadetBlue", "r=95,g=158,b=160");
		_colorDefinitions.put("medium_aquamarine", "r=102,g=205,b=170");
		_colorDefinitions.put("MediumAquamarine", "r=102,g=205,b=170");
		_colorDefinitions.put("aquamarine", "r=127,g=255,b=212");
		_colorDefinitions.put("dark_green", "r=0,g=100,b=0  ");
		_colorDefinitions.put("DarkGreen", "r=0,g=100,b=0  ");
		_colorDefinitions.put("dark_olive_green", "r=85,g=107,b=47 ");
		_colorDefinitions.put("DarkOliveGreen", "r=85,g=107,b=47 ");
		_colorDefinitions.put("dark_sea_green", "r=143,g=188,b=143");
		_colorDefinitions.put("DarkSeaGreen", "r=143,g=188,b=143");
		_colorDefinitions.put("sea_green", "r=46,g=139,b=87 ");
		_colorDefinitions.put("SeaGreen", "r=46,g=139,b=87 ");
		_colorDefinitions.put("medium_sea_green", "r=60,g=179,b=113");
		_colorDefinitions.put("MediumSeaGreen", "r=60,g=179,b=113");
		_colorDefinitions.put("light_sea_green", "r=32,g=178,b=170");
		_colorDefinitions.put("LightSeaGreen", "r=32,g=178,b=170");
		_colorDefinitions.put("pale_green", "r=152,g=251,b=152");
		_colorDefinitions.put("PaleGreen", "r=152,g=251,b=152");
		_colorDefinitions.put("spring_green", "r=0,g=255,b=127");
		_colorDefinitions.put("SpringGreen", "r=0,g=255,b=127");
		_colorDefinitions.put("lawn_green", "r=124,g=252,b=0  ");
		_colorDefinitions.put("LawnGreen", "r=124,g=252,b=0  ");
		_colorDefinitions.put("green", "r=0,g=255,b=0  ");
		_colorDefinitions.put("chartreuse", "r=127,g=255,b=0  ");
		_colorDefinitions.put("medium_spring_green", "r=0,g=250,b=154");
		_colorDefinitions.put("MediumSpringGreen", "r=0,g=250,b=154");
		_colorDefinitions.put("green_yellow", "r=173,g=255,b=47 ");
		_colorDefinitions.put("GreenYellow", "r=173,g=255,b=47 ");
		_colorDefinitions.put("lime_green", "r=50,g=205,b=50 ");
		_colorDefinitions.put("LimeGreen", "r=50,g=205,b=50 ");
		_colorDefinitions.put("yellow_green", "r=154,g=205,b=50 ");
		_colorDefinitions.put("YellowGreen", "r=154,g=205,b=50 ");
		_colorDefinitions.put("forest_green", "r=34,g=139,b=34 ");
		_colorDefinitions.put("ForestGreen", "r=34,g=139,b=34 ");
		_colorDefinitions.put("olive_drab", "r=107,g=142,b=35 ");
		_colorDefinitions.put("OliveDrab", "r=107,g=142,b=35 ");
		_colorDefinitions.put("dark_khaki", "r=189,g=183,b=107");
		_colorDefinitions.put("DarkKhaki", "r=189,g=183,b=107");
		_colorDefinitions.put("khaki", "r=240,g=230,b=140");
		_colorDefinitions.put("pale_goldenrod", "r=238,g=232,b=170");
		_colorDefinitions.put("PaleGoldenrod", "r=238,g=232,b=170");
		_colorDefinitions.put("light_goldenrod_yellow", "r=250,g=250,b=210");
		_colorDefinitions.put("LightGoldenrodYellow", "r=250,g=250,b=210");
		_colorDefinitions.put("light_yellow", "r=255,g=255,b=224");
		_colorDefinitions.put("LightYellow", "r=255,g=255,b=224");
		_colorDefinitions.put("yellow", "r=255,g=255,b=0  ");
		_colorDefinitions.put("gold", "r=255,g=215,b=0  ");
		_colorDefinitions.put("light_goldenrod", "r=238,g=221,b=130");
		_colorDefinitions.put("LightGoldenrod", "r=238,g=221,b=130");
		_colorDefinitions.put("goldenrod", "r=218,g=165,b=32 ");
		_colorDefinitions.put("dark_goldenrod", "r=184,g=134,b=11 ");
		_colorDefinitions.put("DarkGoldenrod", "r=184,g=134,b=11 ");
		_colorDefinitions.put("rosy_brown", "r=188,g=143,b=143");
		_colorDefinitions.put("RosyBrown", "r=188,g=143,b=143");
		_colorDefinitions.put("indian_red", "r=205,g=92,b=92 ");
		_colorDefinitions.put("IndianRed", "r=205,g=92,b=92 ");
		_colorDefinitions.put("saddle_brown", "r=139,g=69,b=19 ");
		_colorDefinitions.put("SaddleBrown", "r=139,g=69,b=19 ");
		_colorDefinitions.put("sienna", "r=160,g=82,b=45 ");
		_colorDefinitions.put("peru", "r=205,g=133,b=63 ");
		_colorDefinitions.put("burlywood", "r=222,g=184,b=135");
		_colorDefinitions.put("beige", "r=245,g=245,b=220");
		_colorDefinitions.put("wheat", "r=245,g=222,b=179");
		_colorDefinitions.put("sandy_brown", "r=244,g=164,b=96 ");
		_colorDefinitions.put("SandyBrown", "r=244,g=164,b=96 ");
		_colorDefinitions.put("tan", "r=210,g=180,b=140");
		_colorDefinitions.put("chocolate", "r=210,g=105,b=30 ");
		_colorDefinitions.put("firebrick", "r=178,g=34,b=34 ");
		_colorDefinitions.put("brown", "r=165,g=42,b=42 ");
		_colorDefinitions.put("dark_salmon", "r=233,g=150,b=122");
		_colorDefinitions.put("DarkSalmon", "r=233,g=150,b=122");
		_colorDefinitions.put("salmon", "r=250,g=128,b=114");
		_colorDefinitions.put("light_salmon", "r=255,g=160,b=122");
		_colorDefinitions.put("LightSalmon", "r=255,g=160,b=122");
		_colorDefinitions.put("orange", "r=255,g=165,b=0  ");
		_colorDefinitions.put("dark_orange", "r=255,g=140,b=0  ");
		_colorDefinitions.put("DarkOrange", "r=255,g=140,b=0  ");
		_colorDefinitions.put("coral", "r=255,g=127,b=80 ");
		_colorDefinitions.put("light_coral", "r=240,g=128,b=128");
		_colorDefinitions.put("LightCoral", "r=240,g=128,b=128");
		_colorDefinitions.put("tomato", "r=255,g=99,b=71 ");
		_colorDefinitions.put("orange_red", "r=255,g=69,b=0  ");
		_colorDefinitions.put("OrangeRed", "r=255,g=69,b=0  ");
		_colorDefinitions.put("red", "r=255,g=0,b=0  ");
		_colorDefinitions.put("hot_pink", "r=255,g=105,b=180");
		_colorDefinitions.put("HotPink", "r=255,g=105,b=180");
		_colorDefinitions.put("deep_pink", "r=255,g=20,b=147");
		_colorDefinitions.put("DeepPink", "r=255,g=20,b=147");
		_colorDefinitions.put("pink", "r=255,g=192,b=203");
		_colorDefinitions.put("light_pink", "r=255,g=182,b=193");
		_colorDefinitions.put("LightPink", "r=255,g=182,b=193");
		_colorDefinitions.put("pale_violet_red", "r=219,g=112,b=147");
		_colorDefinitions.put("PaleVioletRed", "r=219,g=112,b=147");
		_colorDefinitions.put("maroon", "r=176,g=48,b=96 ");
		_colorDefinitions.put("medium_violet_red", "r=199,g=21,b=133");
		_colorDefinitions.put("MediumVioletRed", "r=199,g=21,b=133");
		_colorDefinitions.put("violet_red", "r=208,g=32,b=144");
		_colorDefinitions.put("VioletRed", "r=208,g=32,b=144");
		_colorDefinitions.put("magenta", "r=255,g=0,b=255");
		_colorDefinitions.put("violet", "r=238,g=130,b=238");
		_colorDefinitions.put("plum", "r=221,g=160,b=221");
		_colorDefinitions.put("orchid", "r=218,g=112,b=214");
		_colorDefinitions.put("medium_orchid", "r=186,g=85,b=211");
		_colorDefinitions.put("MediumOrchid", "r=186,g=85,b=211");
		_colorDefinitions.put("dark_orchid", "r=153,g=50,b=204");
		_colorDefinitions.put("DarkOrchid", "r=153,g=50,b=204");
		_colorDefinitions.put("dark_violet", "r=148,g=0,b=211");
		_colorDefinitions.put("DarkViolet", "r=148,g=0,b=211");
		_colorDefinitions.put("blue_violet", "r=138,g=43,b=226");
		_colorDefinitions.put("BlueViolet", "r=138,g=43,b=226");
		_colorDefinitions.put("purple", "r=160,g=32,b=240");
		_colorDefinitions.put("medium_purple", "r=147,g=112,b=219");
		_colorDefinitions.put("MediumPurple", "r=147,g=112,b=219");
		_colorDefinitions.put("thistle", "r=216,g=191,b=216");
		_colorDefinitions.put("snow1", "r=255,g=250,b=250");
		_colorDefinitions.put("snow2", "r=238,g=233,b=233");
		_colorDefinitions.put("snow3", "r=205,g=201,b=201");
		_colorDefinitions.put("snow4", "r=139,g=137,b=137");
		_colorDefinitions.put("seashell1", "r=255,g=245,b=238");
		_colorDefinitions.put("seashell2", "r=238,g=229,b=222");
		_colorDefinitions.put("seashell3", "r=205,g=197,b=191");
		_colorDefinitions.put("seashell4", "r=139,g=134,b=130");
		_colorDefinitions.put("AntiqueWhite1", "r=255,g=239,b=219");
		_colorDefinitions.put("AntiqueWhite2", "r=238,g=223,b=204");
		_colorDefinitions.put("AntiqueWhite3", "r=205,g=192,b=176");
		_colorDefinitions.put("AntiqueWhite4", "r=139,g=131,b=120");
		_colorDefinitions.put("bisque1", "r=255,g=228,b=196");
		_colorDefinitions.put("bisque2", "r=238,g=213,b=183");
		_colorDefinitions.put("bisque3", "r=205,g=183,b=158");
		_colorDefinitions.put("bisque4", "r=139,g=125,b=107");
		_colorDefinitions.put("PeachPuff1", "r=255,g=218,b=185");
		_colorDefinitions.put("PeachPuff2", "r=238,g=203,b=173");
		_colorDefinitions.put("PeachPuff3", "r=205,g=175,b=149");
		_colorDefinitions.put("PeachPuff4", "r=139,g=119,b=101");
		_colorDefinitions.put("NavajoWhite1", "r=255,g=222,b=173");
		_colorDefinitions.put("NavajoWhite2", "r=238,g=207,b=161");
		_colorDefinitions.put("NavajoWhite3", "r=205,g=179,b=139");
		_colorDefinitions.put("NavajoWhite4", "r=139,g=121,b=94 ");
		_colorDefinitions.put("LemonChiffon1", "r=255,g=250,b=205");
		_colorDefinitions.put("LemonChiffon2", "r=238,g=233,b=191");
		_colorDefinitions.put("LemonChiffon3", "r=205,g=201,b=165");
		_colorDefinitions.put("LemonChiffon4", "r=139,g=137,b=112");
		_colorDefinitions.put("cornsilk1", "r=255,g=248,b=220");
		_colorDefinitions.put("cornsilk2", "r=238,g=232,b=205");
		_colorDefinitions.put("cornsilk3", "r=205,g=200,b=177");
		_colorDefinitions.put("cornsilk4", "r=139,g=136,b=120");
		_colorDefinitions.put("ivory1", "r=255,g=255,b=240");
		_colorDefinitions.put("ivory2", "r=238,g=238,b=224");
		_colorDefinitions.put("ivory3", "r=205,g=205,b=193");
		_colorDefinitions.put("ivory4", "r=139,g=139,b=131");
		_colorDefinitions.put("honeydew1", "r=240,g=255,b=240");
		_colorDefinitions.put("honeydew2", "r=224,g=238,b=224");
		_colorDefinitions.put("honeydew3", "r=193,g=205,b=193");
		_colorDefinitions.put("honeydew4", "r=131,g=139,b=131");
		_colorDefinitions.put("LavenderBlush1", "r=255,g=240,b=245");
		_colorDefinitions.put("LavenderBlush2", "r=238,g=224,b=229");
		_colorDefinitions.put("LavenderBlush3", "r=205,g=193,b=197");
		_colorDefinitions.put("LavenderBlush4", "r=139,g=131,b=134");
		_colorDefinitions.put("MistyRose1", "r=255,g=228,b=225");
		_colorDefinitions.put("MistyRose2", "r=238,g=213,b=210");
		_colorDefinitions.put("MistyRose3", "r=205,g=183,b=181");
		_colorDefinitions.put("MistyRose4", "r=139,g=125,b=123");
		_colorDefinitions.put("azure1", "r=240,g=255,b=255");
		_colorDefinitions.put("azure2", "r=224,g=238,b=238");
		_colorDefinitions.put("azure3", "r=193,g=205,b=205");
		_colorDefinitions.put("azure4", "r=131,g=139,b=139");
		_colorDefinitions.put("SlateBlue1", "r=131,g=111,b=255");
		_colorDefinitions.put("SlateBlue2", "r=122,g=103,b=238");
		_colorDefinitions.put("SlateBlue3", "r=105,g=89,b=205");
		_colorDefinitions.put("SlateBlue4", "r=71,g=60,b=139");
		_colorDefinitions.put("RoyalBlue1", "r=72,g=118,b=255");
		_colorDefinitions.put("RoyalBlue2", "r=67,g=110,b=238");
		_colorDefinitions.put("RoyalBlue3", "r=58,g=95,b=205");
		_colorDefinitions.put("RoyalBlue4", "r=39,g=64,b=139");
		_colorDefinitions.put("blue1", "r=0,g=0,b=255");
		_colorDefinitions.put("blue2", "r=0,g=0,b=238");
		_colorDefinitions.put("blue3", "r=0,g=0,b=205");
		_colorDefinitions.put("blue4", "r=0,g=0,b=139");
		_colorDefinitions.put("DodgerBlue1", "r=30,g=144,b=255");
		_colorDefinitions.put("DodgerBlue2", "r=28,g=134,b=238");
		_colorDefinitions.put("DodgerBlue3", "r=24,g=116,b=205");
		_colorDefinitions.put("DodgerBlue4", "r=16,g=78,b=139");
		_colorDefinitions.put("SteelBlue1", "r=99,g=184,b=255");
		_colorDefinitions.put("SteelBlue2", "r=92,g=172,b=238");
		_colorDefinitions.put("SteelBlue3", "r=79,g=148,b=205");
		_colorDefinitions.put("SteelBlue4", "r=54,g=100,b=139");
		_colorDefinitions.put("DeepSkyBlue1", "r=0,g=191,b=255");
		_colorDefinitions.put("DeepSkyBlue2", "r=0,g=178,b=238");
		_colorDefinitions.put("DeepSkyBlue3", "r=0,g=154,b=205");
		_colorDefinitions.put("DeepSkyBlue4", "r=0,g=104,b=139");
		_colorDefinitions.put("SkyBlue1", "r=135,g=206,b=255");
		_colorDefinitions.put("SkyBlue2", "r=126,g=192,b=238");
		_colorDefinitions.put("SkyBlue3", "r=108,g=166,b=205");
		_colorDefinitions.put("SkyBlue4", "r=74,g=112,b=139");
		_colorDefinitions.put("LightSkyBlue1", "r=176,g=226,b=255");
		_colorDefinitions.put("LightSkyBlue2", "r=164,g=211,b=238");
		_colorDefinitions.put("LightSkyBlue3", "r=141,g=182,b=205");
		_colorDefinitions.put("LightSkyBlue4", "r=96,g=123,b=139");
		_colorDefinitions.put("SlateGray1", "r=198,g=226,b=255");
		_colorDefinitions.put("SlateGray2", "r=185,g=211,b=238");
		_colorDefinitions.put("SlateGray3", "r=159,g=182,b=205");
		_colorDefinitions.put("SlateGray4", "r=108,g=123,b=139");
		_colorDefinitions.put("LightSteelBlue1", "r=202,g=225,b=255");
		_colorDefinitions.put("LightSteelBlue2", "r=188,g=210,b=238");
		_colorDefinitions.put("LightSteelBlue3", "r=162,g=181,b=205");
		_colorDefinitions.put("LightSteelBlue4", "r=110,g=123,b=139");
		_colorDefinitions.put("LightBlue1", "r=191,g=239,b=255");
		_colorDefinitions.put("LightBlue2", "r=178,g=223,b=238");
		_colorDefinitions.put("LightBlue3", "r=154,g=192,b=205");
		_colorDefinitions.put("LightBlue4", "r=104,g=131,b=139");
		_colorDefinitions.put("LightCyan1", "r=224,g=255,b=255");
		_colorDefinitions.put("LightCyan2", "r=209,g=238,b=238");
		_colorDefinitions.put("LightCyan3", "r=180,g=205,b=205");
		_colorDefinitions.put("LightCyan4", "r=122,g=139,b=139");
		_colorDefinitions.put("PaleTurquoise1", "r=187,g=255,b=255");
		_colorDefinitions.put("PaleTurquoise2", "r=174,g=238,b=238");
		_colorDefinitions.put("PaleTurquoise3", "r=150,g=205,b=205");
		_colorDefinitions.put("PaleTurquoise4", "r=102,g=139,b=139");
		_colorDefinitions.put("CadetBlue1", "r=152,g=245,b=255");
		_colorDefinitions.put("CadetBlue2", "r=142,g=229,b=238");
		_colorDefinitions.put("CadetBlue3", "r=122,g=197,b=205");
		_colorDefinitions.put("CadetBlue4", "r=83,g=134,b=139");
		_colorDefinitions.put("turquoise1", "r=0,g=245,b=255");
		_colorDefinitions.put("turquoise2", "r=0,g=229,b=238");
		_colorDefinitions.put("turquoise3", "r=0,g=197,b=205");
		_colorDefinitions.put("turquoise4", "r=0,g=134,b=139");
		_colorDefinitions.put("cyan1", "r=0,g=255,b=255");
		_colorDefinitions.put("cyan2", "r=0,g=238,b=238");
		_colorDefinitions.put("cyan3", "r=0,g=205,b=205");
		_colorDefinitions.put("cyan4", "r=0,g=139,b=139");
		_colorDefinitions.put("DarkSlateGray1", "r=151,g=255,b=255");
		_colorDefinitions.put("DarkSlateGray2", "r=141,g=238,b=238");
		_colorDefinitions.put("DarkSlateGray3", "r=121,g=205,b=205");
		_colorDefinitions.put("DarkSlateGray4", "r=82,g=139,b=139");
		_colorDefinitions.put("aquamarine1", "r=127,g=255,b=212");
		_colorDefinitions.put("aquamarine2", "r=118,g=238,b=198");
		_colorDefinitions.put("aquamarine3", "r=102,g=205,b=170");
		_colorDefinitions.put("aquamarine4", "r=69,g=139,b=116");
		_colorDefinitions.put("DarkSeaGreen1", "r=193,g=255,b=193");
		_colorDefinitions.put("DarkSeaGreen2", "r=180,g=238,b=180");
		_colorDefinitions.put("DarkSeaGreen3", "r=155,g=205,b=155");
		_colorDefinitions.put("DarkSeaGreen4", "r=105,g=139,b=105");
		_colorDefinitions.put("SeaGreen1", "r=84,g=255,b=159");
		_colorDefinitions.put("SeaGreen2", "r=78,g=238,b=148");
		_colorDefinitions.put("SeaGreen3", "r=67,g=205,b=128");
		_colorDefinitions.put("SeaGreen4", "r=46,g=139,b=87 ");
		_colorDefinitions.put("PaleGreen1", "r=154,g=255,b=154");
		_colorDefinitions.put("PaleGreen2", "r=144,g=238,b=144");
		_colorDefinitions.put("PaleGreen3", "r=124,g=205,b=124");
		_colorDefinitions.put("PaleGreen4", "r=84,g=139,b=84 ");
		_colorDefinitions.put("SpringGreen1", "r=0,g=255,b=127");
		_colorDefinitions.put("SpringGreen2", "r=0,g=238,b=118");
		_colorDefinitions.put("SpringGreen3", "r=0,g=205,b=102");
		_colorDefinitions.put("SpringGreen4", "r=0,g=139,b=69 ");
		_colorDefinitions.put("green1", "r=0,g=255,b=0  ");
		_colorDefinitions.put("green2", "r=0,g=238,b=0  ");
		_colorDefinitions.put("green3", "r=0,g=205,b=0  ");
		_colorDefinitions.put("green4", "r=0,g=139,b=0  ");
		_colorDefinitions.put("chartreuse1", "r=127,g=255,b=0  ");
		_colorDefinitions.put("chartreuse2", "r=118,g=238,b=0  ");
		_colorDefinitions.put("chartreuse3", "r=102,g=205,b=0  ");
		_colorDefinitions.put("chartreuse4", "r=69,g=139,b=0  ");
		_colorDefinitions.put("OliveDrab1", "r=192,g=255,b=62 ");
		_colorDefinitions.put("OliveDrab2", "r=179,g=238,b=58 ");
		_colorDefinitions.put("OliveDrab3", "r=154,g=205,b=50 ");
		_colorDefinitions.put("OliveDrab4", "r=105,g=139,b=34 ");
		_colorDefinitions.put("DarkOliveGreen1", "r=202,g=255,b=112");
		_colorDefinitions.put("DarkOliveGreen2", "r=188,g=238,b=104");
		_colorDefinitions.put("DarkOliveGreen3", "r=162,g=205,b=90 ");
		_colorDefinitions.put("DarkOliveGreen4", "r=110,g=139,b=61 ");
		_colorDefinitions.put("khaki1", "r=255,g=246,b=143");
		_colorDefinitions.put("khaki2", "r=238,g=230,b=133");
		_colorDefinitions.put("khaki3", "r=205,g=198,b=115");
		_colorDefinitions.put("khaki4", "r=139,g=134,b=78 ");
		_colorDefinitions.put("LightGoldenrod1", "r=255,g=236,b=139");
		_colorDefinitions.put("LightGoldenrod2", "r=238,g=220,b=130");
		_colorDefinitions.put("LightGoldenrod3", "r=205,g=190,b=112");
		_colorDefinitions.put("LightGoldenrod4", "r=139,g=129,b=76 ");
		_colorDefinitions.put("LightYellow1", "r=255,g=255,b=224");
		_colorDefinitions.put("LightYellow2", "r=238,g=238,b=209");
		_colorDefinitions.put("LightYellow3", "r=205,g=205,b=180");
		_colorDefinitions.put("LightYellow4", "r=139,g=139,b=122");
		_colorDefinitions.put("yellow1", "r=255,g=255,b=0  ");
		_colorDefinitions.put("yellow2", "r=238,g=238,b=0  ");
		_colorDefinitions.put("yellow3", "r=205,g=205,b=0  ");
		_colorDefinitions.put("yellow4", "r=139,g=139,b=0  ");
		_colorDefinitions.put("gold1", "r=255,g=215,b=0  ");
		_colorDefinitions.put("gold2", "r=238,g=201,b=0  ");
		_colorDefinitions.put("gold3", "r=205,g=173,b=0  ");
		_colorDefinitions.put("gold4", "r=139,g=117,b=0  ");
		_colorDefinitions.put("goldenrod1", "r=255,g=193,b=37 ");
		_colorDefinitions.put("goldenrod2", "r=238,g=180,b=34 ");
		_colorDefinitions.put("goldenrod3", "r=205,g=155,b=29 ");
		_colorDefinitions.put("goldenrod4", "r=139,g=105,b=20 ");
		_colorDefinitions.put("DarkGoldenrod1", "r=255,g=185,b=15 ");
		_colorDefinitions.put("DarkGoldenrod2", "r=238,g=173,b=14 ");
		_colorDefinitions.put("DarkGoldenrod3", "r=205,g=149,b=12 ");
		_colorDefinitions.put("DarkGoldenrod4", "r=139,g=101,b=8  ");
		_colorDefinitions.put("RosyBrown1", "r=255,g=193,b=193");
		_colorDefinitions.put("RosyBrown2", "r=238,g=180,b=180");
		_colorDefinitions.put("RosyBrown3", "r=205,g=155,b=155");
		_colorDefinitions.put("RosyBrown4", "r=139,g=105,b=105");
		_colorDefinitions.put("IndianRed1", "r=255,g=106,b=106");
		_colorDefinitions.put("IndianRed2", "r=238,g=99,b=99 ");
		_colorDefinitions.put("IndianRed3", "r=205,g=85,b=85 ");
		_colorDefinitions.put("IndianRed4", "r=139,g=58,b=58 ");
		_colorDefinitions.put("sienna1", "r=255,g=130,b=71 ");
		_colorDefinitions.put("sienna2", "r=238,g=121,b=66 ");
		_colorDefinitions.put("sienna3", "r=205,g=104,b=57 ");
		_colorDefinitions.put("sienna4", "r=139,g=71,b=38 ");
		_colorDefinitions.put("burlywood1", "r=255,g=211,b=155");
		_colorDefinitions.put("burlywood2", "r=238,g=197,b=145");
		_colorDefinitions.put("burlywood3", "r=205,g=170,b=125");
		_colorDefinitions.put("burlywood4", "r=139,g=115,b=85 ");
		_colorDefinitions.put("wheat1", "r=255,g=231,b=186");
		_colorDefinitions.put("wheat2", "r=238,g=216,b=174");
		_colorDefinitions.put("wheat3", "r=205,g=186,b=150");
		_colorDefinitions.put("wheat4", "r=139,g=126,b=102");
		_colorDefinitions.put("tan1", "r=255,g=165,b=79");
		_colorDefinitions.put("tan2", "r=238,g=154,b=73");
		_colorDefinitions.put("tan3", "r=205,g=133,b=63");
		_colorDefinitions.put("tan4", "r=139,g=90,b=43");
		_colorDefinitions.put("chocolate1", "r=255,g=127,b=36");
		_colorDefinitions.put("chocolate2", "r=238,g=118,b=33");
		_colorDefinitions.put("chocolate3", "r=205,g=102,b=29");
		_colorDefinitions.put("chocolate4", "r=139,g=69,b=19");
		_colorDefinitions.put("firebrick1", "r=255,g=48,b=48");
		_colorDefinitions.put("firebrick2", "r=238,g=44,b=44");
		_colorDefinitions.put("firebrick3", "r=205,g=38,b=38");
		_colorDefinitions.put("firebrick4", "r=139,g=26,b=26");
		_colorDefinitions.put("brown1", "r=255,g=64,b=64");
		_colorDefinitions.put("brown2", "r=238,g=59,b=59");
		_colorDefinitions.put("brown3", "r=205,g=51,b=51");
		_colorDefinitions.put("brown4", "r=139,g=35,b=35");
		_colorDefinitions.put("salmon1", "r=255,g=140,b=105");
		_colorDefinitions.put("salmon2", "r=238,g=130,b=98");
		_colorDefinitions.put("salmon3", "r=205,g=112,b=84");
		_colorDefinitions.put("salmon4", "r=139,g=76,b=57");
		_colorDefinitions.put("LightSalmon1", "r=255,g=160,b=122");
		_colorDefinitions.put("LightSalmon2", "r=238,g=149,b=114");
		_colorDefinitions.put("LightSalmon3", "r=205,g=129,b=98");
		_colorDefinitions.put("LightSalmon4", "r=139,g=87,b=66");
		_colorDefinitions.put("orange1", "r=255,g=165,b=0");
		_colorDefinitions.put("orange2", "r=238,g=154,b=0");
		_colorDefinitions.put("orange3", "r=205,g=133,b=0");
		_colorDefinitions.put("orange4", "r=139,g=90,b=0");
		_colorDefinitions.put("DarkOrange1", "r=255,g=127,b=0");
		_colorDefinitions.put("DarkOrange2", "r=238,g=118,b=0");
		_colorDefinitions.put("DarkOrange3", "r=205,g=102,b=0");
		_colorDefinitions.put("DarkOrange4", "r=139,g=69,b=0");
		_colorDefinitions.put("coral1", "r=255,g=114,b=86");
		_colorDefinitions.put("coral2", "r=238,g=106,b=80");
		_colorDefinitions.put("coral3", "r=205,g=91,b=69");
		_colorDefinitions.put("coral4", "r=139,g=62,b=47");
		_colorDefinitions.put("tomato1", "r=255,g=99,b=71");
		_colorDefinitions.put("tomato2", "r=238,g=92,b=66");
		_colorDefinitions.put("tomato3", "r=205,g=79,b=57");
		_colorDefinitions.put("tomato4", "r=139,g=54,b=38");
		_colorDefinitions.put("OrangeRed1", "r=255,g=69,b=0");
		_colorDefinitions.put("OrangeRed2", "r=238,g=64,b=0");
		_colorDefinitions.put("OrangeRed3", "r=205,g=55,b=0");
		_colorDefinitions.put("OrangeRed4", "r=139,g=37,b=0");
		_colorDefinitions.put("red1", "r=255,g=0,b=0");
		_colorDefinitions.put("red2", "r=238,g=0,b=0");
		_colorDefinitions.put("red3", "r=205,g=0,b=0");
		_colorDefinitions.put("red4", "r=139,g=0,b=0");
		_colorDefinitions.put("DeepPink1", "r=255,g=20,b=147");
		_colorDefinitions.put("DeepPink2", "r=238,g=18,b=137");
		_colorDefinitions.put("DeepPink3", "r=205,g=16,b=118");
		_colorDefinitions.put("DeepPink4", "r=139,g=10,b=80");
		_colorDefinitions.put("HotPink1", "r=255,g=110,b=180");
		_colorDefinitions.put("HotPink2", "r=238,g=106,b=167");
		_colorDefinitions.put("HotPink3", "r=205,g=96,b=144");
		_colorDefinitions.put("HotPink4", "r=139,g=58,b=98");
		_colorDefinitions.put("pink1", "r=255,g=181,b=197");
		_colorDefinitions.put("pink2", "r=238,g=169,b=184");
		_colorDefinitions.put("pink3", "r=205,g=145,b=158");
		_colorDefinitions.put("pink4", "r=139,g=99,b=108");
		_colorDefinitions.put("LightPink1", "r=255,g=174,b=185");
		_colorDefinitions.put("LightPink2", "r=238,g=162,b=173");
		_colorDefinitions.put("LightPink3", "r=205,g=140,b=149");
		_colorDefinitions.put("LightPink4", "r=139,g=95,b=101");
		_colorDefinitions.put("PaleVioletRed1", "r=255,g=130,b=171");
		_colorDefinitions.put("PaleVioletRed2", "r=238,g=121,b=159");
		_colorDefinitions.put("PaleVioletRed3", "r=205,g=104,b=137");
		_colorDefinitions.put("PaleVioletRed4", "r=139,g=71,b=93");
		_colorDefinitions.put("maroon1", "r=255,g=52,b=179");
		_colorDefinitions.put("maroon2", "r=238,g=48,b=167");
		_colorDefinitions.put("maroon3", "r=205,g=41,b=144");
		_colorDefinitions.put("maroon4", "r=139,g=28,b=98");
		_colorDefinitions.put("VioletRed1", "r=255,g=62,b=150");
		_colorDefinitions.put("VioletRed2", "r=238,g=58,b=140");
		_colorDefinitions.put("VioletRed3", "r=205,g=50,b=120");
		_colorDefinitions.put("VioletRed4", "r=139,g=34,b=82");
		_colorDefinitions.put("magenta1", "r=255,g=0,b=255");
		_colorDefinitions.put("magenta2", "r=238,g=0,b=238");
		_colorDefinitions.put("magenta3", "r=205,g=0,b=205");
		_colorDefinitions.put("magenta4", "r=139,g=0,b=139");
		_colorDefinitions.put("orchid1", "r=255,g=131,b=250");
		_colorDefinitions.put("orchid2", "r=238,g=122,b=233");
		_colorDefinitions.put("orchid3", "r=205,g=105,b=201");
		_colorDefinitions.put("orchid4", "r=139,g=71,b=137");
		_colorDefinitions.put("plum1", "r=255,g=187,b=255");
		_colorDefinitions.put("plum2", "r=238,g=174,b=238");
		_colorDefinitions.put("plum3", "r=205,g=150,b=205");
		_colorDefinitions.put("plum4", "r=139,g=102,b=139");
		_colorDefinitions.put("MediumOrchid1", "r=224,g=102,b=255");
		_colorDefinitions.put("MediumOrchid2", "r=209,g=95,b=238");
		_colorDefinitions.put("MediumOrchid3", "r=180,g=82,b=205");
		_colorDefinitions.put("MediumOrchid4", "r=122,g=55,b=139");
		_colorDefinitions.put("DarkOrchid1", "r=191,g=62,b=255");
		_colorDefinitions.put("DarkOrchid2", "r=178,g=58,b=238");
		_colorDefinitions.put("DarkOrchid3", "r=154,g=50,b=205");
		_colorDefinitions.put("DarkOrchid4", "r=104,g=34,b=139");
		_colorDefinitions.put("purple1", "r=155,g=48,b=255");
		_colorDefinitions.put("purple2", "r=145,g=44,b=238");
		_colorDefinitions.put("purple3", "r=125,g=38,b=205");
		_colorDefinitions.put("purple4", "r=85,g=26,b=139");
		_colorDefinitions.put("MediumPurple1", "r=171,g=130,b=255");
		_colorDefinitions.put("MediumPurple2", "r=159,g=121,b=238");
		_colorDefinitions.put("MediumPurple3", "r=137,g=104,b=205");
		_colorDefinitions.put("MediumPurple4", "r=93,g=71,b=139");
		_colorDefinitions.put("thistle1", "r=255,g=225,b=255");
		_colorDefinitions.put("thistle2", "r=238,g=210,b=238");
		_colorDefinitions.put("thistle3", "r=205,g=181,b=205");
		_colorDefinitions.put("thistle4", "r=139,g=123,b=139");
		_colorDefinitions.put("gray0", "r=0,g=0,b=0");
		_colorDefinitions.put("grey0", "r=0,g=0,b=0");
		_colorDefinitions.put("gray1", "r=3,g=3,b=3");
		_colorDefinitions.put("grey1", "r=3,g=3,b=3");
		_colorDefinitions.put("gray2", "r=5,g=5,b=5");
		_colorDefinitions.put("grey2", "r=5,g=5,b=5");
		_colorDefinitions.put("gray3", "r=8,g=8,b=8");
		_colorDefinitions.put("grey3", "r=8,g=8,b=8");
		_colorDefinitions.put("gray4", "r=10,g=10,b=10");
		_colorDefinitions.put("grey4", "r=10,g=10,b=10");
		_colorDefinitions.put("gray5", "r=13,g=13,b=13");
		_colorDefinitions.put("grey5", "r=13,g=13,b=13");
		_colorDefinitions.put("gray6", "r=15,g=15,b=15");
		_colorDefinitions.put("grey6", "r=15,g=15,b=15");
		_colorDefinitions.put("gray7", "r=18,g=18,b=18");
		_colorDefinitions.put("grey7", "r=18,g=18,b=18");
		_colorDefinitions.put("gray8", "r=20,g=20,b=20");
		_colorDefinitions.put("grey8", "r=20,g=20,b=20");
		_colorDefinitions.put("gray9", "r=23,g=23,b=23");
		_colorDefinitions.put("grey9", "r=23,g=23,b=23");
		_colorDefinitions.put("gray10", "r=26,g=26,b=26");
		_colorDefinitions.put("grey10", "r=26,g=26,b=26");
		_colorDefinitions.put("gray11", "r=28,g=28,b=28");
		_colorDefinitions.put("grey11", "r=28,g=28,b=28");
		_colorDefinitions.put("gray12", "r=31,g=31,b=31");
		_colorDefinitions.put("grey12", "r=31,g=31,b=31");
		_colorDefinitions.put("gray13", "r=33,g=33,b=33");
		_colorDefinitions.put("grey13", "r=33,g=33,b=33");
		_colorDefinitions.put("gray14", "r=36,g=36,b=36");
		_colorDefinitions.put("grey14", "r=36,g=36,b=36");
		_colorDefinitions.put("gray15", "r=38,g=38,b=38");
		_colorDefinitions.put("grey15", "r=38,g=38,b=38");
		_colorDefinitions.put("gray16", "r=41,g=41,b=41");
		_colorDefinitions.put("grey16", "r=41,g=41,b=41");
		_colorDefinitions.put("gray17", "r=43,g=43,b=43");
		_colorDefinitions.put("grey17", "r=43,g=43,b=43");
		_colorDefinitions.put("gray18", "r=46,g=46,b=46");
		_colorDefinitions.put("grey18", "r=46,g=46,b=46");
		_colorDefinitions.put("gray19", "r=48,g=48,b=48");
		_colorDefinitions.put("grey19", "r=48,g=48,b=48");
		_colorDefinitions.put("gray20", "r=51,g=51,b=51");
		_colorDefinitions.put("grey20", "r=51,g=51,b=51");
		_colorDefinitions.put("gray21", "r=54,g=54,b=54");
		_colorDefinitions.put("grey21", "r=54,g=54,b=54");
		_colorDefinitions.put("gray22", "r=56,g=56,b=56");
		_colorDefinitions.put("grey22", "r=56,g=56,b=56");
		_colorDefinitions.put("gray23", "r=59,g=59,b=59");
		_colorDefinitions.put("grey23", "r=59,g=59,b=59");
		_colorDefinitions.put("gray24", "r=61,g=61,b=61");
		_colorDefinitions.put("grey24", "r=61,g=61,b=61");
		_colorDefinitions.put("gray25", "r=64,g=64,b=64");
		_colorDefinitions.put("grey25", "r=64,g=64,b=64");
		_colorDefinitions.put("gray26", "r=66,g=66,b=66");
		_colorDefinitions.put("grey26", "r=66,g=66,b=66");
		_colorDefinitions.put("gray27", "r=69,g=69,b=69");
		_colorDefinitions.put("grey27", "r=69,g=69,b=69");
		_colorDefinitions.put("gray28", "r=71,g=71,b=71");
		_colorDefinitions.put("grey28", "r=71,g=71,b=71");
		_colorDefinitions.put("gray29", "r=74,g=74,b=74");
		_colorDefinitions.put("grey29", "r=74,g=74,b=74");
		_colorDefinitions.put("gray30", "r=77,g=77,b=77");
		_colorDefinitions.put("grey30", "r=77,g=77,b=77");
		_colorDefinitions.put("gray31", "r=79,g=79,b=79");
		_colorDefinitions.put("grey31", "r=79,g=79,b=79");
		_colorDefinitions.put("gray32", "r=82,g=82,b=82");
		_colorDefinitions.put("grey32", "r=82,g=82,b=82");
		_colorDefinitions.put("gray33", "r=84,g=84,b=84");
		_colorDefinitions.put("grey33", "r=84,g=84,b=84");
		_colorDefinitions.put("gray34", "r=87,g=87,b=87");
		_colorDefinitions.put("grey34", "r=87,g=87,b=87");
		_colorDefinitions.put("gray35", "r=89,g=89,b=89");
		_colorDefinitions.put("grey35", "r=89,g=89,b=89");
		_colorDefinitions.put("gray36", "r=92,g=92,b=92");
		_colorDefinitions.put("grey36", "r=92,g=92,b=92");
		_colorDefinitions.put("gray37", "r=94,g=94,b=94");
		_colorDefinitions.put("grey37", "r=94,g=94,b=94");
		_colorDefinitions.put("gray38", "r=97,g=97,b=97");
		_colorDefinitions.put("grey38", "r=97,g=97,b=97");
		_colorDefinitions.put("gray39", "r=99,g=99,b=99");
		_colorDefinitions.put("grey39", "r=99,g=99,b=99");
		_colorDefinitions.put("gray40", "r=102,g=102,b=102");
		_colorDefinitions.put("grey40", "r=102,g=102,b=102");
		_colorDefinitions.put("gray41", "r=105,g=105,b=105");
		_colorDefinitions.put("grey41", "r=105,g=105,b=105");
		_colorDefinitions.put("gray42", "r=107,g=107,b=107");
		_colorDefinitions.put("grey42", "r=107,g=107,b=107");
		_colorDefinitions.put("gray43", "r=110,g=110,b=110");
		_colorDefinitions.put("grey43", "r=110,g=110,b=110");
		_colorDefinitions.put("gray44", "r=112,g=112,b=112");
		_colorDefinitions.put("grey44", "r=112,g=112,b=112");
		_colorDefinitions.put("gray45", "r=115,g=115,b=115");
		_colorDefinitions.put("grey45", "r=115,g=115,b=115");
		_colorDefinitions.put("gray46", "r=117,g=117,b=117");
		_colorDefinitions.put("grey46", "r=117,g=117,b=117");
		_colorDefinitions.put("gray47", "r=120,g=120,b=120");
		_colorDefinitions.put("grey47", "r=120,g=120,b=120");
		_colorDefinitions.put("gray48", "r=122,g=122,b=122");
		_colorDefinitions.put("grey48", "r=122,g=122,b=122");
		_colorDefinitions.put("gray49", "r=125,g=125,b=125");
		_colorDefinitions.put("grey49", "r=125,g=125,b=125");
		_colorDefinitions.put("gray50", "r=127,g=127,b=127");
		_colorDefinitions.put("grey50", "r=127,g=127,b=127");
		_colorDefinitions.put("gray51", "r=130,g=130,b=130");
		_colorDefinitions.put("grey51", "r=130,g=130,b=130");
		_colorDefinitions.put("gray52", "r=133,g=133,b=133");
		_colorDefinitions.put("grey52", "r=133,g=133,b=133");
		_colorDefinitions.put("gray53", "r=135,g=135,b=135");
		_colorDefinitions.put("grey53", "r=135,g=135,b=135");
		_colorDefinitions.put("gray54", "r=138,g=138,b=138");
		_colorDefinitions.put("grey54", "r=138,g=138,b=138");
		_colorDefinitions.put("gray55", "r=140,g=140,b=140");
		_colorDefinitions.put("grey55", "r=140,g=140,b=140");
		_colorDefinitions.put("gray56", "r=143,g=143,b=143");
		_colorDefinitions.put("grey56", "r=143,g=143,b=143");
		_colorDefinitions.put("gray57", "r=145,g=145,b=145");
		_colorDefinitions.put("grey57", "r=145,g=145,b=145");
		_colorDefinitions.put("gray58", "r=148,g=148,b=148");
		_colorDefinitions.put("grey58", "r=148,g=148,b=148");
		_colorDefinitions.put("gray59", "r=150,g=150,b=150");
		_colorDefinitions.put("grey59", "r=150,g=150,b=150");
		_colorDefinitions.put("gray60", "r=153,g=153,b=153");
		_colorDefinitions.put("grey60", "r=153,g=153,b=153");
		_colorDefinitions.put("gray61", "r=156,g=156,b=156");
		_colorDefinitions.put("grey61", "r=156,g=156,b=156");
		_colorDefinitions.put("gray62", "r=158,g=158,b=158");
		_colorDefinitions.put("grey62", "r=158,g=158,b=158");
		_colorDefinitions.put("gray63", "r=161,g=161,b=161");
		_colorDefinitions.put("grey63", "r=161,g=161,b=161");
		_colorDefinitions.put("gray64", "r=163,g=163,b=163");
		_colorDefinitions.put("grey64", "r=163,g=163,b=163");
		_colorDefinitions.put("gray65", "r=166,g=166,b=166");
		_colorDefinitions.put("grey65", "r=166,g=166,b=166");
		_colorDefinitions.put("gray66", "r=168,g=168,b=168");
		_colorDefinitions.put("grey66", "r=168,g=168,b=168");
		_colorDefinitions.put("gray67", "r=171,g=171,b=171");
		_colorDefinitions.put("grey67", "r=171,g=171,b=171");
		_colorDefinitions.put("gray68", "r=173,g=173,b=173");
		_colorDefinitions.put("grey68", "r=173,g=173,b=173");
		_colorDefinitions.put("gray69", "r=176,g=176,b=176");
		_colorDefinitions.put("grey69", "r=176,g=176,b=176");
		_colorDefinitions.put("gray70", "r=179,g=179,b=179");
		_colorDefinitions.put("grey70", "r=179,g=179,b=179");
		_colorDefinitions.put("gray71", "r=181,g=181,b=181");
		_colorDefinitions.put("grey71", "r=181,g=181,b=181");
		_colorDefinitions.put("gray72", "r=184,g=184,b=184");
		_colorDefinitions.put("grey72", "r=184,g=184,b=184");
		_colorDefinitions.put("gray73", "r=186,g=186,b=186");
		_colorDefinitions.put("grey73", "r=186,g=186,b=186");
		_colorDefinitions.put("gray74", "r=189,g=189,b=189");
		_colorDefinitions.put("grey74", "r=189,g=189,b=189");
		_colorDefinitions.put("gray75", "r=191,g=191,b=191");
		_colorDefinitions.put("grey75", "r=191,g=191,b=191");
		_colorDefinitions.put("gray76", "r=194,g=194,b=194");
		_colorDefinitions.put("grey76", "r=194,g=194,b=194");
		_colorDefinitions.put("gray77", "r=196,g=196,b=196");
		_colorDefinitions.put("grey77", "r=196,g=196,b=196");
		_colorDefinitions.put("gray78", "r=199,g=199,b=199");
		_colorDefinitions.put("grey78", "r=199,g=199,b=199");
		_colorDefinitions.put("gray79", "r=201,g=201,b=201");
		_colorDefinitions.put("grey79", "r=201,g=201,b=201");
		_colorDefinitions.put("gray80", "r=204,g=204,b=204");
		_colorDefinitions.put("grey80", "r=204,g=204,b=204");
		_colorDefinitions.put("gray81", "r=207,g=207,b=207");
		_colorDefinitions.put("grey81", "r=207,g=207,b=207");
		_colorDefinitions.put("gray82", "r=209,g=209,b=209");
		_colorDefinitions.put("grey82", "r=209,g=209,b=209");
		_colorDefinitions.put("gray83", "r=212,g=212,b=212");
		_colorDefinitions.put("grey83", "r=212,g=212,b=212");
		_colorDefinitions.put("gray84", "r=214,g=214,b=214");
		_colorDefinitions.put("grey84", "r=214,g=214,b=214");
		_colorDefinitions.put("gray85", "r=217,g=217,b=217");
		_colorDefinitions.put("grey85", "r=217,g=217,b=217");
		_colorDefinitions.put("gray86", "r=219,g=219,b=219");
		_colorDefinitions.put("grey86", "r=219,g=219,b=219");
		_colorDefinitions.put("gray87", "r=222,g=222,b=222");
		_colorDefinitions.put("grey87", "r=222,g=222,b=222");
		_colorDefinitions.put("gray88", "r=224,g=224,b=224");
		_colorDefinitions.put("grey88", "r=224,g=224,b=224");
		_colorDefinitions.put("gray89", "r=227,g=227,b=227");
		_colorDefinitions.put("grey89", "r=227,g=227,b=227");
		_colorDefinitions.put("gray90", "r=229,g=229,b=229");
		_colorDefinitions.put("grey90", "r=229,g=229,b=229");
		_colorDefinitions.put("gray91", "r=232,g=232,b=232");
		_colorDefinitions.put("grey91", "r=232,g=232,b=232");
		_colorDefinitions.put("gray92", "r=235,g=235,b=235");
		_colorDefinitions.put("grey92", "r=235,g=235,b=235");
		_colorDefinitions.put("gray93", "r=237,g=237,b=237");
		_colorDefinitions.put("grey93", "r=237,g=237,b=237");
		_colorDefinitions.put("gray94", "r=240,g=240,b=240");
		_colorDefinitions.put("grey94", "r=240,g=240,b=240");
		_colorDefinitions.put("gray95", "r=242,g=242,b=242");
		_colorDefinitions.put("grey95", "r=242,g=242,b=242");
		_colorDefinitions.put("gray96", "r=245,g=245,b=245");
		_colorDefinitions.put("grey96", "r=245,g=245,b=245");
		_colorDefinitions.put("gray97", "r=247,g=247,b=247");
		_colorDefinitions.put("grey97", "r=247,g=247,b=247");
		_colorDefinitions.put("gray98", "r=250,g=250,b=250");
		_colorDefinitions.put("grey98", "r=250,g=250,b=250");
		_colorDefinitions.put("gray99", "r=252,g=252,b=252");
		_colorDefinitions.put("grey99", "r=252,g=252,b=252");
		_colorDefinitions.put("gray100", "r=255,g=255,b=255");
		_colorDefinitions.put("grey100", "r=255,g=255,b=255");
		_colorDefinitions.put("dark_grey", "r=169,g=169,b=169");
		_colorDefinitions.put("DarkGrey", "r=169,g=169,b=169");
		_colorDefinitions.put("dark_gray", "r=169,g=169,b=169");
		_colorDefinitions.put("DarkGray", "r=169,g=169,b=169");
		_colorDefinitions.put("dark_blue", "r=0,g=0,b=139");
		_colorDefinitions.put("DarkBlue", "r=0,g=0,b=139");
		_colorDefinitions.put("dark_cyan", "r=0,g=139,b=139");
		_colorDefinitions.put("DarkCyan", "r=0,g=139,b=139");
		_colorDefinitions.put("dark_magenta", "r=139,g=0,b=139");
		_colorDefinitions.put("DarkMagenta", "r=139,g=0,b=139");
		_colorDefinitions.put("dark_red", "r=139,g=0,b=0");
		_colorDefinitions.put("DarkRed", "r=139,g=0,b=0");
		_colorDefinitions.put("light_green", "r=144,g=238,b=144");
		_colorDefinitions.put("LightGreen", "r=144,g=238,b=144");
	}

	/**
	 * color defintions in a hash table
	 */
	private Properties _colorDefinitions = new Properties();
	/**
   *
   */
	private static final boolean DEBUG = false;
}

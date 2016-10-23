package gnu.kawa.models;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

/** The standard HTML/CSS/SVG named colors supported by modern browsers. */

public class StandardColor extends Color {
    String name;

    static Map<String, StandardColor> map =
        new HashMap<String, StandardColor>();

    private StandardColor(String name, int rgb) {
        super(rgb);
        this.name = name;
    }
    private StandardColor(String name, int r, int g, int b, int alpha) {
        super(r, g, b, alpha);
        this.name = name;
    }
    private static StandardColor add(String name, int rgb) {
        StandardColor color = new StandardColor(name, rgb);
        map.put(name.replace("-", ""), color);
        return color;
    }
    private static StandardColor add(StandardColor color) {
        map.put(color.name.replace("-", ""), color);
        return color;
    }
    public String getName() { return name; }

    public static final StandardColor aliceBlue = add("alice-blue", 0xF0F8FF);
    public static final StandardColor antiqueWhite = add("antique-white", 0xFAEBD7);
    public static final StandardColor aqua = add("aqua", 0x00FFFF);
    public static final StandardColor aquamarine = add("aquamarine", 0x7FFFD4);
    public static final StandardColor azure = add("azure", 0xF0FFFF);
    public static final StandardColor beige = add("beige", 0xF5F5DC);
    public static final StandardColor bisque = add("bisque", 0xFFE4C4);
    public static final StandardColor black = add("black", 0x000000);
    public static final StandardColor blanchedAlmond = add("blanched-almond", 0xFFEBCD);
    public static final StandardColor blue = add("blue", 0x0000FF);
    public static final StandardColor blueViolet = add("blue-violet", 0x8A2BE2);
    public static final StandardColor brown = add("brown", 0xA52A2A);
    public static final StandardColor burlyWood = add("burly-wood", 0xDEB887);
    public static final StandardColor cadetBlue = add("cadet-blue", 0x5F9EA0);
    public static final StandardColor chartreuse = add("chartreuse", 0x7FFF00);
    public static final StandardColor chocolate = add("chocolate", 0xD2691E);
    public static final StandardColor coral = add("coral", 0xFF7F50);
    public static final StandardColor cornflowerBlue = add("cornflower-blue", 0x6495ED);
    public static final StandardColor cornsilk = add("cornsilk", 0xFFF8DC);
    public static final StandardColor crimson = add("crimson", 0xDC143C);
    public static final StandardColor cyan = add("cyan", 0x00FFFF);
    public static final StandardColor darkBlue = add("dark-blue", 0x00008B);
    public static final StandardColor darkCyan = add("dark-cyan", 0x008B8B);
    public static final StandardColor darkGoldenrod = add("dark-goldenrod", 0xB8860B);
    public static final StandardColor darkGray = add("dark-gray", 0xA9A9A9);
    public static final StandardColor darkGreen = add("dark-green", 0x006400);
    public static final StandardColor darkGrey = add("dark-grey", 0xA9A9A9);
    public static final StandardColor darkKhaki = add("dark-khaki", 0xBDB76B);
    public static final StandardColor darkMagenta = add("dark-magenta", 0x8B008B);
    public static final StandardColor darkOliveGreen = add("dark-olive-green", 0x556B2F);
    public static final StandardColor darkorange = add("darkorange", 0xFF8C00);
    public static final StandardColor darkOrchid = add("dark-orchid", 0x9932CC);
    public static final StandardColor darkRed = add("dark-red", 0x8B0000);
    public static final StandardColor darkSalmon = add("dark-salmon", 0xE9967A);
    public static final StandardColor darkSeaGreen = add("dark-sea-green", 0x8FBC8F);
    public static final StandardColor darkSlateBlue = add("dark-slate-blue", 0x483D8B);
    public static final StandardColor darkSlateGray = add("dark-slate-gray", 0x2F4F4F);
    public static final StandardColor darkSlateGrey = add("dark-slate-grey", 0x2F4F4F);
    public static final StandardColor darkTurquoise = add("dark-turquoise", 0x00CED1);
    public static final StandardColor darkViolet = add("dark-violet", 0x9400D3);
    public static final StandardColor deepPink = add("deep-pink", 0xFF1493);
    public static final StandardColor deepSkyBlue = add("deep-sky-blue", 0x00BFFF);
    public static final StandardColor dimGray = add("dim-gray", 0x696969);
    public static final StandardColor dimGrey = add("dim-grey", 0x696969);
    public static final StandardColor dodgerBlue = add("dodger-blue", 0x1E90FF);
    public static final StandardColor fireBrick = add("fire-brick", 0xB22222);
    public static final StandardColor floralWhite = add("floral-white", 0xFFFAF0);
    public static final StandardColor forestGreen = add("forest-green", 0x228B22);
    public static final StandardColor fuchsia = add("fuchsia", 0xFF00FF);
    public static final StandardColor gainsboro = add("gainsboro", 0xDCDCDC);
    public static final StandardColor ghostWhite = add("ghost-white", 0xF8F8FF);
    public static final StandardColor gold = add("gold", 0xFFD700);
    public static final StandardColor goldenrod = add("goldenrod", 0xDAA520);
    public static final StandardColor gray = add("gray", 0x808080);
    public static final StandardColor green = add("green", 0x008000);
    public static final StandardColor greenYellow = add("green-yellow", 0xADFF2F);
    public static final StandardColor grey = add("grey", 0x808080);
    public static final StandardColor honeyDew = add("honey-dew", 0xF0FFF0);
    public static final StandardColor hotPink = add("hot-pink", 0xFF69B4);
    public static final StandardColor indianRed = add("indian-red", 0xCD5C5C);
    public static final StandardColor indigo = add("indigo", 0x4B0082);
    public static final StandardColor ivory = add("ivory", 0xFFFFF0);
    public static final StandardColor khaki = add("khaki", 0xF0E68C);
    public static final StandardColor lavender = add("lavender", 0xE6E6FA);
    public static final StandardColor lavenderBlush = add("lavender-blush", 0xFFF0F5);
    public static final StandardColor lawnGreen = add("lawn-green", 0x7CFC00);
    public static final StandardColor lemonChiffon = add("lemon-chiffon", 0xFFFACD);
    public static final StandardColor lightBlue = add("light-blue", 0xADD8E6);
    public static final StandardColor lightCoral = add("light-coral", 0xF08080);
    public static final StandardColor lightCyan = add("light-cyan", 0xE0FFFF);
    public static final StandardColor lightGoldenrodYellow = add("light-goldenrod-yellow", 0xFAFAD2);
    public static final StandardColor lightGray = add("light-gray", 0xD3D3D3);
    public static final StandardColor lightGreen = add("light-green", 0x90EE90);
    public static final StandardColor lightGrey = add("light-grey", 0xD3D3D3);
    public static final StandardColor lightPink = add("light-pink", 0xFFB6C1);
    public static final StandardColor lightSalmon = add("light-salmon", 0xFFA07A);
    public static final StandardColor lightSeaGreen = add("light-sea-green", 0x20B2AA);
    public static final StandardColor lightSkyBlue = add("light-sky-blue", 0x87CEFA);
    public static final StandardColor lightSlateGray = add("light-slate-gray", 0x778899);
    public static final StandardColor lightSlateGrey = add("light-slate-grey", 0x778899);
    public static final StandardColor lightSteelBlue = add("light-steel-blue", 0xB0C4DE);
    public static final StandardColor lightYellow = add("light-yellow", 0xFFFFE0);
    public static final StandardColor lime = add("lime", 0x00FF00);
    public static final StandardColor limeGreen = add("lime-green", 0x32CD32);
    public static final StandardColor linen = add("linen", 0xFAF0E6);
    public static final StandardColor magenta = add("magenta", 0xFF00FF);
    public static final StandardColor maroon = add("maroon", 0x800000);
    public static final StandardColor mediumAquaMarine = add("medium-aqua-marine", 0x66CDAA);
    public static final StandardColor mediumBlue = add("medium-blue", 0x0000CD);
    public static final StandardColor mediumOrchid = add("medium-orchid", 0xBA55D3);
    public static final StandardColor mediumPurple = add("medium-purple", 0x9370DB);
    public static final StandardColor mediumSeaGreen = add("medium-sea-green", 0x3CB371);
    public static final StandardColor mediumSlateBlue = add("medium-slate-blue", 0x7B68EE);
    public static final StandardColor mediumSpringGreen = add("medium-spring-green", 0x00FA9A);
    public static final StandardColor mediumTurquoise = add("medium-turquoise", 0x48D1CC);
    public static final StandardColor mediumVioletRed = add("medium-violet-red", 0xC71585);
    public static final StandardColor midnightBlue = add("midnight-blue", 0x191970);
    public static final StandardColor mintCream = add("mint-cream", 0xF5FFFA);
    public static final StandardColor mistyRose = add("misty-rose", 0xFFE4E1);
    public static final StandardColor moccasin = add("moccasin", 0xFFE4B5);
    public static final StandardColor navajoWhite = add("navajo-white", 0xFFDEAD);
    public static final StandardColor navy = add("navy", 0x000080);
    public static final StandardColor oldLace = add("old-lace", 0xFDF5E6);
    public static final StandardColor olive = add("olive", 0x808000);
    public static final StandardColor oliveDrab = add("olive-drab", 0x6B8E23);
    public static final StandardColor orange = add("orange", 0xFFA500);
    public static final StandardColor orangeRed = add("orange-red", 0xFF4500);
    public static final StandardColor orchid = add("orchid", 0xDA70D6);
    public static final StandardColor paleGoldenrod = add("pale-goldenrod", 0xEEE8AA);
    public static final StandardColor paleGreen = add("pale-green", 0x98FB98);
    public static final StandardColor paleTurquoise = add("pale-turquoise", 0xAFEEEE);
    public static final StandardColor paleVioletRed = add("pale-violet-red", 0xDB7093);
    public static final StandardColor papayaWhip = add("papaya-whip", 0xFFEFD5);
    public static final StandardColor peachPuff = add("peach-puff", 0xFFDAB9);
    public static final StandardColor peru = add("peru", 0xCD853F);
    public static final StandardColor pink = add("pink", 0xFFC0CB);
    public static final StandardColor plum = add("plum", 0xDDA0DD);
    public static final StandardColor powderBlue = add("powder-blue", 0xB0E0E6);
    public static final StandardColor purple = add("purple", 0x800080);
    public static final StandardColor red = add("red", 0xFF0000);
    public static final StandardColor rosyBrown = add("rosy-brown", 0xBC8F8F);
    public static final StandardColor royalBlue = add("royal-blue", 0x4169E1);
    public static final StandardColor saddleBrown = add("saddle-brown", 0x8B4513);
    public static final StandardColor salmon = add("salmon", 0xFA8072);
    public static final StandardColor sandyBrown = add("sandy-brown", 0xF4A460);
    public static final StandardColor seaGreen = add("sea-green", 0x2E8B57);
    public static final StandardColor seaShell = add("sea-shell", 0xFFF5EE);
    public static final StandardColor sienna = add("sienna", 0xA0522D);
    public static final StandardColor silver = add("silver", 0xC0C0C0);
    public static final StandardColor skyBlue = add("sky-blue", 0x87CEEB);
    public static final StandardColor slateBlue = add("slate-blue", 0x6A5ACD);
    public static final StandardColor slateGray = add("slate-gray", 0x708090);
    public static final StandardColor slateGrey = add("slate-grey", 0x708090);
    public static final StandardColor snow = add("snow", 0xFFFAFA);
    public static final StandardColor springGreen = add("spring-green", 0x00FF7F);
    public static final StandardColor steelBlue = add("steel-blue", 0x4682B4);
    public static final StandardColor tan = add("tan", 0xD2B48C);
    public static final StandardColor teal = add("teal", 0x008080);
    public static final StandardColor thistle = add("thistle", 0xD8BFD8);
    public static final StandardColor tomato = add("tomato", 0xFF6347);
    public static final StandardColor turquoise = add("turquoise", 0x40E0D0);
    public static final StandardColor violet = add("violet", 0xEE82EE);
    public static final StandardColor wheat = add("wheat", 0xF5DEB3);
    public static final StandardColor white = add("white", 0xFFFFFF);
    public static final StandardColor whiteSmoke = add("white-smoke", 0xF5F5F5);
    public static final StandardColor yellow = add("yellow", 0xFFFF00);
    public static final StandardColor yellowGreen = add("yellow-green", 0x9ACD32);
    public static final StandardColor transparent =
        add(new StandardColor("transparent", 0, 0, 0, 0));

    public static StandardColor valueOf(String name) {
        String cname = name.toLowerCase().replace("-", "");
        return map.get(cname);
    }

    public String toString() {
        return "StandardColor[r="+getRed()+",g="+getGreen()+",b="+getBlue()+";"+name+"]";
    }
}

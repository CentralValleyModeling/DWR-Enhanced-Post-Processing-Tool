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

function getUnits(data) {
    var units;
    if (data['taf']) {
        units = 'TAF';
    } else {
        units = 'CFS';
    }
    return units;
}

var chart;

plot({
    "scenario_run_data": [{
        "statistically_computed_time_series": [["Oct", 1400.7485390792303]],
        "scenario_name": "Base",
        "period_filtered_time_series": [[-1491148800000, 1659.4229485153348], [-1459612800000, 1261.0759671508933], [-1427990400000, 611.4144312269789], [-1396454400000, 996.673327085426], [-1364918400000, 912.841174810088], [-1333382400000, 1635.6701190775896], [-1301760000000, 1540.5538689812304], [-1270224000000, 850.7693056766012], [-1238688000000, 883.5944818254104], [-1207152000000, 589.4097418626358], [-1175529600000, 486.9765004211203], [-1143993600000, 621.8415135022351], [-1112457600000, 453.0146360511091], [-1080921600000, 635.8934817938087], [-1049299200000, 761.8687016121054], [-1017763200000, 1005.0616982691404], [-986227200000, 1918.9866933563947], [-954691200000, 1112.1349905124891], [-923068800000, 1424.2191747416352], [-891532800000, 1975], [-859996800000, 1975], [-828460800000, 1691.1272862745425], [-796838400000, 1107.5326037186228], [-765302400000, 1110.4827458595994], [-733766400000, 1382.18781174728], [-702230400000, 1112.8905826864425], [-670608000000, 1453.8397457208137], [-639072000000, 1423.5780259464482], [-607536000000, 1249.999080680636], [-576000000000, 1631.3352618272258], [-544377600000, 1975], [-512841600000, 1968.2064945965153], [-481305600000, 1820.140292654858], [-449769600000, 1415.7787116432064], [-418147200000, 1942.1074959334228], [-386611200000, 1809.8760845845752], [-355075200000, 1975], [-323539200000, 1443.840787527256], [-291916800000, 1313.5847228426162], [-260380800000, 1437.5050301831984], [-228844800000, 1354.9831757380657], [-197308800000, 1797.6939465370258], [-165686400000, 1307.7904551135523], [-134150400000, 1763.7785579565534], [-102614400000, 1730.953963196703], [-71078400000, 1975], [-39456000000, 1502.9699998825467], [-7920000000, 1975.0000000000007], [23616000000, 1449.113072766303], [55152000000, 1975], [86774400000, 1672.4833446603677], [118310400000, 1784.3256406004562], [149846400000, 1975], [181382400000, 1975], [213004800000, 1217.5036396051285], [244540800000, 481.8541893809352], [276076800000, 1286.1241269784032], [307612800000, 1170.382942290066], [339235200000, 1485.3536389127175], [370771200000, 1269.6046071942233], [402307200000, 1975.0000000000002], [433843200000, 1975], [465465600000, 1682.4759826457166], [497001600000, 1223.4511500227306], [528537600000, 1421.9664169930934], [560073600000, 1025.0428892196062], [591696000000, 947.5064248011573], [623232000000, 967.4599503430252], [654768000000, 877.904739509023], [686304000000, 638.4635746226962], [717926400000, 606.642860608267], [749462400000, 1379.3377247035958], [780998400000, 990.2396758464156], [812534400000, 1975], [844156800000, 1777.536900939341], [875692800000, 1256.6536302320894], [907228800000, 1975], [938764800000, 1975], [970387200000, 1869.7521062059038], [1001923200000, 1313.911535287006], [1033459200000, 1381.983647846188], [1064995200000, 1900.700172958508]],
        "full_time_series": [[-1517414400000, 1753.6804118542643], [-1514736000000, 1755.0900871780755], [-1512057600000, 1751.3772037354072], [-1509638400000, 1765.5290382415028], [-1506960000000, 1815.0528898979653], [-1504368000000, 1918.281772846088], [-1501689600000, 1996.9240337687597], [-1499097600000, 2043.0571942984805], [-1496419200000, 1931.174600726217], [-1493740800000, 1808.1031075914016], [-1491148800000, 1659.4229485153348], [-1488470400000, 1604.856104502463], [-1485878400000, 1576.7832066237147], [-1483200000000, 1579.196339049311], [-1480521600000, 1596.5328019564338], [-1478102400000, 1619.2125662747505], [-1475424000000, 1671.659358562132], [-1472832000000, 1790.0994342370925], [-1470153600000, 1748.4610893562387], [-1467561600000, 1680.0381234549586], [-1464883200000, 1541.2226309587447], [-1462204800000, 1374.805025487947], [-1459612800000, 1261.0759671508933], [-1456934400000, 1206.135868293465], [-1454342400000, 1171.3739167893084], [-1451664000000, 1162.1223160518828], [-1448985600000, 1050.9903730600322], [-1446480000000, 1107.6202740582291], [-1443801600000, 1100.0741517853482], [-1441209600000, 1042.0487465339884], [-1438531200000, 960.74845054446], [-1435939200000, 900.2163782655031], [-1433260800000, 776.4526221172409], [-1430582400000, 653.9345365124526], [-1427990400000, 611.4144312269789], [-1425312000000, 603.8801164510467], [-1422720000000, 680.5443988370274], [-1420041600000, 727.6022543888491], [-1417363200000, 680.1445060771199], [-1414944000000, 950.7876088175224], [-1412265600000, 1085.888618872619], [-1409673600000, 1382.673120287736], [-1406995200000, 1367.9618613634645], [-1404403200000, 1286.6880453845447], [-1401724800000, 1137.8457295719377], [-1399046400000, 1053.5533923319977], [-1396454400000, 996.673327085426], [-1393776000000, 970.7680006288967], [-1391184000000, 965.2220418934526], [-1388505600000, 983.2005505936206], [-1385827200000, 984.8318649124604], [-1383408000000, 1129.0190728602058], [-1380729600000, 1231.5332809770441], [-1378137600000, 1450.0917809887235], [-1375459200000, 1344.1721113933925], [-1372867200000, 1273.360857565322], [-1370188800000, 1138.471673513527], [-1367510400000, 1031.689645086241], [-1364918400000, 912.841174810088], [-1362240000000, 886.0521397953465], [-1359648000000, 1041.2346807990698], [-1356969600000, 1236.5987214233924], [-1354291200000, 1354.814582858053], [-1351872000000, 1593.0375248985436], [-1349193600000, 1770.7618399129437], [-1346601600000, 1997.768238223507], [-1343923200000, 1987.37206828242], [-1341331200000, 2013.0362845334798], [-1338652800000, 1891.155392644932], [-1335974400000, 1777.5562567805755], [-1333382400000, 1635.6701190775896], [-1330704000000, 1544.8395034554815], [-1328112000000, 1572.9402932343828], [-1325433600000, 1581.8982004339848], [-1322755200000, 1630.3604519857445], [-1320249600000, 1739.8804538606387], [-1317571200000, 1946.414954508882], [-1314979200000, 2101.9998501972163], [-1312300800000, 2040.1749304862947], [-1309708800000, 1947.8794307505839], [-1307030400000, 1838.9646305902636], [-1304352000000, 1686.158850976276], [-1301760000000, 1540.5538689812304], [-1299081600000, 1446.3843695919113], [-1296489600000, 1369.1049816987008], [-1293811200000, 1360.8640547628138], [-1291132800000, 1169.3980589478654], [-1288713600000, 1193.760300376469], [-1286035200000, 1244.6899838010797], [-1283443200000, 1260.0945638257285], [-1280764800000, 1301.7167963016361], [-1278172800000, 1232.9151710285848], [-1275494400000, 1058.2526834908274], [-1272816000000, 937.2356735898068], [-1270224000000, 850.7693056766012], [-1267545600000, 817.9080774552675], [-1264953600000, 800.769199169701], [-1262275200000, 928.8439342756349], [-1259596800000, 945.1853793819768], [-1257177600000, 1044.1183294369332], [-1254499200000, 1169.9596063708887], [-1251907200000, 1286.3759330964651], [-1249228800000, 1197.4708932466742], [-1246636800000, 1172.56451456227], [-1243958400000, 1073.0908306182669], [-1241280000000, 967.805265738844], [-1238688000000, 883.5944818254104], [-1236009600000, 850.9994944322339], [-1233417600000, 836.7713074141095], [-1230739200000, 826.8153549721698], [-1228060800000, 836.1141290990802], [-1225641600000, 855.3186592634142], [-1222963200000, 913.2691548686729], [-1220371200000, 936.2640309525952], [-1217692800000, 896.7023943256521], [-1215100800000, 870.322391060575], [-1212422400000, 752.5694905837196], [-1209744000000, 630.8142716791643], [-1207152000000, 589.4097418626358], [-1204473600000, 500.0000000000002], [-1201881600000, 492.4239060515704], [-1199203200000, 495.5160596905493], [-1196524800000, 441.8501956354401], [-1194019200000, 463.8383581569014], [-1191340800000, 616.5516745781866], [-1188748800000, 700], [-1186070400000, 723.2377018271463], [-1183478400000, 711.6680943210999], [-1180800000000, 604.0208642000738], [-1178121600000, 527.2174921179673], [-1175529600000, 486.9765004211203], [-1172851200000, 447.3749818310554], [-1170259200000, 435.08562472479605], [-1167580800000, 430.73676266975616], [-1164902400000, 427.6882626206817], [-1162483200000, 429.068431277225], [-1159804800000, 530.8239721737364], [-1157212800000, 684.2667220622504], [-1154534400000, 671.9079797549539], [-1151942400000, 790.5906249295106], [-1149264000000, 699.290250127652], [-1146585600000, 660.0456918874855], [-1143993600000, 621.8415135022351], [-1141315200000, 604.0686651292745], [-1138723200000, 595.4369923413858], [-1136044800000, 605.2777580313532], [-1133366400000, 666.4808519013073], [-1130947200000, 756.4996474219912], [-1128268800000, 904.5837036359916], [-1125676800000, 988.0071160473184], [-1122998400000, 896.1976247013283], [-1120406400000, 857.2684961635401], [-1117728000000, 740.2451776905502], [-1115049600000, 509.62553985046196], [-1112457600000, 453.0146360511091], [-1109779200000, 377.2832830032935], [-1107187200000, 442.5471572287697], [-1104508800000, 479.54619689270595], [-1101830400000, 507.25685168857456], [-1099411200000, 590.747096801216], [-1096732800000, 651.8670688363618], [-1094140800000, 865.5515156762747], [-1091462400000, 896.4991757250184], [-1088870400000, 900.4994297522028], [-1086192000000, 792.61551319063], [-1083513600000, 674.5500951854002], [-1080921600000, 635.8934817938087], [-1078243200000, 621.6911650147765], [-1075651200000, 616.3783628250206], [-1072972800000, 619.4378773240916], [-1070294400000, 732.5102927025775], [-1067788800000, 872.7563611413053], [-1065110400000, 989.2280025780012], [-1062518400000, 1159.6254923474207], [-1059840000000, 1089.6139335725607], [-1057248000000, 1050.8467646932058], [-1054569600000, 945.1179008708748], [-1051891200000, 844.5745562035793], [-1049299200000, 761.8687016121054], [-1046620800000, 728.6295506665085], [-1044028800000, 711.8896113873429], [-1041350400000, 696.1484708829115], [-1038672000000, 687.9880048222245], [-1036252800000, 691.96990343725], [-1033574400000, 797.0105696884784], [-1030982400000, 1022.3258131285834], [-1028304000000, 1165.20201479646], [-1025712000000, 1262.5456850883525], [-1023033600000, 1158.2261723770973], [-1020355200000, 1073.3696914635154], [-1017763200000, 1005.0616982691404], [-1015084800000, 981.0718381332274], [-1012492800000, 1128.6848494455808], [-1009814400000, 1348.0601999649705], [-1007136000000, 1439.4555441317325], [-1004716800000, 1596.3897802251229], [-1002038400000, 1857.6682681928587], [-999446400000, 2165.2382535891393], [-996768000000, 2339.0865452831495], [-994176000000, 2284.0581477489504], [-991497600000, 2254.504999938119], [-988819200000, 2138.4278546641967], [-986227200000, 1918.9866933563947], [-983548800000, 1821.8823772256433], [-980956800000, 1807.233897708358], [-978278400000, 1823.4632606630246], [-975600000000, 1834.6242089950042], [-973180800000, 1848.7502853827639], [-970502400000, 1956.7057484126965], [-967910400000, 2032.0502417187374], [-965232000000, 1922.767035905965], [-962640000000, 1734.1765050823533], [-959961600000, 1502.5665022346216], [-957283200000, 1268.3607305492728], [-954691200000, 1112.1349905124891], [-952012800000, 1019.9956839525178], [-949420800000, 950.801962011357], [-946742400000, 1013.517447568749], [-944064000000, 1188.3488524611832], [-941558400000, 1514.223422181013], [-938880000000, 1816.3973902361256], [-936288000000, 2042.7066774956113], [-933609600000, 1979.7898249310363], [-931017600000, 1864.0557329970536], [-928339200000, 1684.563014930915], [-925660800000, 1534.4639578030765], [-923068800000, 1424.2191747416352], [-920390400000, 1368.282239259385], [-917798400000, 1348.888052369845], [-915120000000, 1472.8203703556246], [-912441600000, 1693.5174875895384], [-910022400000, 1944.2099502051317], [-907344000000, 2100], [-904752000000, 2300], [-902073600000, 2420], [-899481600000, 2447], [-896803200000, 2270], [-894124800000, 2150], [-891532800000, 1975], [-888854400000, 1850], [-886262400000, 1836.877229677874], [-883584000000, 1850], [-880905600000, 1900], [-878486400000, 2000], [-875808000000, 2078.540134242459], [-873216000000, 2265.6747190935216], [-870537600000, 2325.2534440116196], [-867945600000, 2397.035130444411], [-865267200000, 2270], [-862588800000, 2150], [-859996800000, 1975], [-857318400000, 1850.0000000000002], [-854726400000, 1845.1412431106555], [-852048000000, 1850.0000000000002], [-849369600000, 1900], [-846950400000, 2000], [-844272000000, 2100], [-841680000000, 2283.1556612448057], [-839001600000, 2173.318610935777], [-836409600000, 2100.101347862575], [-833731200000, 1955.6327995824556], [-831052800000, 1836.7160819904198], [-828460800000, 1691.1272862745425], [-825782400000, 1602.9649252929848], [-823190400000, 1522.4451291522294], [-820512000000, 1510.583187719439], [-817833600000, 1464.4594398554], [-815328000000, 1498.751543597716], [-812649600000, 1557.6700509760642], [-810057600000, 1597.0047593853574], [-807379200000, 1652.6607804598027], [-804787200000, 1571.5129705696622], [-802108800000, 1356.5376627004844], [-799430400000, 1192.5666310692816], [-796838400000, 1107.5326037186228], [-794160000000, 1075.6332890698152], [-791568000000, 1102.7036421483165], [-788889600000, 1176.5653096117035], [-786211200000, 1231.208530539676], [-783792000000, 1390.9109014146916], [-781113600000, 1444.8128183334832], [-778521600000, 1582.5413719811481], [-775843200000, 1547.289307647043], [-773251200000, 1455.8329867692041], [-770572800000, 1312.9550970199107], [-767894400000, 1180.391006932843], [-765302400000, 1110.4827458595994], [-762624000000, 1098.307427921103], [-760032000000, 1153.3270271022875], [-757353600000, 1344.5681700883722], [-754675200000, 1475.08591636622], [-752256000000, 1530.1154006632075], [-749577600000, 1652.4342456252425], [-746985600000, 1870.6420846264011], [-744307200000, 1866.1605748974935], [-741715200000, 1771.8769663611308], [-739036800000, 1640.804231062008], [-736358400000, 1495.0949190940444], [-733766400000, 1382.18781174728], [-731088000000, 1321.598107654297], [-728496000000, 1315.235348664274], [-725817600000, 1326.9533795055254], [-723139200000, 1326.071522023818], [-720720000000, 1386.0983741285527], [-718041600000, 1505.220383168984], [-715449600000, 1592.6507043988906], [-712771200000, 1501.0437086952402], [-710179200000, 1486.2542825359264], [-707500800000, 1361.2120090353114], [-704822400000, 1197.2732102505433], [-702230400000, 1112.8905826864425], [-699552000000, 1125.839060069024], [-696960000000, 1136.7828943982088], [-694281600000, 1136.1393064733618], [-691603200000, 1328.7113695844953], [-689097600000, 1351.205960848765], [-686419200000, 1376.4715596961962], [-683827200000, 1548.8104495423859], [-681148800000, 1543.2257590641454], [-678556800000, 1622.9564338645184], [-675878400000, 1587.3929270314316], [-673200000000, 1475.7715264652725], [-670608000000, 1453.8397457208137], [-667929600000, 1400.019133810683], [-665337600000, 1406.9217732133252], [-662659200000, 1408.9724314118646], [-659980800000, 1402.391499731875], [-657561600000, 1434.2825644983861], [-654883200000, 1650.499600452303], [-652291200000, 1910.2647053003618], [-649612800000, 1911.2685874000476], [-647020800000, 1860.6678677979753], [-644342400000, 1715.667300973793], [-641664000000, 1567.561190831241], [-639072000000, 1423.5780259464482], [-636393600000, 1361.3247163114534], [-633801600000, 1328.3403320314358], [-631123200000, 1309.4414228934518], [-628444800000, 1327.572280359362], [-626025600000, 1375.4541637003997], [-623347200000, 1496.5243681180104], [-620755200000, 1650.6398596006181], [-618076800000, 1654.5401218442587], [-615484800000, 1636.0897808651052], [-612806400000, 1498.1948614679172], [-610128000000, 1363.1004057223445], [-607536000000, 1249.999080680636], [-604857600000, 1311.8164632816477], [-602265600000, 1427.513608119192], [-599587200000, 1685.142135157033], [-596908800000, 1775.1234666257733], [-594489600000, 1992.8940978202213], [-591811200000, 2093.9496681949954], [-589219200000, 2224.604681667414], [-586540800000, 2156.632682132626], [-583948800000, 2072.531183907833], [-581270400000, 1925.1183440300392], [-578592000000, 1775.8569440957924], [-576000000000, 1631.3352618272258], [-573321600000, 1547.0154436823602], [-570729600000, 1552.1817576429144], [-568051200000, 1699.5821527908242], [-565372800000, 1758.9824209797937], [-562867200000, 1948.9904454306575], [-560188800000, 2100], [-557596800000, 2299.9999999999995], [-554918400000, 2410.963686814417], [-552326400000, 2447], [-549648000000, 2270], [-546969600000, 2150], [-544377600000, 1975], [-541699200000, 1850], [-539107200000, 1845.314650614989], [-536428800000, 1850.0000000000002], [-533750400000, 1900], [-531331200000, 2000], [-528652800000, 2100], [-526060800000, 2261.5523187909903], [-523382400000, 2221.541245040021], [-520790400000, 2315.180711100164], [-518112000000, 2270], [-515433600000, 2150], [-512841600000, 1968.2064945965153], [-510163200000, 1850], [-507571200000, 1850.0000000000002], [-504892800000, 1850], [-502214400000, 1900.0000000000002], [-499795200000, 2000], [-497116800000, 2100], [-494524800000, 2300], [-491846400000, 2271.5030161556515], [-489254400000, 2203.9284707404], [-486576000000, 2067.065926783014], [-483897600000, 1957.2604071701442], [-481305600000, 1820.140292654858], [-478627200000, 1732.1219220757666], [-476035200000, 1752.9806173391985], [-473356800000, 1794.047286566081], [-470678400000, 1812.3275270533768], [-468259200000, 1841.1519822817618], [-465580800000, 1871.1869552473431], [-462988800000, 1917.1761693201586], [-460310400000, 1915.4595638042326], [-457718400000, 1900.4854384173277], [-455040000000, 1680.9562557404156], [-452361600000, 1529.9303154244933], [-449769600000, 1415.7787116432064], [-447091200000, 1353.1574366541486], [-444499200000, 1336.9528083262007], [-441820800000, 1645.055612393604], [-439142400000, 1900], [-436636800000, 2000], [-433958400000, 2100], [-431366400000, 2283.156400683418], [-428688000000, 2385.6613203580655], [-426096000000, 2304.4639732357223], [-423417600000, 2186.7596563241564], [-420739200000, 2103.1471907879], [-418147200000, 1942.1074959334228], [-415468800000, 1850], [-412876800000, 1825.751004581815], [-410198400000, 1818.3635572968415], [-407520000000, 1808.4820371702046], [-405100800000, 1955.6070008089084], [-402422400000, 2100], [-399830400000, 2218.3702490417722], [-397152000000, 2234.4311889348346], [-394560000000, 2211.435471873571], [-391881600000, 2067.2667816062085], [-389203200000, 1949.0156931594838], [-386611200000, 1809.8760845845752], [-383932800000, 1850], [-381340800000, 1850.0000000000002], [-378662400000, 1850], [-375984000000, 1900], [-373564800000, 2208.1742813390756], [-370886400000, 2100], [-368294400000, 2300], [-365616000000, 2420], [-363024000000, 2430.2546446549427], [-360345600000, 2270], [-357667200000, 2150], [-355075200000, 1975], [-352396800000, 1850], [-349804800000, 1850], [-347126400000, 1835.450496285897], [-344448000000, 1900], [-342028800000, 2000.0000000000002], [-339350400000, 2100], [-336758400000, 2257.393165022354], [-334080000000, 2156.3499624837214], [-331488000000, 2036.181998585589], [-328809600000, 1808.6306443958395], [-326131200000, 1613.3728784831906], [-323539200000, 1443.840787527256], [-320860800000, 1381.886721063557], [-318268800000, 1342.9704874693705], [-315590400000, 1321.3375574857537], [-312912000000, 1320.00578137695], [-310406400000, 1453.6647555946456], [-307728000000, 1642.7451199534614], [-305136000000, 1764.060603594479], [-302457600000, 1709.136912774331], [-299865600000, 1709.9293620223455], [-297187200000, 1575.8364816476071], [-294508800000, 1427.8426124999785], [-291916800000, 1313.5847228426162], [-289238400000, 1252.8007589226136], [-286646400000, 1236.5312113852324], [-283968000000, 1286.4679952663791], [-281289600000, 1330.6148116218876], [-278870400000, 1521.9213273609905], [-276192000000, 1619.5603813112855], [-273600000000, 1809.6473266178266], [-270921600000, 1837.7421409973908], [-268329600000, 1872.64384017733], [-265651200000, 1726.1633701331803], [-262972800000, 1579.0999582453305], [-260380800000, 1437.5050301831984], [-257702400000, 1377.072353150769], [-255110400000, 1351.9795166260606], [-252432000000, 1359.3903647005463], [-249753600000, 1376.4920847289234], [-247334400000, 1474.3352905179781], [-244656000000, 1532.8327705407569], [-242064000000, 1858.6086469474599], [-239385600000, 1824.646041037221], [-236793600000, 1758.832108415973], [-234115200000, 1610.6423128561973], [-231436800000, 1466.998420963732], [-228844800000, 1354.9831757380657], [-226166400000, 1456.6570257685041], [-223574400000, 1505.2494862803746], [-220896000000, 1671.1210436709407], [-218217600000, 1712.0921247319334], [-215798400000, 1992.2794413235783], [-213120000000, 2066.548765734465], [-210528000000, 2300], [-207849600000, 2323.768972513454], [-205257600000, 2264.049468236365], [-202579200000, 2119.3853313230097], [-199900800000, 1998.374020308136], [-197308800000, 1797.6939465370258], [-194630400000, 1703.9427674477547], [-192038400000, 1773.6897115155382], [-189360000000, 1808.5428151486706], [-186681600000, 1835.8065654895922], [-184176000000, 1837.2311916875249], [-181497600000, 1866.5815392785194], [-178905600000, 1889.5274241890895], [-176227200000, 1781.8746604747014], [-173635200000, 1722.2663345740852], [-170956800000, 1589.2587265058667], [-168278400000, 1415.0794442843262], [-165686400000, 1307.7904551135523], [-163008000000, 1253.4751949831357], [-160416000000, 1260.0408363632398], [-157737600000, 1778.867081910172], [-155059200000, 1900], [-152640000000, 1987.636434425], [-149961600000, 2054.0129112455243], [-147369600000, 2277.995845500754], [-144691200000, 2184.594332718426], [-142099200000, 2113.8459705101554], [-139420800000, 1977.3865602227434], [-136742400000, 1872.1594698438978], [-134150400000, 1763.7785579565534], [-131472000000, 1703.1522149483765], [-128880000000, 1757.3259267611093], [-126201600000, 1779.1824375038893], [-123523200000, 1850.3243739599254], [-121104000000, 1910.2267771998386], [-118425600000, 2099.9999999999995], [-115833600000, 2300], [-113155200000, 2300.6042666254175], [-110563200000, 2225.6877262898947], [-107884800000, 2089.872610783284], [-105206400000, 1868.1059408827568], [-102614400000, 1730.953963196703], [-99936000000, 1641.893361174386], [-97344000000, 1719.410512766377], [-94665600000, 1849.9999999999998], [-91987200000, 1900.0000000000002], [-89568000000, 2000], [-86889600000, 2100.0000000000005], [-84297600000, 2205.250473305693], [-81619200000, 2310.2516540943648], [-79027200000, 2400.7792474577486], [-76348800000, 2270], [-73670400000, 2150], [-71078400000, 1975], [-68400000000, 1850], [-65808000000, 1850], [-63129600000, 1850], [-60451200000, 1900], [-57945600000, 2000], [-55267200000, 2100], [-52675200000, 2191.009019304407], [-49996800000, 2077.487701506115], [-47404800000, 1971.0970102032252], [-44726400000, 1790.8980060029628], [-42048000000, 1644.1255797112183], [-39456000000, 1502.9699998825467], [-36777600000, 1446.272599615617], [-34185600000, 1439.8289471865573], [-31507200000, 1495.7259974018114], [-28828800000, 1632.2459497275677], [-26409600000, 1750.036211469067], [-23731200000, 1904.7407090907996], [-21139200000, 2226.369137339663], [-18460800000, 2420], [-15868800000, 2340.1647498014195], [-13190400000, 2216.1603051422244], [-10512000000, 2098.208052476607], [-7920000000, 1975.0000000000007], [-5241600000, 1850], [-2649600000, 1850.0000000000002], [28800000, 1850], [2707200000, 1900], [5126400000, 2000], [7804800000, 2100], [10396800000, 2161.3867487183707], [13075200000, 2060.638672231597], [15667200000, 1916.8774545570607], [18345600000, 1742.057008254214], [21024000000, 1592.1717585865413], [23616000000, 1449.113072766303], [26294400000, 1341.5742617927185], [28886400000, 1435.5544880671516], [31564800000, 1561.3522953004995], [34243200000, 1784.1132348354045], [36662400000, 1914.0830760081842], [39340800000, 2100], [41932800000, 2242.155379018981], [44611200000, 2276.783194399593], [47203200000, 2285.466956139133], [49881600000, 2261.924987958861], [52560000000, 2146.4037120946828], [55152000000, 1975], [57830400000, 1850], [60422400000, 1850], [63100800000, 1850], [65779200000, 1900], [68284800000, 2000], [70963200000, 2100], [73555200000, 2233.747818808155], [76233600000, 2163.055107284209], [78825600000, 2112.663296194655], [81504000000, 1964.1649917152063], [84182400000, 1815.8370747342713], [86774400000, 1672.4833446603677], [89452800000, 1618.7766678504565], [92044800000, 1654.6522212992736], [94723200000, 1758.6138273296558], [97401600000, 1900], [99820800000, 2000], [102499200000, 2100], [105091200000, 2268.8148399469706], [107769600000, 2318.0697647471843], [110361600000, 2264.564044198294], [113040000000, 2124.7851030072425], [115718400000, 1927.4703087500043], [118310400000, 1784.3256406004562], [120988800000, 1755.1916391569864], [123580800000, 1850], [126259200000, 1850], [128937600000, 1900], [131356800000, 2000], [134035200000, 2100], [136627200000, 2300], [139305600000, 2383.8642406856357], [141897600000, 2367.4432339418977], [144576000000, 2270], [147254400000, 2150], [149846400000, 1975], [152524800000, 1850], [155116800000, 1843.4598455521957], [157795200000, 1847.274837747011], [160473600000, 1868.5711372167746], [162892800000, 1956.8587068752543], [165571200000, 2100], [168163200000, 2224.726727611187], [170841600000, 2320.563198662463], [173433600000, 2431.4596258410074], [176112000000, 2270], [178790400000, 2150], [181382400000, 1975], [184060800000, 1850], [186652800000, 1805.005001225389], [189331200000, 1835.7864643079815], [192009600000, 1648.3479028931586], [194515200000, 1679.4776741371963], [197193600000, 1727.7940518472383], [199785600000, 1807.9591599258329], [202464000000, 1782.4764421738132], [205056000000, 1702.6718027469055], [207734400000, 1484.2327920659184], [210412800000, 1329.133948821732], [213004800000, 1217.5036396051285], [215683200000, 1155.5186159645116], [218275200000, 1126.8724847769893], [220953600000, 1039.2026634076533], [223632000000, 1000.0000000000001], [226051200000, 991.32123914753], [228729600000, 977.4567240778301], [231321600000, 946.8749997896832], [234000000000, 894.1388198198172], [236592000000, 853.6739638180932], [239270400000, 736.8429275003945], [241948800000, 510.71194309052106], [244540800000, 481.8541893809352], [247219200000, 376.28768579125267], [249811200000, 403.477962322073], [252489600000, 591.3962987467443], [255168000000, 976.4408654102969], [257587200000, 1154.445332520446], [260265600000, 1427.6012753291789], [262857600000, 1618.8916049081172], [265536000000, 1622.907060484759], [268128000000, 1567.4410195074506], [270806400000, 1481.2307945566988], [273484800000, 1371.8938677750896], [276076800000, 1286.1241269784032], [278755200000, 1225.355033254806], [281347200000, 1218.9875690902863], [284025600000, 1199.3819192030028], [286704000000, 1221.0019847456208], [289123200000, 1269.6034415646607], [291801600000, 1412.5579507622153], [294393600000, 1522.9236634448296], [297072000000, 1590.17137680186], [299664000000, 1555.839932093314], [302342400000, 1416.8427639394697], [305020800000, 1283.033742996041], [307612800000, 1170.382942290066], [310291200000, 1165.8282697278144], [312883200000, 1228.3452797548475], [315561600000, 1294.885247035354], [318240000000, 1513.6717785885119], [320745600000, 1802.163093660866], [323424000000, 1933.8512707406187], [326016000000, 2093.343323745477], [328694400000, 1998.3392929107777], [331286400000, 1900.2603620684856], [333964800000, 1777.418801039907], [336643200000, 1628.2748956784912], [339235200000, 1485.3536389127175], [341913600000, 1422.5386426562086], [344505600000, 1398.141814605163], [347184000000, 1426.7686927663697], [349862400000, 1523.424695940396], [352281600000, 1661.397825753223], [354960000000, 1779.4444672183877], [357552000000, 1887.8433977858908], [360230400000, 1820.0008638206316], [362822400000, 1764.3157743926533], [365500800000, 1612.5678964929903], [368179200000, 1382.3632855489411], [370771200000, 1269.6046071942233], [373449600000, 1207.448493808026], [376041600000, 1450.3978571607831], [378720000000, 1773.624505438805], [381398400000, 1862.514311173507], [383817600000, 2000], [386496000000, 2100], [389088000000, 2300], [391766400000, 2317.827169596444], [394358400000, 2187.3578334552903], [397036800000, 2071.1991280557118], [399715200000, 2038.0331018466554], [402307200000, 1975.0000000000002], [404985600000, 1850], [407577600000, 1850.0000000000002], [410256000000, 1850], [412934400000, 1900], [415353600000, 2000], [418032000000, 2100], [420624000000, 2300], [423302400000, 2420], [425894400000, 2447], [428572800000, 2270], [431251200000, 2150], [433843200000, 1975], [436521600000, 1850], [439113600000, 1850], [441792000000, 1850], [444470400000, 1900], [446976000000, 1990.9853133332574], [449654400000, 2100], [452246400000, 2217.6119610357614], [454924800000, 2183.315329659132], [457516800000, 2124.265281481726], [460195200000, 1988.7868550547453], [462873600000, 1796.7743403081345], [465465600000, 1682.4759826457166], [468144000000, 1593.5223517896022], [470736000000, 1723.1534430262138], [473414400000, 1780.243367422721], [476092800000, 1739.2683241013215], [478512000000, 1778.1873851291687], [481190400000, 1826.2557298198146], [483782400000, 1972.8779498277268], [486460800000, 1889.585378166676], [489052800000, 1787.1301818512313], [491731200000, 1561.1452329233962], [494409600000, 1335.7430160251365], [497001600000, 1223.4511500227306], [499680000000, 1161.1415800261962], [502272000000, 1151.2444257278337], [504950400000, 1163.8844157733163], [507628800000, 1290.0127678874012], [510048000000, 1763.5322296844288], [512726400000, 2096.0380561788334], [515318400000, 2102.151283323567], [517996800000, 1998.974490366973], [520588800000, 1887.8189408741734], [523267200000, 1712.1037688446468], [525945600000, 1563.052519192956], [528537600000, 1421.9664169930934], [531216000000, 1368.255596666305], [533808000000, 1334.6285839134023], [536486400000, 1325.215941993554], [539164800000, 1334.3379651715072], [541584000000, 1410.2845255860502], [544262400000, 1612.2610300056895], [546854400000, 1758.7110120487878], [549532800000, 1713.2208960730964], [552124800000, 1628.7162971021378], [554803200000, 1402.893493636965], [557481600000, 1174.7753182717247], [560073600000, 1025.0428892196062], [562752000000, 950.7138647311392], [565344000000, 935.127921198087], [568022400000, 1085.6638537109534], [570700800000, 1163.801608015408], [573206400000, 1245.3988043672366], [575884800000, 1325.8658213077047], [578476800000, 1379.4931943321362], [581155200000, 1323.2303109567229], [583747200000, 1308.1005035937476], [586425600000, 1147.3787644185425], [589104000000, 1029.4727193265967], [591696000000, 947.5064248011573], [594374400000, 916.1752470279633], [596966400000, 936.802486600916], [599644800000, 953.077192025687], [602323200000, 974.4000592123106], [604742400000, 1001.6349813991455], [607420800000, 1342.7622226078702], [610012800000, 1539.2034131858186], [612691200000, 1430.383828271772], [615283200000, 1315.178577798731], [617961600000, 1151.2279475931564], [620640000000, 1049.2730566609935], [623232000000, 967.4599503430252], [625910400000, 980.932436860549], [628502400000, 981.2405528078474], [631180800000, 976.1332320055697], [633859200000, 1038.8324585489208], [636278400000, 1061.5153900955243], [638956800000, 1149.5555498038564], [641548800000, 1188.94120835957], [644227200000, 1155.0458014722738], [646819200000, 1169.5118560728486], [649497600000, 1077.1541024675219], [652176000000, 959.6675531425767], [654768000000, 877.904739509023], [657446400000, 846.6525572622628], [660038400000, 831.2672108360373], [662716800000, 815.6591535001224], [665395200000, 799.8916097203892], [667814400000, 805.4062989608944], [670492800000, 878.3425110752813], [673084800000, 927.5432887602816], [675763200000, 950.6703821794996], [678355200000, 943.9575886326663], [681033600000, 835.0783675490779], [683712000000, 719.507398086222], [686304000000, 638.4635746226962], [688982400000, 622.8353506173814], [691574400000, 613.8747625637609], [694252800000, 608.9191429056721], [696931200000, 614.033030450587], [699436800000, 752.9399156549732], [702115200000, 899.0656173253819], [704707200000, 1132.717313745772], [707385600000, 1124.5025796693776], [709977600000, 1097.5341954361252], [712656000000, 987.6918143661861], [715334400000, 761.4774268014836], [717926400000, 606.642860608267], [720604800000, 507.17768144035085], [723196800000, 497.878325075953], [725875200000, 522.1126683453197], [728553600000, 584.7569891189345], [730972800000, 726.5270837693164], [733651200000, 1104.9481789323643], [736243200000, 1295.2005693829644], [738921600000, 1465.4941200790515], [741513600000, 1581.0346995406583], [744192000000, 1565.1112094044302], [746870400000, 1459.8990350414333], [749462400000, 1379.3377247035958], [752140800000, 1319.4283717456747], [754732800000, 1309.778349314646], [757411200000, 1318.535591054737], [760089600000, 1340.9340604416097], [762508800000, 1374.9216502255163], [765187200000, 1443.0659127842068], [767779200000, 1488.3842902007575], [770457600000, 1471.793694482078], [773049600000, 1368.3870110406206], [775728000000, 1190.2028288181345], [778406400000, 1072.3291744466862], [780998400000, 990.2396758464156], [783676800000, 959.0284276384745], [786268800000, 943.7548443924046], [788947200000, 928.2332934754384], [791625600000, 1328.964951755102], [794044800000, 1582.66656614121], [796723200000, 2016.0045281360694], [799315200000, 2254.0916628787077], [801993600000, 2329.4385450863306], [804585600000, 2314.678843033823], [807264000000, 2270], [809942400000, 2150], [812534400000, 1975], [815212800000, 1850], [817804800000, 1839.886105477417], [820483200000, 1850], [823161600000, 1900], [825667200000, 2000], [828345600000, 2100], [830937600000, 2258.9735730389157], [833616000000, 2214.8799099423914], [836208000000, 2150.884292704605], [838886400000, 2013.5089271905526], [841564800000, 1891.555544728989], [844156800000, 1777.536900939341], [846835200000, 1714.3813847426893], [849427200000, 1720.1924255598026], [852105600000, 1850], [854784000000, 1900], [857203200000, 1984.8881338927588], [859881600000, 2076.4510475636266], [862473600000, 2116.5219869022008], [865152000000, 1930.0471739152879], [867744000000, 1731.4152037461386], [870422400000, 1550.802394715513], [873100800000, 1369.710535149822], [875692800000, 1256.6536302320894], [878371200000, 1178.8968982085414], [880963200000, 1169.9886034760343], [883641600000, 1208.7217947855795], [886320000000, 1519.4403542853092], [888739200000, 1842.015584744576], [891417600000, 2100], [894009600000, 2275.5263122324022], [896688000000, 2410.219111529472], [899280000000, 2447], [901958400000, 2270], [904636800000, 2150], [907228800000, 1975], [909907200000, 1850], [912499200000, 1850], [915177600000, 1850], [917856000000, 1900], [920275200000, 2000], [922953600000, 2100], [925545600000, 2293.223035924462], [928224000000, 2333.1096643226288], [930816000000, 2353.8386253393196], [933494400000, 2219.859084479606], [936172800000, 2111.659317942451], [938764800000, 1975], [941443200000, 1849.9999999999998], [944035200000, 1850], [946713600000, 1850], [949392000000, 1900], [951897600000, 2000], [954576000000, 2100], [957168000000, 2300], [959846400000, 2291.5704881582233], [962438400000, 2306.4403357574015], [965116800000, 2176.011475881167], [967795200000, 2055.2537370865784], [970387200000, 1869.7521062059038], [973065600000, 1809.7604150608627], [975657600000, 1777.7148456446575], [978336000000, 1767.079683308652], [981014400000, 1766.8623435164145], [983433600000, 1797.1530782513996], [986112000000, 1967.493530078274], [988704000000, 2066.814931163264], [991382400000, 2021.7653757128583], [993974400000, 1834.269059437614], [996652800000, 1621.9496179265686], [999331200000, 1422.7719078087691], [1001923200000, 1313.911535287006], [1004601600000, 1241.4426258129515], [1007193600000, 1262.2378498455068], [1009872000000, 1335.0657995580145], [1012550400000, 1565.0570174501388], [1014969600000, 1674.9600090059741], [1017648000000, 1777.4723397304779], [1020240000000, 1984.573926945773], [1022918400000, 1893.5842521500276], [1025510400000, 1795.2841908387963], [1028188800000, 1645.6820325587946], [1030867200000, 1495.3133783357669], [1033459200000, 1381.983647846188], [1036137600000, 1319.347443162511], [1038729600000, 1281.6714252473832], [1041408000000, 1465.1633878692628], [1044086400000, 1777.5308308564508], [1046505600000, 1888.9999381829507], [1049184000000, 2043.6154663406924], [1051776000000, 2243.47145698568], [1054454400000, 2309.345905603767], [1057046400000, 2384.4444143546634], [1059724800000, 2268.21537649069], [1062403200000, 2085.5516639448465], [1064995200000, 1900.700172958508]]
    }], "gui_link_title": "Trinity Reservoir Storage", "taf": false, "month_period_title": "October", "statistics": "Averages"
});

function getSeries(datum) {
    let series = new Array(datum.length);
    for (var i = 0; i < datum.length; i++) {
        let timeSeries = datum[i]['period_filtered_time_series'];
        let dataOnly = [];
        for (var j = 0; j < timeSeries.length; j++) {
            dataOnly.push(timeSeries[j][1]);
        }
        dataOnly.sort((a, b) => a - b);
        let data = [];
        for (var k = 0; k < dataOnly.length; k++) {
            let exceedance = 1 - ((k+ 0.5)/dataOnly.length);
            data.push([exceedance * 100, dataOnly[k]]);
        }
        series[i] = {
            name: datum[i]['scenario_name'],
            data: data
        };
    }
    return series;
}

function plot(data) {
    var units = getUnits(data);
    let datum = data['scenario_run_data'];
    var series = getSeries(datum);
    chart = Highcharts.chart('container', {
        mapNavigation: {
            enableMouseWheelZoom: true
        },
        chart: {
            panKey: 'shift',
            panning: true,
            animation: false,
            zoomType: 'xy'
        },
        title: {
            text: data['month_period_title'] + ' ' + data['gui_link_title']
        },
        xAxis: {
            min: 0,
            max: 100,
            reversed: 'true',
            gridLineWidth: 1,
            title: {
                text: 'Probability of Exceedence',
                align: 'middle'
            },
            scrollbar: {
                enabled: true
            },
            labels: {
                overflow: 'justify',
                formatter: function () {
                    return this.value + "%";
                }
            }
        },
        yAxis: {
            type: 'logarithmic',
            minRange:0.1,
            title: {
                text: 'Volume (' + units + ')',
                align: 'middle'
            },
            labels: {
                overflow: 'justify'
            },
            scrollbar: {
                enabled: true
            },
        },
        tooltip: {
            valueSuffix: ' ' + units,
            headerFormat: '<span style="font-size: 10px">{point.key:.2f}%</span><br/>'
        },
        legend: {
            enabled: true
        },
        series: series
    });
    postInit(chart);
}
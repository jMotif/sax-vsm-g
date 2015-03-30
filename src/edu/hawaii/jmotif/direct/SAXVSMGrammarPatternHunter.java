package edu.hawaii.jmotif.direct;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import org.slf4j.LoggerFactory;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import edu.hawaii.jmotif.repair.BagConstructionStrategy;
import edu.hawaii.jmotif.repair.RePairFactory;
import edu.hawaii.jmotif.sax.NumerosityReductionStrategy;
import edu.hawaii.jmotif.sax.SAXProcessor;
import edu.hawaii.jmotif.sax.TSProcessor;
import edu.hawaii.jmotif.sax.alphabet.NormalAlphabet;
import edu.hawaii.jmotif.sax.datastructures.SAXRecords;
import edu.hawaii.jmotif.text.TextUtils;
import edu.hawaii.jmotif.text.WordBag;
import edu.hawaii.jmotif.util.UCRUtils;

/**
 * This implements a classifier.
 * 
 * @author psenin
 * 
 */
public class SAXVSMGrammarPatternHunter {

  private static final DecimalFormatSymbols otherSymbols = new DecimalFormatSymbols(Locale.US);
  private static DecimalFormat df = new DecimalFormat("0.00###", otherSymbols);

  private static String TRAINING_DATA;
  private static String TEST_DATA;
  private static Map<String, List<double[]>> trainData;
  private static Map<String, List<double[]>> testData;

  private static Integer WINDOW_SIZE;
  private static Integer PAA_SIZE;
  private static Integer ALPHABET_SIZE;
  private static NumerosityReductionStrategy STRATEGY;
  private static BagConstructionStrategy CONSTRUCTION_STRATEGY;

  private static final double DEFAULT_NORMALIZATION_THRESHOLD = 0.05;
  private static double NORMALIZATION_THRESHOLD = DEFAULT_NORMALIZATION_THRESHOLD;
  private static NormalAlphabet na;
  private static TextUtils tu;
  private static SAXProcessor sp;
  private static TSProcessor tsp;

  // static block - we instantiate the logger
  //
  private static final Logger consoleLogger;
  private static final Level LOGGING_LEVEL = Level.INFO;

  private static final String CR = "\n";
  private static final int MAX_PATTERNS_2PRINT = 9;
  private static final int MAX_SERIES_2PRINT = 10;
  static {
    consoleLogger = (Logger) LoggerFactory.getLogger(SAXVSMGrammarPatternHunter.class);
    consoleLogger.setLevel(LOGGING_LEVEL);
  }

  /**
   * Main CLI runnable.
   * 
   * @param args the command line args.
   * @throws Exception if error occurs.
   */
  public static void main(String[] args) throws Exception {

    try {
      // args: <train dataset>, <test dataset>, Wsize , Psize, Asize, Startegy
      consoleLogger.info("processing parameters: " + Arrays.toString(args));

      WINDOW_SIZE = Integer.valueOf(args[2]);
      PAA_SIZE = Integer.valueOf(args[3]);
      ALPHABET_SIZE = Integer.valueOf(args[4]);

      STRATEGY = NumerosityReductionStrategy.valueOf(args[5].toUpperCase());

      if (args.length > 6) {
        NORMALIZATION_THRESHOLD = Double.valueOf(args[6]);
      }
      if (args.length > 7) {
        CONSTRUCTION_STRATEGY = BagConstructionStrategy.valueOf(args[7].toUpperCase());
      }

      TRAINING_DATA = args[0];
      TEST_DATA = args[1];
      trainData = UCRUtils.readUCRData(TRAINING_DATA);
      consoleLogger.info("trainData classes: " + trainData.size() + ", series length: "
          + trainData.entrySet().iterator().next().getValue().get(0).length);
      for (Entry<String, List<double[]>> e : trainData.entrySet()) {
        consoleLogger.info(" training class: " + e.getKey() + " series: " + e.getValue().size());
      }

      testData = UCRUtils.readUCRData(TEST_DATA);
      consoleLogger.info("testData classes: " + testData.size() + ", series length: "
          + testData.entrySet().iterator().next().getValue().get(0).length);
      for (Entry<String, List<double[]>> e : testData.entrySet()) {
        consoleLogger.info(" test class: " + e.getKey() + " series: " + e.getValue().size());
      }

    }
    catch (Exception e) {
      System.err.println("There was parameters error....");
      System.exit(-10);
    }

    na = new NormalAlphabet();
    tu = new TextUtils();
    sp = new SAXProcessor();
    tsp = new TSProcessor();

    // making training bags collection
    List<WordBag> bags = RePairFactory.labeledSeries2GrammarWordBags(trainData, WINDOW_SIZE,
        PAA_SIZE, na.getCuts(ALPHABET_SIZE), STRATEGY, NORMALIZATION_THRESHOLD,
        CONSTRUCTION_STRATEGY);

    // getting TFIDF done
    HashMap<String, HashMap<String, Double>> tfidf = tu.computeTFIDFInstrumented(bags);

    // System.out.println(tu.tfidfToTable(tfidf));
    // get these normalized
    // tfidf = tu.normalizeToUnitVectors(tfidf);

    // get best patterns for each class printed
    //
    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {

      // sort the class' patterns
      //
      String className = e.getKey();
      ArrayList<Entry<String, Double>> values = new ArrayList<Entry<String, Double>>();
      values.addAll(e.getValue().entrySet());
      Collections.sort(values, new TfIdfEntryComparator());

      // print the class' key
      //
      System.out.print("Class key: " + className + CR);

      // while stopping criterion isn't met, iterate
      //
      for (int i = 0; i < MAX_PATTERNS_2PRINT; i++) {

        // the pattern we working with
        String pattern = values.get(i).getKey();
        Double weight = values.get(i).getValue();
        System.out.println("pattern=\"" + pattern + "\"; weight=" + df.format(weight));

        // init buffers
        StringBuffer seriesBuff = new StringBuffer("series = c(");
        StringBuffer startsBuff = new StringBuffer("starts = c(");
        StringBuffer stopsBuff = new StringBuffer("stops = c(");

        Map<Integer, Map<String, Integer[]>> hits = getPatternLocationsForTheClass(className,
            testData, pattern, WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE);

        int k = 0;
        int printedK = 0;
        do {
          if (hits.get(k).get("starts").length > 0) {
            System.out.println(k + ": " + Arrays.toString(hits.get(k).get("starts")));
            for (int j = 0; j < hits.get(k).get("starts").length; j++) {
              seriesBuff.append(String.valueOf(k + 1) + ",");
              startsBuff.append(String.valueOf(hits.get(k).get("starts")[j] + 1) + ",");
              stopsBuff.append(String.valueOf(hits.get(k).get("stops")[j] + 1) + ",");
            }
            printedK++;
          }
          k++;
        }
        while (k < hits.size() && printedK < MAX_SERIES_2PRINT);

        System.out.print(seriesBuff.delete(seriesBuff.length() - 1, seriesBuff.length()).toString()
            + ")" + CR);
        System.out.print(startsBuff.delete(startsBuff.length() - 1, startsBuff.length()).toString()
            + ")" + CR);
        System.out.print(stopsBuff.delete(stopsBuff.length() - 1, stopsBuff.length()).toString()
            + ")" + CR + "#" + CR);

      }
    }

    // classifying
    int testSampleSize = 0;
    int positiveTestCounter = 0;
    for (String label : tfidf.keySet()) {

      List<double[]> testD = testData.get(label);

      for (double[] series : testD) {
        WordBag test = RePairFactory.seriesToGrammarWordBag("tmp", series, WINDOW_SIZE, PAA_SIZE,
            na.getCuts(ALPHABET_SIZE), STRATEGY, NORMALIZATION_THRESHOLD,
            BagConstructionStrategy.REDUCED);
        String testLabel = tu.classify(test, tfidf);

        if (label.equalsIgnoreCase(testLabel)) {
          positiveTestCounter++;

          HashMap<String, Double> ref = tfidf.get(testLabel);

        }
        testSampleSize++;

      }
    }

    // accuracy and error
    double accuracy = (double) positiveTestCounter / (double) testSampleSize;
    double error = 1.0d - accuracy;

    // report results
    consoleLogger.info("classification results: accuracy " + df.format(accuracy) + ", error "
        + df.format(error));

  }

  private static Map<Integer, Map<String, Integer[]>> getPatternLocationsForTheClass(
      String className, Map<String, List<double[]>> trainData, String pattern, int windowSize,
      int paaSize, int alphabetSize) throws Exception {

    // the resulting map
    Map<Integer, Map<String, Integer[]>> res = new HashMap<Integer, Map<String, Integer[]>>();

    // series counter
    int seriesCounter = 0;
    // iterating over class' data
    for (double[] ts : trainData.get(className)) {

      // get the SAX conversion out
      SAXRecords saxData = sp.ts2saxViaWindow(ts, windowSize, paaSize, na.getCuts(alphabetSize),
          STRATEGY, NORMALIZATION_THRESHOLD);
      saxData.buildIndex();
      String str = saxData.getSAXString(" ");

      // search for pattern's hits on the raw string
      List<Integer> starts = new ArrayList<Integer>();
      List<Integer> stops = new ArrayList<Integer>();
      int idx = str.indexOf(pattern, 0);
      while (idx > -1) {
        // if there is a hit
        // [1] extract the string position
        int spaceCount = idx - str.substring(0, idx + 1).replaceAll(" ", "").length();
        // [2] convert this into the real time series index
        starts.add(saxData.mapStringIndexToTSPosition(spaceCount + 1));
        // [3] the length of this pattern
        int patternSpaceCount = pattern.length() - pattern.replaceAll(" ", "").length();
        stops.add(saxData.mapStringIndexToTSPosition(spaceCount + 1 + patternSpaceCount)
            + windowSize);
        // update the current hit index
        idx = str.indexOf(pattern, idx + 1);
      }

      Map<String, Integer[]> entry = new HashMap<String, Integer[]>();
      entry.put("starts", starts.toArray(new Integer[starts.size()]));
      entry.put("stops", stops.toArray(new Integer[stops.size()]));
      res.put(seriesCounter, entry);
      seriesCounter++;
    }

    return res;
  }

}

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
public class SAXVSMGrammarPatternPrinter {

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
  private static final int MAX_PATTERNS_2PRINT = 30;
  private static final int MAX_SERIES_2PRINT = 10;
  static {
    consoleLogger = (Logger) LoggerFactory.getLogger(SAXVSMGrammarPatternPrinter.class);
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
        BagConstructionStrategy.REDUCED);

    // getting TFIDF done
    HashMap<String, HashMap<String, Double>> tfidf = tu.computeTFIDFInstrumented(bags);

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
        StringBuffer offsetBuff = new StringBuffer("offsets = c(");
        StringBuffer lengthBuff = new StringBuffer("lengths = c(");

        Map<Integer, Integer[]> hits = getPatternLocationsForTheClass(className, testData, pattern,
            WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE);

        int k = 0;
        int printedK = 0;
        do {
          if (hits.get(k).length > 0) {
            System.out.println(k + ": " + Arrays.toString(hits.get(k)));
            for (int offset : hits.get(k)) {
              seriesBuff.append(String.valueOf(k + 1) + ",");
              offsetBuff.append(String.valueOf(offset + 1) + ",");
            }
            printedK++;
          }
          k++;
        }
        while (k < hits.size() && printedK < MAX_SERIES_2PRINT);

        System.out.print(seriesBuff.delete(seriesBuff.length() - 1, seriesBuff.length()).toString()
            + ")" + CR);
        System.out.print(offsetBuff.delete(offsetBuff.length() - 1, offsetBuff.length()).toString()
            + ")" + CR + "#" + CR);

      }
      // List<double[]> testD = testData.get(CLASS_UNDER_INVESTIGATION);
      // int seriesIdx = 0;
      // for (double[] series : testD) {
      // int classificationResult = TextUtils.classify(className, series, tfidf, PAA_SIZE,
      // ALPHABET_SIZE, WINDOW_SIZE, strategy);
      // if (0 == classificationResult) {
      // System.out.println(seriesIdx + 1);
      // }
      // seriesIdx++;
      // }
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

  private static Map<Integer, Integer[]> getPatternLocationsForTheClass(String className,
      Map<String, List<double[]>> trainData, String pattern, int windowSize, int paaSize,
      int alphabetSize) throws Exception {

    Map<Integer, Integer[]> res = new HashMap<Integer, Integer[]>();

    int seriesCounter = 0;
    for (double[] ts : trainData.get(className)) {

      SAXRecords saxData = sp.ts2saxViaWindow(ts, windowSize, paaSize, na.getCuts(alphabetSize),
          STRATEGY, NORMALIZATION_THRESHOLD);
      saxData.buildIndex();

      String str = saxData.getSAXString(" ");

      List<Integer> arr = new ArrayList<Integer>();
      int idx = str.indexOf(pattern, 0);
      while (idx > -1) {
        int spaceCount = idx - str.substring(0, idx + 1).replaceAll(" ", "").length();
        arr.add(saxData.mapStringIndexToTSPosition(spaceCount + 1));
        idx = str.indexOf(pattern, idx + 1);
      }

      res.put(seriesCounter, arr.toArray(new Integer[0]));
      seriesCounter++;
    }

    return res;
  }

}

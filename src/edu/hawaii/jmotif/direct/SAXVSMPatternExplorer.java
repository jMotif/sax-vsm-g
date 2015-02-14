package edu.hawaii.jmotif.direct;

import java.io.IOException;
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
import edu.hawaii.jmotif.sax.alphabet.Alphabet;
import edu.hawaii.jmotif.sax.alphabet.NormalAlphabet;
import edu.hawaii.jmotif.text.SAXNumerosityReductionStrategy;
import edu.hawaii.jmotif.text.TextUtils;
import edu.hawaii.jmotif.text.WordBag;
import edu.hawaii.jmotif.timeseries.TSException;
import edu.hawaii.jmotif.timeseries.TSUtils;
import edu.hawaii.jmotif.util.UCRUtils;

/**
 * Hunts for best scoring patterns for the class and prints them out.
 * 
 * @author psenin
 * 
 */
public class SAXVSMPatternExplorer {

  // defines the amount of words from each class's vector to print
  //
  private static final int MAX_WORDS_2_PRINT = 5;

  // how many patterns and how many series to output
  //
  private static final int MAX_SERIES_2PRINT = 5;
  private static final int MAX_PATTERNS_2PRINT = 5;

  private static String TRAINING_DATA;
  private static String TEST_DATA;

  private static Integer WINDOW_SIZE;
  private static Integer PAA_SIZE;
  private static Integer ALPHABET_SIZE;
  private static Map<String, List<double[]>> trainData;
  private static Map<String, List<double[]>> testData;
  private static SAXNumerosityReductionStrategy STRATEGY;

  private static final DecimalFormatSymbols otherSymbols = new DecimalFormatSymbols(Locale.US);
  private static DecimalFormat df = new DecimalFormat("0.00###", otherSymbols);
  private static final String COMMA = ", ";
  private static final String CR = "\n";

  // static block - we instantiate the logger
  //
  private static final Logger consoleLogger;
  private static final Level LOGGING_LEVEL = Level.INFO;
  static {
    consoleLogger = (Logger) LoggerFactory.getLogger(SAXVSMPatternExplorer.class);
    consoleLogger.setLevel(LOGGING_LEVEL);
  }

  /**
   * @param args
   * @throws TSException
   * @throws IndexOutOfBoundsException
   * @throws IOException
   */
  public static void main(String[] args) throws IndexOutOfBoundsException, TSException, IOException {

    // parsing the parameters
    //
    try {
      // args: <train dataset>, <test dataset>, Wsize , Psize, Asize, Startegy
      consoleLogger.info("processing paramleters: " + Arrays.toString(args));

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

      WINDOW_SIZE = Integer.valueOf(args[2]);
      PAA_SIZE = Integer.valueOf(args[3]);
      ALPHABET_SIZE = Integer.valueOf(args[4]);

      STRATEGY = SAXNumerosityReductionStrategy.valueOf(args[5].toUpperCase());

    }
    catch (Exception e) {
      System.err.println("There was parameters error....");
      System.exit(-10);
    }

    int[] params = new int[] { WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE, STRATEGY.index() };

    List<WordBag> bags = TextUtils.labeledSeries2WordBags(trainData, params,
        TSUtils.GLOBAL_NORMALIZATION_THRESHOLD);

    // get tfidf statistics
    HashMap<String, HashMap<String, Double>> tfidf = TextUtils.computeTFIDF(bags);

    // normalize all vectors to a unit - so it will be fair comparison
    tfidf = TextUtils.normalizeToUnitVectors(tfidf);

    // sort words by their weight and print top 10 of these for each class
    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {
      String className = e.getKey();
      ArrayList<Entry<String, Double>> values = new ArrayList<Entry<String, Double>>();
      values.addAll(e.getValue().entrySet());

      Collections.sort(values, new TfIdfEntryComparator());

      System.out.print("Class key: " + className + CR);
      for (int i = 0; i < MAX_WORDS_2_PRINT; i++) {
        String pattern = values.get(i).getKey();
        Double weight = values.get(i).getValue();
        System.out.println("\"" + pattern + "\", " + weight);
      }

    }

    // get best patterns for each class
    //
    // iterating over each of classes
    //
    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {

      // class name and weights vector
      String className = e.getKey();
      ArrayList<Entry<String, Double>> values = new ArrayList<Entry<String, Double>>();
      values.addAll(e.getValue().entrySet());

      // form the output
      Collections.sort(values, new TfIdfEntryComparator());
      System.out.print("Class key: " + className + CR);

      for (int i = 0; i < MAX_PATTERNS_2PRINT; i++) {

        String pattern = values.get(i).getKey();
        Double weight = values.get(i).getValue();
        System.out.println("pattern=\"" + pattern + "\"; weight=" + df.format(weight));

        StringBuffer seriesBuff = new StringBuffer("series = c(");
        StringBuffer offsetBuff = new StringBuffer("offsets = c(");
        Map<Integer, Integer[]> hits = getPatternLocationsForTheClass(className, trainData,
            pattern, WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE);

        int k = 0;
        int printedK = 0;
        do {
          if (hits.get(k).length > 0) {
            System.out.print(k + ": " + Arrays.toString(hits.get(k)) + ", ");
            System.out.println(Arrays.toString(trainData.get(className).get(k)));
            System.out.println(Arrays.toString(seriesValuesAsHeat(trainData.get(className).get(k),
                className, tfidf, WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE)));
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

      System.out.print("Missclassified for Class key: " + className + CR);
      List<double[]> testD = testData.get(className);
      int seriesIdx = 0;
      for (double[] series : testD) {
        int classificationResult = TextUtils.classify(className, series, tfidf, PAA_SIZE,
            ALPHABET_SIZE, WINDOW_SIZE, STRATEGY, TSUtils.GLOBAL_NORMALIZATION_THRESHOLD);
        if (0 == classificationResult) {
          System.out.println(seriesIdx + 1);
        }
        seriesIdx++;
      }
      System.out.println(" ============== ");
    }

  }

  private static double[] seriesValuesAsHeat(double[] series, String className,
      HashMap<String, HashMap<String, Double>> tfidf, int window_size, int paa_size,
      int alphabet_size) throws TSException {

    Alphabet a = new NormalAlphabet();

    double[] weights = new double[series.length];
    HashMap<String, Integer> words = new HashMap<String, Integer>();

    for (int i = 0; i <= series.length - window_size; i++) {
      double[] subseries = TSUtils.subseries(series, i, window_size);
      double[] paa = TSUtils.paa(TSUtils.zNormalize(subseries), paa_size);
      char[] sax = TSUtils.ts2String(paa, a.getCuts(alphabet_size));
      words.put(String.valueOf(sax), i);
    }

    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {
      for (Entry<String, Double> e1 : e.getValue().entrySet()) {
        if (words.containsKey(e1.getKey())) {
          int idx = words.get(e1.getKey());
          double increment = 0.0;
          if (className.equalsIgnoreCase(e.getKey())) {
            increment = e1.getValue();
          }
          else {
            // increment = -e1.getValue();
          }

          for (int i = 0; i < window_size; i++) {
            weights[idx + i] = weights[idx + i] + increment;
          }
        }
      }
    }

    return weights;

  }

  private static Map<Integer, Integer[]> getPatternLocationsForTheClass(String className,
      Map<String, List<double[]>> trainData, String pattern, int windowSize, int paaSize,
      int alphabetSize) throws IndexOutOfBoundsException, TSException {

    Alphabet a = new NormalAlphabet();

    Map<Integer, Integer[]> res = new HashMap<Integer, Integer[]>();

    int seriesCounter = 0;
    for (double[] series : trainData.get(className)) {

      List<Integer> arr = new ArrayList<Integer>();

      for (int i = 0; i <= series.length - windowSize; i++) {
        double[] paa = TSUtils.paa(TSUtils.zNormalize(TSUtils.subseries(series, i, windowSize)),
            paaSize);
        char[] sax = TSUtils.ts2String(paa, a.getCuts(alphabetSize));
        if (pattern.equalsIgnoreCase(String.valueOf(sax))) {
          arr.add(i);
        }
      }

      res.put(seriesCounter, arr.toArray(new Integer[0]));
      seriesCounter++;
    }

    return res;
  }
}

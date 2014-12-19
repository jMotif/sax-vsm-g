package edu.hawaii.jmotif.direct;

import java.io.IOException;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import org.slf4j.LoggerFactory;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import edu.hawaii.jmotif.repair.GrammarRuleRecord;
import edu.hawaii.jmotif.repair.GrammarRules;
import edu.hawaii.jmotif.repair.RePairFactory;
import edu.hawaii.jmotif.repair.RePairRule;
import edu.hawaii.jmotif.sax.NumerosityReductionStrategy;
import edu.hawaii.jmotif.sax.datastructures.SAXRecords;
import edu.hawaii.jmotif.sax.datastructures.SaxRecord;
import edu.hawaii.jmotif.sax.parallel.ParallelSAXImplementation;
import edu.hawaii.jmotif.text.SAXNumerosityReductionStrategy;
import edu.hawaii.jmotif.text.TextUtils;
import edu.hawaii.jmotif.text.WordBag;
import edu.hawaii.jmotif.timeseries.TSException;
import edu.hawaii.jmotif.util.UCRUtils;

/**
 * This implements a classifier.
 * 
 * @author psenin
 * 
 */
public class SAXVSMGrammarClassifier {

  private static final DecimalFormatSymbols otherSymbols = new DecimalFormatSymbols(Locale.US);
  private static DecimalFormat fmt = new DecimalFormat("0.00###", otherSymbols);

  private static final String COMMA = ", ";

  private static String TRAINING_DATA;
  private static String TEST_DATA;

  private static Integer WINDOW_SIZE;
  private static Integer PAA_SIZE;
  private static Integer ALPHABET_SIZE;
  private static Map<String, List<double[]>> trainData;
  private static Map<String, List<double[]>> testData;
  private static SAXNumerosityReductionStrategy STRATEGY;

  // static block - we instantiate the logger
  //
  private static final Logger consoleLogger;
  private static final Level LOGGING_LEVEL = Level.INFO;

  static {
    consoleLogger = (Logger) LoggerFactory.getLogger(SAXVSMGrammarClassifier.class);
    consoleLogger.setLevel(LOGGING_LEVEL);
  }

  public static void main(String[] args) throws IOException, IndexOutOfBoundsException, TSException {

    try {
      // args: <train dataset>, <test dataset>, Wsize , Psize, Asize, Startegy
      consoleLogger.info("processing paramleters: " + Arrays.toString(args));

      WINDOW_SIZE = Integer.valueOf(args[2]);
      PAA_SIZE = Integer.valueOf(args[3]);
      ALPHABET_SIZE = Integer.valueOf(args[4]);

      STRATEGY = SAXNumerosityReductionStrategy.valueOf(args[5].toUpperCase());

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
    int[] params = new int[] { WINDOW_SIZE, PAA_SIZE, ALPHABET_SIZE, STRATEGY.index() };
    classify(params);
  }

  private static void classify(int[] params) throws IndexOutOfBoundsException, TSException {
    // making training bags collection
    List<WordBag> bags = labeledSeries2GrammarWordBags(trainData, params);
    // getting TFIDF done
    HashMap<String, HashMap<String, Double>> tfidf = TextUtils.computeTFIDF(bags);
    // classifying
    int testSampleSize = 0;
    int positiveTestCounter = 0;
    for (String label : tfidf.keySet()) {
      List<double[]> testD = testData.get(label);
      for (double[] series : testD) {
        positiveTestCounter = positiveTestCounter + classifyGrammar(label, series, tfidf, params);
        testSampleSize++;
      }
    }

    // accuracy and error
    double accuracy = (double) positiveTestCounter / (double) testSampleSize;
    double error = 1.0d - accuracy;

    // report results
    consoleLogger.info("classification results: " + toLogStr(params, accuracy, error));

  }

  protected static String toLogStr(int[] p, double accuracy, double error) {

    StringBuffer sb = new StringBuffer();
    if (SAXNumerosityReductionStrategy.CLASSIC.index() == p[3]) {
      sb.append("CLASSIC, ");
    }
    else if (SAXNumerosityReductionStrategy.EXACT.index() == p[3]) {
      sb.append("EXACT, ");
    }
    else if (SAXNumerosityReductionStrategy.NOREDUCTION.index() == p[3]) {
      sb.append("NOREDUCTION, ");
    }
    sb.append("window ").append(p[0]).append(COMMA);
    sb.append("PAA ").append(p[1]).append(COMMA);
    sb.append("alphabet ").append(p[2]).append(COMMA);
    sb.append(" accuracy ").append(fmt.format(accuracy)).append(COMMA);
    sb.append(" error ").append(fmt.format(error));

    return sb.toString();
  }

  private static int classifyGrammar(String classKey, double[] series,
      HashMap<String, HashMap<String, Double>> tfidf, int[] params)
      throws IndexOutOfBoundsException, TSException {

    WordBag test = seriesToGrammarWordBag("test", series, params);

    // it is Cosine similarity,
    //
    // which ranges from 0.0 for the angle of 90 to 1.0 for the angle of 0
    // i.e. LARGES value is a SMALLEST distance
    double minDist = Double.MIN_VALUE;
    String className = "";
    double[] cosines = new double[tfidf.entrySet().size()];

    int index = 0;
    for (Entry<String, HashMap<String, Double>> e : tfidf.entrySet()) {

      double dist = TextUtils.cosineSimilarity(test, e.getValue());
      cosines[index] = dist;
      index++;

      if (dist > minDist) {
        className = e.getKey();
        minDist = dist;
      }

    }

    // sometimes, due to the VECTORs specific layout, all values are the same, NEED to take care
    boolean allEqual = true;
    double cosine = cosines[0];
    for (int i = 1; i < cosines.length; i++) {
      if (!(cosines[i] == cosine)) {
        allEqual = false;
      }
    }

    // report our findings
    if (!(allEqual) && className.equalsIgnoreCase(classKey)) {
      return 1;
    }

    // System.out.println("all equal " + allEqual + ", assigned to " + className + " instead of " +
    // classKey);

    return 0;
  }

  private static List<WordBag> labeledSeries2GrammarWordBags(Map<String, List<double[]>> data,
      int[] params) throws IndexOutOfBoundsException, TSException {
    // make a map of resulting bags
    Map<String, WordBag> preRes = new HashMap<String, WordBag>();

    // process series one by one building word bags
    for (Entry<String, List<double[]>> e : data.entrySet()) {

      String classLabel = e.getKey();
      WordBag bag = new WordBag(classLabel);

      for (double[] series : e.getValue()) {
        WordBag cb = seriesToGrammarWordBag("tmp", series, params);
        bag.mergeWith(cb);
      }

      preRes.put(classLabel, bag);
    }

    List<WordBag> res = new ArrayList<WordBag>();
    res.addAll(preRes.values());
    return res;
  }

  private static WordBag seriesToGrammarWordBag(String label, double[] series, int[] params)
      throws IndexOutOfBoundsException, TSException {

    int windowSize = params[0];
    int paaSize = params[1];
    int alphabetSize = params[2];
    NumerosityReductionStrategy strategy = NumerosityReductionStrategy.fromValue(params[3]);

    WordBag resultBag = new WordBag(label);

    ParallelSAXImplementation ps = new ParallelSAXImplementation();
    SAXRecords saxData = ps.process(series, 1, windowSize, paaSize, alphabetSize, strategy, 0.05);
    saxData.buildIndex();

    @SuppressWarnings("unused")
    RePairRule rePairGrammar = RePairFactory.buildGrammar(saxData);
    RePairRule.expandRules();
    GrammarRules rules = RePairRule.toGrammarRulesData();

    for (GrammarRuleRecord r : rules) {
      if (0 == r.getRuleNumber()) {
        // extracting all basic tokens
        for (SaxRecord sr : saxData) {
          resultBag.addWord(String.valueOf(sr.getPayload()), sr.getIndexes().size());
        }
      }
      else {
        // extracting all longer tokens
        String str = r.getExpandedRuleString();
        resultBag.addWord(str);
      }
    }

    // System.out.println("Strategy: " + strategy.index());

    return resultBag;
  }

}

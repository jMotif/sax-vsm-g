package edu.hawaii.jmotif.direct;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.slf4j.LoggerFactory;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import edu.hawaii.jmotif.repair.BagConstructionStrategy;
import edu.hawaii.jmotif.repair.RePairFactory;
import edu.hawaii.jmotif.sax.NumerosityReductionStrategy;
import edu.hawaii.jmotif.sax.SAXProcessor;
import edu.hawaii.jmotif.sax.alphabet.NormalAlphabet;
import edu.hawaii.jmotif.text.TextUtils;
import edu.hawaii.jmotif.text.WordBag;
import edu.hawaii.jmotif.util.StackTrace;
import edu.hawaii.jmotif.util.UCRUtils;

/**
 * Implements a cross-validation DIRECT-based procedure for SAX-VSM parameters optimization.
 * 
 * @author psenin
 * 
 */
public class SAXVSMGridGrammarSampler {

  // the number formatter
  private static final DecimalFormatSymbols otherSymbols = new DecimalFormatSymbols(Locale.US);
  private static DecimalFormat fmt = new DecimalFormat("0.00###", otherSymbols);

  private static final double DEFAULT_NORMALIZATION_THRESHOLD = 0.05;

  private static SAXVSMGrammarCVErrorFunction function;

  // static block - we instantiate the logger
  //
  private static final Logger consoleLogger;
  private static final Level LOGGING_LEVEL = Level.INFO;

  private static final String COMMA = ", ";
  private static final String CR = "\n";
  static {
    consoleLogger = (Logger) LoggerFactory.getLogger(SAXVSMGridGrammarSampler.class);
    consoleLogger.setLevel(LOGGING_LEVEL);
  }

  private static int[] upperBounds;
  private static int[] lowerBounds;
  private static String TRAINING_DATA;
  private static String TEST_DATA;
  private static int HOLD_OUT_NUM = 1;
  private static Double NORMALIZATION_THRESHOLD = DEFAULT_NORMALIZATION_THRESHOLD;

  private static Map<String, List<double[]>> trainData;
  private static Map<String, List<double[]>> testData;
  private static long tstamp1;

  private static NumerosityReductionStrategy STRATEGY = null;

  private static BagConstructionStrategy BAG_STRATEGY = BagConstructionStrategy.REDUCED;

  /**
   * Main runnable.
   * 
   * @param args
   * @throws Exception
   */
  public static void main(String[] args) throws Exception {

    try {

      // args: <train dataset>, <test dataset>, Wmin Wmax, Pmin Pmax, Amin Amax, Holdout, Iterations
      consoleLogger.info("Current date: " + (new DateTime(DateTimeZone.UTC)).toString());
      consoleLogger.info("processing parameters: " + Arrays.toString(args));
      tstamp1 = System.currentTimeMillis();

      // odd a bit, but whatever, it works
      if (10 == args.length || 11 == args.length || 12 == args.length) {

        // working on train data
        TRAINING_DATA = args[0];
        trainData = UCRUtils.readUCRData(TRAINING_DATA);
        consoleLogger.info(UCRUtils.datasetStats(trainData, "trainData"));

        TEST_DATA = args[1];
        testData = UCRUtils.readUCRData(TEST_DATA);
        consoleLogger.info(UCRUtils.datasetStats(testData, "testData"));

        // args: <train dataset>, <test dataset>, Wmin Wmax, Pmin Pmax, Amin Amax, Holdout,
        // Iterations
        lowerBounds = new int[] { Integer.valueOf(args[2]).intValue(),
            Integer.valueOf(args[4]).intValue(), Integer.valueOf(args[6]).intValue() };
        upperBounds = new int[] { Integer.valueOf(args[3]).intValue(),
            Integer.valueOf(args[5]).intValue(), Integer.valueOf(args[7]).intValue() };

        // args: <train dataset>, <test dataset>, Wmin Wmax, Pmin Pmax, Amin Amax, Holdout,
        // Iterations
        HOLD_OUT_NUM = Integer.valueOf(args[8]);

        if (args.length > 9) {
          NORMALIZATION_THRESHOLD = Double.valueOf(args[9]);
        }

        if (args.length > 10) {
          if (!"*".equalsIgnoreCase(args[10])) {
            STRATEGY = NumerosityReductionStrategy.valueOf(args[10].toUpperCase());
          }
        }

        if (args.length > 11) {
          BAG_STRATEGY = BagConstructionStrategy.valueOf(args[11].toUpperCase());
        }
      }
      else {
        System.out.print(printHelp());
        System.exit(-10);
      }
    }
    catch (Exception e) {
      System.err.println("There was parameters error....");
      System.err.println(StackTrace.toString(e));
      System.out.print(printHelp());
      System.exit(-10);
    }

    if (null == STRATEGY) {
      consoleLogger.info("running sampling for " + NumerosityReductionStrategy.MINDIST.toString()
          + " strategy...");
      int[] classicParams = sample(NumerosityReductionStrategy.MINDIST);

      consoleLogger.info("running sampling for " + NumerosityReductionStrategy.EXACT.toString()
          + " strategy...");
      int[] exactParams = sample(NumerosityReductionStrategy.EXACT);

      consoleLogger.info("running sampling for " + NumerosityReductionStrategy.NONE.toString()
          + " strategy...");
      int[] noredParams = sample(NumerosityReductionStrategy.NONE);

      classify(classicParams, NumerosityReductionStrategy.MINDIST);
      classify(exactParams, NumerosityReductionStrategy.EXACT);
      classify(noredParams, NumerosityReductionStrategy.NONE);
    }
    else {
      consoleLogger.info("running sampling for " + STRATEGY.toString() + " strategy...");
      int[] params = sample(STRATEGY);
      classify(params, STRATEGY);
    }
  }

  private static String printHelp() {
    StringBuffer sb = new StringBuffer();
    sb.append("SAX-VSM parameters optimization sampler ").append(CR);
    sb.append("Expects 12 parameters:").append(CR);
    sb.append(" [1] training dataset filename").append(CR);
    sb.append(" [2] test dataset filename").append(CR);
    sb.append(" [3] minimal sliding window size").append(CR);
    sb.append(" [4] maximal sliding window size").append(CR);
    sb.append(" [5] minimal PAA size").append(CR);
    sb.append(" [6] maximal PAA size").append(CR);
    sb.append(" [7] minimal Alphabet size").append(CR);
    sb.append(" [8] maximal Alphabet size").append(CR);
    sb.append(" [9] cross-validation hold-out number").append(CR);
    sb.append(" [10] OPTIONAL: normalization threshold").append(CR);
    sb.append(" [11] OPTIONAL: specific NR restriction").append(CR);
    sb.append(" [12] OPTIONAL: bag construction strategy").append(CR);
    sb.append("An execution example: $java -cp \"sax-vsm-classic20.jar\" edu.hawaii.jmotif.direct.SAXVSMDirectSampler");
    sb.append(" data/cbf/CBF_TRAIN data/cbf/CBF_TEST 10 120 5 60 2 18 1 0.01 EXACT ALL").append(CR);
    return sb.toString();
  }

  private static void classify(int[] params, NumerosityReductionStrategy strategy) throws Exception {

    int windowSize = Long.valueOf(Math.round(params[0])).intValue();
    int paaSize = Long.valueOf(Math.round(params[1])).intValue();
    int alphabetSize = Long.valueOf(Math.round(params[2])).intValue();

    NormalAlphabet na = new NormalAlphabet();
    TextUtils tu = new TextUtils();

    // making training bags collection
    List<WordBag> bags = RePairFactory.labeledSeries2GrammarWordBags(trainData, windowSize,
        paaSize, na.getCuts(alphabetSize), strategy, NORMALIZATION_THRESHOLD, BAG_STRATEGY);
    // getting TFIDF done
    HashMap<String, HashMap<String, Double>> tfidf = tu.computeTFIDFInstrumented(bags);
    // classifying
    int testSampleSize = 0;
    int positiveTestCounter = 0;
    for (String label : tfidf.keySet()) {
      List<double[]> testD = testData.get(label);
      for (double[] series : testD) {
        WordBag test = RePairFactory.seriesToGrammarWordBag("tmp", series, windowSize, paaSize,
            na.getCuts(alphabetSize), strategy, NORMALIZATION_THRESHOLD, BAG_STRATEGY);
        String testLabel = tu.classify(test, tfidf);
        if (label.equalsIgnoreCase(testLabel)) {
          positiveTestCounter++;
        }
        System.out.println("label " + label + "series " + testSampleSize + " positives "
            + positiveTestCounter);
        testSampleSize++;
      }
    }

    // accuracy and error
    double accuracy = (double) positiveTestCounter / (double) testSampleSize;
    double error = 1.0d - accuracy;

    // report results
    consoleLogger.info("time since start: "
        + SAXProcessor.timeToString(tstamp1, System.currentTimeMillis()));
    consoleLogger.info("classification results: " + toLogStr(params, accuracy, error));

  }

  private static int[] sample(NumerosityReductionStrategy strategy) {

    function = new SAXVSMGrammarCVErrorFunction(trainData, HOLD_OUT_NUM, strategy,
        NORMALIZATION_THRESHOLD, BAG_STRATEGY);

    double minimalValue = Double.MAX_VALUE;
    ArrayList<double[]> minimalParams = new ArrayList<double[]>();

    for (int winSize = lowerBounds[0]; winSize <= upperBounds[0]; winSize++) {
      for (int paaSize = lowerBounds[1]; paaSize <= upperBounds[1]; paaSize++) {
        for (int aSize = lowerBounds[2]; aSize <= upperBounds[2]; aSize++) {

          // double[] coords = point.toArray();
          // int windowSize = Long.valueOf(Math.round(coords[0])).intValue();
          // int paaSize = Long.valueOf(Math.round(coords[1])).intValue();
          // int alphabetSize = Long.valueOf(Math.round(coords[2])).intValue();

          double[] arr = new double[] { winSize, paaSize, aSize };
          Point p = Point.at(arr);
          double cv = function.valueAt(p);
          consoleLogger.info("@" + cv + "\t" + p.toLogString());
          if (cv < minimalValue) {
            minimalValue = cv;
            minimalParams.clear();
            minimalParams.add(Arrays.copyOf(arr, 3));
          }
          else if (cv <= minimalValue) {
            minimalParams.add(Arrays.copyOf(arr, 3));
          }

        }
      }
    }

    StringBuffer sb = new StringBuffer();
    HashSet<String> minimalValueParameters = new HashSet<String>();

    sb.append("min CV error ").append(fmt.format(minimalValue)).append(" reached at ");

    double[] params = null;

    for (double[] ps : minimalParams) {

      if (null == params) {
        double[] asArray = Arrays.copyOf(ps, ps.length);
        params = asArray;
        minimalValueParameters.add(Arrays.toString(asArray));
        sb.append(Arrays.toString(asArray)).append(COMMA);
      }
      else if (ps[0] < params[0]) {
        params = Arrays.copyOf(ps, ps.length);
        sb.append(Arrays.toString(params)).append(COMMA);
      }
    }

    consoleLogger.info(sb.toString());

    int[] res = new int[] { Math.round((float) params[0]), Math.round((float) params[1]),
        Math.round((float) params[2]), 0 };
    res[3] = strategy.index();
    return res;
  }

  protected static String toLogStr(int[] p, double accuracy, double error) {
    StringBuffer sb = new StringBuffer();
    sb.append(NumerosityReductionStrategy.fromValue(p[3]).toString()).append(", ");
    sb.append("window ").append(p[0]).append(COMMA);
    sb.append("PAA ").append(p[1]).append(COMMA);
    sb.append("alphabet ").append(p[2]).append(COMMA);
    sb.append(" accuracy ").append(fmt.format(accuracy)).append(COMMA);
    sb.append(" error ").append(fmt.format(error));
    return sb.toString();
  }

}

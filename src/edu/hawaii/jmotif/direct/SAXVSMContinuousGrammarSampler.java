package edu.hawaii.jmotif.direct;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
public class SAXVSMContinuousGrammarSampler {

  // the number formatter
  private static final DecimalFormatSymbols otherSymbols = new DecimalFormatSymbols(Locale.US);
  private static DecimalFormat fmt = new DecimalFormat("0.00###", otherSymbols);

  // array with all rectangle centerpoints
  private static ArrayList<Double[]> centerPoints;

  // array with all rectangle side lengths in each dimension
  private static ArrayList<Double[]> lengthsSide;

  // array with distances from center points to the vertices
  private static ArrayList<Double> diagonalLength;

  // array vector of all different distances, sorted
  private static ArrayList<Double> differentDiagonalLength;

  // array vector of minimum function value for each distance
  private static double[] diagonalsMinFunc;

  // array with function values
  private static ArrayList<Double> functionValues;

  // array used to track sampled points and function values
  private static ArrayList<ValuePointColored> coordinates;

  private static final double DEFAULT_NORMALIZATION_THRESHOLD = 0.05;

  private static final double precision = 1E-16;
  private static int b = 0;
  private static double[] resultMinimum;

  private static int sampledPoints;
  private static int rectangleCounter;
  private static int indexPotentialBestRec;
  private static double minFunctionValue;

  // init bounds
  //
  private static int dimensions = 3;

  private static SAXVSMGrammarCVErrorFunction function;

  // static block - we instantiate the logger
  //
  private static final Logger consoleLogger;
  private static final Level LOGGING_LEVEL = Level.INFO;

  private static final String COMMA = ", ";
  private static final String CR = "\n";
  static {
    consoleLogger = (Logger) LoggerFactory.getLogger(SAXVSMContinuousGrammarSampler.class);
    consoleLogger.setLevel(LOGGING_LEVEL);
  }

  // the global minimum point
  private static ValuePointColored minimum = ValuePointColored.at(Point.at(0),
      Double.POSITIVE_INFINITY, false);

  private static int[] upperBounds;
  private static int[] lowerBounds;
  private static String TRAINING_DATA;
  private static String TEST_DATA;
  private static int HOLD_OUT_NUM = 1;
  private static int ITERATIONS_NUM = 1;
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
      if (10 == args.length || 11 == args.length || 12 == args.length || 13 == args.length) {

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
        ITERATIONS_NUM = Integer.valueOf(args[9]);

        if (args.length > 10) {
          NORMALIZATION_THRESHOLD = Double.valueOf(args[10]);
        }

        if (args.length > 11) {
          if (!"*".equalsIgnoreCase(args[11])) {
            STRATEGY = NumerosityReductionStrategy.valueOf(args[11].toUpperCase());
          }
        }

        if (args.length > 12) {
          BAG_STRATEGY = BagConstructionStrategy.valueOf(args[12].toUpperCase());
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
    sb.append("Expects 10 parameters:").append(CR);
    sb.append(" [1] training dataset filename").append(CR);
    sb.append(" [2] test dataset filename").append(CR);
    sb.append(" [3] minimal sliding window size").append(CR);
    sb.append(" [4] maximal sliding window size").append(CR);
    sb.append(" [5] minimal PAA size").append(CR);
    sb.append(" [6] maximal PAA size").append(CR);
    sb.append(" [7] minimal Alphabet size").append(CR);
    sb.append(" [8] maximal Alphabet size").append(CR);
    sb.append(" [8] cross-validation hold-out number").append(CR);
    sb.append(" [8] maximal amount of sampling iterations").append(CR);
    sb.append(" [9] OPTIONAL: normalization threshold").append(CR);
    sb.append(" [10] OPTIONAL: specific NR restriction").append(CR);
    sb.append(" [11] OPTIONAL: bag construction strategy").append(CR);
    sb.append("An execution example: $java -cp \"sax-vsm-classic20.jar\" edu.hawaii.jmotif.direct.SAXVSMDirectSampler");
    sb.append(" data/cbf/CBF_TRAIN data/cbf/CBF_TEST 10 120 5 60 2 18 1 10 0.01 EXACT ALL").append(
        CR);
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
    // the whole bunch of inits
    //
    centerPoints = new ArrayList<Double[]>();
    lengthsSide = new ArrayList<Double[]>();
    diagonalLength = new ArrayList<Double>();
    differentDiagonalLength = new ArrayList<Double>();
    diagonalsMinFunc = new double[1];
    functionValues = new ArrayList<Double>();

    coordinates = new ArrayList<ValuePointColored>();

    sampledPoints = 0;
    rectangleCounter = 1;
    indexPotentialBestRec = 0;
    minFunctionValue = 0;

    // init the unit hypercube for sampling
    //
    Double[] scaledCenter = new Double[dimensions];
    double[] realCenter = new double[dimensions];
    Double[] lTmp = new Double[dimensions];
    Double dTmp = 0.0;
    Double[] cooTmp = new Double[dimensions];

    // transform the domain into the unit hyper-cube
    //
    for (int i = 0; i < dimensions; i++) {
      scaledCenter[i] = 0.5;
      lTmp[i] = 0.5;
      dTmp = dTmp + scaledCenter[i] * scaledCenter[i];
      realCenter[i] = lowerBounds[i] + scaledCenter[i] * (upperBounds[i] - lowerBounds[i]);
    }
    centerPoints.add(scaledCenter);
    lengthsSide.add(lTmp);
    dTmp = Math.sqrt(dTmp);
    diagonalLength.add(dTmp);
    Point startingPoint = Point.at(realCenter);

    // sampling center point
    //
    minFunctionValue = function.valueAt(startingPoint);
    sampledPoints = sampledPoints + 1;
    for (int i1 = 0; i1 < dimensions; i1++) {
      cooTmp[i1] = realCenter[i1];
    }
    minimum = ValuePointColored.at(startingPoint, minFunctionValue, true);
    coordinates.add(minimum);
    diagonalsMinFunc[0] = minFunctionValue;
    functionValues.add(minFunctionValue);
    differentDiagonalLength = diagonalLength;

    ArrayList<Integer> potentiallyOptimalRectangles = null;

    // optimization loop
    //
    for (int ctr = 0; ctr < ITERATIONS_NUM; ctr++) {
      resultMinimum = minimum(functionValues);
      double[] params = coordinates.get((int) resultMinimum[1]).getPoint().toArray();
      consoleLogger.info("iteration: " + ctr + ", minimal value " + resultMinimum[0] + " at "
          + params[0] + ", " + params[1] + ", " + params[2]);
      // System.out.println(resultMinimum[0] + ","+params[0] + "," + params[1] + ", " + params[2]);
      potentiallyOptimalRectangles = identifyPotentiallyRec();
      // For each potentially optimal rectangle
      for (int jj = 0; jj < potentiallyOptimalRectangles.size(); jj++) {
        int j = potentiallyOptimalRectangles.get(jj);
        samplingPotentialRec(j);
      }
      update();
    }

    // make result
    resultMinimum = minimum(functionValues);
    // generally, we want a shorter window
    //
    StringBuffer sb = new StringBuffer();
    HashSet<String> minimalValueParameters = new HashSet<String>();
    double minimalValue = resultMinimum[0];

    sb.append("min CV error ").append(fmt.format(minimalValue)).append(" reached at ");

    int[] params = null;

    for (int i = 0; i < functionValues.size(); i++) {

      if (minimalValue == functionValues.get(i)) {

        int[] new_params = coordinates.get(i).getPoint().toIntArray();

        if (null == params) {
          int[] asArray = Arrays.copyOf(new_params, new_params.length);
          params = asArray;
          minimalValueParameters.add(Arrays.toString(asArray));
          sb.append(Arrays.toString(asArray)).append(COMMA);
        }
        else if (new_params[0] < params[0]) {
          params = Arrays.copyOf(new_params, new_params.length);
          sb.append(Arrays.toString(params)).append(COMMA);
        }
      }
    }

    consoleLogger.info(sb.toString());

    int[] res = Arrays.copyOf(params, 4);
    res[3] = strategy.index();
    return res;
  }

  private static void update() {
    resultMinimum = minimum(functionValues);
    // getting minimum and giving it at last points
    minFunctionValue = resultMinimum[0];
    minimum.setBest(false);
    minimum = ValuePointColored.at(Point.at(0), Double.POSITIVE_INFINITY, false);
    int i = 0;
    for (ValuePointColored valuePoint : coordinates) {
      if (valuePoint.getValue() < minimum.getValue()) {
        b = i;
        minimum = valuePoint;
      }
      i++;
    }
    minimum.setBest(true);
    // TODO: a bug?
    // coordinates.remove(b);
    // coordinates.add(minimum);
    coordinates.set(b, minimum);
    double epsilon = 1E-4;
    double e = Math.max(epsilon * Math.abs(minFunctionValue), 1E-8);
    double[] temporaryArray = new double[functionValues.size()];
    for (int i2 = 0; i2 < functionValues.size(); i2++) {
      temporaryArray[i2] = (functionValues.get(i2) - minFunctionValue + e) / diagonalLength.get(i2);
    }
    indexPotentialBestRec = (int) minimum(temporaryArray)[1];

    differentDiagonalLength = diagonalLength;
    int i1 = 0;
    while (true) {
      double diagonalTmp = differentDiagonalLength.get(i1);
      Integer[] indx = findNonMatches(differentDiagonalLength, diagonalTmp);
      ArrayList<Double> diagonalCopy = differentDiagonalLength;
      differentDiagonalLength = new ArrayList<Double>();
      differentDiagonalLength.add(diagonalTmp);

      for (int i2 = 1; i2 < indx.length + 1; i2++) {
        differentDiagonalLength.add(diagonalCopy.get(indx[i2 - 1]));
      }
      if (i1 + 1 == differentDiagonalLength.size()) {
        break;
      }
      else {
        i1++;
      }
    }
    Collections.sort(differentDiagonalLength);
    diagonalsMinFunc = new double[differentDiagonalLength.size()];
    for (i1 = 0; i1 < differentDiagonalLength.size(); i1++) {
      Integer[] indx1 = findMatches(diagonalLength, differentDiagonalLength.get(i1));
      ArrayList<Double> fTmp = new ArrayList<Double>();
      for (int i2 = 0; i2 < indx1.length; i2++) {
        fTmp.add(functionValues.get(indx1[i2]));
      }
      diagonalsMinFunc[i1] = minimum(fTmp)[0];
    }
  }

  /**
   * Determine where to sample within rectangle j and how to divide the rectangle into
   * subrectangles. Update minFunctionValue and set m=m+delta_m, where delta_m is the number of new
   * points sampled.
   * 
   * @param j
   */
  private static void samplingPotentialRec(int j) {

    double max_L = lengthsSide.get(j)[0], delta;
    Integer[] maxSideLengths;

    // get the longest side
    //
    for (int i1 = 0; i1 < lengthsSide.get(j).length; i1++) {
      max_L = Math.max(max_L, lengthsSide.get(j)[i1]);
    }

    // Identify the array maxSideLengths of dimensions with the maximum side length.
    //
    maxSideLengths = findMatches(lengthsSide.get(j), max_L);
    delta = 2 * max_L / 3;
    double[] w = new double[0];
    double i1;
    double[] e_i;

    // Sample the function at the points c +- delta*e_i for all ii in maxSideLengths.
    for (int ii = 0; ii < maxSideLengths.length; ii++) {
      Double[] c_m1 = new Double[dimensions];
      double[] x_m1 = new double[dimensions];
      Double[] c_m2 = new Double[dimensions];
      double[] x_m2 = new double[dimensions];
      i1 = maxSideLengths[ii];
      e_i = new double[dimensions];
      e_i[(int) i1] = 1;

      // Center point for a new rectangle
      //
      for (int i2 = 0; i2 < centerPoints.get(j).length; i2++) {
        c_m1[i2] = centerPoints.get(j)[i2] + delta * e_i[i2];
      }
      // Transform c_m1 to original search space
      for (int i2 = 0; i2 < c_m1.length; i2++) {
        x_m1[i2] = lowerBounds[i2] + c_m1[i2] * (upperBounds[i2] - lowerBounds[i2]);
      }
      // Function value at x_m1
      Point pointToSample1 = Point.at(x_m1);
      // TODO: here needs to be a check
      Double f_m1 = function.valueAt(pointToSample1);
      consoleLogger.info("@" + f_m1 + "\t" + pointToSample1.toLogString());

      // add to all points
      coordinates.add(ValuePointColored.at(pointToSample1, f_m1, false));
      sampledPoints = sampledPoints + 1;

      // Center point for a new rectangle
      //
      for (int i2 = 0; i2 < centerPoints.get(j).length; i2++) {
        c_m2[i2] = centerPoints.get(j)[i2] - delta * e_i[i2];
      }
      // Transform c_m2 to original search space
      for (int i2 = 0; i2 < c_m2.length; i2++) {
        x_m2[i2] = lowerBounds[i2] + c_m2[i2] * (upperBounds[i2] - lowerBounds[i2]);
      }
      // Function value at x_m2
      Point pointToSample2 = Point.at(x_m2);
      // TODO: here needs to be a check
      Double f_m2 = function.valueAt(pointToSample2);
      consoleLogger.info("@" + f_m2 + "\t" + pointToSample2.toLogString());

      // add to all points
      coordinates.add(ValuePointColored.at(pointToSample2, f_m2, false));
      sampledPoints = sampledPoints + 1;

      double[] w_pom;
      w_pom = w;
      w = new double[ii + 1];
      System.arraycopy(w_pom, 0, w, 0, w_pom.length);
      w[ii] = Math.min(f_m2, f_m1);

      centerPoints.add(c_m1);
      centerPoints.add(c_m2);
      functionValues.add(f_m1);
      functionValues.add(f_m2);

      // System.out.println(Arrays.toString(x_m1) + ", " + f_m1);
      // System.out.println(Arrays.toString(x_m2) + ", " + f_m2);
    }

    devideRec(w, maxSideLengths, delta, j);

  }

  /**
   * Divide the rectangle containing centerPoints.get(j) into thirds along the dimension in
   * maxSideLengths, starting with the dimension with the lowest value of w[ii]
   * 
   * @param w
   * @param maxSideLengths
   * @param delta
   * @param j
   */

  private static void devideRec(double[] w, Integer[] maxSideLengths, double delta, int j) {

    double[][] ab = sort(w);

    for (int ii = 0; ii < maxSideLengths.length; ii++) {
      int i1 = maxSideLengths[(int) ab[1][ii]];
      int index1 = rectangleCounter + 2 * (int) ab[1][ii]; // Index for new rectangle
      int index2 = rectangleCounter + 2 * (int) ab[1][ii] + 1; // Index for new rectangle
      lengthsSide.get(j)[i1] = delta / 2;
      int index = 0;
      if (index2 + 1 > index1 + 1) {
        index = index2 + 1;
      }
      else {
        index = index1 + 1;
      }

      Double[] lTmp = new Double[dimensions];
      Double[] lTmp2 = new Double[dimensions];
      for (int i2 = 0; i2 < lengthsSide.get(0).length; i2++) {
        lTmp[i2] = lengthsSide.get(j)[i2];
        lTmp2[i2] = lengthsSide.get(j)[i2];
      }
      if (index == lengthsSide.size() + 2) {
        lengthsSide.add(lTmp);
        lengthsSide.add(lTmp2);
      }
      else {
        Double[] lTmp3;
        int lengthsSize = lengthsSide.size();
        for (int i2 = 0; i2 < index - lengthsSize; i2++) {
          lTmp3 = new Double[dimensions];
          lengthsSide.add(lTmp3);
        }
        lengthsSide.set(index1, lTmp);
        lengthsSide.set(index2, lTmp2);
      }

      diagonalLength.set(j, 0.0);
      Double dTmp;
      for (int i2 = 0; i2 < lengthsSide.get(j).length; i2++) {
        dTmp = diagonalLength.get(j) + lengthsSide.get(j)[i2] * lengthsSide.get(j)[i2];
        diagonalLength.set(j, dTmp);
      }
      diagonalLength.set(j, Math.sqrt(diagonalLength.get(j)));
      dTmp = diagonalLength.get(j);
      Double d_kop2 = diagonalLength.get(j);
      if (index == diagonalLength.size() + 2) {
        diagonalLength.add(dTmp);
        diagonalLength.add(d_kop2);
      }
      else {
        Double dTmp3;
        int size = diagonalLength.size();
        for (int i2 = 0; i2 < index - size; i2++) {
          dTmp3 = 0.0;
          diagonalLength.add(dTmp3);
        }
        diagonalLength.set(index1, diagonalLength.get(j));
        diagonalLength.set(index2, diagonalLength.get(j));
      }
    }
    rectangleCounter = rectangleCounter + 2 * maxSideLengths.length;
  }

  /**
   * Identify the set of all potentially optimal rectangles.
   */
  private static ArrayList<Integer> identifyPotentiallyRec() {

    double localPrecision = 1E-12;

    // find rectangles with the same diagonal
    //
    Integer[] sameDiagonalIdxs = findMatches(differentDiagonalLength,
        diagonalLength.get(indexPotentialBestRec));

    ArrayList<Integer> s_1 = new ArrayList<Integer>();
    for (int i = sameDiagonalIdxs[0]; i < differentDiagonalLength.size(); i++) {
      Integer[] indx3 = findMatches(functionValues, diagonalsMinFunc[i]);
      Integer[] indx4 = findMatches(diagonalLength, differentDiagonalLength.get(i));
      Integer[] idx2 = findArrayIntersection(indx3, indx4);
      s_1.addAll(Arrays.asList(idx2));
    }

    // s_1 now includes all rectangles i, with diagonals[i] >= diagonals(indexPotentialBestRec)
    //
    ArrayList<Integer> s_2 = new ArrayList<Integer>();
    ArrayList<Integer> s_3 = new ArrayList<Integer>();
    if (differentDiagonalLength.size() - sameDiagonalIdxs[0] > 2) {

      double a1 = diagonalLength.get(indexPotentialBestRec), a2 = differentDiagonalLength
          .get(differentDiagonalLength.size() - 1), b1 = functionValues.get(indexPotentialBestRec), b2 = diagonalsMinFunc[differentDiagonalLength
          .size() - 1];

      // The line is defined by: y = slope*x + const
      double slope = (b2 - b1) / (a2 - a1);
      double consta = b1 - slope * a1;

      for (int i1 = 0; i1 < s_1.size(); i1++) {
        int j = s_1.get(i1).intValue();
        if (functionValues.get(j) <= slope * diagonalLength.get(j) + consta + localPrecision) {
          s_2.add(j);
        }
      }

      if (0 == s_2.size()) {
        return s_1;
      }

      // s_2 now contains all points in S_1 which lie on or below the line
      // Find the points on the convex hull defined by the points in s_2
      double[] xx = new double[s_2.size()];
      double[] yy = new double[s_2.size()];
      for (int i1 = 0; i1 < xx.length; i1++) {
        xx[i1] = diagonalLength.get(s_2.get(i1).intValue());
        yy[i1] = functionValues.get(s_2.get(i1).intValue());
      }
      double[] h = conhull(xx, yy);
      for (int i1 = 0; i1 < h.length; i1++) {
        s_3.add(s_2.get((int) h[i1]));
      }
    }
    else {
      s_3 = s_1;
    }
    return s_3;
  }

  /**
   * Finds all points on the convex hull, even redundant ones.
   */
  private static double[] conhull(double[] x, double[] y) {
    // System.out.println(Arrays.toString(x) + " : " + Arrays.toString(y));
    int m = x.length;
    double[] h;
    int start = 0, flag = 0, v, w, a, b, c, leftturn, j, k;
    double determinant;
    if (x.length != y.length) {
      System.out.println("Input dimension must agree");
      return null;
    }
    if (m == 2) {
      h = new double[2];
      h[0] = 0;
      h[1] = 1;
      return h;
    }
    if (m == 1) {
      h = new double[1];
      h[0] = 0;
      return h;
    }
    v = start;
    w = x.length - 1;
    h = new double[x.length];
    for (int i = 0; i < x.length; i++) {
      h[i] = i + 1;
    }
    while ((next(v, m) != 0) || (flag == 0)) {
      if (next(v, m) == w) {
        flag = 1;
      }
      // getting three points
      a = v;
      b = next(v, m);
      c = next(next(v, m), m);
      determinant = (x[a] * y[b] * 1) + (x[b] * y[c] * 1) + (x[c] * y[a] * 1) - (1 * y[b] * x[c])
          - (1 * y[c] * x[a]) - (1 * y[a] * x[b]);

      if (determinant >= 0) {
        leftturn = 1;
      }
      else {
        leftturn = 0;
      }
      if (leftturn == 1) {
        v = next(v, m);
      }
      else {
        j = next(v, m);
        k = 0;
        double[] x1 = new double[x.length - 1];
        for (int i = 0; i < x1.length; i++) {
          if (j == i) {

            k++;
          }
          x1[i] = x[k];
          k++;
        }
        x = x1;
        k = 0;
        x1 = new double[y.length - 1];
        for (int i = 0; i < x1.length; i++) {
          if (j == i) {

            k++;
          }
          x1[i] = y[k];
          k++;
        }
        y = x1;
        k = 0;
        x1 = new double[h.length - 1];
        for (int i = 0; i < x1.length; i++) {
          if (j == i) {

            k++;
          }
          x1[i] = h[k];
          k++;
        }
        h = x1;
        m = m - 1;
        w = w - 1;
        v = pred(v, m);
      }
    }
    for (int i = 0; i < h.length; i++) {
      h[i] = h[i] - 1;
    }
    return h;
  }

  /**
   * returns next point if the last then the first
   * 
   * @param v
   * @param m
   * @return
   */
  private static int next(int v, int m) {
    if ((v + 1) == m) {
      return 0;
    }
    else {
      if ((v + 1) < m) {
        return (v + 1);
      }
      else {
        return -1;
      }
    }
  }

  /**
   * M is the size, v is the index, returns the previous index value.
   */
  private static int pred(int idx, int size) {
    if ((idx + 1) == 1) {
      return size - 1;
    }
    else {
      if ((idx + 1) > 1) {
        return (idx - 1);
      }
      else {
        return -1;
      }
    }
  }

  /**
   * returns sorted array and the original indicies
   * 
   * @param array
   * @return
   */
  private static double[][] sort(double[] array) {
    double[][] arr1 = new double[3][array.length];
    double[][] arr2 = new double[2][array.length];
    System.arraycopy(array, 0, arr1[0], 0, array.length);
    Arrays.sort(array);
    for (int i = 0; i < array.length; i++) {
      for (int i1 = 0; i1 < array.length; i1++) {
        if (array[i] == arr1[0][i1] && arr1[2][i1] != 1) {
          arr1[2][i1] = 1;
          arr1[1][i] = i1;
          break;
        }
      }
    }
    arr2[0] = array;
    arr2[1] = arr1[1];
    return arr2;
  }

  /**
   * Finds an index and a minimal value of an array.
   */
  private static double[] minimum(double[] array) {
    Double min = array[0];
    double[] res = { min, 0.0 };
    for (int i = 0; i < array.length; i++) {
      if (min > array[i]) {
        min = array[i];
        res[0] = min;
        res[1] = i;
      }
    }
    return res;
  }

  /**
   * Finds an index and a minimal value of an array.
   */
  private static double[] minimum(ArrayList<Double> array) {
    Double min = array.get(0);
    double[] res = { min, 0.0 };
    for (int i = 0; i < array.size(); i++) {
      if (min > array.get(i)) {
        min = array.get(i);
        res[0] = min;
        res[1] = i;
      }
    }
    return res;
  }

  /**
   * Finds matches.
   */
  private static Integer[] findMatches(Double[] array, double value) {
    ArrayList<Integer> res = new ArrayList<Integer>();
    for (int i = 0; i < array.length; i++) {
      if (Math.abs(array[i] - value) <= precision) {
        res.add(i);
      }
    }
    return res.toArray(new Integer[res.size()]);
  }

  /**
   * Finds matches.
   */
  private static Integer[] findMatches(ArrayList<Double> array, double value) {
    ArrayList<Integer> res = new ArrayList<Integer>();
    for (int i = 0; i < array.size(); i++) {
      if (Math.abs(array.get(i) - value) <= precision) {
        res.add(i);
      }
    }
    return res.toArray(new Integer[res.size()]);
  }

  /**
   * Finds array elements that are not equal to the value up to threshold.
   */
  private static Integer[] findNonMatches(ArrayList<Double> array, double value) {
    ArrayList<Integer> res = new ArrayList<Integer>();
    for (int i = 0; i < array.size(); i++) {
      if (Math.abs(array.get(i) - value) > precision) {
        res.add(i);
      }
    }
    return res.toArray(new Integer[res.size()]);
  }

  /**
   * Returns arrays intersection.
   */
  private static Integer[] findArrayIntersection(Integer[] arr1, Integer[] arr2) {
    ArrayList<Integer> res = new ArrayList<Integer>();
    for (int i1 = 0; i1 < arr1.length; i1++) {
      for (int i2 = 0; i2 < arr2.length; i2++) {
        if (arr1[i1] == arr2[i2]) {
          res.add(arr2[i2]);
        }
      }
    }
    return res.toArray(new Integer[res.size()]);
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

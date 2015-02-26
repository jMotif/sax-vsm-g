package edu.hawaii.jmotif.text;

import static org.junit.Assert.assertEquals;
import java.util.HashMap;
import java.util.Map.Entry;
import org.junit.Test;

/**
 * Test the cosine similarity implementation.
 * 
 * @author psenin
 * 
 */
public class TestCosineSimilarity {

  private static final double TEST_VALUE = 0.8215838362577491D;

  private static final double TEST_PASS_PRECISION = 0.0001;

  private static final double TEST_FAIL_PRECISION = 0.000001;

  /**
   * Using a dumb example.
   */
  @Test
  public void testCosineSimilarity() {

    WordBag wb1 = new WordBag("first");
    WordBag wb2 = new WordBag("second");

    wb1.addWord("me", 2);
    wb1.addWord("Julie", 1);
    wb1.addWord("likes", 0);
    wb1.addWord("loves", 2);
    wb1.addWord("Jane", 0);
    wb1.addWord("Linda", 1);
    wb1.addWord("than", 1);
    wb1.addWord("more", 1);

    wb2.addWord("me", 2);
    wb2.addWord("Julie", 1);
    wb2.addWord("likes", 1);
    wb2.addWord("loves", 1);
    wb2.addWord("Jane", 1);
    wb2.addWord("Linda", 0);
    wb2.addWord("than", 1);
    wb2.addWord("more", 1);

    TextUtils textUtils = new TextUtils();

    double cosine = textUtils.cosineSimilarity(wb1.getWordsAsDoubles(), wb2.getWordsAsDoubles());

    assertEquals("Testing cosine similarity", TEST_VALUE, cosine, TEST_PASS_PRECISION);
  }

  /**
   * Making sure normalization works as intended - i.e doesn't change anything.
   */
  @Test
  public void testCosineSimilarityNorm() {

    WordBag wb1 = new WordBag("first");
    WordBag wb2 = new WordBag("second");

    wb1.addWord("me", 2);
    wb1.addWord("Julie", 1);
    wb1.addWord("likes", 0);
    wb1.addWord("loves", 2);
    wb1.addWord("Jane", 0);
    wb1.addWord("Linda", 1);
    wb1.addWord("than", 1);
    wb1.addWord("more", 1);

    wb2.addWord("me", 2);
    wb2.addWord("Julie", 1);
    wb2.addWord("likes", 1);
    wb2.addWord("loves", 1);
    wb2.addWord("Jane", 1);
    wb2.addWord("Linda", 0);
    wb2.addWord("than", 1);
    wb2.addWord("more", 1);

    TextUtils textUtils = new TextUtils();

    double cosine = textUtils.cosineSimilarity(wb1.getWordsAsDoubles(), wb2.getWordsAsDoubles());
    assertEquals("Testing cosine similarity", TEST_VALUE, cosine, TEST_PASS_PRECISION);

    // grow the vector
    HashMap<String, Double> wbLong = wb1.getWordsAsDoubles();
    double multiplier = 8.24864813846848348486;
    for (Entry<String, Double> e : wbLong.entrySet()) {
      wbLong.put(e.getKey(), e.getValue() * multiplier);
    }
    double distLong = textUtils.cosineSimilarity(wbLong, wb2.getWordsAsDoubles());

    assertEquals("Testing cosine similarity", TEST_VALUE, distLong, TEST_PASS_PRECISION);

    // normalize vectors
    HashMap<String, HashMap<String, Double>> vectors = new HashMap<String, HashMap<String, Double>>();
    vectors.put("first", wbLong);
    vectors.put("second", wb2.getWordsAsDoubles());
    vectors = textUtils.normalizeToUnitVectors(vectors);

    double distNorm = textUtils.cosineSimilarity(vectors.get("first"), vectors.get("second"));

    assertEquals("Testing cosine similarity", TEST_VALUE, distNorm, TEST_PASS_PRECISION);
  }
}

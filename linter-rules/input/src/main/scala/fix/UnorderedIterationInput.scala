/*
rule = UnorderedIteration
 */
package fix

import scala.collection.mutable
import scala.collection.immutable

object UnorderedIterationInput {

  // --- Direct type: mutable.HashMap ---

  val hashMap: mutable.HashMap[String, Int] = mutable.HashMap.empty

  def directForeach(): Unit = {
    hashMap.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  def directMap(): List[(String, Int)] = {
    hashMap.map/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => (key, value) }.toList
  }

  def directToList(): List[(String, Int)] = {
    hashMap.toList/* assert: UnorderedIteration.UnorderedIterationRule*/
  }

  def directHead(): (String, Int) = {
    hashMap.head/* assert: UnorderedIteration.UnorderedIterationRule*/
  }

  def directFind(): Option[(String, Int)] = {
    hashMap.find/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (_, value) => value > 0 }
  }

  def directSum(): Int = {
    hashMap.values.sum/* assert: UnorderedIteration.UnorderedIterationRule*/
  }

  def directMinBy(): (String, Int) = {
    hashMap.minBy/* assert: UnorderedIteration.UnorderedIterationRule*/(_._2)
  }

  def directDropWhile(): mutable.HashMap[String, Int] = {
    hashMap.dropWhile/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (_, value) => value > 0 }
  }

  // --- Direct type: mutable.HashSet ---

  val hashSet: mutable.HashSet[String] = mutable.HashSet.empty

  def setForeach(): Unit = {
    hashSet.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/(println)
  }

  def setToSeq(): Seq[String] = {
    hashSet.toSeq/* assert: UnorderedIteration.UnorderedIterationRule*/
  }

  def setTakeWhile(): mutable.HashSet[String] = {
    hashSet.takeWhile/* assert: UnorderedIteration.UnorderedIterationRule*/(_.nonEmpty)
  }

  // --- Type-inferred local (requires SemanticDB) ---

  def typeInferred(): Unit = {
    val inferred = mutable.HashMap.empty[String, Int]
    inferred.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  // --- Abstract-typed val (RHS tracking via companion matcher) ---

  val abstractVal: mutable.Map[String, Int] = mutable.HashMap.empty

  def abstractValForeach(): Unit = {
    abstractVal.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  // --- Abstract-typed var (NOT flagged — excluded to avoid false positives from reassignment) ---

  var abstractVar: mutable.Map[String, Int] = mutable.HashMap.empty

  def abstractVarForeach(): Unit = {
    abstractVar.foreach { case (key, value) => println(s"$key=$value") }
  }

  // --- Cross-scope name collision regression: a List with the same name must NOT be flagged ---

  object ScopeA {
    val coll: mutable.Map[String, Int] = mutable.HashMap.empty
    def use(): Unit = coll.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  object ScopeB {
    val coll: List[Int] = List(1, 2, 3)
    def use(): Unit = coll.foreach(println) // must NOT be flagged
  }

  // --- One-hop projections ---

  def valuesProjection(): Unit = {
    hashMap.values.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/(println)
  }

  def keySetProjection(): List[String] = {
    hashMap.keySet.toList/* assert: UnorderedIteration.UnorderedIterationRule*/
  }

  def keysProjection(): Iterable[String] = {
    hashMap.keys.map/* assert: UnorderedIteration.UnorderedIterationRule*/(_.toUpperCase)
  }

  // --- Java types ---

  val javaHashMap: java.util.HashMap[String, Int] = new java.util.HashMap[String, Int]()

  def javaForeach(): Unit = {
    import scala.jdk.CollectionConverters._
    javaHashMap.asScala.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  // --- Java raw constructor (no type arguments at the new-site) ---

  val rawJavaHashMap: java.util.HashMap[String, Int] = new java.util.HashMap()

  def rawJavaForeach(): Unit = {
    import scala.jdk.CollectionConverters._
    rawJavaHashMap.asScala.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  // --- For-comprehension generator ---

  def forComprehension(): Unit = {
    for (entry <- hashMap/* assert: UnorderedIteration.UnorderedIterationRule*/) {
      println(entry)
    }
  }

  def forComprehensionProjection(): Unit = {
    for (key <- hashMap.keys/* assert: UnorderedIteration.UnorderedIterationRule*/) {
      println(key)
    }
  }

  // --- Infix syntax ---

  def infixForeach(): Unit = {
    hashSet foreach/* assert: UnorderedIteration.UnorderedIterationRule*/println
  }

  def infixMap(): Iterable[String] = {
    hashSet map/* assert: UnorderedIteration.UnorderedIterationRule*/identity
  }

  // --- Chained projection local (val it = map.iterator; it.foreach) ---

  def chainedIterator(): Unit = {
    val it = hashMap.iterator
    it.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  // --- Fixpoint taint tracking: aliases and order-preserving derivations ---

  def aliasChain(): Unit = {
    val it = hashMap.iterator
    val it2 = it
    it2.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  def takeChain(): Unit = {
    val it = hashMap.iterator
    val sub = it.take/* assert: UnorderedIteration.UnorderedIterationRule*/(2)
    sub.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  def multiHopChain(): Unit = {
    val it = hashMap.iterator
    val dropped = it.drop/* assert: UnorderedIteration.UnorderedIterationRule*/(1)
    val taken = dropped.take/* assert: UnorderedIteration.UnorderedIterationRule*/(2)
    taken.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  def infixDerivation(): Unit = {
    val it = hashMap.iterator
    val sub = it take/* assert: UnorderedIteration.UnorderedIterationRule*/2
    sub.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  // distinct is not flagged at the call site (result contents do not depend on encounter
  // order), but propagates order-taint into derived vals
  def distinctPropagates(): Unit = {
    val it = hashSet.iterator
    val sub = it.distinct
    sub.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/(println)
  }

  // --- Materialization does not propagate: flagged at toList, not downstream ---

  def materializedNotTracked(): Unit = {
    val xs = hashSet.toList/* assert: UnorderedIteration.UnorderedIterationRule*/
    xs.foreach(println) // must NOT be flagged
  }

  // --- Ordered derivations must NOT be flagged ---

  def orderedChain(): Unit = {
    val ok = List(1, 2, 3).iterator
    val ok2 = ok.take(1)
    ok2.foreach(println)
  }

  def orderedAlias(): Unit = {
    val ordered = List(1, 2, 3)
    val aliasOfOrdered = ordered
    aliasOfOrdered.foreach(println)
  }

  // --- Method receiver (def makeMap: mutable.HashMap[...] = ...; makeMap.foreach) ---

  def makeMap: mutable.HashMap[String, Int] = mutable.HashMap.empty

  def methodReceiver(): Unit = {
    makeMap.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

  // --- Safe operations (must NOT be flagged) ---

  def safeContains(): Boolean = {
    hashMap.contains("key")
  }

  def safeSize(): Int = {
    hashMap.size
  }

  def safeExists(): Boolean = {
    hashMap.exists { case (_, value) => value > 0 }
  }

  def safeGet(): Option[Int] = {
    hashMap.get("key")
  }

  // --- Escape hatch: scalafix:ok suppression ---

  def suppressedIteration(): Unit = {
    hashMap.foreach { case (key, value) => println(s"$key=$value") } // scalafix:ok UnorderedIteration
  }

  // --- Immutable HashMap ---

  val immutableHashMap: immutable.HashMap[String, Int] = immutable.HashMap.empty

  def immutableForeach(): Unit = {
    immutableHashMap.foreach/* assert: UnorderedIteration.UnorderedIterationRule*/{ case (key, value) => println(s"$key=$value") }
  }

}

package org.scalameta.interpreter.integration

import org.scalameta.interpreter._

import scala.meta._

class ScalaCourseraWeek3 extends ScalametaInterpreterSpec {
  val Tweet = Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Tweet"))

  implicit val mirror: ScalametaMirror = {
    case Term.Name("filter") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("filter", "(Lscala/Function1;)LTweetSet;"))
    case Term.Name("next") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmptyTrending")), Signature.TermParameter("next"))
    case Term.Name("Unit") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Unit"))
    case Term.Name("tw") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmpty")), Signature.Method("remove", "(LTweet;)LTweetSet;")), Signature.TermParameter("tw"))
    case Term.Name("ascendingByRetweet") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("ascendingByRetweet", "()LTrending;"))
    case Term.Name("head") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Trending")), Signature.Method("head", "()LTweet;"))
    case Term.Name("toString") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Tweet")), Signature.Method("toString", "()Ljava/lang/String;"))
    case Term.Name("right") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmpty")), Signature.TermParameter("right"))
    case Term.Name("p") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmpty")), Signature.Method("filter0", "(Lscala/Function1;LTweetSet;)LTweetSet;")), Signature.TermParameter("p"))
    case Term.Name("tail") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("tail", "()LTweetSet;"))
    case Term.Name("contains") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmpty")), Signature.Method("contains", "(LTweet;)Z"))
    case Term.Name("Boolean") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Boolean"))
    case Term.Name("f") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("foreach", "(Lscala/Function1;)V")), Signature.TermParameter("f"))
    case Term.Name("that") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("union", "(LTweetSet;)LTweetSet;")), Signature.TermParameter("that"))
    case Term.Name("Trending") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Trending"))
    case Type.Name("Trending") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Trending"))
    case Term.Name("NonEmpty") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmpty"))
    case Type.Name("NonEmpty") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmpty"))
    case Term.Name("Tweet") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Tweet"))
    case Type.Name("Tweet") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Tweet"))
    case Term.Name("remove") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("remove", "(LTweet;)LTweetSet;"))
    case Term.Name("Exception") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Term("package")), Signature.Type("Exception"))
    case Term.Name("text") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Tweet")), Signature.TermParameter("text"))
    case Term.Name("elem") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmptyTrending")), Signature.TermParameter("elem"))
    case Term.Name("incl") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("incl", "(LTweet;)LTweetSet;"))
    case Term.Name("foreach") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("foreach", "(Lscala/Function1;)V"))
    case Term.Name("filter0") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmpty")), Signature.Method("filter0", "(Lscala/Function1;LTweetSet;)LTweetSet;"))
    case Term.Name("TweetSet") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet"))
    case Type.Name("TweetSet") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet"))
    case Term.Name("union") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("union", "(LTweetSet;)LTweetSet;"))
    case Term.Name("leftright") => Symbol.Local("library/src/main/scala/Test.scala@1682..1737")
    case Term.Name("accu") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmpty")), Signature.Method("filter0", "(Lscala/Function1;LTweetSet;)LTweetSet;")), Signature.TermParameter("accu"))
    case Term.Name("Empty") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Empty"))
    case Type.Name("Empty") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Empty"))
    case Term.Name("findMin") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("findMin", "()LTweet;"))
    case Term.Name("left") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmpty")), Signature.TermParameter("left"))
    case Term.Name("x") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmpty")), Signature.Method("incl", "(LTweet;)LTweetSet;")), Signature.TermParameter("x"))
    case Term.Name("isEmpty") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("isEmpty", "()Z"))
    case Term.Name("EmptyTrending") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("EmptyTrending"))
    case Type.Name("EmptyTrending") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("EmptyTrending"))
    case Term.Name("user") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Tweet")), Signature.TermParameter("user"))
    case Term.Name("findMin0") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("findMin0", "(LTweet;)LTweet;"))
    case Term.Name("min") => Symbol.Local("library/src/main/scala/Test.scala@543..560")
    case Term.Name("curr") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("TweetSet")), Signature.Method("findMin0", "(LTweet;)LTweet;")), Signature.TermParameter("curr"))
    case Term.Name("retweets") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Tweet")), Signature.TermParameter("retweets"))
    case Term.Name("NonEmptyTrending") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmptyTrending"))
    case Type.Name("NonEmptyTrending") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("NonEmptyTrending"))
    case Term.Name("retweets") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Type("Tweet")), Signature.TermParameter("retweets"))
    case Term.Name("size") => Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_empty_")), Signature.Method("size", "(LTweetSet;)I"))
    case Term.Name("set1") => Symbol.Local("set1")
    case Term.Name("set2") => Symbol.Local("set2")
    case Term.Name("set3") => Symbol.Local("set3")
    case Term.Name("c") => Symbol.Local("c")
    case Term.Name("d") => Symbol.Local("d")
    case Term.Name("set4c") => Symbol.Local("set4c")
    case Term.Name("set4d") => Symbol.Local("set4d")
    case Term.Name("set5") => Symbol.Local("set5")
  }

  ignore should "be able to declare FunSets" in {
    checkCode(q"""
         class Tweet(val user: String, val text: String, val retweets: Int) {
           override def toString: String =
             "User: " + user + "\n" +
             "Text: " + text + " [" + retweets + "]"
         }
 
         abstract class TweetSet {
           def filter(p: Tweet => Boolean): TweetSet = filter0(p, new Empty)
           def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet
         
           def union(that: TweetSet): TweetSet = {
             if (that.isEmpty) this
             else incl(that.head) union that.tail
           }
           
           def ascendingByRetweet: Trending = {
             if (isEmpty) new EmptyTrending
             else {
               val min = findMin
               new NonEmptyTrending(min, remove(min).ascendingByRetweet)
             }
           }
           
           def incl(x: Tweet): TweetSet
           def contains(x: Tweet): Boolean
           def isEmpty: Boolean
           def head: Tweet
           def tail: TweetSet
                 
           def foreach(f: Tweet => Unit): Unit = {
             if (!this.isEmpty) {
               f(this.head)
               this.tail.foreach(f)
             }
           }
         
           def remove(tw: Tweet): TweetSet
           
           def findMin0(curr: Tweet): Tweet =
             if (this.isEmpty) curr
             else if (this.head.retweets < curr.retweets) this.tail.findMin0(this.head)
             else this.tail.findMin0(curr)
         
           def findMin: Tweet =
             this.tail.findMin0(this.head)
         }
         
         class Empty extends TweetSet {
           def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = accu
                 
           def contains(x: Tweet): Boolean = false
           def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)
           def isEmpty = true
           def head = throw new Exception("Empty.head")
           def tail = throw new Exception("Empty.tail")
           def remove(tw: Tweet): TweetSet = this
         }
         
         class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
           def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = {
             val leftright = left.filter0(p, right.filter0(p, accu))
             if (p(elem)) leftright.incl(elem)
             else leftright
           }
         
           def contains(x: Tweet): Boolean =
             if (augmentString(x.text) < elem.text) left.contains(x)
             else if (elem.text < x.text) right.contains(x)
             else true
         
           def incl(x: Tweet): TweetSet = {
             if (augmentString(x.text) < elem.text) new NonEmpty(elem, left.incl(x), right)
             else if (augmentString(elem.text) < x.text) new NonEmpty(elem, left, right.incl(x))
             else this
           }
         
           def isEmpty = false
           def head = if (left.isEmpty) elem else left.head
           def tail = if (left.isEmpty) right else new NonEmpty(elem, left.tail, right)
       
           def remove(tw: Tweet): TweetSet =
             if (augmentString(tw.text) < elem.text) new NonEmpty(elem, left.remove(tw), right)
             else if (augmentString(elem.text) < tw.text) new NonEmpty(elem, left, right.remove(tw))
             else left.union(right)
         }
         
         /** This class provides a linear sequence of tweets.
          */
         abstract class Trending {
           def + (tw: Tweet): Trending
           def head: Tweet
           def tail: Trending
           def isEmpty: Boolean
           def foreach(f: Tweet => Unit): Unit = {
             if (!this.isEmpty) {
               f(this.head)
               this.tail.foreach(f)
             }
           }
         }
         
         class EmptyTrending extends Trending {
           def + (tw: Tweet) = new NonEmptyTrending(tw, new EmptyTrending)
           def head: Tweet = throw new Exception
           def tail: Trending = throw new Exception
           def isEmpty: Boolean = true
           override def toString = "EmptyTrending"
         }
          
         class NonEmptyTrending(elem: Tweet, next: Trending) extends Trending {
           /** Appends tw to the end of this sequence.
            */
           def + (tw: Tweet): Trending =
             new NonEmptyTrending(elem, next + tw)
           def head: Tweet = elem
           def tail: Trending = next
           def isEmpty: Boolean = false
           override def toString =
             "NonEmptyTrending(" + elem.retweets + ", " + next + ")"
         }

         def size(set: TweetSet): Int = {
           if (set.isEmpty) 0
           else 1 + size(set.tail)
         }
         
         val set1 = new Empty
         val set2 = set1.incl(new Tweet("a", "a body", 20))
         val set3 = set2.incl(new Tweet("b", "b body", 20))
         val c = new Tweet("c", "c body", 7)
         val d = new Tweet("d", "d body", 9)
         val set4c = set3.incl(c)
         val set4d = set3.incl(d)
         val set5 = set4c.incl(d)
         set1.filter(tw => tw.user == "a")
       """, (), Seq())
  }
}

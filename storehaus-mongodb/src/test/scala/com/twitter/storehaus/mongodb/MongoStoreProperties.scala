/*
 * Copyright 2014 Twitter Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.twitter.storehaus.mongodb

import com.twitter.storehaus.testing.generator.NonEmpty
import com.twitter.storehaus.Store
import com.twitter.util.Await

import org.scalacheck._
import org.scalacheck.Prop._

import com.mongodb.casbah.Imports._

/**
 *  @author Bin Lan
 */

object MongoStoreProperties extends Properties("MongoStore") {
  val mongoClient = MongoClient("127.0.0.1", 27017)

  def putAndGetStoreTest[K, V](store: Store[K, V], pairs: Gen[List[(K, Option[V])]]): Prop =
    forAll(pairs) {
      (examples: List[(K, Option[V])]) => {
        examples.forall {
          case (k, v) =>
            Await.result(store.put((k, v)))
            val found = Await.result(store.get(k))
            found == v
        }
      }
    }

  property("MongoStore[String, String]") =
    putAndGetStoreTest[String, String](MongoStore[String, String](
        mongoClient,
        "storehaus",
        "data.stringMap"
      ), NonEmpty.Pairing.alphaStrs())

  property("MongoStore[Long, Long]") =
    putAndGetStoreTest[Long, Long](MongoStore[Long, Long](
        mongoClient,
        "storehaus",
        "data.longMap"
      ), NonEmpty.Pairing.numerics[Long]())

  property("MongoStore[Int, Int]") =
    putAndGetStoreTest[Int, Int](MongoStore[Int, Int](
        mongoClient,
        "storehaus",
        "data.intMap"
      ), NonEmpty.Pairing.numerics[Int]())

  property("MongoStore[Double, Double]") =
    putAndGetStoreTest[Double, Double](MongoStore[Double, Double](
        mongoClient,
        "storehaus",
        "data.doubleMap"
      ), NonEmpty.Pairing.numerics[Double]())

  property("MongoStore[String, Int]") =
    putAndGetStoreTest[String, Int](MongoStore[String, Int](
        mongoClient,
        "storehaus",
        "data.stringIntMap"
      ), NonEmpty.Pairing.alphaStrNumerics[Int]())

  property("MongoStore[String, Long]") =
    putAndGetStoreTest[String, Long](MongoStore[String, Long](
        mongoClient,
        "storehaus",
        "data.stringLongMap"
      ), NonEmpty.Pairing.alphaStrNumerics[Long]())

  property("MongoStore[String, Double]") =
    putAndGetStoreTest[String, Double](MongoStore[String, Double](
        mongoClient,
        "storehaus",
        "data.stringDoubleMap"
      ), NonEmpty.Pairing.alphaStrNumerics[Double]())
}


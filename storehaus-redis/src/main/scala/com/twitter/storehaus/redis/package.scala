/*
 * Copyright 2013 Twitter Inc.
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
package com.twitter.storehaus
import com.twitter.bijection.Bijection
import org.jboss.netty.buffer.{ ChannelBuffer, ChannelBuffers }
import org.jboss.netty.util.CharsetUtil.UTF_8

package object redis {
  implicit def str2ChannelBuffer(str: String): ChannelBuffer =
    ChannelBuffers.copiedBuffer(str, UTF_8)

  /** The finagle redis client is ChannelBuffer-based in keys and values
   *  RedisStores are polymophic in value types to support algebraic
   *  merging operations. A ChannelBuffered represents a bijection
   *  that can produce and extract a ChannelBuffer from said type.
   */
  trait ChannelBuffered[T] extends Bijection[ChannelBuffer, T]
}

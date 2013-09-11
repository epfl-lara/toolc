package toolc
package utils

abstract class Pipeline[-F, +T] {
  self =>

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F,G] {
    def run(ctx : Context)(v : F) : G = thenn.run(ctx)(self.run(ctx)(v))
  }

  def run(ctx: Context)(v: F): T
}

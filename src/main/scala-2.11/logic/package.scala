/**
  * Created by weijiayi on 16/10/2016.
  */
package object logic {
  def formula(f: Formula) = f

  implicit class symbol_tilde(a: Symbol){
    def ~>[B](b: B) = (FVar(a),b)
  }

  implicit def toNF(f: Formula): NormalForm = new NormalForm(f)
}
